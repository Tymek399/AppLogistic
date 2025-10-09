package com.military.applogistic.service;

import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.OverpassService.InfrastructurePoint;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.*;
import java.util.stream.Collectors;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

@Service
@RequiredArgsConstructor
@Slf4j
public class GoogleMapsService {

    private final ApiKeysConfig apiKeysConfig;
    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper = new ObjectMapper();
    private final HereMapsService hereMapsService;
    private final OverpassService overpassService;
    private final BridgeDataService bridgeDataService;

    private static final int LIGHT_VEHICLE_THRESHOLD_KG = 5000;
    private static final double DETOUR_DISTANCE_KM = 2.0;
    private static final int MAX_ROUTE_ATTEMPTS = 10;

    public Map<String, Object> getRoute(String startAddress, String endAddress,
                                        TransportSet transportSet, Set<String> excludedBridges) {
        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            throw new RuntimeException("Google Maps API not configured");
        }

        try {
            log.info("🗺️ Pobieranie trasy: {} → {}", startAddress, endAddress);
            log.info("Parametry: masa={}kg, wysokość={}cm",
                    transportSet.getTotalWeightKg(), transportSet.getTotalHeightCm());

            // ✅ LEKKIE POJAZDY: Pomiń walidację
            if (transportSet.getTotalWeightKg() <= LIGHT_VEHICLE_THRESHOLD_KG) {
                log.info("⚡ Pojazd lekki (≤5t) - pomijam walidację mostów");
                return createLightVehicleRoute(startAddress, endAddress, transportSet);
            }

            // ✅ CIĘŻKIE POJAZDY: Pełna walidacja z omijaniem
            return createHeavyVehicleRouteWithAvoidance(startAddress, endAddress, transportSet, excludedBridges);

        } catch (Exception e) {
            log.error("❌ Błąd pobierania trasy", e);
            throw new RuntimeException("Failed to create route: " + e.getMessage(), e);
        }
    }

    private Map<String, Object> createLightVehicleRoute(
            String startAddress, String endAddress, TransportSet transportSet) {

        Map<String, Object> googleResponse = performGoogleMapsApiCall(startAddress, endAddress, new HashSet<>());

        googleResponse.put("lightVehicle", true);
        googleResponse.put("validationSkipped", true);
        googleResponse.put("reason", "Pojazd ≤5t - walidacja mostów pominięta");
        googleResponse.put("searchAttempts", 1);
        googleResponse.put("successfulAttempt", 1);
        googleResponse.put("routeSource", "GOOGLE_MAPS");
        googleResponse.put("hasViolations", false);
        googleResponse.put("hasRestrictions", false);
        googleResponse.put("hasWarnings", false);
        googleResponse.put("transportSet", createTransportSetInfo(transportSet));

        return googleResponse;
    }

    private Map<String, Object> createHeavyVehicleRouteWithAvoidance(
            String startAddress, String endAddress,
            TransportSet transportSet, Set<String> excludedBridges) {

        Set<String> allExcluded = new HashSet<>(excludedBridges);
        List<Map<String, Object>> attemptReports = new ArrayList<>();

        for (int attempt = 1; attempt <= MAX_ROUTE_ATTEMPTS; attempt++) {
            log.info("🔄 Próba #{} - wykluczonych obiektów: {}", attempt, allExcluded.size());

            Map<String, Object> googleResponse = performGoogleMapsApiCall(startAddress, endAddress, allExcluded);
            List<double[]> routeCoordinates = extractDetailedRouteCoordinates(googleResponse);

            List<InfrastructurePoint> infrastructure = overpassService.getInfrastructureAlongRoute(routeCoordinates);
            infrastructure = filterExcludedBridges(infrastructure, allExcluded);

            List<InfrastructurePoint> blockedObjects = infrastructure.stream()
                    .filter(point -> !canPassInfrastructure(point, transportSet))
                    .collect(Collectors.toList());

            // Raport próby
            Map<String, Object> attemptReport = new HashMap<>();
            attemptReport.put("attemptNumber", attempt);
            attemptReport.put("excludedCount", allExcluded.size());
            attemptReport.put("infrastructureChecked", infrastructure.size());
            attemptReport.put("blockedObjects", blockedObjects.size());
            attemptReports.add(attemptReport);

            if (blockedObjects.isEmpty()) {
                log.info("✅ Znaleziono przejezdną trasę w próbie #{}", attempt);

                Map<String, Object> hereValidation = hereMapsService.validateRoute(
                        routeCoordinates.get(0)[0], routeCoordinates.get(0)[1],
                        routeCoordinates.get(routeCoordinates.size()-1)[0],
                        routeCoordinates.get(routeCoordinates.size()-1)[1],
                        transportSet.getTotalWeightKg(),
                        transportSet.getTotalHeightCm(),
                        allExcluded
                );

                Map<String, Object> result = combineAllValidations(
                        googleResponse, infrastructure, hereValidation, transportSet
                );
                result.put("searchAttempts", attempt);
                result.put("attemptReports", attemptReports);

                return result;
            }

            for (InfrastructurePoint blocked : blockedObjects) {
                allExcluded.add(blocked.getName());
                log.warn("🚫 Wykluczam: {} (masa: {}t, wysokość: {}m)",
                        blocked.getName(),
                        blocked.getMaxWeightTons(),
                        blocked.getMaxHeightMeters());
            }
        }

        throw new RuntimeException("❌ Nie znaleziono przejezdnej trasy po " + MAX_ROUTE_ATTEMPTS + " próbach. " +
                "Zablokowane obiekty: " + String.join(", ", allExcluded));
    }

    private boolean canPassInfrastructure(InfrastructurePoint point, TransportSet transportSet) {
        MilitaryLoadCalculator.BridgeSpecification spec =
                bridgeDataService.enrichBridgeData(point, transportSet);

        if (spec == null) {
            return true;
        }

        double transportWeightTons = transportSet.getTotalWeightKg() / 1000.0;
        double transportHeightM = transportSet.getTotalHeightCm() / 100.0;

        boolean weightOk = spec.getMaxWeight() == null ||
                transportWeightTons <= spec.getMaxWeight().doubleValue();

        boolean heightOk = spec.getMaxHeight() == null ||
                transportHeightM <= spec.getMaxHeight().doubleValue();

        if (!weightOk) {
            log.warn("⚠️ {} - za ciężki: {}t > {}t",
                    point.getName(), transportWeightTons, spec.getMaxWeight());
        }

        if (!heightOk) {
            log.warn("⚠️ {} - za wysoki: {}m > {}m",
                    point.getName(), transportHeightM, spec.getMaxHeight());
        }

        return weightOk && heightOk;
    }

    private List<InfrastructurePoint> filterExcludedBridges(
            List<InfrastructurePoint> infrastructure,
            Set<String> excludedBridges) {

        if (excludedBridges.isEmpty()) {
            return infrastructure;
        }

        return infrastructure.stream()
                .filter(point -> {
                    for (String excluded : excludedBridges) {
                        if (point.getName().toLowerCase().contains(excluded.toLowerCase()) ||
                                excluded.toLowerCase().contains(point.getName().toLowerCase())) {
                            return false;
                        }
                    }
                    return true;
                })
                .collect(Collectors.toList());
    }

    private Map<String, Object> performGoogleMapsApiCall(
            String startAddress, String endAddress, Set<String> excludedBridges) {

        String url = String.format(
                "%s/directions/json?origin=%s&destination=%s&key=%s&mode=driving&alternatives=true&language=pl&avoid=tolls&region=pl",
                apiKeysConfig.getGoogleMaps().getBaseUrl(),
                encodeAddress(startAddress),
                encodeAddress(endAddress),
                apiKeysConfig.getGoogleMaps().getKey()
        );

        log.info("📡 Calling Google Maps Directions API");

        try {
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response == null) {
                throw new RuntimeException("Empty response from Google Maps API");
            }

            String status = (String) response.get("status");
            if (!"OK".equals(status)) {
                String errorMessage = (String) response.get("error_message");
                throw new RuntimeException("Google Maps API error: " + status +
                        (errorMessage != null ? " - " + errorMessage : ""));
            }

            log.info("✅ Google Maps route retrieved successfully");
            return response;

        } catch (Exception e) {
            log.error("❌ Google Maps API call failed", e);
            throw new RuntimeException("Google Maps API call failed: " + e.getMessage(), e);
        }
    }

    private List<double[]> extractDetailedRouteCoordinates(Map<String, Object> googleRoute) {
        List<double[]> coordinates = new ArrayList<>();

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) googleRoute.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> route = routes.get(0);
                Map<String, Object> overviewPolyline = (Map<String, Object>) route.get("overview_polyline");

                if (overviewPolyline != null) {
                    String encodedPolyline = (String) overviewPolyline.get("points");
                    if (encodedPolyline != null) {
                        coordinates = decodePolyline(encodedPolyline);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error extracting coordinates: {}", e.getMessage());
        }

        return coordinates;
    }

    private List<double[]> decodePolyline(String encoded) {
        List<double[]> path = new ArrayList<>();
        int index = 0;
        int len = encoded.length();
        int lat = 0;
        int lng = 0;

        while (index < len) {
            int b;
            int shift = 0;
            int result = 0;
            do {
                if (index >= len) break;
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20 && index < len);

            int dlat = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lat += dlat;

            shift = 0;
            result = 0;
            do {
                if (index >= len) break;
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20 && index < len);

            int dlng = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lng += dlng;

            path.add(new double[]{lat / 1E5, lng / 1E5});
        }

        return path;
    }

    // ✅ KLUCZOWA METODA - combineAllValidations

    private Map<String, Object> combineAllValidations(
            Map<String, Object> googleRoute,
            List<InfrastructurePoint> osmInfrastructure,
            Map<String, Object> hereValidation,
            TransportSet transportSet) {

        Map<String, Object> combined = new HashMap<>(googleRoute);

        // ✅ SPRAWDŹ CZY HERE ZABLOKOWAŁ TRASĘ
        if (hereValidation != null && Boolean.TRUE.equals(hereValidation.get("routeBlocked"))) {
            log.error("🚨 HERE Maps zablokował trasę: {}", hereValidation.get("blockReason"));

            combined.put("routeBlocked", true);
            combined.put("blockReason", hereValidation.get("blockReason"));
            combined.put("hasViolations", true);
            combined.put("violations", List.of(hereValidation.get("blockReason")));
            combined.put("hasWarnings", false);
            combined.put("hasRestrictions", false);
            combined.put("routeAvailable", false);

            return combined;
        }

        List<String> allWarnings = new ArrayList<>();
        List<String> allRestrictions = new ArrayList<>();
        List<String> allViolations = new ArrayList<>();
        List<Map<String, Object>> infrastructureDetails = new ArrayList<>();

        // ✅ POPRAWIONA ANALIZA - RZECZYWISTA WALIDACJA
        for (InfrastructurePoint point : osmInfrastructure) {
            Map<String, Object> detail = analyzeInfrastructurePoint(point, transportSet);
            infrastructureDetails.add(detail);

            // ✅ LOGUJ KAŻDY OBIEKT INFRASTRUKTURY
            MilitaryLoadCalculator.BridgeSpecification spec =
                    bridgeDataService.enrichBridgeData(point, transportSet);

            if (spec != null) {
                log.info("📍 Sprawdzam: {} ({})", point.getName(), point.getType().getPolish());

                if (spec.getMaxWeight() != null) {
                    double transportWeight = transportSet.getTotalWeightKg() / 1000.0;
                    double bridgeWeight = spec.getMaxWeight().doubleValue();
                    log.info("   ⚖️ Nośność: transport {}t vs limit {}t → {}",
                            transportWeight, bridgeWeight,
                            transportWeight <= bridgeWeight ? "✅ OK" : "❌ PRZEKROCZENIE");
                }

                if (spec.getMaxHeight() != null) {
                    double transportHeight = transportSet.getTotalHeightCm() / 100.0;
                    double bridgeHeight = spec.getMaxHeight().doubleValue();
                    log.info("   📏 Wysokość: transport {}m vs limit {}m → {}",
                            transportHeight, bridgeHeight,
                            transportHeight <= bridgeHeight ? "✅ OK" : "❌ PRZEKROCZENIE");
                }
            }

            // Dodaj naruszenia
            if (!(Boolean) detail.get("canPass")) {
                String violation = (String) detail.get("violation");
                if (violation != null) {
                    allViolations.add(violation);
                    log.warn("⚠️ NARUSZENIE: {}", violation);
                }
            }

            // Dodaj ostrzeżenia
            List<String> pointWarnings = (List<String>) detail.get("warnings");
            if (pointWarnings != null && !pointWarnings.isEmpty()) {
                allWarnings.addAll(pointWarnings);
                pointWarnings.forEach(w -> log.info("   ⚠️ {}", w));
            }
        }

        // ✅ STATYSTYKI WALIDACJI
        log.info("╔═════════════════════════════════════════════════════════");
        log.info("║ 📊 PODSUMOWANIE WALIDACJI TRASY");
        log.info("╠═════════════════════════════════════════════════════════");
        log.info("║  Sprawdzono obiektów:     {}", osmInfrastructure.size());
        log.info("║  Naruszeń:                {}", allViolations.size());
        log.info("║  Ostrzeżeń:               {}", allWarnings.size());
        log.info("║  Trasa przejezdna:        {}", allViolations.isEmpty() ? "✅ TAK" : "❌ NIE");
        log.info("╚═════════════════════════════════════════════════════════");

        combined.put("hasViolations", !allViolations.isEmpty());
        combined.put("hasRestrictions", !allRestrictions.isEmpty());
        combined.put("hasWarnings", !allWarnings.isEmpty());
        combined.put("violations", allViolations);
        combined.put("restrictions", allRestrictions);
        combined.put("warnings", allWarnings);
        combined.put("infrastructureDetails", infrastructureDetails);
        combined.put("routeJustification", buildDetailedJustification(
                osmInfrastructure, transportSet, allViolations, allRestrictions, allWarnings));
        combined.put("validation_source", "optimized_multi_source");
        combined.put("transportSet", createTransportSetInfo(transportSet));
        combined.put("infrastructureCount", osmInfrastructure.size());
        combined.put("routeAvailable", allViolations.isEmpty());

        return combined;
    }// ✅ ANALIZA POJEDYNCZEGO OBIEKTU INFRASTRUKTURY
    private Map<String, Object> analyzeInfrastructurePoint(
            InfrastructurePoint point, TransportSet transportSet) {

        Map<String, Object> detail = new HashMap<>();
        detail.put("name", point.getName());
        detail.put("type", point.getType().getPolish());
        detail.put("lat", point.getLatitude());  // ✅ POPRAWIONE
        detail.put("lng", point.getLongitude()); // ✅ POPRAWIONE

        MilitaryLoadCalculator.BridgeSpecification spec =
                bridgeDataService.enrichBridgeData(point, transportSet);

        if (spec == null) {
            detail.put("canPass", true);
            detail.put("dataSource", "unknown");
            detail.put("warnings", List.of("Brak danych o ograniczeniach"));
            return detail;
        }

        double transportWeightTons = transportSet.getTotalWeightKg() / 1000.0;
        double transportHeightM = transportSet.getTotalHeightCm() / 100.0;

        List<String> warnings = new ArrayList<>();
        String violation = null;
        boolean canPass = true;

        // ✅ WALIDACJA MASY
        if (spec.getMaxWeight() != null) {
            double limitWeight = spec.getMaxWeight().doubleValue();
            detail.put("maxWeightTons", limitWeight);

            if (transportWeightTons > limitWeight) {
                violation = String.format(
                        "%s: Przekroczenie nośności (%.1ft > %.1ft)",
                        point.getName(), transportWeightTons, limitWeight
                );
                canPass = false;
            } else {
                double margin = ((limitWeight - transportWeightTons) / limitWeight) * 100;
                if (margin < 20) {
                    warnings.add(String.format(
                            "Niska rezerwa nośności: %.1f%%", margin
                    ));
                }
            }
        } else {
            warnings.add("Brak danych o nośności");
        }

        // ✅ WALIDACJA WYSOKOŚCI
        if (spec.getMaxHeight() != null) {
            double limitHeight = spec.getMaxHeight().doubleValue();
            detail.put("maxHeightMeters", limitHeight);

            if (transportHeightM > limitHeight) {
                violation = String.format(
                        "%s: Przekroczenie wysokości (%.2fm > %.2fm)",
                        point.getName(), transportHeightM, limitHeight
                );
                canPass = false;
            } else {
                double margin = limitHeight - transportHeightM;
                if (margin < 0.5) {
                    warnings.add(String.format(
                            "Mały prześwit: %.2fm", margin
                    ));
                }
            }
        } else {
            warnings.add("Brak danych o wysokości");
        }

        detail.put("canPass", canPass);
        detail.put("violation", violation);
        detail.put("warnings", warnings);
        // ✅ USUNIĘTE nieistniejące metody getDataSource() i getLoadClass()

        return detail;
    }

    // ✅ BUDOWANIE SZCZEGÓŁOWEGO UZASADNIENIA
    private String buildDetailedJustification(
            List<InfrastructurePoint> infrastructure,
            TransportSet transportSet,
            List<String> violations,
            List<String> restrictions,
            List<String> warnings) {

        StringBuilder sb = new StringBuilder();

        sb.append("Analiza trasy dla pojazdu: ");
        sb.append(String.format("masa %.1ft, wysokość %.2fm\n\n",
                transportSet.getTotalWeightKg() / 1000.0,
                transportSet.getTotalHeightCm() / 100.0));

        if (violations.isEmpty()) {
            sb.append("✅ TRASA PRZEJEZDNA\n\n");
        } else {
            sb.append("❌ TRASA ZABLOKOWANA\n\n");
            sb.append("Naruszenia:\n");
            violations.forEach(v -> sb.append("  • ").append(v).append("\n"));
            sb.append("\n");
        }

        if (!infrastructure.isEmpty()) {
            sb.append(String.format("Sprawdzono %d obiektów infrastruktury:\n", infrastructure.size()));

            Map<String, Long> typeCounts = infrastructure.stream()
                    .collect(Collectors.groupingBy(
                            p -> p.getType().getPolish(),
                            Collectors.counting()
                    ));

            typeCounts.forEach((type, count) ->
                    sb.append(String.format("  • %s: %d\n", type, count))
            );
            sb.append("\n");
        }

        if (!warnings.isEmpty()) {
            sb.append("Ostrzeżenia:\n");
            warnings.forEach(w -> sb.append("  ⚠️ ").append(w).append("\n"));
        }

        return sb.toString();
    }

    // ✅ INFORMACJE O TRANSPORCIE
    private Map<String, Object> createTransportSetInfo(TransportSet transportSet) {
        Map<String, Object> info = new HashMap<>();
        info.put("id", transportSet.getId());
        // ✅ USUNIĘTE nieistniejące pola name i vehicles
        info.put("totalWeightKg", transportSet.getTotalWeightKg());
        info.put("totalHeightCm", transportSet.getTotalHeightCm());
        info.put("totalWeightTons", transportSet.getTotalWeightKg() / 1000.0);
        info.put("totalHeightMeters", transportSet.getTotalHeightCm() / 100.0);
        return info;
    }

    // ✅ KODOWANIE ADRESÓW
    private String encodeAddress(String address) {
        try {
            return URLEncoder.encode(address, StandardCharsets.UTF_8.toString());
        } catch (Exception e) {
            log.error("Error encoding address: {}", address, e);
            return address;
        }
    }

    // ✅ GEOCODING - Adres → Współrzędne
    public Map<String, Object> geocode(String address) {
        String url = String.format(
                "%s/geocode/json?address=%s&key=%s&region=pl&language=pl",
                apiKeysConfig.getGoogleMaps().getBaseUrl(),
                encodeAddress(address),
                apiKeysConfig.getGoogleMaps().getKey()
        );

        log.info("🔍 Geocoding: {}", address);

        try {
            @SuppressWarnings("unchecked")
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response == null || !"OK".equals(response.get("status"))) {
                log.error("❌ Geocoding failed for: {}", address);
                return Collections.emptyMap();
            }

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> results = (List<Map<String, Object>>) response.get("results");
            if (results == null || results.isEmpty()) {
                log.error("❌ No geocoding results for: {}", address);
                return Collections.emptyMap();
            }

            Map<String, Object> result = results.get(0);
            @SuppressWarnings("unchecked")
            Map<String, Object> geometry = (Map<String, Object>) result.get("geometry");
            @SuppressWarnings("unchecked")
            Map<String, Object> location = (Map<String, Object>) geometry.get("location");

            Map<String, Object> geocoded = new HashMap<>();
            geocoded.put("lat", location.get("lat"));
            geocoded.put("lng", location.get("lng"));
            geocoded.put("formatted_address", result.get("formatted_address"));

            log.info("✅ Geocoded: {} → [{}, {}]",
                    address, location.get("lat"), location.get("lng"));

            return geocoded;

        } catch (Exception e) {
            log.error("❌ Geocoding error for: {}", address, e);
            return Collections.emptyMap();
        }
    }

    // ✅ REVERSE GEOCODING - Współrzędne → Adres
    public String reverseGeocode(double lat, double lng) {
        String url = String.format(
                "%s/geocode/json?latlng=%f,%f&key=%s&language=pl",
                apiKeysConfig.getGoogleMaps().getBaseUrl(),
                lat, lng,
                apiKeysConfig.getGoogleMaps().getKey()
        );

        try {
            @SuppressWarnings("unchecked")
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response == null || !"OK".equals(response.get("status"))) {
                return String.format("%.6f, %.6f", lat, lng);
            }

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> results = (List<Map<String, Object>>) response.get("results");
            if (results != null && !results.isEmpty()) {
                return (String) results.get(0).get("formatted_address");
            }

            return String.format("%.6f, %.6f", lat, lng);

        } catch (Exception e) {
            log.error("❌ Reverse geocoding error", e);
            return String.format("%.6f, %.6f", lat, lng);
        }
    }

    // ✅ OBLICZANIE ODLEGŁOŚCI HAVERSINE
    private double calculateDistance(double lat1, double lng1, double lat2, double lng2) {
        final int R = 6371; // Promień Ziemi w km

        double latDistance = Math.toRadians(lat2 - lat1);
        double lngDistance = Math.toRadians(lng2 - lng1);

        double a = Math.sin(latDistance / 2) * Math.sin(latDistance / 2)
                + Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2))
                * Math.sin(lngDistance / 2) * Math.sin(lngDistance / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return R * c;
    }

    // ✅ WALIDACJA POJEDYNCZEGO PUNKTU TRASY
    private boolean isPointPassable(double[] point, TransportSet transportSet) {
        List<InfrastructurePoint> nearby = overpassService.getInfrastructureAlongRoute(
                List.of(point)
        );

        for (InfrastructurePoint infra : nearby) {
            if (!canPassInfrastructure(infra, transportSet)) {
                return false;
            }
        }

        return true;
    }

    // ✅ FORMATOWANIE CZASU TRWANIA
    private String formatDuration(int seconds) {
        int hours = seconds / 3600;
        int minutes = (seconds % 3600) / 60;

        if (hours > 0) {
            return String.format("%dh %dmin", hours, minutes);
        } else {
            return String.format("%dmin", minutes);
        }
    }

    // ✅ FORMATOWANIE DYSTANSU
    private String formatDistance(int meters) {
        if (meters >= 1000) {
            return String.format("%.1f km", meters / 1000.0);
        } else {
            return String.format("%d m", meters);
        }
    }

    // ✅ PUBLICZNE GETTERY DO TESTÓW
    public int getMaxRouteAttempts() {
        return MAX_ROUTE_ATTEMPTS;
    }

    public double getDetourDistanceKm() {
        return DETOUR_DISTANCE_KM;
    }

    public int getLightVehicleThresholdKg() {
        return LIGHT_VEHICLE_THRESHOLD_KG;
    }
}  // ✅ KONIEC KLASY GoogleMapsService