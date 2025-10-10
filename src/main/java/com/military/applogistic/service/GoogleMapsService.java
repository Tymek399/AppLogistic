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
    private final MilitaryRoadPermissions militaryRoadPermissions;

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

            if (transportSet.getTotalWeightKg() <= LIGHT_VEHICLE_THRESHOLD_KG) {
                log.info("⚡ Pojazd lekki (≤5t) - pomijam walidację mostów");
                return createLightVehicleRoute(startAddress, endAddress, transportSet);
            }

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

            // ✅ NOWA WALIDACJA Z MARGINESAMI I POZWOLENIAMI
            List<InfrastructurePoint> blockedObjects = infrastructure.stream()
                    .filter(point -> !canPassInfrastructure(point, transportSet))
                    .collect(Collectors.toList());

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

    // ✅ ZAKTUALIZOWANA METODA - sprawdza czy można przejechać
    private boolean canPassInfrastructure(InfrastructurePoint point, TransportSet transportSet) {
        MilitaryLoadCalculator.BridgeSpecification spec =
                bridgeDataService.enrichBridgeData(point, transportSet);

        if (spec == null) {
            return true;
        }

        double transportWeightTons = transportSet.getTotalWeightKg() / 1000.0;
        double transportHeightM = transportSet.getTotalHeightCm() / 100.0;

        // ✅ NOWA WALIDACJA Z MARGINESAMI I UPRAWNIENIAMI
        MilitaryRoadPermissions.ValidationResult validation =
                militaryRoadPermissions.validatePassage(
                        transportWeightTons,
                        transportHeightM,
                        spec.getMaxWeight() != null ? spec.getMaxWeight().doubleValue() : null,
                        spec.getMaxHeight() != null ? spec.getMaxHeight().doubleValue() : null,
                        spec.getBridgeType()
                );

        // ✅ USTAW INFORMACJE O LOKALIZACJI W VALIDATION RESULT
        validation.setInfrastructureName(point.getName());
        validation.setCity(spec.getCity());
        validation.setRoadName(spec.getLocation());

        if (!validation.isCanPass()) {
            log.warn("❌ {} - {}", point.getName(), validation.getReason());
            return false;
        }

        if (validation.isRequiresPermit()) {
            log.info("⚠️ {} - {} (typ: {})",
                    point.getName(),
                    validation.getReason(),
                    validation.getPermitType()
            );
        } else {
            log.debug("✅ {} - przejazd OK", point.getName());
        }

        return true;
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

    // ✅ ZAKTUALIZOWANA METODA - combineAllValidations Z POZWOLENIAMI I MIASTAMI
    private Map<String, Object> combineAllValidations(
            Map<String, Object> googleRoute,
            List<InfrastructurePoint> osmInfrastructure,
            Map<String, Object> hereValidation,
            TransportSet transportSet) {

        Map<String, Object> combined = new HashMap<>(googleRoute);

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
        List<String> allPermits = new ArrayList<>(); // ✅ LISTA POZWOLEŃ
        List<Map<String, Object>> infrastructureDetails = new ArrayList<>();

        for (InfrastructurePoint point : osmInfrastructure) {
            Map<String, Object> detail = analyzeInfrastructurePoint(point, transportSet);
            infrastructureDetails.add(detail);

            MilitaryLoadCalculator.BridgeSpecification spec =
                    bridgeDataService.enrichBridgeData(point, transportSet);

            if (spec != null) {
                log.info("🔍 Sprawdzam: {} ({})", point.getName(), point.getType().getPolish());

                double transportWeightTons = transportSet.getTotalWeightKg() / 1000.0;
                double transportHeightM = transportSet.getTotalHeightCm() / 100.0;

                // ✅ WALIDACJA Z MARGINESAMI
                MilitaryRoadPermissions.ValidationResult validation =
                        militaryRoadPermissions.validatePassage(
                                transportWeightTons,
                                transportHeightM,
                                spec.getMaxWeight() != null ? spec.getMaxWeight().doubleValue() : null,
                                spec.getMaxHeight() != null ? spec.getMaxHeight().doubleValue() : null,
                                spec.getBridgeType()
                        );

                // ✅ USTAW INFORMACJE O LOKALIZACJI
                validation.setInfrastructureName(point.getName());
                validation.setCity(spec.getCity());
                validation.setRoadName(spec.getLocation());

                if (validation.isRequiresPermit()) {
                    // ✅ UŻYJ NOWEJ METODY Z MIASTEM
                    String permitMsg = militaryRoadPermissions.getPermitDescriptionWithLocation(validation);
                    allPermits.add(permitMsg);
                    log.info("⚠️ POZWOLENIE: {}", permitMsg);
                }

                if (!validation.isCanPass()) {
                    String violation = String.format("%s: %s",
                            point.getName(),
                            validation.getReason());
                    allViolations.add(violation);
                    log.warn("❌ NARUSZENIE: {}", violation);
                }
            }

            if (!(Boolean) detail.get("canPass")) {
                String violation = (String) detail.get("violation");
                if (violation != null && !allViolations.contains(violation)) {
                    allViolations.add(violation);
                }
            }

            List<String> pointWarnings = (List<String>) detail.get("warnings");
            if (pointWarnings != null && !pointWarnings.isEmpty()) {
                allWarnings.addAll(pointWarnings);
            }
        }

        log.info("╔═══════════════════════════════════════════════════════════");
        log.info("║ 📊 PODSUMOWANIE WALIDACJI TRASY");
        log.info("╠═══════════════════════════════════════════════════════════");
        log.info("║  Sprawdzono obiektów:     {}", osmInfrastructure.size());
        log.info("║  Naruszeń:                {}", allViolations.size());
        log.info("║  Pozwoleń wymaganych:     {}", allPermits.size());
        log.info("║  Ostrzeżeń:               {}", allWarnings.size());
        log.info("║  Trasa przejezdna:        {}", allViolations.isEmpty() ? "✅ TAK" : "❌ NIE");
        log.info("╚═══════════════════════════════════════════════════════════");

        combined.put("hasViolations", !allViolations.isEmpty());
        combined.put("hasRestrictions", !allRestrictions.isEmpty());
        combined.put("hasWarnings", !allWarnings.isEmpty());
        combined.put("requiresPermits", !allPermits.isEmpty()); // ✅ NOWE
        combined.put("violations", allViolations);
        combined.put("restrictions", allRestrictions);
        combined.put("warnings", allWarnings);
        combined.put("permits", allPermits); // ✅ NOWE - LISTA POZWOLEŃ Z MIASTAMI
        combined.put("infrastructureDetails", infrastructureDetails);
        combined.put("routeJustification", buildDetailedJustification(
                osmInfrastructure, transportSet, allViolations, allRestrictions, allWarnings, allPermits));
        combined.put("validation_source", "optimized_multi_source");
        combined.put("transportSet", createTransportSetInfo(transportSet));
        combined.put("infrastructureCount", osmInfrastructure.size());
        combined.put("routeAvailable", allViolations.isEmpty());

        return combined;
    }

    // ✅ ZAKTUALIZOWANA METODA - analyzeInfrastructurePoint
    private Map<String, Object> analyzeInfrastructurePoint(
            InfrastructurePoint point, TransportSet transportSet) {

        Map<String, Object> detail = new HashMap<>();
        detail.put("name", point.getName());
        detail.put("type", point.getType().getPolish());
        detail.put("lat", point.getLatitude());
        detail.put("lng", point.getLongitude());

        MilitaryLoadCalculator.BridgeSpecification spec =
                bridgeDataService.enrichBridgeData(point, transportSet);

        if (spec == null) {
            detail.put("canPass", true);
            detail.put("dataSource", "unknown");
            detail.put("warnings", List.of("Brak danych o ograniczeniach"));
            return detail;
        }

        // ✅ DODAJ MIASTO DO SZCZEGÓŁÓW
        if (spec.getCity() != null) {
            detail.put("city", spec.getCity());
        }
        if (spec.getLocation() != null) {
            detail.put("roadName", spec.getLocation());
        }

        double transportWeightTons = transportSet.getTotalWeightKg() / 1000.0;
        double transportHeightM = transportSet.getTotalHeightCm() / 100.0;

        // ✅ NOWA WALIDACJA Z MARGINESAMI
        MilitaryRoadPermissions.ValidationResult validation =
                militaryRoadPermissions.validatePassage(
                        transportWeightTons,
                        transportHeightM,
                        spec.getMaxWeight() != null ? spec.getMaxWeight().doubleValue() : null,
                        spec.getMaxHeight() != null ? spec.getMaxHeight().doubleValue() : null,
                        spec.getBridgeType()
                );

        // ✅ USTAW LOKALIZACJĘ
        validation.setInfrastructureName(point.getName());
        validation.setCity(spec.getCity());
        validation.setRoadName(spec.getLocation());

        List<String> warnings = new ArrayList<>();
        String violation = null;

        if (!validation.isCanPass()) {
            violation = validation.getReason();
        } else if (validation.isRequiresPermit()) {
            warnings.add("⚠️ " + militaryRoadPermissions.getPermitDescriptionWithLocation(validation));
            detail.put("requiresPermit", true);
            detail.put("permitType", validation.getPermitType());
        }

        if (spec.getMaxWeight() != null) {
            detail.put("maxWeightTons", spec.getMaxWeight().doubleValue());
        }

        if (spec.getMaxHeight() != null) {
            detail.put("maxHeightMeters", spec.getMaxHeight().doubleValue());
        }

        detail.put("canPass", validation.isCanPass());
        detail.put("violation", violation);
        detail.put("warnings", warnings);

        return detail;
    }

    // ✅ ZAKTUALIZOWANA METODA - buildDetailedJustification
    private String buildDetailedJustification(
            List<InfrastructurePoint> infrastructure,
            TransportSet transportSet,
            List<String> violations,
            List<String> restrictions,
            List<String> warnings,
            List<String> permits) {

        StringBuilder sb = new StringBuilder();

        sb.append("Analiza trasy dla pojazdu: ");
        sb.append(String.format("masa %.1ft, wysokość %.2fm\n\n",
                transportSet.getTotalWeightKg() / 1000.0,
                transportSet.getTotalHeightCm() / 100.0));

        if (violations.isEmpty()) {
            sb.append("✅ TRASA PRZEJEZDNA\n\n");

            if (!permits.isEmpty()) {
                sb.append("⚠️ WYMAGA POZWOLEŃ:\n");
                permits.forEach(p -> sb.append("  • ").append(p).append("\n"));
                sb.append("\n");
            }
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

    private Map<String, Object> createTransportSetInfo(TransportSet transportSet) {
        Map<String, Object> info = new HashMap<>();
        info.put("id", transportSet.getId());
        info.put("totalWeightKg", transportSet.getTotalWeightKg());
        info.put("totalHeightCm", transportSet.getTotalHeightCm());
        info.put("totalWeightTons", transportSet.getTotalWeightKg() / 1000.0);
        info.put("totalHeightMeters", transportSet.getTotalHeightCm() / 100.0);
        return info;
    }

    private String encodeAddress(String address) {
        try {
            return URLEncoder.encode(address, StandardCharsets.UTF_8.toString());
        } catch (Exception e) {
            log.error("Error encoding address: {}", address, e);
            return address;
        }
    }

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

    public int getMaxRouteAttempts() {
        return MAX_ROUTE_ATTEMPTS;
    }

    public double getDetourDistanceKm() {
        return DETOUR_DISTANCE_KM;
    }

    public int getLightVehicleThresholdKg() {
        return LIGHT_VEHICLE_THRESHOLD_KG;
    }
}