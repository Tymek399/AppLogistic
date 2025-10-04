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

    // Cache dla tras
    private final Map<String, Map<String, Object>> routeCache = new HashMap<>();

    public Map<String, Object> getRoute(String startAddress, String endAddress,
                                        TransportSet transportSet, Set<String> excludedBridges) {
        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            throw new RuntimeException("Google Maps API not configured");
        }

        try {
            log.info("🗺️ Pobieranie trasy: {} → {}", startAddress, endAddress);
            log.info("Parametry: masa={}kg, wysokość={}cm",
                    transportSet.getTotalWeightKg(), transportSet.getTotalHeightCm());

            // Cache
            String cacheKey = buildCacheKey(startAddress, endAddress, excludedBridges);
            if (routeCache.containsKey(cacheKey)) {
                log.info("✅ CACHE HIT - używam zapisanej trasy");
                return enrichCachedRoute(routeCache.get(cacheKey), transportSet);
            }

            // 1. Google Maps - JEDNA trasa
            Map<String, Object> googleResponse = performGoogleMapsApiCall(
                    startAddress, endAddress, excludedBridges);

            routeCache.put(cacheKey, googleResponse);
            if (routeCache.size() > 100) {
                routeCache.clear();
            }

            List<double[]> routeCoordinates = extractDetailedRouteCoordinates(googleResponse);

            // 2. OpenStreetMap - mosty/tunele (DARMOWE)
            log.info("🔍 Sprawdzanie infrastruktury przez OpenStreetMap...");
            List<InfrastructurePoint> infrastructure = overpassService.getInfrastructureAlongRoute(routeCoordinates);
            infrastructure = filterExcludedBridges(infrastructure, excludedBridges);

            log.info("✅ Po filtrowaniu pozostało {} obiektów", infrastructure.size());

            // 3. HERE Maps - JEDNO WYWOŁANIE dla całej trasy (tylko ciężkie pojazdy)
            Map<String, Object> hereValidation = null;
            if (transportSet.getTotalWeightKg() > 5000) {
                log.info("📡 Walidacja HERE Maps (pojazd ciężki)");
                hereValidation = hereMapsService.validateRouteRestrictions(
                        routeCoordinates.get(0)[0], routeCoordinates.get(0)[1],
                        routeCoordinates.get(routeCoordinates.size()-1)[0],
                        routeCoordinates.get(routeCoordinates.size()-1)[1],
                        transportSet
                );
            } else {
                log.info("⚡ Pojazd lekki (≤5t) - pomijam walidację HERE Maps");
            }

            Map<String, Object> enrichedRoute = combineAllValidations(
                    googleResponse, infrastructure, hereValidation, transportSet
            );

            log.info("✅ Trasa utworzona z walidacją");
            return enrichedRoute;

        } catch (Exception e) {
            log.error("❌ Błąd pobierania trasy", e);
            throw new RuntimeException("Failed to create route: " + e.getMessage(), e);
        }
    }

    private String buildCacheKey(String start, String end, Set<String> excluded) {
        return start + "|" + end + "|" + excluded.size();
    }

    private Map<String, Object> enrichCachedRoute(Map<String, Object> cachedRoute,
                                                  TransportSet transportSet) {
        Map<String, Object> enriched = new HashMap<>(cachedRoute);
        enriched.put("fromCache", true);
        enriched.put("transportSet", createTransportSetInfo(transportSet));
        return enriched;
    }

    private List<InfrastructurePoint> filterExcludedBridges(
            List<InfrastructurePoint> infrastructure,
            Set<String> excludedBridges) {

        if (excludedBridges.isEmpty()) {
            return infrastructure;
        }

        List<InfrastructurePoint> filtered = new ArrayList<>();
        for (InfrastructurePoint point : infrastructure) {
            boolean excluded = false;
            for (String excludedName : excludedBridges) {
                if (point.getName().contains(excludedName) || excludedName.contains(point.getName())) {
                    log.debug("🚫 Pomijam wykluczony most: {}", point.getName());
                    excluded = true;
                    break;
                }
            }
            if (!excluded) {
                filtered.add(point);
            }
        }

        return filtered;
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

                if (coordinates.isEmpty()) {
                    List<Map<String, Object>> legs = (List<Map<String, Object>>) route.get("legs");
                    if (legs != null && !legs.isEmpty()) {
                        for (Map<String, Object> leg : legs) {
                            List<Map<String, Object>> steps = (List<Map<String, Object>>) leg.get("steps");
                            if (steps != null) {
                                for (Map<String, Object> step : steps) {
                                    Map<String, Object> startLoc = (Map<String, Object>) step.get("start_location");
                                    if (startLoc != null) {
                                        coordinates.add(new double[]{
                                                ((Number) startLoc.get("lat")).doubleValue(),
                                                ((Number) startLoc.get("lng")).doubleValue()
                                        });
                                    }
                                }
                            }
                        }
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
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20);
            int dlat = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lat += dlat;

            shift = 0;
            result = 0;
            do {
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20);
            int dlng = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lng += dlng;

            path.add(new double[]{lat / 1E5, lng / 1E5});
        }

        return path;
    }

    private Map<String, Object> combineAllValidations(
            Map<String, Object> googleRoute,
            List<InfrastructurePoint> osmInfrastructure,
            Map<String, Object> hereValidation,
            TransportSet transportSet) {

        Map<String, Object> combined = new HashMap<>(googleRoute);

        List<String> allWarnings = new ArrayList<>();
        List<String> allRestrictions = new ArrayList<>();
        List<String> allViolations = new ArrayList<>();
        List<Map<String, Object>> infrastructureDetails = new ArrayList<>();

        log.info("📊 Znaleziono {} obiektów infrastruktury na trasie", osmInfrastructure.size());

        // Analiza każdego mostu/tunelu
        for (InfrastructurePoint point : osmInfrastructure) {
            Map<String, Object> detail = analyzeInfrastructurePoint(point, transportSet);
            infrastructureDetails.add(detail);

            if (!(Boolean) detail.get("canPass")) {
                allViolations.add((String) detail.get("violation"));
                allRestrictions.add(String.format("%s '%s' - FIZYCZNA BLOKADA",
                        point.getType().getPolish(), point.getName()));
            }

            List<String> pointWarnings = (List<String>) detail.get("warnings");
            if (pointWarnings != null) {
                allWarnings.addAll(pointWarnings);
            }
        }

        // HERE Maps validation
        if (hereValidation != null) {
            mergeValidationData(hereValidation, allWarnings, allRestrictions, allViolations);
        }

        List<String> routeJustification = buildDetailedJustification(
                osmInfrastructure, transportSet, allViolations, allRestrictions, allWarnings
        );

        combined.put("hasViolations", !allViolations.isEmpty());
        combined.put("hasRestrictions", !allRestrictions.isEmpty());
        combined.put("hasWarnings", !allWarnings.isEmpty());
        combined.put("violations", allViolations);
        combined.put("restrictions", allRestrictions);
        combined.put("warnings", allWarnings);
        combined.put("infrastructureDetails", infrastructureDetails);
        combined.put("routeJustification", routeJustification);
        combined.put("validation_source", "optimized_multi_source");
        combined.put("transportSet", createTransportSetInfo(transportSet));
        combined.put("infrastructureCount", osmInfrastructure.size());
        combined.put("routeAvailable", allViolations.isEmpty());

        return combined;
    }

    private Map<String, Object> analyzeInfrastructurePoint(InfrastructurePoint point, TransportSet transportSet) {
        Map<String, Object> detail = new HashMap<>();
        detail.put("source", "OpenStreetMap");
        detail.put("type", point.getType().getPolish());
        detail.put("name", point.getName());
        detail.put("location", String.format("%.6f, %.6f", point.getLatitude(), point.getLongitude()));
        detail.put("roadName", point.getRoadName());

        List<String> warnings = new ArrayList<>();
        String violation = null;
        boolean canPass = true;
        StringBuilder checkResult = new StringBuilder();

        // Wzbogacenie danych (HEURYSTYKA - nie API!)
        if (point.getMaxWeightTons() == null && point.getMaxHeightMeters() == null) {
            MilitaryLoadCalculator.BridgeSpecification enrichedSpec =
                    bridgeDataService.enrichBridgeData(point, transportSet);

            if (enrichedSpec != null) {
                point = updatePointWithEnrichedData(point, enrichedSpec);
            }
        }

        // NOŚNOŚĆ
        if (point.getMaxWeightTons() != null) {
            double transportWeight = transportSet.getTotalWeightKg() / 1000.0;
            detail.put("maxWeight_tons", point.getMaxWeightTons());
            detail.put("transportWeight_tons", transportWeight);

            double margin = point.getMaxWeightTons() - transportWeight;

            if (margin < 0) {
                if (Math.abs(margin) <= 15.0) {
                    warnings.add(String.format("%s - przekroczenie %.1ft - MOŻLIWE Z POZWOLENIEM",
                            point.getName(), Math.abs(margin)));
                } else {
                    canPass = false;
                    violation = String.format("%s - PRZEKROCZONA NOŚNOŚĆ: limit %.1ft, twój zestaw %.1ft",
                            point.getName(), point.getMaxWeightTons(), transportWeight);
                }
            } else if (margin < 10) {
                warnings.add(String.format("%s - margines nośności tylko %.1ft", point.getName(), margin));
            }
        }

        // WYSOKOŚĆ
        if (point.getMaxHeightMeters() != null) {
            double transportHeight = transportSet.getTotalHeightCm() / 100.0;
            detail.put("maxHeight_m", point.getMaxHeightMeters());
            detail.put("transportHeight_m", transportHeight);

            double margin = point.getMaxHeightMeters() - transportHeight;

            if (margin < 0) {
                canPass = false;
                if (violation == null) {
                    violation = String.format("%s - ZA NISKI PRZEJAZD: limit %.2fm, twój zestaw %.2fm",
                            point.getName(), point.getMaxHeightMeters(), transportHeight);
                }
            } else if (margin < 0.3) {
                warnings.add(String.format("%s - minimalny zapas wysokości %.0fcm", point.getName(), margin * 100));
            }
        }

        detail.put("checkResult", checkResult.toString());
        detail.put("canPass", canPass);
        detail.put("warnings", warnings);
        detail.put("violation", violation);

        return detail;
    }
    private InfrastructurePoint updatePointWithEnrichedData(
            InfrastructurePoint original,
            MilitaryLoadCalculator.BridgeSpecification enriched) {

        return new InfrastructurePoint(
                original.getType(),
                original.getName(),
                original.getLatitude(),
                original.getLongitude(),
                enriched.getMaxWeight() != null ? enriched.getMaxWeight().doubleValue() : original.getMaxWeightTons(),
                enriched.getMaxHeight() != null ? enriched.getMaxHeight().doubleValue() : original.getMaxHeightMeters(),
                original.getRoadName(),
                original.getTags()
        );
    }

    private void mergeValidationData(Map<String, Object> validation,
                                     List<String> allWarnings,
                                     List<String> allRestrictions,
                                     List<String> allViolations) {
        if (validation.get("warnings") instanceof List) {
            allWarnings.addAll((List<String>) validation.get("warnings"));
        }
        if (validation.get("restrictions") instanceof List) {
            allRestrictions.addAll((List<String>) validation.get("restrictions"));
        }
        if (validation.get("violations") instanceof List) {
            allViolations.addAll((List<String>) validation.get("violations"));
        }
    }

    private List<String> buildDetailedJustification(
            List<InfrastructurePoint> infrastructure,
            TransportSet transportSet,
            List<String> violations,
            List<String> restrictions,
            List<String> warnings) {

        List<String> justification = new ArrayList<>();

        justification.add("╔════════════════════════════════════════════╗");
        justification.add("    ANALIZA INFRASTRUKTURY NA TRASIE");
        justification.add("╚════════════════════════════════════════════╝");
        justification.add("");

        justification.add("PARAMETRY ZESTAWU TRANSPORTOWEGO:");
        justification.add("────────────────────────────────────────────");
        justification.add(String.format("Opis: %s", transportSet.getDescription()));
        justification.add(String.format("Ciężarówka: %s", transportSet.getTransporter().getModel()));
        justification.add(String.format("Ładunek: %s", transportSet.getCargo().getModel()));
        justification.add("");
        justification.add("Parametry techniczne:");

        // Szczegóły wagi
        if (Boolean.TRUE.equals(transportSet.getCargo().getCanDriveAlone())) {
            justification.add(String.format("  • Masa pojazdu: %.1f ton (samojezdny)",
                    transportSet.getTotalWeightKg() / 1000.0));
        } else {
            int tractorWeight = (int) (transportSet.getTransporter().getTotalWeightKg() * 0.4);
            int trailerWeight = estimateSemiTrailerWeight(transportSet.getCargo().getTotalWeightKg());
            int cargoWeight = transportSet.getCargo().getTotalWeightKg();

            justification.add(String.format("  • Masa całkowita: %.1f ton", transportSet.getTotalWeightKg() / 1000.0));
            justification.add(String.format("    - Ciągnik: %.1f ton", tractorWeight / 1000.0));
            justification.add(String.format("    - Naczepa: %.1f ton", trailerWeight / 1000.0));
            justification.add(String.format("    - Ładunek: %.1f ton", cargoWeight / 1000.0));
        }

        justification.add(String.format("  • Wysokość: %.2f m", transportSet.getTotalHeightCm() / 100.0));

        if (transportSet.getTrailerHeightCm() != null && transportSet.getTrailerHeightCm() > 0) {
            justification.add(String.format("    (naczepa %.2f m + ładunek %.2f m)",
                    transportSet.getTrailerHeightCm() / 100.0,
                    transportSet.getCargo().getHeightCm() / 100.0));
        } else {
            justification.add("    (pojazd samojezdny)");
        }

        justification.add(String.format("  • Długość: %.2f m", transportSet.getTotalLengthCm() / 100.0));
        justification.add(String.format("  • Szerokość: %.2f m", transportSet.getTotalWidthCm() / 100.0));
        justification.add("");

        justification.add("SPRAWDZONE OBIEKTY INFRASTRUKTURY:");
        justification.add("────────────────────────────────────────────");

        if (infrastructure.isEmpty()) {
            justification.add("Brak danych o mostach/tunelach na tej trasie");
        } else {
            justification.add(String.format("Znaleziono %d obiektów:", infrastructure.size()));
            justification.add("");

            int bridgeCount = 0;
            int tunnelCount = 0;

            for (InfrastructurePoint point : infrastructure) {
                String icon = point.getType() == OverpassService.InfrastructureType.BRIDGE ? "🌉" :
                        point.getType() == OverpassService.InfrastructureType.TUNNEL ? "🚇" : "⚠️";

                if (point.getType() == OverpassService.InfrastructureType.BRIDGE) bridgeCount++;
                if (point.getType() == OverpassService.InfrastructureType.TUNNEL) tunnelCount++;

                justification.add(String.format("%s %s: %s", icon, point.getType().getPolish(), point.getName()));

                if (point.getRoadName() != null) {
                    justification.add(String.format("   Droga: %s", point.getRoadName()));
                }

                boolean canPass = point.canHandle(transportSet);

                if (point.getMaxWeightTons() != null) {
                    double margin = point.getMaxWeightTons() - (transportSet.getTotalWeightKg() / 1000.0);
                    if (margin < 0) {
                        justification.add(String.format("   ❌ NOŚNOŚĆ: limit %.1ft < twój zestaw %.1ft",
                                point.getMaxWeightTons(), transportSet.getTotalWeightKg() / 1000.0));
                    } else {
                        justification.add(String.format("   ✅ NOŚNOŚĆ: limit %.1ft, twój zestaw %.1ft (zapas %.1ft)",
                                point.getMaxWeightTons(), transportSet.getTotalWeightKg() / 1000.0, margin));
                    }
                } else {
                    justification.add("   ⚠️ Brak danych o nośności (użyto heurystyki)");
                }

                if (point.getMaxHeightMeters() != null) {
                    double margin = point.getMaxHeightMeters() - (transportSet.getTotalHeightCm() / 100.0);
                    if (margin < 0) {
                        justification.add(String.format("   ❌ WYSOKOŚĆ: limit %.2fm < twój zestaw %.2fm",
                                point.getMaxHeightMeters(), transportSet.getTotalHeightCm() / 100.0));
                    } else {
                        justification.add(String.format("   ✅ WYSOKOŚĆ: limit %.2fm, twój zestaw %.2fm (zapas %.0fcm)",
                                point.getMaxHeightMeters(), transportSet.getTotalHeightCm() / 100.0, margin * 100));
                    }
                } else {
                    justification.add("   ⚠️ Brak danych o wysokości (użyto heurystyki)");
                }

                justification.add("");
            }

            justification.add("────────────────────────────────────────────");
            justification.add(String.format("STATYSTYKI: %d mostów, %d tuneli", bridgeCount, tunnelCount));
        }

        justification.add("");
        justification.add("WYNIK ANALIZY TRASY:");
        justification.add("────────────────────────────────────────────");

        if (violations.isEmpty() && restrictions.isEmpty()) {
            justification.add("✅ TRASA ZATWIERDZONA");
            justification.add("   Wszystkie obiekty mogą być bezpiecznie pokonane");
        } else if (!violations.isEmpty()) {
            justification.add("❌ TRASA ZABLOKOWANA");
            violations.forEach(v -> justification.add("   • " + v));
        } else if (!restrictions.isEmpty()) {
            justification.add("⚠️ TRASA Z OGRANICZENIAMI");
            restrictions.forEach(r -> justification.add("   • " + r));
        }

        if (!warnings.isEmpty()) {
            justification.add("");
            justification.add("DODATKOWE OSTRZEŻENIA:");
            warnings.forEach(w -> justification.add("   • " + w));
        }

        justification.add("");
        justification.add("╚════════════════════════════════════════════╝");
        justification.add("  Źródła: OpenStreetMap + Heurystyka (bez wielokrotnych API)");
        justification.add("  Data: " + new Date());
        justification.add("╚════════════════════════════════════════════╝");

        return justification;
    }

    // Helper do obliczenia wagi naczepy
    private int estimateSemiTrailerWeight(int cargoWeight) {
        if (cargoWeight > 50000) return 15000;
        if (cargoWeight > 30000) return 12000;
        if (cargoWeight > 15000) return 10000;
        return 8000;
    }

    private Map<String, Object> createTransportSetInfo(TransportSet transportSet) {
        Map<String, Object> info = new HashMap<>();
        info.put("id", transportSet.getId());
        info.put("description", transportSet.getDescription());
        info.put("totalWeight_kg", transportSet.getTotalWeightKg());
        info.put("totalHeight_cm", transportSet.getTotalHeightCm());
        info.put("trailerHeight_cm", transportSet.getTrailerHeightCm());
        info.put("cargoHeight_cm", transportSet.getCargo().getHeightCm());
        info.put("totalLength_cm", transportSet.getTotalLengthCm());
        info.put("totalWidth_cm", transportSet.getTotalWidthCm());
        info.put("maxAxleLoad_kg", transportSet.getMaxAxleLoadKg());
        info.put("transporterModel", transportSet.getTransporter().getModel());
        info.put("cargoModel", transportSet.getCargo().getModel());
        info.put("isLowLoader", transportSet.isLowLoader());
        info.put("trailerType", transportSet.getTrailerType());
        info.put("canDriveAlone", transportSet.getCargo().getCanDriveAlone());
        return info;
    }

    private String encodeAddress(String address) {
        try {
            return URLEncoder.encode(address, StandardCharsets.UTF_8.toString());
        } catch (Exception e) {
            return address.replace(" ", "+").replace(",", "%2C");
        }
    }

    public boolean isConfigured() {
        return apiKeysConfig.isGoogleMapsEnabled();
    }
}

