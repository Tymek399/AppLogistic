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
    private final TomTomService tomTomService;
    private final BridgeDataService bridgeDataService;
    private final MilitaryLoadCalculator loadCalculator;

    // CACHE dla tras - oszczędność wywołań API
    private final Map<String, Map<String, Object>> routeCache = new HashMap<>();

    public Map<String, Object> getRoute(String startAddress, String endAddress,
                                        TransportSet transportSet, Set<String> excludedBridges) {
        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            throw new RuntimeException("Google Maps API not configured");
        }

        try {
            log.info("Pobieranie trasy z {} do {}", startAddress, endAddress);

            // CACHE KEY
            String cacheKey = buildCacheKey(startAddress, endAddress, excludedBridges);

            // Sprawdź cache
            if (routeCache.containsKey(cacheKey)) {
                log.info("CACHE HIT - używam zapisanej trasy");
                Map<String, Object> cachedRoute = routeCache.get(cacheKey);

                // Nadal wykonaj walidację (lekka operacja bez ponownego pobierania trasy)
                return enrichCachedRoute(cachedRoute, transportSet);
            }

            // Pobierz nową trasę z Google Maps
            Map<String, Object> googleResponse = performGoogleMapsApiCall(
                    startAddress, endAddress, excludedBridges);

            if (googleResponse == null) {
                throw new RuntimeException("Google Maps returned null response");
            }

            // Zapisz do cache
            routeCache.put(cacheKey, googleResponse);

            // Wyczyść cache jeśli przekroczy 100 wpisów
            if (routeCache.size() > 100) {
                routeCache.clear();
                log.info("Cache wyczyszczony");
            }

            List<double[]> routeCoordinates = extractDetailedRouteCoordinates(googleResponse);

            log.info("Sprawdzanie infrastruktury przez OpenStreetMap...");
            List<InfrastructurePoint> infrastructure = overpassService.getInfrastructureAlongRoute(routeCoordinates);

            infrastructure = filterExcludedBridges(infrastructure, excludedBridges);

            log.info("Po filtrowaniu pozostało {} obiektów do sprawdzenia", infrastructure.size());

            // HERE Maps walidacja (TYLKO dla ciężkich pojazdów)
            Map<String, Object> hereValidation = null;
            if (transportSet.getTotalWeightKg() > 5000) {
                hereValidation = hereMapsService.validateRouteRestrictions(
                        routeCoordinates.get(0)[0], routeCoordinates.get(0)[1],
                        routeCoordinates.get(routeCoordinates.size()-1)[0],
                        routeCoordinates.get(routeCoordinates.size()-1)[1],
                        transportSet
                );
            } else {
                log.info("Pojazd lekki - pomijam walidację HERE Maps");
            }

            Map<String, Object> enrichedRoute = combineAllValidations(
                    googleResponse, infrastructure, hereValidation, null, transportSet
            );

            log.info("Trasa utworzona z walidacją infrastruktury");
            return enrichedRoute;

        } catch (Exception e) {
            log.error("Błąd pobierania trasy", e);
            throw new RuntimeException("Failed to create route: " + e.getMessage(), e);
        }
    }

    private String buildCacheKey(String start, String end, Set<String> excluded) {
        return start + "|" + end + "|" + excluded.size();
    }

    private Map<String, Object> enrichCachedRoute(Map<String, Object> cachedRoute,
                                                  TransportSet transportSet) {
        // Szybka walidacja bez ponownego pobierania trasy
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
                    log.debug("Pomijam wykluczony most: {}", point.getName());
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

        log.info("Wywołanie Google Maps Directions API");

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

            log.info("Google Maps route retrieved successfully");
            return response;

        } catch (Exception e) {
            log.error("Google Maps API call failed", e);
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
            Map<String, Object> tomtomValidation,
            TransportSet transportSet) {

        Map<String, Object> combined = new HashMap<>(googleRoute);

        List<String> allWarnings = new ArrayList<>();
        List<String> allRestrictions = new ArrayList<>();
        List<String> allViolations = new ArrayList<>();
        List<Map<String, Object>> infrastructureDetails = new ArrayList<>();

        log.info("Znaleziono {} obiektów infrastruktury na trasie", osmInfrastructure.size());

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

        if (hereValidation != null) {
            mergeValidationData(hereValidation, allWarnings, allRestrictions, allViolations);
        }

        if (tomtomValidation != null && !tomtomValidation.isEmpty()) {
            mergeValidationData(tomtomValidation, allWarnings, allRestrictions, allViolations);
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
        combined.put("validation_source", "multi_source");
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

        if (point.getMaxWeightTons() == null && point.getMaxHeightMeters() == null) {
            log.warn("Brak danych dla: {} - pobieranie z zewnętrznego API", point.getName());

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
                    warnings.add(String.format("%s - przekroczenie %.1ft - MOŻLIWE Z POZWOLENIEM WOJSKOWYM",
                            point.getName(), Math.abs(margin)));
                    checkResult.append(String.format("Limit %.1ft, transport %.1ft (przekroczenie %.1ft)",
                            point.getMaxWeightTons(), transportWeight, Math.abs(margin)));
                } else {
                    canPass = false;
                    violation = String.format("%s - PRZEKROCZONA NOŚNOŚĆ: limit %.1ft, twój zestaw %.1ft",
                            point.getName(), point.getMaxWeightTons(), transportWeight);
                    checkResult.append(String.format("Limit %.1ft < transport %.1ft",
                            point.getMaxWeightTons(), transportWeight));
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
        justification.add(String.format("  • Masa całkowita: %.1f ton", transportSet.getTotalWeightKg() / 1000.0));
        justification.add(String.format("  • Wysokość: %.2f m", transportSet.getTotalHeightCm() / 100.0));

        if (transportSet.getTrailerHeightCm() != null && transportSet.getTrailerHeightCm() > 0) {
            justification.add(String.format("    (naczepa %.2f m + ładunek %.2f m)",
                    transportSet.getTrailerHeightCm() / 100.0,
                    transportSet.getCargo().getHeightCm() / 100.0));
        } else {
            justification.add("    (pojazd samojezdny - bez naczepy)");
        }

        justification.add("");

        justification.add("SPRAWDZONE OBIEKTY INFRASTRUKTURY:");
        justification.add("────────────────────────────────────────────");

        if (infrastructure.isEmpty()) {
            justification.add("Brak danych o mostach/tunelach na tej trasie");
        } else {
            justification.add(String.format("Znaleziono %d obiektów do sprawdzenia:", infrastructure.size()));
            justification.add("");

            for (InfrastructurePoint point : infrastructure) {
                String icon = point.getType() == OverpassService.InfrastructureType.BRIDGE ? "🌉" : "🚇";
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
                }

                justification.add("");
            }
        }

        justification.add("WYNIK ANALIZY TRASY:");
        justification.add("────────────────────────────────────────────");

        if (violations.isEmpty() && restrictions.isEmpty()) {
            justification.add("✅ TRASA ZATWIERDZONA - wszystkie obiekty mogą być bezpiecznie pokonane");
        } else if (!violations.isEmpty()) {
            justification.add("❌ TRASA ZABLOKOWANA - wykryto fizyczne przeszkody:");
            violations.forEach(v -> justification.add("   • " + v));
        } else if (!restrictions.isEmpty()) {
            justification.add("⚠️ TRASA Z OGRANICZENIAMI - wymagana szczególna ostrożność:");
            restrictions.forEach(r -> justification.add("   • " + r));
        }

        if (!warnings.isEmpty()) {
            justification.add("");
            justification.add("DODATKOWE OSTRZEŻENIA:");
            warnings.forEach(w -> justification.add("   • " + w));
        }

        justification.add("");
        justification.add("╚════════════════════════════════════════════╝");
        justification.add("  Źródła danych: OpenStreetMap, HERE Maps, Google Maps");
        justification.add("  Data analizy: " + new Date());
        justification.add("╚════════════════════════════════════════════╝");

        return justification;
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