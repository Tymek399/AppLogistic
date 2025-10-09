package com.military.applogistic.service.api;

import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.transport.TransportSetCalculator;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class HereMapsService {

    private final ApiKeysConfig apiKeysConfig;
    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper = new ObjectMapper();
    private final TransportSetCalculator transportSetCalculator;

    public Map<String, Object> validateRouteRestrictions(double startLat, double startLng,
                                                         double endLat, double endLng,
                                                         TransportSet transportSet) {
        if (!apiKeysConfig.isHereMapsEnabled()) {
            log.warn("HERE Maps API not configured");
            return createBasicValidation(transportSet);
        }

        try {
            log.info("Validating route with parameters: weight={}kg, height={}cm (trailer={}cm + cargo={}cm)",
                    transportSet.getTotalWeightKg(),
                    transportSet.getTotalHeightCm(),
                    transportSet.getTrailerHeightCm(),
                    transportSet.getCargo().getHeightCm());

            Map<String, Object> hereResponse = performRealHereApiCall(startLat, startLng, endLat, endLng, transportSet);
            return analyzeHereResponse(hereResponse, transportSet);

        } catch (Exception e) {
            log.error("HERE Maps validation failed", e);
            return createBasicValidation(transportSet);
        }
    }

    private Map<String, Object> performRealHereApiCall(double startLat, double startLng,
                                                       double endLat, double endLng,
                                                       TransportSet transportSet) {
        StringBuilder url = new StringBuilder();
        url.append(apiKeysConfig.getHereMaps().getBaseUrl())
                .append("/routes")
                .append("?apikey=").append(apiKeysConfig.getHereMaps().getKey())
                .append("&transportMode=truck")
                .append("&origin=").append(startLat).append(",").append(startLng)
                .append("&destination=").append(endLat).append(",").append(endLng)
                .append("&return=polyline,summary,actions,instructions,travelSummary")
                .append("&lang=pl")
                .append("&units=metric");

        url.append("&truck[grossWeight]=").append(transportSet.getTotalWeightKg());
        url.append("&truck[axleLoad]=").append(transportSet.getMaxAxleLoadKg());
        url.append("&truck[height]=").append(transportSet.getTotalHeightCm());
        url.append("&truck[width]=").append(transportSet.getTotalWidthCm()); // w cm
        url.append("&truck[length]=").append(transportSet.getTotalLengthCm()); // w cm

        url.append("&truck[hazardousGoods]=none");
        url.append("&avoid[features]=ferry");
        url.append("&routingMode=fast");

        log.info("HERE Maps API call with truck parameters:");
        log.info("   Weight: {}kg ({} tons)", transportSet.getTotalWeightKg(), transportSet.getTotalWeightKg()/1000);
        log.info("   Height: {}cm ({} m)", transportSet.getTotalHeightCm(), transportSet.getTotalHeightCm()/100.0);

        try {
            Map<String, Object> response = restTemplate.getForObject(url.toString(), Map.class);

            if (response == null) {
                throw new RuntimeException("Empty response from HERE Maps");
            }

            // Loguj pełną odpowiedź dla debugowania
            try {
                String jsonResponse = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(response);
                log.debug("HERE Maps Full Response:\n{}", jsonResponse);
            } catch (Exception e) {
                log.warn("Could not serialize response for logging", e);
            }

            if (response.containsKey("error")) {
                Map<String, Object> error = (Map<String, Object>) response.get("error");
                throw new RuntimeException("HERE Maps API error: " + error.get("message"));
            }

            log.info("HERE Maps route retrieved successfully");
            return response;

        } catch (Exception e) {
            log.error("HERE Maps API call failed: {}", e.getMessage());
            throw new RuntimeException("HERE Maps API call failed: " + e.getMessage(), e);
        }
    }

    private Map<String, Object> analyzeHereResponse(Map<String, Object> hereResponse, TransportSet transportSet) {
        Map<String, Object> validation = new HashMap<>();
        List<String> violations = new ArrayList<>();
        List<String> restrictions = new ArrayList<>();
        List<String> warnings = new ArrayList<>();
        List<Map<String, Object>> validationDetails = new ArrayList<>();
        boolean routeAvailable = true;
        List<Map<String, Object>> allNotices = new ArrayList<>();

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) hereResponse.get("routes");

            if (routes == null || routes.isEmpty()) {
                routeAvailable = false;
                violations.add("BRAK DOSTĘPNEJ TRASY - HERE Maps nie może znaleźć trasy dla tych parametrów");
                log.warn("NO ROUTE AVAILABLE from HERE Maps");
            } else {
                Map<String, Object> route = routes.get(0);

                // KLUCZOWE: Zbieraj notices z sections (poprawka według dokumentacji HERE)
                List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");

                if (sections != null && !sections.isEmpty()) {
                    log.info("Found {} sections in route", sections.size());

                    for (Map<String, Object> section : sections) {
                        // Notices na poziomie sekcji
                        List<Map<String, Object>> sectionNotices = (List<Map<String, Object>>) section.get("notices");
                        if (sectionNotices != null && !sectionNotices.isEmpty()) {
                            log.info("Found {} notices in section", sectionNotices.size());
                            allNotices.addAll(sectionNotices);
                        }

                        // Notices na poziomie spans (szczegółowe odcinki)
                        List<Map<String, Object>> spans = (List<Map<String, Object>>) section.get("spans");
                        if (spans != null) {
                            log.info("Found {} spans in section", spans.size());
                            for (Map<String, Object> span : spans) {
                                List<Map<String, Object>> spanNotices = (List<Map<String, Object>>) span.get("notices");
                                if (spanNotices != null && !spanNotices.isEmpty()) {
                                    log.info("Found {} notices in span", spanNotices.size());
                                    allNotices.addAll(spanNotices);
                                }
                            }
                        }
                    }
                }

                log.info("Total notices collected from HERE: {}", allNotices.size());

                // Analizuj zebrane notices
                if (!allNotices.isEmpty()) {
                    analyzeCollectedNotices(allNotices, transportSet, restrictions, warnings, violations, validationDetails);
                }

                List<String> justification;

                if (allNotices.isEmpty()) {
                    log.warn("HERE Maps returned no notices - using fallback validation");
                    justification = buildFallbackValidation(transportSet, route);
                    validation.put("validation_source", "fallback_local");
                    warnings.add("HERE Maps nie zwrócił szczegółowych danych o infrastrukturze");
                    warnings.add("Wykonano lokalną walidację parametrów pojazdu");
                } else {
                    justification = buildRouteJustification(route, transportSet, allNotices, violations);
                    validation.put("validation_source", "here_maps_api_real");
                }

                validation.put("routeJustification", justification);
            }

        } catch (Exception e) {
            log.error("Error analyzing HERE response", e);
            warnings.add("Błąd podczas analizy odpowiedzi HERE Maps: " + e.getMessage());
        }

        addTransportSetValidationDetails(transportSet, validationDetails);

        validation.put("routeAvailable", routeAvailable);
        validation.put("hasViolations", !violations.isEmpty());
        validation.put("hasRestrictions", !restrictions.isEmpty());
        validation.put("hasWarnings", !warnings.isEmpty());
        validation.put("violations", violations);
        validation.put("restrictions", restrictions);
        validation.put("warnings", warnings);
        validation.put("validationDetails", validationDetails);
        validation.put("transportSet", createTransportSetInfo(transportSet));

        log.info("Validation result: {} violations, {} restrictions, {} warnings",
                violations.size(), restrictions.size(), warnings.size());

        return validation;
    }

    private void analyzeCollectedNotices(List<Map<String, Object>> notices,
                                         TransportSet transportSet,
                                         List<String> restrictions,
                                         List<String> warnings,
                                         List<String> violations,
                                         List<Map<String, Object>> details) {

        for (Map<String, Object> notice : notices) {
            String code = (String) notice.get("code");
            String title = (String) notice.get("title");
            String message = title != null ? title : code;

            Map<String, Object> detail = new HashMap<>();
            detail.put("type", "notice");
            detail.put("code", code);
            detail.put("message", message);

            log.info("Processing notice: code={}, title={}", code, title);

            if (code == null) continue;

            // Według dokumentacji HERE Maps API
            switch (code) {
                case "violatedWeightRestriction":
                    violations.add(String.format("NARUSZENIE: Przekroczono limit wagi - %s (Twój zestaw: %.1ft)",
                            message, transportSet.getTotalWeightKg() / 1000.0));
                    restrictions.add("FIZYCZNA BLOKADA: " + message);
                    detail.put("severity", "critical");
                    detail.put("vehicleParam", "weight");
                    break;

                case "violatedHeightRestriction":
                    violations.add(String.format("NARUSZENIE: Przekroczono limit wysokości - %s (Twój zestaw: %.2fm)",
                            message, transportSet.getTotalHeightCm() / 100.0));
                    restrictions.add("FIZYCZNA BLOKADA: " + message);
                    detail.put("severity", "critical");
                    detail.put("vehicleParam", "height");
                    break;

                case "violatedWidthRestriction":
                    violations.add(String.format("NARUSZENIE: Przekroczono limit szerokości - %s (Twój zestaw: %.2fm)",
                            message, transportSet.getTotalWidthCm() / 100.0));
                    restrictions.add("FIZYCZNA BLOKADA: " + message);
                    detail.put("severity", "critical");
                    detail.put("vehicleParam", "width");
                    break;

                case "violatedLengthRestriction":
                    violations.add(String.format("NARUSZENIE: Przekroczono limit długości - %s (Twój zestaw: %.2fm)",
                            message, transportSet.getTotalLengthCm() / 100.0));
                    restrictions.add("FIZYCZNA BLOKADA: " + message);
                    detail.put("severity", "critical");
                    detail.put("vehicleParam", "length");
                    break;

                case "violatedAxleWeightRestriction":
                    violations.add(String.format("NARUSZENIE: Przekroczono nacisk na oś - %s (Twój zestaw: %.1ft)",
                            message, transportSet.getMaxAxleLoadKg() / 1000.0));
                    restrictions.add("FIZYCZNA BLOKADA: " + message);
                    detail.put("severity", "critical");
                    detail.put("vehicleParam", "axleLoad");
                    break;

                case "violatedAvoidTruckRestriction":
                    warnings.add("OSTRZEŻENIE: Ograniczenie dla ciężarówek - " + message);
                    restrictions.add("Ograniczenie dla ciężarówek: " + message);
                    detail.put("severity", "high");
                    break;

                case "truckRestriction":
                    warnings.add("Ograniczenie dla ciężarówek: " + message);
                    detail.put("severity", "medium");
                    break;

                case "tollRoad":
                    warnings.add("Droga płatna: " + message);
                    detail.put("severity", "info");
                    break;

                case "ferry":
                    warnings.add("Prom na trasie: " + message);
                    detail.put("severity", "info");
                    break;

                default:
                    log.debug("Unknown notice code: {} - {}", code, message);
                    warnings.add(message);
                    detail.put("severity", "info");
            }

            details.add(detail);
        }
    }

    private List<String> buildFallbackValidation(TransportSet transportSet, Map<String, Object> route) {
        List<String> justification = new ArrayList<>();

        justification.add("═══════════════════════════════════════");
        justification.add("LOKALNA WALIDACJA - BRAK DANYCH HERE MAPS");
        justification.add("═══════════════════════════════════════");
        justification.add("");
        justification.add("HERE Maps nie zwrócił szczegółowych informacji o mostach");
        justification.add("i tunelach na tej trasie. Wykonano lokalną analizę parametrów.");
        justification.add("");

        justification.add("═══════════════════════════════════════");
        justification.add("PARAMETRY ZESTAWU TRANSPORTOWEGO");
        justification.add("═══════════════════════════════════════");
        justification.add(String.format("Opis: %s", transportSet.getDescription()));
        justification.add(String.format("Ciężarówka: %s", transportSet.getTransporter().getModel()));
        justification.add(String.format("Ładunek: %s", transportSet.getCargo().getModel()));
        justification.add(String.format("Typ naczepy: %s", transportSetCalculator.getTrailerType(transportSet)));
        justification.add("");
        justification.add("Wymiary i parametry:");
        justification.add(String.format("  Masa całkowita: %.1f ton", transportSet.getTotalWeightKg() / 1000.0));
        justification.add(String.format("  Wysokość: %.2f m (naczepa %.2f m + ładunek %.2f m)",
                transportSet.getTotalHeightCm() / 100.0,
                transportSet.getTrailerHeightCm() / 100.0,
                transportSet.getCargo().getHeightCm() / 100.0));
        justification.add(String.format("  Długość: %.2f m", transportSet.getTotalLengthCm() / 100.0));
        justification.add(String.format("  Szerokość: %.2f m", transportSet.getTotalWidthCm() / 100.0));
        justification.add(String.format("  Nacisk na oś: %.1f ton", transportSet.getMaxAxleLoadKg() / 1000.0));
        justification.add("");

        justification.add("═══════════════════════════════════════");
        justification.add("ANALIZA POTENCJALNYCH ZAGROŻEŃ");
        justification.add("═══════════════════════════════════════");

        boolean hasCriticalIssues = false;

        // WYSOKOŚĆ
        double heightM = transportSet.getTotalHeightCm() / 100.0;
        if (heightM > 4.5) {
            justification.add("");
            justification.add("KRYTYCZNE: Wysokość > 4.5m");
            justification.add("   Problem: Większość mostów/wiaduktów w Polsce: 4.5-5.0m prześwit");
            justification.add("   Ryzyko: Wysokie - możliwe uszkodzenie ładunku");
            justification.add("   WYMAGA: Ręcznego sprawdzenia każdego mostu/wiaduktu");
            hasCriticalIssues = true;
        } else if (heightM > 4.0) {
            justification.add("");
            justification.add("UWAGA: Wysokość 4.0-4.5m");
            justification.add(String.format("   Margines bezpieczeństwa: %.2f m", 4.5 - heightM));
            justification.add("   Zalecenia: Główne trasy, unikaj dróg lokalnych");
        } else {
            justification.add("");
            justification.add("WYSOKOŚĆ: < 4.0m - większość infrastruktury dostępna");
        }
        justification.add("");

        // WAGA
        double weightT = transportSet.getTotalWeightKg() / 1000.0;
        if (weightT > 70.0) {
            justification.add("KRYTYCZNE: Masa > 60 ton");
            justification.add("   WYMAGA: Sprawdzenia nośności mostów");
            hasCriticalIssues = true;
        } else if (weightT > 50.0) {
            justification.add("UWAGA: Masa 40-60 ton");
            justification.add("   Drogi krajowe: OK (limit 60t)");
            justification.add("   Unikaj starych mostów (sprzed 1990)");
        } else {
            justification.add("WAGA: < 40 ton - standardowa infrastruktura");
        }
        justification.add("");

        justification.add("═══════════════════════════════════════");
        justification.add("PODSUMOWANIE");
        justification.add("═══════════════════════════════════════");

        if (hasCriticalIssues) {
            justification.add("WYKRYTO KRYTYCZNE PROBLEMY - wymagana szczególna uwaga");
        } else if (heightM > 4.0 || weightT > 40.0) {
            justification.add("TRASA WYMAGA ZWIĘKSZONEJ UWAGI");
        } else {
            justification.add("TRASA BEZPIECZNA - brak wykrytych zagrożeń");
        }
        justification.add("");
        justification.add("UWAGA: Ta walidacja jest lokalna - brak danych HERE Maps");
        justification.add("Dla bezpieczeństwa wykonaj rekonesans trasy.");
        justification.add("═══════════════════════════════════════");

        return justification;
    }

    private List<String> buildRouteJustification(Map<String, Object> route, TransportSet transportSet,
                                                 List<Map<String, Object>> allNotices,
                                                 List<String> violations) {
        List<String> justification = new ArrayList<>();

        justification.add("═══════════════════════════════════════");
        justification.add("PARAMETRY ZESTAWU TRANSPORTOWEGO");
        justification.add("═══════════════════════════════════════");
        justification.add(String.format("Opis: %s", transportSet.getDescription()));
        justification.add(String.format("Ciężarówka: %s", transportSet.getTransporter().getModel()));
        justification.add(String.format("Ładunek: %s", transportSet.getCargo().getModel()));
        justification.add("");
        justification.add("Wymiary:");
        justification.add(String.format("  Masa: %.1f ton", transportSet.getTotalWeightKg() / 1000.0));
        justification.add(String.format("  Wysokość: %.2f m (naczepa %.2f m + ładunek %.2f m)",
                transportSet.getTotalHeightCm() / 100.0,
                transportSet.getTrailerHeightCm() / 100.0,
                transportSet.getCargo().getHeightCm() / 100.0));
        justification.add("");

        if (allNotices != null && !allNotices.isEmpty()) {
            justification.add("═══════════════════════════════════════");
            justification.add("INFRASTRUKTURA SPRAWDZONA PRZEZ HERE MAPS");
            justification.add("═══════════════════════════════════════");

            for (Map<String, Object> notice : allNotices) {
                String code = (String) notice.get("code");
                String title = (String) notice.get("title");
                String message = title != null ? title : code;
                justification.add(String.format("  %s", message));
            }
            justification.add("");
        }

        justification.add("═══════════════════════════════════════");
        justification.add("PODSUMOWANIE");
        justification.add("═══════════════════════════════════════");

        if (violations == null || violations.isEmpty()) {
            justification.add("TRASA ZATWIERDZONA - transport możliwy");
            justification.add(String.format("Sprawdzono: %d ograniczeń", allNotices.size()));
        } else {
            justification.add("UWAGA: Użyto trasy alternatywnej");
        }
        justification.add("═══════════════════════════════════════");

        return justification;
    }

    private void addTransportSetValidationDetails(TransportSet transportSet, List<Map<String, Object>> details) {
        Map<String, Object> setInfo = new HashMap<>();
        setInfo.put("type", "transport_set_info");
        setInfo.put("description", transportSet.getDescription());
        setInfo.put("totalWeight_kg", transportSet.getTotalWeightKg());
        setInfo.put("totalHeight_cm", transportSet.getTotalHeightCm());
        setInfo.put("trailerHeight_cm", transportSet.getTrailerHeightCm());
        setInfo.put("cargoHeight_cm", transportSet.getCargo().getHeightCm());
        details.add(setInfo);
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
        return info;
    }

    public List<Map<String, Object>> getAlternativeRoutes(double startLat, double startLng,
                                                          double endLat, double endLng,
                                                          TransportSet transportSet) {
        if (!apiKeysConfig.isHereMapsEnabled()) {
            return Collections.emptyList();
        }

        try {
            String url = buildHereApiUrl(startLat, startLng, endLat, endLng, transportSet, true);
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response == null || !response.containsKey("routes")) {
                return Collections.emptyList();
            }

            return analyzeAlternativeRoutes(response, transportSet);

        } catch (Exception e) {
            log.error("Error getting alternatives", e);
            return Collections.emptyList();
        }
    }

    // W HereMapsService.java - metoda buildHereApiUrl
    private String buildHereApiUrl(double startLat, double startLng, double endLat, double endLng,
                                   TransportSet transportSet, boolean alternatives) {
        StringBuilder url = new StringBuilder();
        url.append(apiKeysConfig.getHereMaps().getBaseUrl())
                .append("/routes")
                .append("?apikey=").append(apiKeysConfig.getHereMaps().getKey())
                .append("&transportMode=truck")
                .append("&origin=").append(startLat).append(",").append(startLng)
                .append("&destination=").append(endLat).append(",").append(endLng)

                // ✅ Poprawne parametry według dokumentacji HERE v8
                .append("&return=polyline,summary,actions,instructions")

                .append("&lang=pl")
                .append("&units=metric")
                .append("&truck[grossWeight]=").append(transportSet.getTotalWeightKg())
                .append("&truck[height]=").append(transportSet.getTotalHeightCm() / 100.0)
                .append("&truck[axleLoad]=").append(transportSet.getMaxAxleLoadKg())
                .append("&truck[width]=").append(transportSet.getTotalWidthCm() / 100.0)
                .append("&truck[length]=").append(transportSet.getTotalLengthCm() / 100.0)
                .append("&avoid[features]=ferry")
                .append("&routingMode=fast");

        if (alternatives) {
            url.append("&alternatives=3");
        }

        return url.toString();
    }

    private List<Map<String, Object>> analyzeAlternativeRoutes(Map<String, Object> response, TransportSet transportSet) {
        List<Map<String, Object>> alternatives = new ArrayList<>();
        List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");

        if (routes != null) {
            for (int i = 0; i < routes.size() && i < 3; i++) {
                Map<String, Object> route = routes.get(i);
                Map<String, Object> alternative = new HashMap<>();
                alternative.put("routeIndex", i);
                alternative.put("validation", analyzeHereResponse(Map.of("routes", List.of(route)), transportSet));
                alternatives.add(alternative);
            }
        }

        return alternatives;
    }

    private Map<String, Object> createBasicValidation(TransportSet transportSet) {
        Map<String, Object> validation = new HashMap<>();
        List<String> warnings = new ArrayList<>();

        if (transportSet.getTotalHeightCm() > 400) {
            warnings.add("Wysokość " + (transportSet.getTotalHeightCm()/100.0) + "m - sprawdź przepusty");
        }
        if (transportSet.getTotalWeightKg() > 40000) {
            warnings.add("Masa " + (transportSet.getTotalWeightKg()/1000) + "t - sprawdź nośność mostów");
        }

        validation.put("routeAvailable", true);
        validation.put("hasViolations", false);
        validation.put("hasRestrictions", !warnings.isEmpty());
        validation.put("hasWarnings", !warnings.isEmpty());
        validation.put("violations", new ArrayList<>());
        validation.put("restrictions", new ArrayList<>());
        validation.put("warnings", warnings);
        validation.put("source", "basic_validation");
        validation.put("transportSet", createTransportSetInfo(transportSet));
        validation.put("routeJustification", List.of("Podstawowa walidacja - API niedostępne"));

        return validation;
    }

    public boolean isConfigured() {
        return apiKeysConfig.isHereMapsEnabled();
    }

    public String getApiStatus() {
        return apiKeysConfig.isHereMapsEnabled() ? "configured" : "not_configured";
    }
}