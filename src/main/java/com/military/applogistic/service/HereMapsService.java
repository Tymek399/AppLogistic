package com.military.applogistic.service;

import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.entity.Vehicle;
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

    /**
     * Sprawdza ograniczenia infrastruktury dla danej trasy i pojazdu
     */
    public Map<String, Object> validateRouteRestrictions(double startLat, double startLng,
                                                         double endLat, double endLng,
                                                         TransportSet transportSet) {
        if (!apiKeysConfig.isHereMapsEnabled()) {
            throw new RuntimeException("HERE Maps API not configured. Please set HERE_MAPS_API_KEY environment variable.");
        }

        try {
            log.info("Validating route restrictions for transport set: {} using HERE Maps API",
                    transportSet.getDescription());

            return performRealValidation(startLat, startLng, endLat, endLng, transportSet);
        } catch (Exception e) {
            log.error("Error during HERE Maps validation", e);
            throw new RuntimeException("HERE Maps validation failed: " + e.getMessage(), e);
        }
    }

    /**
     * Pobiera alternatywne trasy z uwzględnieniem ograniczeń pojazdu
     */
    public List<Map<String, Object>> getAlternativeRoutes(double startLat, double startLng,
                                                          double endLat, double endLng,
                                                          TransportSet transportSet) {
        if (!apiKeysConfig.isHereMapsEnabled()) {
            throw new RuntimeException("HERE Maps API not configured for alternative routes");
        }

        try {
            log.info("Getting alternative routes for transport set: {}", transportSet.getDescription());
            return performRealAlternativeRoutesRequest(startLat, startLng, endLat, endLng, transportSet);
        } catch (Exception e) {
            log.error("Error getting alternative routes from HERE Maps", e);
            throw new RuntimeException("Failed to get alternative routes: " + e.getMessage(), e);
        }
    }

    /**
     * Rzeczywista walidacja przez HERE Maps Routing API v8
     */
    private Map<String, Object> performRealValidation(double startLat, double startLng,
                                                      double endLat, double endLng,
                                                      TransportSet transportSet) {
        String url = buildHereApiUrl(startLat, startLng, endLat, endLng, transportSet, false);

        log.info("Calling HERE Maps API for route validation");

        try {
            Map<String, Object> hereResponse = restTemplate.getForObject(url, Map.class);

            if (hereResponse == null) {
                throw new RuntimeException("Empty response from HERE Maps API");
            }

            // Check for API errors
            if (hereResponse.containsKey("error")) {
                Map<String, Object> error = (Map<String, Object>) hereResponse.get("error");
                String errorMessage = (String) error.get("message");
                throw new RuntimeException("HERE Maps API error: " + errorMessage);
            }

            return analyzeHereResponse(hereResponse, transportSet);

        } catch (Exception e) {
            log.error("Error calling HERE Maps API", e);
            throw new RuntimeException("HERE Maps API call failed: " + e.getMessage(), e);
        }
    }

    /**
     * Pobiera alternatywne trasy z HERE Maps
     */
    private List<Map<String, Object>> performRealAlternativeRoutesRequest(double startLat, double startLng,
                                                                          double endLat, double endLng,
                                                                          TransportSet transportSet) {
        String url = buildHereApiUrl(startLat, startLng, endLat, endLng, transportSet, true);

        log.info("Getting alternative routes from HERE Maps");

        try {
            Map<String, Object> hereResponse = restTemplate.getForObject(url, Map.class);

            if (hereResponse == null || !hereResponse.containsKey("routes")) {
                throw new RuntimeException("No alternative routes returned from HERE Maps");
            }

            return analyzeAlternativeRoutes(hereResponse, transportSet);

        } catch (Exception e) {
            log.error("Error getting alternatives from HERE Maps", e);
            throw new RuntimeException("Failed to get alternatives: " + e.getMessage(), e);
        }
    }

    /**
     * Buduje URL dla HERE Maps Routing API v8 z parametrami ciężarówki
     */
    private String buildHereApiUrl(double startLat, double startLng, double endLat, double endLng,
                                   TransportSet transportSet, boolean alternatives) {
        StringBuilder url = new StringBuilder();
        url.append(apiKeysConfig.getHereMaps().getBaseUrl())
                .append("/routes")
                .append("?apikey=").append(apiKeysConfig.getHereMaps().getKey())
                .append("&transportMode=truck")
                .append("&origin=").append(startLat).append(",").append(startLng)
                .append("&destination=").append(endLat).append(",").append(endLng)
                .append("&return=polyline,summary,instructions,notices,violations,spans")
                .append("&lang=pl")
                .append("&units=metric");

        // Parametry ciężarówki na podstawie TransportSet
        url.append("&truck[grossWeight]=").append(transportSet.getTotalWeightKg());
        url.append("&truck[height]=").append(transportSet.getTotalHeightCm() / 100.0); // HERE oczekuje metrów
        url.append("&truck[axleLoad]=").append(transportSet.getMaxAxleLoadKg());

        // Estymacja szerokości i długości na podstawie typu pojazdu
        Vehicle transporter = transportSet.getTransporter();
        Vehicle cargo = transportSet.getCargo();

        double estimatedWidth = Math.max(getEstimatedWidth(transporter), getEstimatedWidth(cargo));
        url.append("&truck[width]=").append(estimatedWidth);

        double estimatedLength = getEstimatedLength(transporter) + getEstimatedLength(cargo);
        url.append("&truck[length]=").append(estimatedLength);

        // Materiały niebezpieczne - domyślnie brak dla sprzętu wojskowego
        url.append("&truck[hazardousGoods]=none");

        // Alternatywne trasy
        if (alternatives) {
            url.append("&alternatives=3");
        }

        // Dodatkowe parametry dla lepszej walidacji
        url.append("&avoid[features]=ferry");
        url.append("&routingMode=fast");

        return url.toString();
    }

    /**
     * Analizuje odpowiedź HERE Maps i wyciąga ograniczenia
     */
    private Map<String, Object> analyzeHereResponse(Map<String, Object> hereResponse, TransportSet transportSet) {
        Map<String, Object> validation = new HashMap<>();
        List<String> warnings = new ArrayList<>();
        List<String> restrictions = new ArrayList<>();
        List<String> violations = new ArrayList<>();
        boolean routeAvailable = true;

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) hereResponse.get("routes");

            if (routes == null || routes.isEmpty()) {
                routeAvailable = false;
                restrictions.add("Brak dostępnej trasy dla podanych parametrów pojazdu");
                log.warn("No routes available for vehicle parameters");
            } else {
                Map<String, Object> route = routes.get(0);

                // Analiza notices (ostrzeżenia)
                analyzeNotices(route, warnings, restrictions);

                // Analiza violations (naruszenia)
                analyzeViolations(route, violations, restrictions);

                // Analiza sections/spans trasy
                analyzeRouteSpans(route, warnings, restrictions);

                // Dodatkowa analiza na podstawie parametrów pojazdu
                addVehicleSpecificWarnings(transportSet, warnings, restrictions);
            }

        } catch (Exception e) {
            log.error("Error analyzing HERE Maps response", e);
            restrictions.add("Błąd podczas analizy odpowiedzi HERE Maps: " + e.getMessage());
        }

        validation.put("routeAvailable", routeAvailable);
        validation.put("hasRestrictions", !restrictions.isEmpty());
        validation.put("hasWarnings", !warnings.isEmpty());
        validation.put("hasViolations", !violations.isEmpty());
        validation.put("restrictions", restrictions);
        validation.put("warnings", warnings);
        validation.put("violations", violations);
        validation.put("source", "here_maps_api");
        validation.put("transportSet", Map.of(
                "id", transportSet.getId(),
                "description", transportSet.getDescription(),
                "weight", transportSet.getTotalWeightKg(),
                "height", transportSet.getTotalHeightCm(),
                "axleLoad", transportSet.getMaxAxleLoadKg()
        ));

        log.info("HERE Maps validation completed: {} restrictions, {} warnings, {} violations",
                restrictions.size(), warnings.size(), violations.size());

        return validation;
    }

    /**
     * Analizuje notices (ostrzeżenia) z odpowiedzi HERE
     */
    private void analyzeNotices(Map<String, Object> route, List<String> warnings, List<String> restrictions) {
        List<Map<String, Object>> notices = (List<Map<String, Object>>) route.get("notices");
        if (notices != null) {
            for (Map<String, Object> notice : notices) {
                String code = (String) notice.get("code");
                String title = (String) notice.get("title");
                String message = title != null ? title : code;

                switch (code != null ? code : "") {
                    case "restrictedTurn":
                        restrictions.add("Ograniczony zakręt: " + message);
                        break;
                    case "weightRestriction":
                        restrictions.add("Ograniczenie wagowe: " + message);
                        break;
                    case "heightRestriction":
                        restrictions.add("Ograniczenie wysokości: " + message);
                        break;
                    case "bridgeRestriction":
                        restrictions.add("Ograniczenie mostowe: " + message);
                        break;
                    case "tunnelRestriction":
                        restrictions.add("Ograniczenie tunelowe: " + message);
                        break;
                    case "truckRestriction":
                        restrictions.add("Ograniczenie dla ciężarówek: " + message);
                        break;
                    default:
                        warnings.add("Ostrzeżenie: " + message);
                }
            }
        }
    }

    /**
     * Analizuje violations (naruszenia) z odpowiedzi HERE
     */
    private void analyzeViolations(Map<String, Object> route, List<String> violations, List<String> restrictions) {
        List<Map<String, Object>> violationsList = (List<Map<String, Object>>) route.get("violations");
        if (violationsList != null) {
            for (Map<String, Object> violation : violationsList) {
                String code = (String) violation.get("code");
                String description = (String) violation.get("description");
                String message = description != null ? description : code;

                violations.add("NARUSZENIE: " + message);
                restrictions.add("Trasa narusza ograniczenia: " + message);
            }
        }
    }

    /**
     * Analizuje spans trasy pod kątem ograniczeń
     */
    private void analyzeRouteSpans(Map<String, Object> route, List<String> warnings, List<String> restrictions) {
        List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");
        if (sections != null) {
            for (Map<String, Object> section : sections) {
                List<Map<String, Object>> spans = (List<Map<String, Object>>) section.get("spans");
                if (spans != null) {
                    for (Map<String, Object> span : spans) {
                        List<Map<String, Object>> notices = (List<Map<String, Object>>) span.get("notices");
                        if (notices != null) {
                            for (Map<String, Object> notice : notices) {
                                String code = (String) notice.get("code");
                                String title = (String) notice.get("title");
                                warnings.add("Odcinek trasy: " + (title != null ? title : code));
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Dodaje ostrzeżenia specyficzne dla pojazdu wojskowego
     */
    private void addVehicleSpecificWarnings(TransportSet transportSet, List<String> warnings, List<String> restrictions) {
        String cargoModel = transportSet.getCargo().getModel().toLowerCase();

        // Ostrzeżenia dla różnych typów ładunku
        if (cargoModel.contains("leopard") || cargoModel.contains("tank")) {
            warnings.add("Transport czołgu - sprawdź nośność wszystkich mostów");
            restrictions.add("Czołg - wymagane specjalne pozwolenia na transport");
        }

        if (cargoModel.contains("radar") || cargoModel.contains("pilica")) {
            warnings.add("Sprzęt radarowy - zwiększona wysokość, sprawdź przepusty");
        }

        if (cargoModel.contains("krab") || cargoModel.contains("haubica")) {
            warnings.add("Haubica - sprawdź ograniczenia wagowe i manewrowe");
        }

        if (cargoModel.contains("rosomak") || cargoModel.contains("apc")) {
            warnings.add("Transporter opancerzony - sprawdź ograniczenia mostowe");
        }

        // Ostrzeżenia wagowe
        if (transportSet.getTotalWeightKg() > 50000) {
            restrictions.add("Masa >50t - wymagane specjalne pozwolenia");
        }

        if (transportSet.getTotalWeightKg() > 40000) {
            warnings.add("Masa >40t - sprawdź nośność mostów");
        }

        // Ostrzeżenia dotyczące wysokości
        if (transportSet.getTotalHeightCm() > 420) {
            warnings.add("Wysokość >4.2m - sprawdź wszystkie przepusty i linie energetyczne");
        }

        if (transportSet.getTotalHeightCm() > 380) {
            warnings.add("Wysokość >3.8m - sprawdź mosty i tunele");
        }

        // Ostrzeżenia dotyczące nacisku na oś
        if (transportSet.getMaxAxleLoadKg() > 12000) {
            restrictions.add("Nacisk na oś >12t - sprawdź ograniczenia drogowe");
        }
    }

    /**
     * Analizuje alternatywne trasy
     */
    private List<Map<String, Object>> analyzeAlternativeRoutes(Map<String, Object> hereResponse, TransportSet transportSet) {
        List<Map<String, Object>> alternatives = new ArrayList<>();

        List<Map<String, Object>> routes = (List<Map<String, Object>>) hereResponse.get("routes");
        if (routes != null) {
            for (int i = 0; i < routes.size() && i < 3; i++) {
                Map<String, Object> route = routes.get(i);
                Map<String, Object> alternative = new HashMap<>();

                alternative.put("routeIndex", i);
                alternative.put("validation", analyzeHereResponse(Map.of("routes", List.of(route)), transportSet));

                // Dodaj podstawowe informacje o trasie
                Map<String, Object> summary = (Map<String, Object>) route.get("summary");
                if (summary != null) {
                    alternative.put("distance", summary.get("length"));
                    alternative.put("duration", summary.get("duration"));
                    alternative.put("baseDuration", summary.get("baseDuration"));
                }

                alternatives.add(alternative);
            }
        }

        log.info("Found {} alternative routes", alternatives.size());
        return alternatives;
    }

    /**
     * Estymuje szerokość pojazdu na podstawie modelu
     */
    private double getEstimatedWidth(Vehicle vehicle) {
        String model = vehicle.getModel().toLowerCase();

        if (model.contains("leopard")) return 3.7;
        if (model.contains("rosomak")) return 2.9;
        if (model.contains("krab")) return 3.4;
        if (model.contains("radar") || model.contains("pilica")) return 3.2;
        if (model.contains("man") || model.contains("mercedes") || model.contains("volvo")) return 2.5;

        return 2.5; // domyślna szerokość
    }

    /**
     * Estymuje długość pojazdu na podstawie modelu
     */
    private double getEstimatedLength(Vehicle vehicle) {
        String model = vehicle.getModel().toLowerCase();

        if (model.contains("leopard")) return 9.97;
        if (model.contains("rosomak")) return 7.7;
        if (model.contains("krab")) return 12.0;
        if (model.contains("radar") || model.contains("pilica")) return 8.0;
        if (model.contains("man") || model.contains("mercedes") || model.contains("volvo")) return 16.5;

        return 12.0; // domyślna długość
    }

    /**
     * Sprawdza czy HERE Maps jest skonfigurowane
     */
    public boolean isConfigured() {
        return apiKeysConfig.isHereMapsEnabled();
    }

    /**
     * Zwraca status API
     */
    public String getApiStatus() {
        return apiKeysConfig.isHereMapsEnabled() ? "configured" : "not_configured";
    }
}