package com.military.applogistic.service;

import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.TransportSet;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class GoogleMapsService {

    private final ApiKeysConfig apiKeysConfig;
    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper = new ObjectMapper();
    private final HereMapsService hereMapsService;

    /**
     * Główna metoda pobierająca trasę z walidacją ograniczeń infrastruktury
     */
    public Map<String, Object> getRoute(String startAddress, String endAddress, TransportSet transportSet) {
        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            throw new RuntimeException("Google Maps API not configured. Please set GOOGLE_MAPS_API_KEY environment variable.");
        }

        try {
            log.info("Creating route from {} to {} with transport set: {}",
                    startAddress, endAddress, transportSet.getDescription());

            // Pobierz trasę z Google Maps
            Map<String, Object> googleResponse = performGoogleMapsApiCall(startAddress, endAddress);

            if (googleResponse == null) {
                throw new RuntimeException("Google Maps API returned null response");
            }

            // Walidacja przez HERE Maps jeśli dostępne
            if (transportSet != null) {
                return validateAndEnhanceWithHere(googleResponse, transportSet);
            }

            return googleResponse;

        } catch (Exception e) {
            log.error("Error getting route from Google Maps for {} to {}", startAddress, endAddress, e);
            throw new RuntimeException("Failed to create route: " + e.getMessage(), e);
        }
    }

    /**
     * Wykonuje rzeczywiste wywołanie Google Maps Directions API
     */
    private Map<String, Object> performGoogleMapsApiCall(String startAddress, String endAddress) {
        String url = String.format(
                "%s/directions/json?origin=%s&destination=%s&key=%s&mode=driving&alternatives=true&language=pl&avoid=tolls",
                apiKeysConfig.getGoogleMaps().getBaseUrl(),
                encodeAddress(startAddress),
                encodeAddress(endAddress),
                apiKeysConfig.getGoogleMaps().getKey()
        );

        log.info("Calling Google Maps Directions API for route calculation");

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

            log.info("Successfully retrieved route from Google Maps");
            return response;

        } catch (Exception e) {
            log.error("Failed to call Google Maps API", e);
            throw new RuntimeException("Google Maps API call failed: " + e.getMessage(), e);
        }
    }

    /**
     * Waliduje i wzbogaca trasę Google Maps przez HERE Maps
     */
    private Map<String, Object> validateAndEnhanceWithHere(Map<String, Object> googleRoute, TransportSet transportSet) {
        try {
            // Wyciągnij współrzędne z trasy Google Maps
            List<double[]> routeCoordinates = extractCoordinatesFromGoogleRoute(googleRoute);

            if (routeCoordinates.size() >= 2) {
                double[] start = routeCoordinates.get(0);
                double[] end = routeCoordinates.get(routeCoordinates.size() - 1);

                log.info("Validating route through HERE Maps for vehicle restrictions");

                // Walidacja przez HERE Maps
                Map<String, Object> hereValidation = hereMapsService.validateRouteRestrictions(
                        start[0], start[1], end[0], end[1], transportSet);

                // Wzbogać odpowiedź Google Maps o dane z HERE
                enhanceGoogleResponseWithHereData(googleRoute, hereValidation, transportSet);

                // Pobierz alternatywne trasy z HERE Maps jeśli potrzebne
                if (hereMapsService.isConfigured() && hasRestrictiveViolations(hereValidation)) {
                    log.info("Route has restrictions, fetching alternatives from HERE Maps");
                    List<Map<String, Object>> alternatives = hereMapsService.getAlternativeRoutes(
                            start[0], start[1], end[0], end[1], transportSet);
                    googleRoute.put("here_alternatives", alternatives);
                }
            } else {
                log.warn("Could not extract coordinates from Google route for HERE validation");
                addBasicVehicleValidation(googleRoute, transportSet);
            }

        } catch (Exception e) {
            log.warn("Error during HERE Maps validation, using basic validation", e);
            addBasicVehicleValidation(googleRoute, transportSet);
        }

        return googleRoute;
    }

    /**
     * Wyciąga współrzędne z odpowiedzi Google Maps
     */
    private List<double[]> extractCoordinatesFromGoogleRoute(Map<String, Object> googleRoute) {
        List<double[]> coordinates = new ArrayList<>();

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) googleRoute.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> route = routes.get(0);
                List<Map<String, Object>> legs = (List<Map<String, Object>>) route.get("legs");

                if (legs != null && !legs.isEmpty()) {
                    Map<String, Object> leg = legs.get(0);

                    // Punkt startowy
                    Map<String, Object> startLocation = (Map<String, Object>) leg.get("start_location");
                    if (startLocation != null) {
                        coordinates.add(new double[]{
                                (Double) startLocation.get("lat"),
                                (Double) startLocation.get("lng")
                        });
                    }

                    // Punkt końcowy
                    Map<String, Object> endLocation = (Map<String, Object>) leg.get("end_location");
                    if (endLocation != null) {
                        coordinates.add(new double[]{
                                (Double) endLocation.get("lat"),
                                (Double) endLocation.get("lng")
                        });
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Could not extract coordinates from Google route", e);
        }

        return coordinates;
    }

    /**
     * Wzbogaca odpowiedź Google Maps o dane z HERE Maps
     */
    private void enhanceGoogleResponseWithHereData(Map<String, Object> googleRoute,
                                                   Map<String, Object> hereValidation,
                                                   TransportSet transportSet) {
        // Dodaj informacje o ograniczeniach
        googleRoute.put("hasRestrictions", hereValidation.get("hasRestrictions"));
        googleRoute.put("hasWarnings", hereValidation.get("hasWarnings"));
        googleRoute.put("hasViolations", hereValidation.get("hasViolations"));

        // Połącz ostrzeżenia i ograniczenia
        List<String> allWarnings = new ArrayList<>();
        List<String> allRestrictions = new ArrayList<>();
        List<String> allViolations = new ArrayList<>();

        if (hereValidation.get("warnings") instanceof List) {
            allWarnings.addAll((List<String>) hereValidation.get("warnings"));
        }

        if (hereValidation.get("restrictions") instanceof List) {
            allRestrictions.addAll((List<String>) hereValidation.get("restrictions"));
        }

        if (hereValidation.get("violations") instanceof List) {
            allViolations.addAll((List<String>) hereValidation.get("violations"));
            allRestrictions.addAll((List<String>) hereValidation.get("violations")); // Violations are critical restrictions
        }

        googleRoute.put("warnings", allWarnings);
        googleRoute.put("restrictions", allRestrictions);
        googleRoute.put("violations", allViolations);
        googleRoute.put("validation_source", hereValidation.get("source"));
        googleRoute.put("here_validation", hereValidation);

        // Informacje o pojeździe
        googleRoute.put("transport_set_info", Map.of(
                "id", transportSet.getId(),
                "description", transportSet.getDescription(),
                "weight_kg", transportSet.getTotalWeightKg(),
                "height_cm", transportSet.getTotalHeightCm(),
                "axle_load_kg", transportSet.getMaxAxleLoadKg(),
                "transporter", transportSet.getTransporter().getModel(),
                "cargo", transportSet.getCargo().getModel()
        ));

        log.info("Enhanced Google Maps route with HERE validation: {} warnings, {} restrictions, {} violations",
                allWarnings.size(), allRestrictions.size(), allViolations.size());
    }

    /**
     * Sprawdza czy walidacja HERE zawiera krytyczne naruszenia
     */
    private boolean hasRestrictiveViolations(Map<String, Object> hereValidation) {
        Boolean hasViolations = (Boolean) hereValidation.get("hasViolations");
        Boolean routeAvailable = (Boolean) hereValidation.get("routeAvailable");

        return Boolean.TRUE.equals(hasViolations) || Boolean.FALSE.equals(routeAvailable);
    }

    /**
     * Dodaje podstawową walidację pojazdu (fallback gdy HERE Maps niedostępne)
     */
    private void addBasicVehicleValidation(Map<String, Object> googleRoute, TransportSet transportSet) {
        List<String> warnings = new ArrayList<>();
        List<String> restrictions = new ArrayList<>();

        // Walidacja wysokości
        if (transportSet.getTotalHeightCm() > 400) {
            warnings.add("Wysokość pojazdu > 4m - sprawdź przepusty i mosty");
            if (transportSet.getTotalHeightCm() > 450) {
                restrictions.add("Wysokość > 4.5m - możliwe ograniczenia na autostradach");
            }
        }

        // Walidacja masy
        if (transportSet.getTotalWeightKg() > 40000) {
            warnings.add("Masa pojazdu > 40t - sprawdź nośność mostów");
            if (transportSet.getTotalWeightKg() > 60000) {
                restrictions.add("Masa > 60t - wymagane specjalne pozwolenia");
            }
        }

        // Walidacja nacisku na oś
        if (transportSet.getMaxAxleLoadKg() > 11500) {
            warnings.add("Nacisk na oś > 11.5t - sprawdź ograniczenia drogowe");
        }

        // Specjalne ostrzeżenia dla sprzętu wojskowego
        String cargoModel = transportSet.getCargo().getModel().toLowerCase();
        if (cargoModel.contains("leopard") || cargoModel.contains("tank")) {
            restrictions.add("Transport czołgu - wymagane pozwolenia specjalne");
            warnings.add("Transport czołgu - sprawdź nośność wszystkich mostów");
        }
        if (cargoModel.contains("radar") || cargoModel.contains("pilica")) {
            warnings.add("Sprzęt radarowy - zwiększona wysokość, sprawdź przepusty");
        }
        if (cargoModel.contains("krab") || cargoModel.contains("haubica")) {
            warnings.add("Haubica - sprawdź ograniczenia wagowe i manewrowe");
        }

        googleRoute.put("hasRestrictions", !restrictions.isEmpty());
        googleRoute.put("hasWarnings", !warnings.isEmpty());
        googleRoute.put("hasViolations", false);
        googleRoute.put("warnings", warnings);
        googleRoute.put("restrictions", restrictions);
        googleRoute.put("violations", new ArrayList<>());
        googleRoute.put("validation_source", "basic_validation");

        log.info("Applied basic vehicle validation: {} warnings, {} restrictions", warnings.size(), restrictions.size());
    }

    /**
     * Geocoding address to coordinates
     */
    public Map<String, Object> geocodeAddress(String address) {
        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            throw new RuntimeException("Google Maps API not configured");
        }

        try {
            String url = String.format(
                    "%s/geocode/json?address=%s&key=%s&language=pl&components=country:PL",
                    apiKeysConfig.getGoogleMaps().getBaseUrl(),
                    encodeAddress(address),
                    apiKeysConfig.getGoogleMaps().getKey()
            );

            log.info("Geocoding address: {}", address);
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response == null) {
                throw new RuntimeException("Empty geocoding response");
            }

            String status = (String) response.get("status");
            if (!"OK".equals(status)) {
                throw new RuntimeException("Geocoding failed with status: " + status);
            }

            return response;

        } catch (Exception e) {
            log.error("Error geocoding address: {}", address, e);
            throw new RuntimeException("Failed to geocode address: " + address, e);
        }
    }

    /**
     * Koduje adres dla URL
     */
    private String encodeAddress(String address) {
        return address.replace(" ", "+").replace(",", "%2C");
    }

    // Utility methods
    public boolean isConfigured() {
        return apiKeysConfig.isGoogleMapsEnabled();
    }

    public String getApiStatus() {
        return apiKeysConfig.isGoogleMapsEnabled() ? "configured" : "not_configured";
    }
}