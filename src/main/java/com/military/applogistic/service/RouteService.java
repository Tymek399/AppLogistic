package com.military.applogistic.service;

import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.dto.CreateRouteRequest;
import com.military.applogistic.dto.RouteResponse;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RouteService {

    private final RouteRepository routeRepository;
    private final TransportSetRepository transportSetRepository;
    private final GoogleMapsService googleMapsService;
    private final HereMapsService hereMapsService;
    private final ObjectMapper objectMapper = new ObjectMapper();

    public RouteResponse createRoute(CreateRouteRequest request, String createdByUsername) {
        log.info("Creating route from {} to {} for user {} with transport set {}",
                request.getStartAddress(), request.getEndAddress(), createdByUsername, request.getTransportSetId());

        TransportSet transportSet = transportSetRepository.findById(request.getTransportSetId())
                .orElseThrow(() -> new RuntimeException("Transport set not found with id: " + request.getTransportSetId()));

        // Pobierz trasę z Google Maps z pełną walidacją HERE Maps
        Map<String, Object> routeData = googleMapsService.getRoute(
                request.getStartAddress(),
                request.getEndAddress(),
                transportSet
        );

        // Zapisz szczegółowe dane walidacji
        Route route = buildRouteWithValidation(request, transportSet, createdByUsername, routeData);
        Route savedRoute = routeRepository.save(route);

        log.info("Route {} created successfully with validation data", savedRoute.getId());

        return convertToResponse(savedRoute, routeData);
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getRoutesByDriver(String driverUsername) {
        List<Route> routes = routeRepository.findByAssignedDriverUsername(driverUsername);
        return routes.stream()
                .map(this::convertToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getAllRoutes() {
        List<Route> routes = routeRepository.findAll();
        return routes.stream()
                .map(this::convertToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getActiveRoutes() {
        List<Route> routes = routeRepository.findActiveRoutes();
        return routes.stream()
                .map(this::convertToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public RouteResponse getRouteById(Long id) {
        Route route = routeRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Route not found with id: " + id));
        return convertToResponse(route);
    }

    public RouteResponse assignDriverToRoute(Long routeId, String driverUsername) {
        log.info("Assigning driver {} to route {}", driverUsername, routeId);

        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found with id: " + routeId));

        if (route.getAssignedDriverUsername() != null) {
            log.warn("Route {} already has assigned driver: {}", routeId, route.getAssignedDriverUsername());
        }

        route.setAssignedDriverUsername(driverUsername);
        route.setStatus(Route.RouteStatus.ASSIGNED);
        route.setAssignedAt(LocalDateTime.now());

        Route savedRoute = routeRepository.save(route);
        log.info("Driver {} assigned to route {} successfully", driverUsername, routeId);

        return convertToResponse(savedRoute);
    }

    public RouteResponse startRoute(Long routeId, String driverUsername) {
        log.info("Starting route {} by driver {}", routeId, driverUsername);

        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found with id: " + routeId));

        if (!driverUsername.equals(route.getAssignedDriverUsername())) {
            throw new RuntimeException("Driver is not assigned to this route");
        }

        route.setStatus(Route.RouteStatus.ACTIVE);
        route.setStartedAt(LocalDateTime.now());

        Route savedRoute = routeRepository.save(route);
        log.info("Route {} started successfully by driver {}", routeId, driverUsername);

        return convertToResponse(savedRoute);
    }

    public RouteResponse completeRoute(Long routeId) {
        log.info("Completing route {}", routeId);

        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found with id: " + routeId));

        route.setStatus(Route.RouteStatus.COMPLETED);
        route.setCompletedAt(LocalDateTime.now());

        Route savedRoute = routeRepository.save(route);
        log.info("Route {} completed successfully", routeId);

        return convertToResponse(savedRoute);
    }

    public void deleteRoute(Long routeId) {
        log.info("Deleting route {}", routeId);

        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found with id: " + routeId));

        if (route.getStatus() == Route.RouteStatus.ACTIVE) {
            throw new RuntimeException("Cannot delete active route");
        }

        routeRepository.delete(route);
        log.info("Route {} deleted successfully", routeId);
    }

    /**
     * Buduje Route z pełnymi danymi walidacji HERE Maps
     */
    private Route buildRouteWithValidation(CreateRouteRequest request, TransportSet transportSet,
                                           String createdByUsername, Map<String, Object> routeData) {
        Route route = new Route();
        route.setStartAddress(request.getStartAddress());
        route.setEndAddress(request.getEndAddress());
        route.setStartLatitude(request.getStartLatitude());
        route.setStartLongitude(request.getStartLongitude());
        route.setEndLatitude(request.getEndLatitude());
        route.setEndLongitude(request.getEndLongitude());
        route.setTransportSet(transportSet);
        route.setCreatedByUsername(createdByUsername);
        route.setStatus(Route.RouteStatus.CREATED);

        // Przechowuj kompletne dane trasy z walidacją jako JSON
        try {
            String routeDataJson = objectMapper.writeValueAsString(routeData);
            route.setRouteDataJson(routeDataJson);
            log.debug("Stored route data with validation: {} characters", routeDataJson.length());
        } catch (Exception e) {
            log.warn("Failed to serialize route data: {}", e.getMessage());
            route.setRouteDataJson("{}");
        }

        // Wyciągnij metryki z danych trasy
        extractRouteMetrics(route, routeData);

        return route;
    }

    /**
     * Wyciąga metryki trasy z danych Google Maps
     */
    private void extractRouteMetrics(Route route, Map<String, Object> routeData) {
        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> firstRoute = routes.get(0);
                List<Map<String, Object>> legs = (List<Map<String, Object>>) firstRoute.get("legs");
                if (legs != null && !legs.isEmpty()) {
                    Map<String, Object> leg = legs.get(0);

                    // Wyciągnij dystans
                    Object distanceObj = leg.get("distance");
                    if (distanceObj instanceof Map) {
                        Object valueObj = ((Map<String, Object>) distanceObj).get("value");
                        if (valueObj instanceof Number) {
                            route.setTotalDistanceKm(((Number) valueObj).doubleValue() / 1000.0);
                        }
                    }

                    // Wyciągnij czas
                    Object durationObj = leg.get("duration");
                    if (durationObj instanceof Map) {
                        Object valueObj = ((Map<String, Object>) durationObj).get("value");
                        if (valueObj instanceof Number) {
                            route.setEstimatedTimeMinutes(((Number) valueObj).intValue() / 60);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Failed to extract route metrics: {}", e.getMessage());
            // Ustaw wartości fallback
            route.setTotalDistanceKm(calculateStraightLineDistance(
                    route.getStartLatitude(), route.getStartLongitude(),
                    route.getEndLatitude(), route.getEndLongitude()
            ));
            route.setEstimatedTimeMinutes((int) (route.getTotalDistanceKm() * 1.2)); // przybliżona estymacja
        }
    }

    /**
     * Oblicza dystans w linii prostej
     */
    private double calculateStraightLineDistance(double lat1, double lon1, double lat2, double lon2) {
        double R = 6371; // Promień Ziemi w km
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);
        double a = Math.sin(dLat/2) * Math.sin(dLat/2) +
                Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
                        Math.sin(dLon/2) * Math.sin(dLon/2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
        return R * c;
    }

    /**
     * Konwertuje Route na RouteResponse z pełnymi danymi walidacji
     */
    private RouteResponse convertToResponse(Route route) {
        return convertToResponse(route, null);
    }

    private RouteResponse convertToResponse(Route route, Map<String, Object> routeData) {
        RouteResponse response = RouteResponse.from(route);

        // Dodaj informacje o walidacji z zapisanych danych lub z parametru
        try {
            Map<String, Object> validationData = routeData;

            // Jeśli nie przekazano danych, spróbuj odczytać z bazy
            if (validationData == null && route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                validationData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
            }

            if (validationData != null) {
                // Dodaj informacje o ograniczeniach
                if (validationData.containsKey("hasRestrictions")) {
                    response.setHasRestrictions((Boolean) validationData.get("hasRestrictions"));
                }

                if (validationData.containsKey("warnings")) {
                    Object warningsObj = validationData.get("warnings");
                    if (warningsObj instanceof List) {
                        response.setWarnings((List<String>) warningsObj);
                    }
                }

                // Dodaj dodatkowe informacje o walidacji
                if (validationData.containsKey("restrictions")) {
                    Object restrictionsObj = validationData.get("restrictions");
                    if (restrictionsObj instanceof List) {
                        List<String> restrictions = (List<String>) restrictionsObj;
                        response.setHasRestrictions(response.isHasRestrictions() || !restrictions.isEmpty());

                        // Połącz ograniczenia z ostrzeżeniami
                        List<String> allWarnings = response.getWarnings() != null ?
                                response.getWarnings() : new java.util.ArrayList<>();
                        allWarnings.addAll(restrictions);
                        response.setWarnings(allWarnings);
                    }
                }
            }

        } catch (Exception e) {
            log.warn("Error processing validation data for route {}: {}", route.getId(), e.getMessage());
        }

        return response;
    }

    /**
     * Waliduje trasę ponownie (np. przy zmianie parametrów pojazdu)
     */
    public RouteResponse revalidateRoute(Long routeId) {
        log.info("Revalidating route {}", routeId);

        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found with id: " + routeId));

        if (route.getTransportSet() == null) {
            throw new RuntimeException("Cannot revalidate route without transport set");
        }

        // Wykonaj ponowną walidację przez HERE Maps
        Map<String, Object> newValidation = hereMapsService.validateRouteRestrictions(
                route.getStartLatitude(), route.getStartLongitude(),
                route.getEndLatitude(), route.getEndLongitude(),
                route.getTransportSet()
        );

        // Zaktualizuj dane trasy
        try {
            Map<String, Object> routeData = route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}") ?
                    objectMapper.readValue(route.getRouteDataJson(), Map.class) : new java.util.HashMap<>();

            // Wzbogać o nowe dane walidacji
            routeData.putAll(newValidation);
            routeData.put("last_validation", java.time.LocalDateTime.now().toString());

            route.setRouteDataJson(objectMapper.writeValueAsString(routeData));
            routeRepository.save(route);

            log.info("Route {} revalidated successfully", routeId);
            return convertToResponse(route, routeData);

        } catch (Exception e) {
            log.error("Error updating route validation data", e);
            throw new RuntimeException("Failed to update route validation");
        }
    }

    /**
     * Pobiera alternatywne trasy dla danej trasy
     */
    public List<Map<String, Object>> getAlternativeRoutes(Long routeId) {
        log.info("Getting alternative routes for route {}", routeId);

        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found with id: " + routeId));

        if (route.getTransportSet() == null) {
            throw new RuntimeException("Cannot get alternatives without transport set");
        }

        return hereMapsService.getAlternativeRoutes(
                route.getStartLatitude(), route.getStartLongitude(),
                route.getEndLatitude(), route.getEndLongitude(),
                route.getTransportSet()
        );
    }

    /**
     * Sprawdza czy trasa ma krytyczne ograniczenia
     */
    public boolean hasRestrictiveViolations(Long routeId) {
        try {
            Route route = routeRepository.findById(routeId).orElse(null);
            if (route == null || route.getRouteDataJson() == null) {
                return false;
            }

            Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

            // Sprawdź czy są naruszenia (violations)
            Boolean hasViolations = (Boolean) routeData.get("hasViolations");
            if (Boolean.TRUE.equals(hasViolations)) {
                return true;
            }

            // Sprawdź czy trasa jest dostępna
            Boolean routeAvailable = (Boolean) routeData.get("routeAvailable");
            if (Boolean.FALSE.equals(routeAvailable)) {
                return true;
            }

            // Sprawdź restrykcyjne ostrzeżenia
            Object restrictions = routeData.get("restrictions");
            if (restrictions instanceof List) {
                List<String> restrictionList = (List<String>) restrictions;
                return restrictionList.stream()
                        .anyMatch(r -> r.toLowerCase().contains("naruszenie") ||
                                r.toLowerCase().contains("violation") ||
                                r.toLowerCase().contains("prohibited"));
            }

            return false;

        } catch (Exception e) {
            log.warn("Error checking route violations for route {}: {}", routeId, e.getMessage());
            return false;
        }
    }

    /**
     * Pobiera statystyki walidacji tras
     */
    public Map<String, Object> getValidationStatistics() {
        List<Route> allRoutes = routeRepository.findAll();

        long totalRoutes = allRoutes.size();
        long routesWithRestrictions = 0;
        long routesWithWarnings = 0;
        long routesWithViolations = 0;
        long routesValidatedByHere = 0;

        for (Route route : allRoutes) {
            try {
                if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                    Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

                    if (Boolean.TRUE.equals(routeData.get("hasRestrictions"))) {
                        routesWithRestrictions++;
                    }

                    if (Boolean.TRUE.equals(routeData.get("hasWarnings"))) {
                        routesWithWarnings++;
                    }

                    if (Boolean.TRUE.equals(routeData.get("hasViolations"))) {
                        routesWithViolations++;
                    }

                    String validationSource = (String) routeData.get("validation_source");
                    if ("here_maps_api".equals(validationSource)) {
                        routesValidatedByHere++;
                    }
                }
            } catch (Exception e) {
                log.warn("Error processing route {} for statistics", route.getId());
            }
        }

        Map<String, Object> stats = new java.util.HashMap<>();
        stats.put("total_routes", totalRoutes);
        stats.put("routes_with_restrictions", routesWithRestrictions);
        stats.put("routes_with_warnings", routesWithWarnings);
        stats.put("routes_with_violations", routesWithViolations);
        stats.put("routes_validated_by_here", routesValidatedByHere);
        stats.put("here_maps_coverage", totalRoutes > 0 ? (double) routesValidatedByHere / totalRoutes * 100 : 0);

        return stats;
    }
}