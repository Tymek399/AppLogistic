package com.military.applogistic.service.route;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.entity.Route;
import com.military.applogistic.mapper.RouteMapper;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.service.api.HereMapsService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.*;

/**
 * Serwis odpowiedzialny za walidację tras
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RouteValidationService {

    private final RouteRepository routeRepository;
    private final RouteMapper routeMapper;
    private final ObjectMapper objectMapper;

    /**
     * Wyciąga szczegóły walidacji z trasy
     */
    public Map<String, Object> extractValidationDetails(Route route) {
        try {
            if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
                return createNoDataResponse(route.getId());
            }

            Map<String, Object> routeData = objectMapper.readValue(
                    route.getRouteDataJson(), Map.class);

            Map<String, Object> validationDetails = new HashMap<>();
            validationDetails.put("routeId", route.getId());
            validationDetails.put("validationAvailable", true);
            validationDetails.put("validationSource", routeData.getOrDefault("validation_source", "unknown"));
            validationDetails.put("hasRestrictions", routeData.getOrDefault("hasRestrictions", false));
            validationDetails.put("hasWarnings", routeData.getOrDefault("hasWarnings", false));
            validationDetails.put("hasViolations", routeData.getOrDefault("hasViolations", false));
            validationDetails.put("warnings", routeData.getOrDefault("warnings", Collections.emptyList()));
            validationDetails.put("restrictions", routeData.getOrDefault("restrictions", Collections.emptyList()));
            validationDetails.put("violations", routeData.getOrDefault("violations", Collections.emptyList()));
            validationDetails.put("validationDetails", routeData.getOrDefault("validationDetails", Collections.emptyList()));
            validationDetails.put("transportSetInfo", routeData.getOrDefault("transportSet", Collections.emptyMap()));
            validationDetails.put("routeJustification", routeData.getOrDefault("routeJustification", Collections.emptyList()));
            validationDetails.put("searchAttempts", routeData.get("searchAttempts"));
            validationDetails.put("successfulAttempt", routeData.get("successfulAttempt"));

            return validationDetails;

        } catch (Exception e) {
            log.error("Error extracting validation details: {}", e.getMessage());
            return createErrorResponse(route.getId(), e);
        }
    }

    /**
     * Ponowna walidacja trasy
     */
    public RouteResponse revalidate(Route route, HereMapsService hereMapsService) {
        try {
            Map<String, Object> newValidation = hereMapsService.validateRouteRestrictions(
                    route.getStartLatitude(), route.getStartLongitude(),
                    route.getEndLatitude(), route.getEndLongitude(),
                    route.getTransportSet()
            );

            Map<String, Object> routeData = getRouteDataOrEmpty(route);
            routeData.putAll(newValidation);
            routeData.put("last_validation", LocalDateTime.now().toString());

            route.setRouteDataJson(objectMapper.writeValueAsString(routeData));
            routeRepository.save(route);

            return routeMapper.toResponse(route);

        } catch (Exception e) {
            throw new RuntimeException("Failed to update route validation", e);
        }
    }

    /**
     * Pobiera alternatywne trasy
     */
    public List<Map<String, Object>> getAlternativeRoutes(Route route, HereMapsService hereMapsService) {
        return hereMapsService.getAlternativeRoutes(
                route.getStartLatitude(), route.getStartLongitude(),
                route.getEndLatitude(), route.getEndLongitude(),
                route.getTransportSet()
        );
    }

    /**
     * Oblicza statystyki walidacji
     */
    public Map<String, Object> calculateStatistics(List<Route> allRoutes) {
        long totalRoutes = allRoutes.size();
        long routesWithRestrictions = 0;
        long routesWithWarnings = 0;
        long routesWithViolations = 0;

        for (Route route : allRoutes) {
            try {
                Map<String, Object> routeData = getRouteDataOrEmpty(route);
                if (Boolean.TRUE.equals(routeData.get("hasRestrictions"))) routesWithRestrictions++;
                if (Boolean.TRUE.equals(routeData.get("hasWarnings"))) routesWithWarnings++;
                if (Boolean.TRUE.equals(routeData.get("hasViolations"))) routesWithViolations++;
            } catch (Exception e) {
                log.debug("Could not parse route data for statistics", e);
            }
        }

        Map<String, Object> stats = new HashMap<>();
        stats.put("total_routes", totalRoutes);
        stats.put("routes_with_restrictions", routesWithRestrictions);
        stats.put("routes_with_warnings", routesWithWarnings);
        stats.put("routes_with_violations", routesWithViolations);
        return stats;
    }

    // ==================== HELPER METHODS ====================

    private Map<String, Object> getRouteDataOrEmpty(Route route) {
        try {
            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                return objectMapper.readValue(route.getRouteDataJson(), Map.class);
            }
        } catch (Exception e) {
            log.warn("Could not parse route data", e);
        }
        return new HashMap<>();
    }

    private Map<String, Object> createNoDataResponse(Long routeId) {
        Map<String, Object> response = new HashMap<>();
        response.put("routeId", routeId);
        response.put("validationAvailable", false);
        response.put("message", "Brak danych walidacji dla tej trasy");
        return response;
    }

    private Map<String, Object> createErrorResponse(Long routeId, Exception e) {
        Map<String, Object> response = new HashMap<>();
        response.put("routeId", routeId);
        response.put("validationAvailable", false);
        response.put("error", "Błąd podczas odczytu danych walidacji: " + e.getMessage());
        return response;
    }
}