package com.military.applogistic.mapper;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.entity.Location;
import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.RouteStatus;
import com.military.applogistic.entity.TransportSet;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
@Slf4j
public class RouteMapper {

    private final ObjectMapper objectMapper;

    /**
     * Mapuje encję Route na RouteResponse DTO
     */
    public RouteResponse toResponse(Route route) {
        return RouteResponse.builder()
                .id(route.getId())
                .startAddress(route.getStartAddress())
                .endAddress(route.getEndAddress())
                .status(route.getStatus().toString())
                .distance(RouteResponse.DistanceInfo.fromKilometers(route.getTotalDistanceKm()))
                .time(RouteResponse.TimeInfo.fromMinutes(route.getEstimatedTimeMinutes()))
                .assignedDriver(route.getAssignedDriverUsername())
                .hasRestrictions(extractHasRestrictions(route))
                .warnings(extractWarnings(route))
                .build();
    }

    /**
     * Mapuje CreateRouteRequest + dane na encję Route
     */
    public Route toEntity(CreateRouteRequest request,
                          TransportSet transportSet,
                          String createdByUsername,
                          Map<String, Object> routeData) {

        Route route = Route.builder()
                .startAddress(request.getStartAddress())
                .endAddress(request.getEndAddress())
                .startLocation(new Location(request.getStartLatitude(), request.getStartLongitude()))
                .endLocation(new Location(request.getEndLatitude(), request.getEndLongitude()))
                .transportSet(transportSet)
                .createdByUsername(createdByUsername)
                .status(RouteStatus.CREATED)
                .build();

        enrichWithRouteData(route, routeData);

        return route;
    }

    /**
     * Wzbogaca encję Route danymi z Google Maps / HERE / itp.
     */
    private void enrichWithRouteData(Route route, Map<String, Object> routeData) {
        try {
            route.setRouteDataJson(objectMapper.writeValueAsString(routeData));
            extractAndSetMetrics(route, routeData);
        } catch (Exception e) {
            log.warn("Failed to serialize route data", e);
            route.setRouteDataJson("{}");
        }
    }

    /**
     * Wyciąga i ustawia metryki trasy (dystans, czas) z JSON-a Google Maps API
     */
    private void extractAndSetMetrics(Route route, Map<String, Object> routeData) {
        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> firstRoute = routes.get(0);
                List<Map<String, Object>> legs = (List<Map<String, Object>>) firstRoute.get("legs");

                if (legs != null && !legs.isEmpty()) {
                    Map<String, Object> leg = legs.get(0);

                    // Dystans
                    Object distanceObj = leg.get("distance");
                    if (distanceObj instanceof Map<?, ?> distanceMap) {
                        Object valueObj = distanceMap.get("value");
                        if (valueObj instanceof Number number) {
                            route.setTotalDistanceKm(number.doubleValue() / 1000.0);
                        }
                    }

                    // Czas
                    Object durationObj = leg.get("duration");
                    if (durationObj instanceof Map<?, ?> durationMap) {
                        Object valueObj = durationMap.get("value");
                        if (valueObj instanceof Number number) {
                            route.setEstimatedTimeMinutes(number.intValue() / 60);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Cannot extract route metrics: {}", e.getMessage());
        }
    }

    /**
     * Wyciąga informację o ograniczeniach z JSON-a
     */
    private boolean extractHasRestrictions(Route route) {
        try {
            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
                Object hasRestrictions = routeData.get("hasRestrictions");
                if (hasRestrictions instanceof Boolean) {
                    return (Boolean) hasRestrictions;
                }
            }
        } catch (Exception e) {
            log.debug("Cannot extract restrictions flag", e);
        }
        return false;
    }

    /**
     * Wyciąga ostrzeżenia z JSON-a
     */
    private List<RouteResponse.RouteWarning> extractWarnings(Route route) {
        try {
            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
                Object warningsObj = routeData.get("warnings");
                if (warningsObj instanceof List<?> warningsList) {
                    // mapowanie prostych ostrzeżeń (stringów) na DTO
                    return warningsList.stream()
                            .filter(w -> w instanceof String)
                            .map(w -> RouteResponse.RouteWarning.builder()
                                    .description((String) w)
                                    .severity(RouteResponse.RouteWarning.WarningSeverity.INFO)
                                    .build())
                            .toList();
                }
            }
        } catch (Exception e) {
            log.debug("Cannot extract warnings", e);
        }
        return List.of();
    }
}
