package com.military.applogistic.service;

import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.mapper.RouteMapper;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.service.api.GoogleMapsService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Serwis odpowiedzialny za tworzenie tras
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RouteCreationService {

    private static final int MAX_ROUTE_ATTEMPTS = 10;

    private final GoogleMapsService googleMapsService;
    private final RouteRepository routeRepository;
    private final RouteMapper routeMapper;

    /**
     * Tworzy trasę dla lekkiego pojazdu (bez walidacji mostów)
     */
    public RouteResponse createLightVehicleRoute(CreateRouteRequest request,
                                                 TransportSet transportSet,
                                                 String createdByUsername) {
        try {
            Map<String, Object> routeData = googleMapsService.getRoute(
                    request.getStartAddress(),
                    request.getEndAddress(),
                    transportSet,
                    new HashSet<>()
            );

            enrichLightVehicleData(routeData);

            Route route = routeMapper.toEntity(request, transportSet, createdByUsername, routeData);
            Route savedRoute = routeRepository.save(route);

            log.info("✅ Light vehicle route created (ID: {})", savedRoute.getId());

            return routeMapper.toResponse(savedRoute);

        } catch (Exception e) {
            log.error("Error creating light vehicle route", e);
            throw new RuntimeException("Failed to create route: " + e.getMessage());
        }
    }

    /**
     * Tworzy trasę dla ciężkiego pojazdu (z pełną walidacją mostów)
     */
    public RouteResponse createHeavyVehicleRoute(CreateRouteRequest request,
                                                 TransportSet transportSet,
                                                 String createdByUsername) {

        List<RouteAttemptReport> allAttempts = new ArrayList<>();
        Set<String> excludedBridges = new HashSet<>();

        for (int attempt = 1; attempt <= MAX_ROUTE_ATTEMPTS; attempt++) {
            logAttempt(attempt, excludedBridges);

            try {
                Map<String, Object> routeData = googleMapsService.getRoute(
                        request.getStartAddress(),
                        request.getEndAddress(),
                        transportSet,
                        excludedBridges
                );

                RouteAttemptReport attemptReport = analyzeRouteAttempt(
                        routeData, attempt, excludedBridges, transportSet
                );
                allAttempts.add(attemptReport);

                if (attemptReport.isFullyPassable()) {
                    log.info("SUCCESS! Found passable route in attempt #{}", attempt);
                    return buildSuccessResponse(request, transportSet, createdByUsername,
                            routeData, allAttempts, attempt);
                }

                updateExcludedBridges(excludedBridges, attemptReport.getCriticalBridges());

            } catch (Exception e) {
                log.error("Error in attempt #{}: {}", attempt, e.getMessage());
                allAttempts.add(createErrorReport(attempt, e));
            }
        }

        log.error("NO PHYSICALLY POSSIBLE ROUTE after {} attempts", allAttempts.size());
        throw buildFailureException(transportSet, allAttempts);
    }

    // ==================== HELPER METHODS ====================

    private void enrichLightVehicleData(Map<String, Object> routeData) {
        routeData.put("lightVehicle", true);
        routeData.put("validationSkipped", true);
        routeData.put("reason", "Pojazd ≤5t - walidacja mostów pominięta");
        routeData.put("searchAttempts", 1);
        routeData.put("successfulAttempt", 1);
    }

    private void logAttempt(int attempt, Set<String> excludedBridges) {
        log.info("╔═══════════════════════════════════════════╗");
        log.info("ATTEMPT #{} - Searching for route...", attempt);
        if (!excludedBridges.isEmpty()) {
            log.info("Excluding {} bridges", excludedBridges.size());
        }
    }

    private RouteAttemptReport analyzeRouteAttempt(Map<String, Object> routeData,
                                                   int attemptNumber,
                                                   Set<String> excludedBridges,
                                                   TransportSet transportSet) {

        RouteAttemptReport report = new RouteAttemptReport();
        report.setAttemptNumber(attemptNumber);
        report.setExcludedBridges(new ArrayList<>(excludedBridges));

        List<String> violations = extractList(routeData, "violations");
        List<String> restrictions = extractList(routeData, "restrictions");
        List<Map<String, Object>> infrastructure = extractInfrastructureList(routeData);

        report.setViolations(violations);
        report.setRestrictions(restrictions);
        report.setTotalInfrastructureChecked(infrastructure.size());

        long blockedBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .count();

        report.setBlockedBridges((int) blockedBridges);
        report.setPassable(violations.isEmpty() && blockedBridges == 0);

        List<String> criticalBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .map(i -> (String) i.get("name"))
                .limit(5)
                .toList();

        report.setCriticalBridges(criticalBridges);

        return report;
    }

    private void updateExcludedBridges(Set<String> excludedBridges, List<String> criticalBridges) {
        if (criticalBridges.isEmpty()) {
            log.warn("No more route options - all alternatives exhausted");
            return;
        }

        int bridgesToExclude = Math.min(3, criticalBridges.size());
        for (int i = 0; i < bridgesToExclude; i++) {
            excludedBridges.add(criticalBridges.get(i));
        }
    }

    private RouteResponse buildSuccessResponse(CreateRouteRequest request,
                                               TransportSet transportSet,
                                               String createdByUsername,
                                               Map<String, Object> routeData,
                                               List<RouteAttemptReport> allAttempts,
                                               int successfulAttempt) {

        routeData.put("searchAttempts", allAttempts.size());
        routeData.put("successfulAttempt", successfulAttempt);
        routeData.put("attemptReports", allAttempts);

        Route route = routeMapper.toEntity(request, transportSet, createdByUsername, routeData);
        Route savedRoute = routeRepository.save(route);

        log.info("Route #{} created successfully after {} attempts",
                savedRoute.getId(), allAttempts.size());

        return routeMapper.toResponse(savedRoute);
    }

    private RouteAttemptReport createErrorReport(int attempt, Exception e) {
        RouteAttemptReport errorReport = new RouteAttemptReport();
        errorReport.setAttemptNumber(attempt);
        errorReport.setError(e.getMessage());
        return errorReport;
    }

    private RuntimeException buildFailureException(TransportSet transportSet,
                                                   List<RouteAttemptReport> allAttempts) {
        StringBuilder message = new StringBuilder();
        message.append("NO PHYSICALLY POSSIBLE ROUTE\n\n");
        message.append("Checked ").append(allAttempts.size()).append(" alternative routes\n");
        message.append("Transport set weight: ").append(transportSet.getTotalWeightKg() / 1000.0).append(" tons\n");

        return new RuntimeException(message.toString());
    }

    @SuppressWarnings("unchecked")
    private List<String> extractList(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof List) {
            return (List<String>) value;
        }
        return new ArrayList<>();
    }

    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> extractInfrastructureList(Map<String, Object> data) {
        Object value = data.get("infrastructureDetails");
        if (value instanceof List) {
            return (List<Map<String, Object>>) value;
        }
        return new ArrayList<>();
    }

    // ==================== INNER CLASS ====================

    /**
     * Raport z pojedynczej próby utworzenia trasy
     */
    public static class RouteAttemptReport {
        private int attemptNumber;
        private List<String> excludedBridges = new ArrayList<>();
        private int totalInfrastructureChecked;
        private int blockedBridges;
        private List<String> violations = new ArrayList<>();
        private List<String> restrictions = new ArrayList<>();
        private List<String> criticalBridges = new ArrayList<>();
        private boolean passable;
        private String error;

        public boolean isFullyPassable() {
            return passable && violations.isEmpty() && blockedBridges == 0;
        }

        // Getters and Setters
        public int getAttemptNumber() { return attemptNumber; }
        public void setAttemptNumber(int attemptNumber) { this.attemptNumber = attemptNumber; }
        public List<String> getExcludedBridges() { return excludedBridges; }
        public void setExcludedBridges(List<String> excludedBridges) { this.excludedBridges = excludedBridges; }
        public int getTotalInfrastructureChecked() { return totalInfrastructureChecked; }
        public void setTotalInfrastructureChecked(int totalInfrastructureChecked) { this.totalInfrastructureChecked = totalInfrastructureChecked; }
        public int getBlockedBridges() { return blockedBridges; }
        public void setBlockedBridges(int blockedBridges) { this.blockedBridges = blockedBridges; }
        public List<String> getViolations() { return violations; }
        public void setViolations(List<String> violations) { this.violations = violations; }
        public List<String> getRestrictions() { return restrictions; }
        public void setRestrictions(List<String> restrictions) { this.restrictions = restrictions; }
        public List<String> getCriticalBridges() { return criticalBridges; }
        public void setCriticalBridges(List<String> criticalBridges) { this.criticalBridges = criticalBridges; }
        public boolean isPassable() { return passable; }
        public void setPassable(boolean passable) { this.passable = passable; }
        public String getError() { return error; }
        public void setError(String error) { this.error = error; }
    }
}