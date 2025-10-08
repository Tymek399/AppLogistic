package com.military.applogistic.service;

import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.RouteStatus;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.mapper.RouteMapper;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.service.api.GoogleMapsService;
import com.military.applogistic.service.api.HereMapsService;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
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
    private final RouteMapper routeMapper;
    private final RouteCreationService routeCreationService;
    private final RouteValidationService routeValidationService;
    private final NavigationFileService navigationFileService;
    private final ObjectMapper objectMapper;

    private static final int LIGHT_VEHICLE_THRESHOLD_KG = 5000;

    /**
     * Tworzy nową trasę z walidacją
     */
    public RouteResponse createRoute(CreateRouteRequest request, String createdByUsername) {
        log.info("Creating route from {} to {}", request.getStartAddress(), request.getEndAddress());

        TransportSet transportSet = findTransportSetOrThrow(request.getTransportSetId());

        logTransportSetParameters(transportSet);

        if (transportSet.getTotalWeightKg() <= LIGHT_VEHICLE_THRESHOLD_KG) {
            log.info("🚗 LIGHT VEHICLE (≤5t) - SKIPPING BRIDGE VALIDATION");
            return routeCreationService.createLightVehicleRoute(request, transportSet, createdByUsername);
        }

        return routeCreationService.createHeavyVehicleRoute(request, transportSet, createdByUsername);
    }

    public ResponseEntity<Map<String, Object>> createRouteResponse(CreateRouteRequest request, String createdByUsername) {
        log.info("Tworzenie trasy: {} -> {}", request.getStartAddress(), request.getEndAddress());
        try {
            RouteResponse response = createRoute(request, createdByUsername);
            Route savedRoute = findRouteOrThrow(response.getId());
            Map<String, Object> routeData = parseRouteData(savedRoute);

            Map<String, Object> detailedResponse = new HashMap<>();
            detailedResponse.put("success", true);
            detailedResponse.put("route", response);
            detailedResponse.put("routeId", response.getId());

            if (routeData != null) {
                if (routeData.containsKey("routes")) {
                    detailedResponse.put("googleMapsRoute", routeData.get("routes"));
                }

                detailedResponse.put("validation", Map.of(
                        "hasRestrictions", routeData.getOrDefault("hasRestrictions", false),
                        "hasWarnings", routeData.getOrDefault("hasWarnings", false),
                        "hasViolations", routeData.getOrDefault("hasViolations", false),
                        "warnings", routeData.getOrDefault("warnings", new ArrayList<>()),
                        "routeJustification", routeData.getOrDefault("routeJustification", new ArrayList<>())
                ));
            }

            return ResponseEntity.ok(detailedResponse);
        } catch (RuntimeException e) {
            log.error("Nie udało się utworzyć trasy: {}", e.getMessage());

            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("success", false);
            errorResponse.put("error", e.getMessage());
            errorResponse.put("detailedReport", e.getMessage());

            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse);
        }
    }

    /**
     * Generuje plik nawigacyjny (GPX/KML)
     */
    public byte[] generateNavigationFile(Long routeId, String format) {
        Route route = findRouteOrThrow(routeId);
        return navigationFileService.generate(route, format);
    }

    public ResponseEntity<?> getNavigationFileResponse(Long id, String format) {
        try {
            byte[] fileData = generateNavigationFile(id, format);
            format = format.toLowerCase();

            String contentType;
            String fileName;

            switch (format) {
                case "gpx":
                    contentType = "application/gpx+xml";
                    fileName = "route-" + id + ".gpx";
                    break;
                case "kml":
                    contentType = "application/vnd.google-earth.kml+xml";
                    fileName = "route-" + id + ".kml";
                    break;
                default:
                    return ResponseEntity.badRequest()
                            .body(Map.of("error", "Unsupported format: " + format));
            }

            return ResponseEntity.ok()
                    .header("Content-Disposition", "attachment; filename=" + fileName)
                    .header("Content-Type", contentType)
                    .body(fileData);
        } catch (RuntimeException e) {
            log.error("Błąd generowania pliku nawigacyjnego: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of("error", e.getMessage()));
        }
    }

    /**
     * Pobiera szczegóły walidacji trasy
     */
    @Transactional(readOnly = true)
    public Map<String, Object> getValidationDetails(Long routeId) {
        Route route = findRouteOrThrow(routeId);
        return routeValidationService.extractValidationDetails(route);
    }

    public ResponseEntity<Map<String, Object>> getValidationDetailsResponse(Long id) {
        return ResponseEntity.ok(getValidationDetails(id));
    }

    @Transactional(readOnly = true)
    public Map<String, Object> getNavigationData(Long routeId) {
        Route route = findRouteOrThrow(routeId);

        Map<String, Object> navigationData = new HashMap<>();
        navigationData.put("routeId", routeId);
        navigationData.put("startAddress", route.getStartAddress());
        navigationData.put("endAddress", route.getEndAddress());
        navigationData.put("startLat", route.getStartLatitude());
        navigationData.put("startLng", route.getStartLongitude());
        navigationData.put("endLat", route.getEndLatitude());
        navigationData.put("endLng", route.getEndLongitude());
        navigationData.put("status", route.getStatus());

        Map<String, Object> routeData = parseRouteData(route);

        if (routeData != null) {
            if (routeData.containsKey("routes")) {
                navigationData.put("googleMapsRoutes", routeData.get("routes"));
            }

            navigationData.put("validation", Map.of(
                    "hasWarnings", routeData.getOrDefault("hasWarnings", false),
                    "warnings", routeData.getOrDefault("warnings", new ArrayList<>()),
                    "routeJustification", routeData.getOrDefault("routeJustification", new ArrayList<>())
            ));
        }

        return navigationData;
    }

    public ResponseEntity<Map<String, Object>> getNavigationDataResponse(Long id) {
        try {
            Map<String, Object> navigationData = getNavigationData(id);
            return ResponseEntity.ok(navigationData);
        } catch (RuntimeException e) {
            log.error("Błąd pobierania danych nawigacyjnych: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    @Transactional(readOnly = true)
    public Map<String, Object> getSearchAttempts(Long routeId) {
        Route route = findRouteOrThrow(routeId);

        Map<String, Object> routeData = parseRouteData(route);

        if (routeData == null) {
            Map<String, Object> noData = new HashMap<>();
            noData.put("message", "Brak danych o próbach szukania trasy");
            return noData;
        }

        Map<String, Object> attemptsReport = new HashMap<>();
        attemptsReport.put("routeId", routeId);
        attemptsReport.put("totalAttempts", routeData.get("searchAttempts"));
        attemptsReport.put("successfulAttempt", routeData.get("successfulAttempt"));
        attemptsReport.put("attempts", routeData.get("attemptReports"));

        return attemptsReport;
    }

    public ResponseEntity<Map<String, Object>> getSearchAttemptsResponse(Long id) {
        try {
            Map<String, Object> attemptsReport = getSearchAttempts(id);
            return ResponseEntity.ok(attemptsReport);
        } catch (RuntimeException e) {
            log.error("Błąd pobierania raportów prób: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * Przypisuje kierowcę do trasy
     */
    public RouteResponse assignDriverToRoute(Long routeId, String driverUsername) {
        Route route = findRouteOrThrow(routeId);

        route.setAssignedDriverUsername(driverUsername);
        route.setStatus(RouteStatus.ASSIGNED);
        route.setAssignedAt(LocalDateTime.now());

        Route savedRoute = routeRepository.save(route);
        return routeMapper.toResponse(savedRoute);
    }

    public ResponseEntity<RouteResponse> assignDriverToRouteResponse(Long routeId, String driverUsername) {
        return ResponseEntity.ok(assignDriverToRoute(routeId, driverUsername));
    }

    /**
     * Rozpoczyna trasę
     */
    public RouteResponse startRoute(Long routeId, String driverUsername) {
        Route route = findRouteOrThrow(routeId);

        validateDriverAssignment(route, driverUsername);

        route.setStatus(RouteStatus.ACTIVE);
        route.setStartedAt(LocalDateTime.now());

        Route savedRoute = routeRepository.save(route);
        return routeMapper.toResponse(savedRoute);
    }

    public ResponseEntity<RouteResponse> startRouteResponse(Long routeId, String driverUsername) {
        return ResponseEntity.ok(startRoute(routeId, driverUsername));
    }

    /**
     * Kończy trasę
     */
    public RouteResponse completeRoute(Long routeId) {
        Route route = findRouteOrThrow(routeId);

        route.setStatus(RouteStatus.COMPLETED);
        route.setCompletedAt(LocalDateTime.now());

        Route savedRoute = routeRepository.save(route);
        return routeMapper.toResponse(savedRoute);
    }

    public ResponseEntity<RouteResponse> completeRouteResponse(Long routeId) {
        return ResponseEntity.ok(completeRoute(routeId));
    }

    /**
     * Zmienia zestaw transportowy
     */
    public RouteResponse changeTransportSet(Long routeId, Long newTransportSetId) {
        Route route = findRouteOrThrow(routeId);

        validateRouteCanBeModified(route);

        TransportSet newTransportSet = findTransportSetOrThrow(newTransportSetId);
        route.setTransportSet(newTransportSet);

        Route savedRoute = routeRepository.save(route);
        return routeMapper.toResponse(savedRoute);
    }

    public ResponseEntity<RouteResponse> changeTransportSetResponse(Long routeId, Long newTransportSetId) {
        return ResponseEntity.ok(changeTransportSet(routeId, newTransportSetId));
    }

    /**
     * Ponowna walidacja trasy
     */
    public RouteResponse revalidateRoute(Long routeId) {
        Route route = findRouteOrThrow(routeId);
        return routeValidationService.revalidate(route, hereMapsService);
    }

    public ResponseEntity<RouteResponse> revalidateRouteResponse(Long id) {
        return ResponseEntity.ok(revalidateRoute(id));
    }

    /**
     * Usuwa trasę
     */
    public void deleteRoute(Long routeId) {
        if (!routeRepository.existsById(routeId)) {
            throw new RuntimeException("Route not found: " + routeId);
        }
        routeRepository.deleteById(routeId);
    }

    public ResponseEntity<Void> deleteRouteResponse(Long id) {
        deleteRoute(id);
        return ResponseEntity.ok().build();
    }

    // ==================== QUERY METHODS ====================

    @Transactional(readOnly = true)
    public List<RouteResponse> getRoutesByDriver(String driverUsername) {
        return routeRepository.findByAssignedDriverUsername(driverUsername)
                .stream()
                .map(routeMapper::toResponse)
                .collect(Collectors.toList());
    }

    public ResponseEntity<List<RouteResponse>> getRoutesByDriverResponse(String driverUsername) {
        return ResponseEntity.ok(getRoutesByDriver(driverUsername));
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getAllRoutes() {
        return routeRepository.findAll()
                .stream()
                .map(routeMapper::toResponse)
                .collect(Collectors.toList());
    }

    public ResponseEntity<List<RouteResponse>> getAllRoutesResponse() {
        return ResponseEntity.ok(getAllRoutes());
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getActiveRoutes() {
        return routeRepository.findActiveRoutes()
                .stream()
                .map(routeMapper::toResponse)
                .collect(Collectors.toList());
    }

    public ResponseEntity<List<RouteResponse>> getActiveRoutesResponse() {
        return ResponseEntity.ok(getActiveRoutes());
    }

    @Transactional(readOnly = true)
    public RouteResponse getRouteById(Long id) {
        Route route = findRouteOrThrow(id);
        return routeMapper.toResponse(route);
    }

    public ResponseEntity<RouteResponse> getRouteByIdResponse(Long id) {
        return ResponseEntity.ok(getRouteById(id));
    }

    @Transactional(readOnly = true)
    public List<Map<String, Object>> getAlternativeRoutes(Long routeId) {
        Route route = findRouteOrThrow(routeId);
        return routeValidationService.getAlternativeRoutes(route, hereMapsService);
    }

    public ResponseEntity<List<Map<String, Object>>> getAlternativeRoutesResponse(Long id) {
        return ResponseEntity.ok(getAlternativeRoutes(id));
    }

    @Transactional(readOnly = true)
    public Map<String, Object> getValidationStatistics() {
        return routeValidationService.calculateStatistics(routeRepository.findAll());
    }

    public ResponseEntity<Map<String, Object>> getValidationStatisticsResponse() {
        return ResponseEntity.ok(getValidationStatistics());
    }

    // ==================== HELPER METHODS ====================

    private Route findRouteOrThrow(Long routeId) {
        return routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found: " + routeId));
    }

    private TransportSet findTransportSetOrThrow(Long transportSetId) {
        return transportSetRepository.findById(transportSetId)
                .orElseThrow(() -> new RuntimeException("Transport set not found: " + transportSetId));
    }

    private void validateDriverAssignment(Route route, String driverUsername) {
        if (!driverUsername.equals(route.getAssignedDriverUsername())) {
            throw new IllegalStateException("Driver not assigned to this route");
        }
    }

    private void validateRouteCanBeModified(Route route) {
        if (route.getStatus() != RouteStatus.CREATED) {
            throw new IllegalStateException("Can only modify CREATED routes");
        }
    }

    private void logTransportSetParameters(TransportSet transportSet) {
        log.info("╔═══════════════════════════════════════════╗");
        log.info("TRANSPORT SET: {}", transportSet.getDescription());
        log.info("Weight: {} kg ({} tons)",
                transportSet.getTotalWeightKg(),
                transportSet.getTotalWeightKg() / 1000.0);
        log.info("Height: {} cm", transportSet.getTotalHeightCm());
        log.info("╚═══════════════════════════════════════════╝");
    }

    private Map<String, Object> parseRouteData(Route route) {
        if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
            return null;
        }
        try {
            return objectMapper.readValue(route.getRouteDataJson(), Map.class);
        } catch (Exception e) {
            log.error("Error parsing route data", e);
            return null;
        }
    }
}