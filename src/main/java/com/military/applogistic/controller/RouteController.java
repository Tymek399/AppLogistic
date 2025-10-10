package com.military.applogistic.controller;

import com.military.applogistic.entity.Route;
import com.military.applogistic.service.RouteService;
import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.repository.RouteRepository;
import jakarta.annotation.security.PermitAll;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.security.Principal;
import java.util.*;

@RestController
@RequestMapping("/api/routes")
@RequiredArgsConstructor
@Slf4j
public class RouteController {

    private final RouteService routeService;
    private final RouteRepository routeRepository;
    private final ObjectMapper objectMapper = new ObjectMapper();

    @PostMapping("/create")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> createRoute(
            @RequestBody CreateRouteRequest request,
            Principal principal) {

        log.info("Tworzenie trasy: {} -> {}", request.getStartAddress(), request.getEndAddress());

        try {
            RouteResponse response = routeService.createRoute(request, principal.getName());

            Route savedRoute = routeRepository.findById(response.getId())
                    .orElseThrow(() -> new RuntimeException("Route not found"));

            Map<String, Object> detailedResponse = new HashMap<>();
            detailedResponse.put("success", true);
            detailedResponse.put("route", response);
            detailedResponse.put("routeId", response.getId());

            if (savedRoute.getRouteDataJson() != null && !savedRoute.getRouteDataJson().equals("{}")) {
                try {
                    Map<String, Object> routeData = objectMapper.readValue(
                            savedRoute.getRouteDataJson(), Map.class);

                    // ✅ DANE GOOGLE MAPS DO WYŚWIETLENIA NA MAPIE
                    if (routeData.containsKey("routes")) {
                        detailedResponse.put("googleMapsRoute", routeData.get("routes"));
                    }

                    // ✅ POLYLINE DO NAWIGACJI
                    detailedResponse.put("routeSource", routeData.getOrDefault("routeSource", "GOOGLE_MAPS"));
                    if (routeData.containsKey("herePolyline")) {
                        detailedResponse.put("herePolyline", routeData.get("herePolyline"));
                    }

                    // ✅ PEŁNE INFORMACJE O WALIDACJI
                    Map<String, Object> validationInfo = new HashMap<>();
                    validationInfo.put("hasRestrictions", routeData.getOrDefault("hasRestrictions", false));
                    validationInfo.put("hasWarnings", routeData.getOrDefault("hasWarnings", false));
                    validationInfo.put("hasViolations", routeData.getOrDefault("hasViolations", false));
                    validationInfo.put("requiresPermit", routeData.getOrDefault("requiresPermit", false));
                    validationInfo.put("warnings", routeData.getOrDefault("warnings", new ArrayList<>()));
                    validationInfo.put("violations", routeData.getOrDefault("violations", new ArrayList<>()));

                    // ✅ KLUCZOWE: LISTA WYMAGANYCH POZWOLEŃ
                    List<String> permits = (List<String>) routeData.getOrDefault("permits", new ArrayList<>());
                    validationInfo.put("permits", permits);
                    validationInfo.put("permitsCount", permits.size());

                    // ✅ SZCZEGÓŁY INFRASTRUKTURY (mosty, tunele)
                    List<Map<String, Object>> infrastructure =
                            (List<Map<String, Object>>) routeData.getOrDefault("infrastructureDetails", new ArrayList<>());
                    validationInfo.put("infrastructureDetails", infrastructure);
                    validationInfo.put("infrastructureCount", infrastructure.size());

                    // ✅ POLICZ PRZEJEZDNE I ZABLOKOWANE
                    long passableCount = infrastructure.stream()
                            .filter(i -> Boolean.TRUE.equals(i.get("canPass")))
                            .count();
                    long blockedCount = infrastructure.stream()
                            .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                            .count();

                    validationInfo.put("passableInfrastructure", passableCount);
                    validationInfo.put("blockedInfrastructure", blockedCount);

                    // ✅ INFORMACJE O PRÓBACH SZUKANIA TRASY
                    validationInfo.put("searchAttempts", routeData.getOrDefault("searchAttempts", 1));
                    validationInfo.put("attemptReports", routeData.getOrDefault("attemptReports", new ArrayList<>()));

                    // ✅ CZY TO BYŁ LEKKI POJAZD (POMINIĘTA WALIDACJA)
                    validationInfo.put("lightVehicle", routeData.getOrDefault("lightVehicle", false));
                    validationInfo.put("validationSkipped", routeData.getOrDefault("validationSkipped", false));

                    detailedResponse.put("validation", validationInfo);

                    // ✅ METRYKI TRASY
                    Map<String, Object> metrics = new HashMap<>();
                    metrics.put("distance", savedRoute.getTotalDistanceKm());
                    metrics.put("estimatedTime", savedRoute.getEstimatedTimeMinutes());
                    detailedResponse.put("metrics", metrics);

                    // ✅ INFORMACJE O ZESTAWIE TRANSPORTOWYM
                    Map<String, Object> transportInfo = new HashMap<>();
                    transportInfo.put("id", savedRoute.getTransportSet().getId());
                    transportInfo.put("description", savedRoute.getTransportSet().getDescription());
                    transportInfo.put("weightKg", savedRoute.getTransportSet().getTotalWeightKg());
                    transportInfo.put("weightTon", savedRoute.getTransportSet().getTotalWeightKg() / 1000.0);
                    transportInfo.put("heightCm", savedRoute.getTransportSet().getTotalHeightCm());
                    transportInfo.put("heightM", savedRoute.getTransportSet().getTotalHeightCm() / 100.0);
                    transportInfo.put("lengthCm", savedRoute.getTransportSet().getTotalLengthCm());
                    transportInfo.put("widthCm", savedRoute.getTransportSet().getTotalWidthCm());
                    transportInfo.put("maxAxleLoadKg", savedRoute.getTransportSet().getMaxAxleLoadKg());
                    detailedResponse.put("transportSet", transportInfo);

                } catch (Exception e) {
                    log.error("Error parsing route data", e);
                }
            }

            return ResponseEntity.ok(detailedResponse);

        } catch (RuntimeException e) {
            log.error("Nie udało się utworzyć trasy: {}", e.getMessage());

            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("success", false);
            errorResponse.put("error", e.getMessage());
            errorResponse.put("detailedReport", e.getMessage());

            return ResponseEntity
                    .status(HttpStatus.BAD_REQUEST)
                    .body(errorResponse);
        }
    }

    /**
     * ✅ ENDPOINT - Szczegóły trasy z pełnymi informacjami dla operatora
     */
    @GetMapping("/{id}/details")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getRouteDetails(@PathVariable Long id) {
        try {
            Route route = routeRepository.findById(id)
                    .orElseThrow(() -> new RuntimeException("Route not found"));

            Map<String, Object> details = new HashMap<>();
            details.put("routeId", id);
            details.put("startAddress", route.getStartAddress());
            details.put("endAddress", route.getEndAddress());
            details.put("startLat", route.getStartLatitude());
            details.put("startLng", route.getStartLongitude());
            details.put("endLat", route.getEndLatitude());
            details.put("endLng", route.getEndLongitude());
            details.put("status", route.getStatus());
            details.put("createdBy", route.getCreatedByUsername());
            details.put("createdAt", route.getCreatedAt());
            details.put("assignedDriver", route.getAssignedDriverUsername());

            // ✅ METRYKI
            details.put("distance", route.getTotalDistanceKm());
            details.put("estimatedTime", route.getEstimatedTimeMinutes());

            // ✅ ZESTAW TRANSPORTOWY
            Map<String, Object> transportInfo = new HashMap<>();
            transportInfo.put("id", route.getTransportSet().getId());
            transportInfo.put("description", route.getTransportSet().getDescription());
            transportInfo.put("weightKg", route.getTransportSet().getTotalWeightKg());
            transportInfo.put("weightTon", route.getTransportSet().getTotalWeightKg() / 1000.0);
            transportInfo.put("heightCm", route.getTransportSet().getTotalHeightCm());
            transportInfo.put("heightM", route.getTransportSet().getTotalHeightCm() / 100.0);
            transportInfo.put("lengthCm", route.getTransportSet().getTotalLengthCm());
            transportInfo.put("widthCm", route.getTransportSet().getTotalWidthCm());
            transportInfo.put("maxAxleLoadKg", route.getTransportSet().getMaxAxleLoadKg());
            details.put("transportSet", transportInfo);

            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(
                        route.getRouteDataJson(), Map.class);

                // ✅ DANE DO MAPY
                if (routeData.containsKey("routes")) {
                    details.put("googleMapsRoute", routeData.get("routes"));
                }

                details.put("routeSource", routeData.getOrDefault("routeSource", "GOOGLE_MAPS"));

                if (routeData.containsKey("herePolyline")) {
                    details.put("herePolyline", routeData.get("herePolyline"));
                }

                // ✅ PEŁNA WALIDACJA
                Map<String, Object> validation = new HashMap<>();
                validation.put("hasRestrictions", routeData.getOrDefault("hasRestrictions", false));
                validation.put("hasWarnings", routeData.getOrDefault("hasWarnings", false));
                validation.put("requiresPermit", routeData.getOrDefault("requiresPermit", false));
                validation.put("warnings", routeData.getOrDefault("warnings", new ArrayList<>()));
                validation.put("violations", routeData.getOrDefault("violations", new ArrayList<>()));
                validation.put("permits", routeData.getOrDefault("permits", new ArrayList<>()));
                validation.put("infrastructureDetails", routeData.getOrDefault("infrastructureDetails", new ArrayList<>()));
                validation.put("searchAttempts", routeData.getOrDefault("searchAttempts", 1));
                validation.put("lightVehicle", routeData.getOrDefault("lightVehicle", false));

                details.put("validation", validation);
            }

            return ResponseEntity.ok(details);

        } catch (Exception e) {
            log.error("Błąd pobierania szczegółów trasy: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of("error", e.getMessage()));
        }
    }

    /**
     * ✅ ENDPOINT - Pełne dane nawigacyjne dla kierowcy
     */
    @GetMapping("/{id}/navigation-data")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getNavigationData(@PathVariable Long id) {
        try {
            Route route = routeRepository.findById(id)
                    .orElseThrow(() -> new RuntimeException("Route not found"));

            Map<String, Object> navigationData = new HashMap<>();
            navigationData.put("routeId", id);
            navigationData.put("startAddress", route.getStartAddress());
            navigationData.put("endAddress", route.getEndAddress());
            navigationData.put("startLat", route.getStartLatitude());
            navigationData.put("startLng", route.getStartLongitude());
            navigationData.put("endLat", route.getEndLatitude());
            navigationData.put("endLng", route.getEndLongitude());
            navigationData.put("status", route.getStatus());

            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(
                        route.getRouteDataJson(), Map.class);

                // ✅ PRIORYTET 1: HERE Maps polyline (zwalidowana trasa)
                if (routeData.containsKey("herePolyline")) {
                    navigationData.put("herePolyline", routeData.get("herePolyline"));
                    navigationData.put("routeSource", "HERE_MAPS_VALIDATED");
                    log.info("✅ Zwracam zwalidowaną trasę HERE Maps dla route #{}", id);
                }
                // ✅ PRIORYTET 2: Google Maps routes (zapisana trasa)
                else if (routeData.containsKey("routes")) {
                    navigationData.put("googleMapsRoutes", routeData.get("routes"));
                    navigationData.put("routeSource", "GOOGLE_MAPS");
                    log.info("🔵 Zwracam zapisaną trasę Google Maps dla route #{}", id);
                }

                // ✅ OSTRZEŻENIA I POZWOLENIA DLA KIEROWCY
                navigationData.put("validation", Map.of(
                        "hasWarnings", routeData.getOrDefault("hasWarnings", false),
                        "requiresPermit", routeData.getOrDefault("requiresPermit", false),
                        "warnings", routeData.getOrDefault("warnings", new ArrayList<>()),
                        "permits", routeData.getOrDefault("permits", new ArrayList<>()),
                        "routeJustification", routeData.getOrDefault("routeJustification", new ArrayList<>()),
                        "transportSetInfo", routeData.getOrDefault("transportSet", new HashMap<>())
                ));
            } else {
                navigationData.put("routeSource", "NONE");
                log.warn("⚠️ Brak zapisanych danych trasy dla route #{}", id);
            }

            return ResponseEntity.ok(navigationData);

        } catch (Exception e) {
            log.error("Błąd pobierania danych nawigacyjnych: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of("error", e.getMessage()));
        }
    }

    /**
     * ✅ ENDPOINT - Generowanie pliku GPX/KML z pełnymi danymi walidacji
     */
    @GetMapping("/{id}/navigation-file")
    @PermitAll
    public ResponseEntity<?> getNavigationFile(
            @PathVariable Long id,
            @RequestParam(defaultValue = "gpx") String format) {

        try {
            byte[] fileData = routeService.generateNavigationFile(id, format);

            String contentType;
            String fileName;

            switch (format.toLowerCase()) {
                case "gpx" -> {
                    contentType = "application/gpx+xml";
                    fileName = "route-" + id + ".gpx";
                }
                case "kml" -> {
                    contentType = "application/vnd.google-earth.kml+xml";
                    fileName = "route-" + id + ".kml";
                }
                default -> {
                    return ResponseEntity.badRequest()
                            .body(Map.of("error", "Unsupported format: " + format));
                }
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

    @GetMapping("/{id}/search-attempts")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getSearchAttempts(@PathVariable Long id) {
        try {
            Route route = routeRepository.findById(id)
                    .orElseThrow(() -> new RuntimeException("Route not found"));

            if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
                Map<String, Object> noData = new HashMap<>();
                noData.put("message", "Brak danych o próbach szukania trasy");
                return ResponseEntity.ok(noData);
            }

            Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

            Map<String, Object> attemptsReport = new HashMap<>();
            attemptsReport.put("routeId", id);
            attemptsReport.put("totalAttempts", routeData.get("searchAttempts"));
            attemptsReport.put("successfulAttempt", routeData.get("successfulAttempt"));
            attemptsReport.put("attempts", routeData.get("attemptReports"));

            return ResponseEntity.ok(attemptsReport);

        } catch (Exception e) {
            log.error("Błąd pobierania raportów prób: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    @GetMapping("/{id}/validation-details")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getValidationDetails(@PathVariable Long id) {
        Map<String, Object> details = routeService.getValidationDetails(id);
        return ResponseEntity.ok(details);
    }

    @PostMapping("/{routeId}/assign-driver")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> assignDriver(
            @PathVariable Long routeId,
            @RequestParam String driverUsername) {
        RouteResponse response = routeService.assignDriverToRoute(routeId, driverUsername);
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{routeId}/change-transport-set")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> changeTransportSet(
            @PathVariable Long routeId,
            @RequestParam Long transportSetId) {
        RouteResponse response = routeService.changeTransportSet(routeId, transportSetId);
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{routeId}/start")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<RouteResponse> startRoute(
            @PathVariable Long routeId,
            Principal principal) {
        RouteResponse response = routeService.startRoute(routeId, principal.getName());
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{routeId}/complete")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<RouteResponse> completeRoute(@PathVariable Long routeId) {
        RouteResponse response = routeService.completeRoute(routeId);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/my-routes")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<List<RouteResponse>> getMyAssignedRoutes(Principal principal) {
        List<RouteResponse> routes = routeService.getRoutesByDriver(principal.getName());
        return ResponseEntity.ok(routes);
    }

    @GetMapping("/all")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<RouteResponse>> getAllRoutes() {
        List<RouteResponse> routes = routeService.getAllRoutes();
        return ResponseEntity.ok(routes);
    }

    @GetMapping("/active")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<RouteResponse>> getActiveRoutes() {
        List<RouteResponse> routes = routeService.getActiveRoutes();
        return ResponseEntity.ok(routes);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<RouteResponse> getRoute(@PathVariable Long id) {
        RouteResponse route = routeService.getRouteById(id);
        return ResponseEntity.ok(route);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Void> deleteRoute(@PathVariable Long id) {
        routeService.deleteRoute(id);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{id}/revalidate")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> revalidateRoute(@PathVariable Long id) {
        RouteResponse response = routeService.revalidateRoute(id);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/{id}/alternatives")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<List<Map<String, Object>>> getAlternativeRoutes(@PathVariable Long id) {
        List<Map<String, Object>> alternatives = routeService.getAlternativeRoutes(id);
        return ResponseEntity.ok(alternatives);
    }

    @GetMapping("/validation-statistics")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getValidationStatistics() {
        Map<String, Object> stats = routeService.getValidationStatistics();
        return ResponseEntity.ok(stats);
    }
}