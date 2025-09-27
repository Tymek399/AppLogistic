package com.military.applogistic.controller;

import com.military.applogistic.entity.Route;
import com.military.applogistic.service.NavigationFileService;
import com.military.applogistic.service.RouteService;
import com.military.applogistic.dto.CreateRouteRequest;
import com.military.applogistic.dto.RouteResponse;
import com.military.applogistic.repository.RouteRepository;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import lombok.RequiredArgsConstructor;
import java.security.Principal;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

@RestController
@RequestMapping("/api/routes")
@RequiredArgsConstructor
public class RouteController {

    private final RouteService routeService;
    private final NavigationFileService navigationFileService;
    private final RouteRepository routeRepository;

    @PostMapping("/create")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> createRoute(@RequestBody CreateRouteRequest request, Principal principal) {
        RouteResponse response = routeService.createRoute(request, principal.getName());
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{routeId}/assign-driver")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> assignDriver(@PathVariable Long routeId, @RequestParam String driverUsername) {
        RouteResponse response = routeService.assignDriverToRoute(routeId, driverUsername);
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{routeId}/start")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<RouteResponse> startRoute(@PathVariable Long routeId, Principal principal) {
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

    @GetMapping("/{id}/navigation-file")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<byte[]> downloadNavigationFile(@PathVariable Long id, @RequestParam String format) {
        Route route = routeRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        byte[] file;
        String contentType;
        String filename;

        if ("kml".equalsIgnoreCase(format)) {
            file = navigationFileService.generateKMLFile(route);
            contentType = "application/vnd.google-earth.kml+xml";
            filename = "route_" + id + ".kml";
        } else {
            file = navigationFileService.generateGPXFile(route);
            contentType = "application/gpx+xml";
            filename = "route_" + id + ".gpx";
        }

        return ResponseEntity.ok()
                .header("Content-Type", contentType)
                .header("Content-Disposition", "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    // === NOWE ENDPOINTY DLA WALIDACJI HERE MAPS ===

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

    @GetMapping("/{id}/violations-check")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> checkRouteViolations(@PathVariable Long id) {
        boolean hasViolations = routeService.hasRestrictiveViolations(id);

        Map<String, Object> result = new HashMap<>();
        result.put("routeId", id);
        result.put("hasRestrictiveViolations", hasViolations);
        result.put("recommendation", hasViolations ?
                "Trasa ma krytyczne ograniczenia - rozważ trasę alternatywną" :
                "Trasa może być bezpiecznie realizowana");

        return ResponseEntity.ok(result);
    }

    @GetMapping("/validation-statistics")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getValidationStatistics() {
        Map<String, Object> stats = routeService.getValidationStatistics();
        return ResponseEntity.ok(stats);
    }

    @GetMapping("/{id}/validation-details")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getValidationDetails(@PathVariable Long id) {
        try {
            Route route = routeRepository.findById(id)
                    .orElseThrow(() -> new RuntimeException("Route not found"));

            if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
                Map<String, Object> noDataResponse = new HashMap<>();
                noDataResponse.put("routeId", id);
                noDataResponse.put("validationAvailable", false);
                noDataResponse.put("message", "Brak danych walidacji dla tej trasy");
                return ResponseEntity.ok(noDataResponse);
            }

            com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
            Map<String, Object> routeData = mapper.readValue(route.getRouteDataJson(), Map.class);

            Map<String, Object> validationDetails = new HashMap<>();
            validationDetails.put("routeId", id);
            validationDetails.put("validationAvailable", true);
            validationDetails.put("validationSource", routeData.getOrDefault("validation_source", "unknown"));
            validationDetails.put("hasRestrictions", routeData.getOrDefault("hasRestrictions", false));
            validationDetails.put("hasWarnings", routeData.getOrDefault("hasWarnings", false));
            validationDetails.put("hasViolations", routeData.getOrDefault("hasViolations", false));
            validationDetails.put("warnings", routeData.getOrDefault("warnings", Collections.emptyList()));
            validationDetails.put("restrictions", routeData.getOrDefault("restrictions", Collections.emptyList()));
            validationDetails.put("violations", routeData.getOrDefault("violations", Collections.emptyList()));
            validationDetails.put("hereValidation", routeData.getOrDefault("here_validation", Collections.emptyMap()));
            validationDetails.put("transportSetInfo", routeData.getOrDefault("transport_set_info", Collections.emptyMap()));

            return ResponseEntity.ok(validationDetails);

        } catch (Exception e) {
            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("routeId", id);
            errorResponse.put("validationAvailable", false);
            errorResponse.put("error", "Błąd podczas odczytu danych walidacji: " + e.getMessage());
            return ResponseEntity.ok(errorResponse);
        }
    }
}