package com.military.applogistic.controller;

import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.service.route.RouteService;
import jakarta.annotation.security.PermitAll;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import lombok.RequiredArgsConstructor;

import java.security.Principal;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/routes")
@RequiredArgsConstructor
public class RouteController {

    private final RouteService routeService;

    @PostMapping("/create")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> createRoute(
            @RequestBody CreateRouteRequest request,
            Principal principal) {
        return routeService.createRouteResponse(request, principal.getName());
    }

    @GetMapping("/{id}/navigation-file")
    @PermitAll
    public ResponseEntity<?> getNavigationFile(
            @PathVariable Long id,
            @RequestParam(defaultValue = "gpx") String format) {
        return routeService.getNavigationFileResponse(id, format);
    }

    @GetMapping("/{id}/navigation-data")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getNavigationData(@PathVariable Long id) {
        return routeService.getNavigationDataResponse(id);
    }

    @GetMapping("/{id}/search-attempts")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getSearchAttempts(@PathVariable Long id) {
        return routeService.getSearchAttemptsResponse(id);
    }

    @GetMapping("/{id}/validation-details")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getValidationDetails(@PathVariable Long id) {
        return routeService.getValidationDetailsResponse(id);
    }

    @PostMapping("/{routeId}/assign-driver")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> assignDriver(
            @PathVariable Long routeId,
            @RequestParam String driverUsername) {
        return routeService.assignDriverToRouteResponse(routeId, driverUsername);
    }

    @PostMapping("/{routeId}/change-transport-set")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> changeTransportSet(
            @PathVariable Long routeId,
            @RequestParam Long transportSetId) {
        return routeService.changeTransportSetResponse(routeId, transportSetId);
    }

    @PostMapping("/{routeId}/start")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<RouteResponse> startRoute(
            @PathVariable Long routeId,
            Principal principal) {
        return routeService.startRouteResponse(routeId, principal.getName());
    }

    @PostMapping("/{routeId}/complete")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<RouteResponse> completeRoute(@PathVariable Long routeId) {
        return routeService.completeRouteResponse(routeId);
    }

    @GetMapping("/my-routes")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<List<RouteResponse>> getMyAssignedRoutes(Principal principal) {
        return routeService.getRoutesByDriverResponse(principal.getName());
    }

    @GetMapping("/all")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<RouteResponse>> getAllRoutes() {
        return routeService.getAllRoutesResponse();
    }

    @GetMapping("/active")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<RouteResponse>> getActiveRoutes() {
        return routeService.getActiveRoutesResponse();
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<RouteResponse> getRoute(@PathVariable Long id) {
        return routeService.getRouteByIdResponse(id);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Void> deleteRoute(@PathVariable Long id) {
        return routeService.deleteRouteResponse(id);
    }

    @PostMapping("/{id}/revalidate")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<RouteResponse> revalidateRoute(@PathVariable Long id) {
        return routeService.revalidateRouteResponse(id);
    }

    @GetMapping("/{id}/alternatives")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<List<Map<String, Object>>> getAlternativeRoutes(@PathVariable Long id) {
        return routeService.getAlternativeRoutesResponse(id);
    }

    @GetMapping("/validation-statistics")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getValidationStatistics() {
        return routeService.getValidationStatisticsResponse();
    }
}