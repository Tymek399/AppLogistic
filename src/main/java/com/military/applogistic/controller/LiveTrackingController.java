package com.military.applogistic.controller;

import com.military.applogistic.dto.LiveDriverInfo;
import com.military.applogistic.dto.PositionUpdate;
import com.military.applogistic.service.LiveTrackingService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/api/tracking")
@RequiredArgsConstructor
public class LiveTrackingController {

    private final LiveTrackingService trackingService;

    @PostMapping("/start-session")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<String> startNavigationSession(@RequestParam Long routeId, Principal principal) {
        return trackingService.startDriverSessionResponse(principal.getName(), routeId);
    }

    @PostMapping("/position")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<Void> updatePosition(@RequestBody PositionUpdate position, Principal principal) {
        return trackingService.updateDriverPositionResponse(principal.getName(), position);
    }

    @PostMapping("/end-session")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<Void> endNavigationSession(Principal principal) {
        return trackingService.endDriverSessionResponse(principal.getName());
    }

    @GetMapping("/active-drivers")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<List<LiveDriverInfo>> getActiveDrivers() {
        return trackingService.getAllActiveDriversResponse();
    }

    @GetMapping("/driver/{username}/status")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<LiveDriverInfo> getDriverStatus(@PathVariable String username) {
        return trackingService.getDriverStatusResponse(username);
    }

    @MessageMapping("/route-update")
    @SendTo("/topic/driver-routes")
    public LiveDriverInfo broadcastRouteUpdate(PositionUpdate position) {
        return trackingService.getUpdatedDriverInfo(position);
    }
}
