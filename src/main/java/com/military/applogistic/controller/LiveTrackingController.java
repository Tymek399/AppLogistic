package com.military.applogistic.controller;

import com.military.applogistic.dto.*;
import com.military.applogistic.service.LiveTrackingService;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import lombok.RequiredArgsConstructor;
import java.security.Principal;
import java.util.List;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.SendTo;

@RestController
@RequestMapping("/api/tracking")
@RequiredArgsConstructor
public class LiveTrackingController {

    private final LiveTrackingService trackingService;

    @PostMapping("/login")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<String> loginToNavigation(@RequestParam Long routeId, Principal principal) {
        trackingService.startDriverSession(principal.getName(), routeId);
        return ResponseEntity.ok("Navigation session started");
    }

    @PostMapping("/position")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<Void> updatePosition(@RequestBody PositionUpdate position, Principal principal) {
        trackingService.updateDriverPosition(principal.getName(), position);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/logout")
    @PreAuthorize("hasRole('DRIVER')")
    public ResponseEntity<Void> logoutFromNavigation(Principal principal) {
        trackingService.endDriverSession(principal.getName());
        return ResponseEntity.ok().build();
    }

    @GetMapping("/active-drivers")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<List<LiveDriverInfo>> getActiveDrivers() {
        List<LiveDriverInfo> drivers = trackingService.getAllActiveDrivers();
        return ResponseEntity.ok(drivers);
    }

    @GetMapping("/driver/{username}/status")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<LiveDriverInfo> getDriverStatus(@PathVariable String username) {
        LiveDriverInfo driverInfo = trackingService.getDriverStatus(username);
        return ResponseEntity.ok(driverInfo);
    }

    // WebSocket endpoint to broadcast driver position updates to operators
    @MessageMapping("/route-update")
    @SendTo("/topic/driver-routes")
    public LiveDriverInfo broadcastRouteUpdate(PositionUpdate position) {
        return trackingService.getUpdatedDriverInfo(position);
    }
}