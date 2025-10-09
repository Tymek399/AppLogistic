package com.military.applogistic.controller;

import com.military.applogistic.config.ApiKeysConfig;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.util.Map;
import java.util.HashMap;

@RestController
@RequestMapping("/api/config")
@RequiredArgsConstructor
@Slf4j
public class ConfigController {

    private final ApiKeysConfig apiKeysConfig;

    @GetMapping("/google-maps-key")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<String> getGoogleMapsKey() {
        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            log.error("Google Maps API not configured properly");
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body("Google Maps API not configured");
        }

        return ResponseEntity.ok(apiKeysConfig.getGoogleMaps().getKey());
    }

    @GetMapping("/api-status")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getApiStatus() {
        Map<String, Object> status = new HashMap<>();

        boolean googleMapsReady = apiKeysConfig.isGoogleMapsEnabled();
        boolean hereMapsReady = apiKeysConfig.isHereMapsEnabled();

        status.put("googleMaps", Map.of(
                "configured", googleMapsReady,
                "status", googleMapsReady ? "active" : "not_configured",
                "required", true
        ));

        status.put("hereMaps", Map.of(
                "configured", hereMapsReady,
                "status", hereMapsReady ? "active" : "not_configured",
                "required", true
        ));

        status.put("systemReady", googleMapsReady && hereMapsReady);

        return ResponseEntity.ok(status);
    }
}