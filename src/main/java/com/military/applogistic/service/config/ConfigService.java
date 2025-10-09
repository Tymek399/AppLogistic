package com.military.applogistic.service.config;

import com.military.applogistic.config.ApiKeysConfig;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.HashMap;

@Service
@RequiredArgsConstructor
@Slf4j
public class ConfigService {

    private final ApiKeysConfig apiKeysConfig;

    public ResponseEntity<String> getGoogleMapsKeyResponse() {
        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            log.error("Google Maps API not configured properly");
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body("Google Maps API not configured");
        }

        return ResponseEntity.ok(apiKeysConfig.getGoogleMaps().getKey());
    }

    public ResponseEntity<Map<String, Object>> getApiStatusResponse() {
        boolean googleMapsReady = apiKeysConfig.isGoogleMapsEnabled();
        boolean hereMapsReady = apiKeysConfig.isHereMapsEnabled();

        Map<String, Object> status = new HashMap<>();
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

        log.info("API status checked: systemReady={}, googleMaps={}, hereMaps={}",
                status.get("systemReady"), googleMapsReady, hereMapsReady);

        return ResponseEntity.ok(status);
    }
}
