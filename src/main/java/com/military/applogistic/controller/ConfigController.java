package com.military.applogistic.controller;

import com.military.applogistic.service.config.ConfigService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/config")
@RequiredArgsConstructor
public class ConfigController {

    private final ConfigService configService;

    @GetMapping("/google-maps-key")
    @PreAuthorize("hasAnyRole('OPERATOR', 'DRIVER', 'ADMIN')")
    public ResponseEntity<String> getGoogleMapsKey() {
        return configService.getGoogleMapsKeyResponse();
    }

    @GetMapping("/api-status")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Map<String, Object>> getApiStatus() {
        return configService.getApiStatusResponse();
    }
}
