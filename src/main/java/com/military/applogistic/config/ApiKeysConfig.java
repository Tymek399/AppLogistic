package com.military.applogistic.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import lombok.Data;

@Configuration
@ConfigurationProperties(prefix = "api")
@Data
public class ApiKeysConfig {

    private GoogleMaps googleMaps = new GoogleMaps();
    private HereMaps hereMaps = new HereMaps();
    private Tomtom tomtom = new Tomtom();

    @Data
    public static class GoogleMaps {
        private String key = "";
        private boolean enabled = true;
        private String baseUrl = "https://maps.googleapis.com/maps/api";
    }

    @Data
    public static class HereMaps {
        private String key = "";
        private boolean enabled = true;
        private String baseUrl = "https://router.hereapi.com/v8";
    }

    @Data
    public static class Tomtom {
        private String key = "";
        private boolean enabled = false;
        private String baseUrl = "https://api.tomtom.com/routing/1";
    }

    // Metody pomocnicze
    public boolean isGoogleMapsEnabled() {
        return googleMaps.enabled && isValidKey(googleMaps.key);
    }

    public boolean isHereMapsEnabled() {
        return hereMaps.enabled && isValidKey(hereMaps.key);
    }

    public boolean isTomtomEnabled() {
        return tomtom.enabled && isValidKey(tomtom.key);
    }

    private boolean isValidKey(String key) {
        return key != null &&
                !key.isEmpty() &&
                !key.equals("demo_key_replace_with_real") &&
                key.length() > 10;
    }
}