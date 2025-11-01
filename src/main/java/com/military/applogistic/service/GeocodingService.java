package com.military.applogistic.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.military.applogistic.config.ApiKeysConfig;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * ✅ NOWY SERWIS - Naprawia problem z parsowaniem adresów
 *
 * PROBLEM: "ul. Łódzka, Gdańsk" było parsowane jako "Łódź"
 * ROZWIĄZANIE: Używamy Google Geocoding API do weryfikacji pełnych adresów
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GeocodingService {

    private final ApiKeysConfig apiKeysConfig;
    private final RestTemplate restTemplate = new RestTemplate();
    private final ObjectMapper objectMapper = new ObjectMapper();

    public GeocodingResult geocodeAddress(String address) {
        if (address == null || address.trim().isEmpty()) {
            return createFallbackResult(address);
        }

        if (!apiKeysConfig.isGoogleMapsEnabled()) {
            log.warn("Google Maps API wyłączone - zwracam oryginalny adres");
            return createFallbackResult(address);
        }

        try {
            String url = UriComponentsBuilder
                    .fromHttpUrl(apiKeysConfig.getGoogleMaps().getBaseUrl() + "/geocode/json")
                    .queryParam("address", address)
                    .queryParam("key", apiKeysConfig.getGoogleMaps().getKey())
                    .queryParam("language", "pl")
                    .queryParam("region", "pl")
                    .build()
                    .toUriString();

            log.info("🔍 Geokodowanie: '{}'", address);
            String response = restTemplate.getForObject(url, String.class);
            JsonNode root = objectMapper.readTree(response);

            String status = root.get("status").asText();
            if (!"OK".equals(status)) {
                log.warn("❌ Geocoding status: {}", status);
                return createFallbackResult(address);
            }

            JsonNode result = root.get("results").get(0);
            String formattedAddress = result.get("formatted_address").asText();

            GeocodingResult geocodingResult = new GeocodingResult();
            geocodingResult.setFormattedAddress(formattedAddress);
            geocodingResult.setOriginalAddress(address);

            log.info("✅ '{}' → '{}'", address, formattedAddress);
            return geocodingResult;

        } catch (Exception e) {
            log.error("❌ Błąd geocoding: {}", e.getMessage());
            return createFallbackResult(address);
        }
    }

    private GeocodingResult createFallbackResult(String address) {
        GeocodingResult result = new GeocodingResult();
        result.setFormattedAddress(address);
        result.setOriginalAddress(address);
        return result;
    }

    @Data
    public static class GeocodingResult {
        private String formattedAddress;
        private String originalAddress;
    }
}