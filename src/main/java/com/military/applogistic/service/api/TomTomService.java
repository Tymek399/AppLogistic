package com.military.applogistic.service.api;

import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.MilitaryLoadCalculator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.math.BigDecimal;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class TomTomService {

    private final ApiKeysConfig apiKeysConfig;
    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper = new ObjectMapper();

    public boolean isConfigured() {
        return apiKeysConfig.getTomtom() != null &&
                apiKeysConfig.getTomtom().getKey() != null &&
                !apiKeysConfig.getTomtom().getKey().isEmpty();
    }

    public Map<String, Object> validateRoute(double startLat, double startLng,
                                             double endLat, double endLng,
                                             TransportSet transportSet) {
        if (!isConfigured()) {
            log.warn("TomTom API not configured");
            return Collections.emptyMap();
        }

        try {
            String url = buildTomTomUrl(startLat, startLng, endLat, endLng, transportSet);
            log.info("🗺️ Calling TomTom API for infrastructure validation");
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);
            return analyzeTomTomResponse(response, transportSet);
        } catch (Exception e) {
            log.error("TomTom API error: {}", e.getMessage());
            return Collections.emptyMap();
        }
    }

    private String buildTomTomUrl(double startLat, double startLng,
                                  double endLat, double endLng,
                                  TransportSet transportSet) {
        StringBuilder url = new StringBuilder();
        url.append("https://api.tomtom.com/routing/1/calculateRoute/");
        url.append(String.format("%.6f,%.6f:%.6f,%.6f", startLat, startLng, endLat, endLng));
        url.append("/json");
        url.append("?key=").append(apiKeysConfig.getTomtom().getKey());

        // Parametry pojazdu
        url.append("&vehicleWeight=").append(transportSet.getTotalWeightKg());
        url.append("&vehicleHeight=").append(transportSet.getTotalHeightCm() / 100.0);
        url.append("&vehicleWidth=").append(transportSet.getTotalWidthCm() / 100.0);
        url.append("&vehicleLength=").append(transportSet.getTotalLengthCm() / 100.0);
        url.append("&vehicleCommercial=true");

        // Dodatkowe parametry
        url.append("&routeType=safest");
        url.append("&traffic=true");
        url.append("&travelMode=truck");
        url.append("&avoid=unpavedRoads");
        url.append("&language=pl-PL");
        url.append("&sectionType=traffic,country,tollRoad,motorway,tunnel");
        return url.toString();
    }

    private Map<String, Object> analyzeTomTomResponse(Map<String, Object> response,
                                                      TransportSet transportSet) {
        Map<String, Object> validation = new HashMap<>();
        List<String> warnings = new ArrayList<>();
        List<Map<String, Object>> restrictions = new ArrayList<>();

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> route = routes.get(0);

                // Sekcje
                List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");
                if (sections != null) {
                    for (Map<String, Object> section : sections) {
                        String sectionType = (String) section.get("sectionType");
                        if ("tunnel".equals(sectionType)) {
                            Map<String, Object> tunnel = new HashMap<>();
                            tunnel.put("type", "TUNNEL");
                            tunnel.put("name", section.get("name"));
                            tunnel.put("length", section.get("lengthInMeters"));
                            restrictions.add(tunnel);
                            warnings.add("🚇 Tunel na trasie: " + section.get("name"));
                        }
                        if ("tollRoad".equals(sectionType)) {
                            warnings.add("💰 Odcinek płatny");
                        }
                    }
                }

                // Guidance
                Map<String, Object> guidance = (Map<String, Object>) route.get("guidance");
                if (guidance != null) {
                    List<Map<String, Object>> instructions =
                            (List<Map<String, Object>>) guidance.get("instructions");
                    if (instructions != null) {
                        for (Map<String, Object> instruction : instructions) {
                            String message = (String) instruction.get("message");
                            if (message != null) {
                                if (message.toLowerCase().contains("most")) {
                                    warnings.add("🌉 " + message);
                                }
                                if (message.toLowerCase().contains("tunel")) {
                                    warnings.add("🚇 " + message);
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error analyzing TomTom response: {}", e.getMessage());
        }

        validation.put("provider", "TomTom");
        validation.put("warnings", warnings);
        validation.put("restrictions", restrictions);

        return validation;
    }

    /**
     * Konwersja odpowiedzi TomTom na BridgeSpecification.
     */
    public MilitaryLoadCalculator.BridgeSpecification toBridgeSpecification(Map<String, Object> validation,
                                                                            OverpassService.InfrastructurePoint point,
                                                                            TransportSet set) {
        return MilitaryLoadCalculator.BridgeSpecification.builder()
                .name(point.getName())
                .location(point.getRoadName())
                .maxWeight(BigDecimal.valueOf(set.getTotalWeightKg() / 1000.0))
                .maxHeight(BigDecimal.valueOf(set.getTotalHeightCm() / 100.0))
                .maxWidth(BigDecimal.valueOf(set.getTotalWidthCm() / 100.0))
                .maxAxleLoad(BigDecimal.valueOf(set.getMaxAxleLoadKg() / 1000.0))
                .bridgeType("tomtom_validated")
                .condition("validated_tomtom")
                .build();
    }
}
