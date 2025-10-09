package com.military.applogistic.service.navigation;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.Route;
import com.military.applogistic.repository.RouteRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class NavigationService {

    private final RouteRepository routeRepository;
    private final ApiKeysConfig apiKeysConfig;
    private final ObjectMapper objectMapper = new ObjectMapper();

    public Route getRouteById(Long routeId) {
        return routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
    }

    public Map<String, Object> getSavedRoute(Route route) {
        try {
            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

                if (routeData.containsKey("routes")) {
                    log.info("✅ Załadowano zapisaną trasę dla nawigacji (route #{})", route.getId());
                    return (Map<String, Object>) routeData.get("routes");
                }
            }
        } catch (Exception e) {
            log.error("Błąd odczytu zapisanej trasy: {}", e.getMessage());
        }
        return null;
    }

    public String getGoogleMapsKey() {
        return apiKeysConfig.getGoogleMaps().getKey();
    }
}
