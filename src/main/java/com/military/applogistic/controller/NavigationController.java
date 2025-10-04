package com.military.applogistic.controller;

import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.Route;
import com.military.applogistic.repository.RouteRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.util.Map;

@Controller
@RequiredArgsConstructor
@Slf4j
public class NavigationController {

    private final RouteRepository routeRepository;
    private final ApiKeysConfig apiKeysConfig;
    private final ObjectMapper objectMapper = new ObjectMapper();

    @GetMapping("/navigation/{routeId}")
    public String navigation(@PathVariable Long routeId, Model model) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        model.addAttribute("route", route);
        model.addAttribute("googleMapsKey", apiKeysConfig.getGoogleMaps().getKey());
        try {
            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(
                        route.getRouteDataJson(), Map.class);

                if (routeData.containsKey("routes")) {
                    model.addAttribute("savedRoute", routeData.get("routes"));
                    log.info("✅ Załadowano zapisaną trasę dla nawigacji (route #{})", routeId);
                }
            }
        } catch (Exception e) {
            log.error("Błąd odczytu zapisanej trasy: {}", e.getMessage());
        }

        return "navigation";
    }
}