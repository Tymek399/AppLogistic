package com.military.applogistic.controller;

import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.Route;
import com.military.applogistic.repository.RouteRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import lombok.RequiredArgsConstructor;

@Controller
@RequiredArgsConstructor
public class NavigationController {

    private final RouteRepository routeRepository;
    private final ApiKeysConfig apiKeysConfig;

    @GetMapping("/navigation/{routeId}")
    public String navigation(@PathVariable Long routeId, Model model) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        model.addAttribute("route", route);
        model.addAttribute("googleMapsKey", apiKeysConfig.getGoogleMaps().getKey());

        return "navigation"; // templates/navigation.html
    }
}