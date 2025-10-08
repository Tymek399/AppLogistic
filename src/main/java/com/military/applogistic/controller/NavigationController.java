package com.military.applogistic.controller;

import com.military.applogistic.entity.Route;
import com.military.applogistic.service.NavigationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Controller
@RequiredArgsConstructor
@Slf4j
public class NavigationController {

    private final NavigationService navigationService;

    @GetMapping("/navigation/{routeId}")
    public String navigation(@PathVariable Long routeId, Model model) {
        Route route = navigationService.getRouteById(routeId);
        model.addAttribute("route", route);
        model.addAttribute("googleMapsKey", navigationService.getGoogleMapsKey());
        model.addAttribute("savedRoute", navigationService.getSavedRoute(route));
        return "navigation";
    }
}
