package com.military.applogistic.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.military.applogistic.config.ApiKeysConfig;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.util.FlexiblePolyline;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
public class HereMapsService {

    private final ApiKeysConfig apiKeysConfig;
    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper;
    private final MilitaryRoadPermissions militaryRoadPermissions;

    private static final double EXCLUSION_RADIUS_KM = 0.5;

    public HereMapsService(ApiKeysConfig apiKeysConfig, RestTemplate restTemplate,
                           ObjectMapper objectMapper, MilitaryRoadPermissions militaryRoadPermissions) {
        this.apiKeysConfig = apiKeysConfig;
        this.restTemplate = restTemplate;
        this.objectMapper = objectMapper;
        this.militaryRoadPermissions = militaryRoadPermissions;
    }

    private String getApiKey() {
        return apiKeysConfig.getHereMaps().getKey();
    }

    /**
     * ✅ GŁÓWNA METODA WALIDACJI - Z TWARDYM BLOKOWANIEM
     */
    public Map<String, Object> validateRoute(
            double startLat, double startLng,
            double endLat, double endLng,
            int weightKg, int heightCm,
            Set<String> excludedNames) {

        log.info("🔍 Walidacja trasy: waga={}kg, wysokość={}cm", weightKg, heightCm);

        if (excludedNames != null && !excludedNames.isEmpty()) {
            log.info("🚫 Wykluczone obiekty ({}): {}", excludedNames.size(),
                    excludedNames.size() > 10 ? excludedNames.size() + " obiektów" : excludedNames);
        }

        UriComponentsBuilder builder = buildRouteRequest(startLat, startLng, endLat, endLng, weightKg, heightCm);
        String url = builder.toUriString();

        log.info("📡 HERE Maps API call: weight={}kg, height={}m", weightKg, heightCm / 100.0);

        try {
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response == null || !response.containsKey("routes")) {
                log.error("❌ HERE Maps nie zwróciło trasy");
                return buildBlockedResult("HERE Maps nie zwróciło odpowiedzi");
            }

            logHereRouteDetails(response);
            logViolations(response);
            compareTransportWithRoute(weightKg, heightCm, response);

            List<Map<String, Object>> notices = extractNotices(response);
            analyzeNotices(notices);

            // ✅ KLUCZOWA ZMIANA: Sprawdź violatedVehicleRestriction
            if (hasViolatedVehicleRestriction(notices)) {
                log.error("🚨 KRYTYCZNE: violatedVehicleRestriction - TRASA ZABLOKOWANA");
                return buildBlockedResult("Naruszono ograniczenia pojazdu - przejazd niemożliwy");
            }

            // ✅ Sprawdź wykluczenia
            if (excludedNames != null && !excludedNames.isEmpty()) {
                List<ExcludedPoint> excludedPoints = findExcludedPointsOnRoute(response, excludedNames);

                if (!excludedPoints.isEmpty()) {
                    log.warn("⚠️ Znaleziono {} wykluczonych obiektów na trasie", excludedPoints.size());
                    return recalculateWithExcludedPoints(
                            startLat, startLng, endLat, endLng,
                            weightKg, heightCm, excludedPoints, response
                    );
                }
            }

            String polyline = extractPolyline(response);

            Map<String, Object> result = new HashMap<>();
            result.put("polyline", polyline);
            result.put("herePolyline", polyline);
            result.put("violations", 0);
            result.put("restrictions", 0);
            result.put("warnings", notices.size());
            result.put("notices", notices);
            result.put("validationSource", "HERE_MAPS");
            result.put("routeBlocked", false);

            log.info("✅ Walidacja zakończona: {} ostrzeżeń", notices.size());

            return result;

        } catch (Exception e) {
            log.error("❌ Błąd HERE Maps API", e);
            return buildBlockedResult("Błąd API: " + e.getMessage());
        }
    }

    /**
     * ✅ NOWA METODA: Sprawdza czy jest violatedVehicleRestriction
     */
    private boolean hasViolatedVehicleRestriction(List<Map<String, Object>> notices) {
        for (Map<String, Object> notice : notices) {
            String code = (String) notice.get("code");
            if ("violatedVehicleRestriction".equals(code)) {
                return true;
            }
        }
        return false;
    }

    /**
     * ✅ ZAKTUALIZOWANA METODA: Buduje wynik zablokowanej trasy
     */
    private Map<String, Object> buildBlockedResult(String reason) {
        Map<String, Object> result = new HashMap<>();
        result.put("polyline", null);
        result.put("herePolyline", null);
        result.put("violations", 1);
        result.put("restrictions", 0);
        result.put("warnings", 0);

        // ✅ SPRAWDŹ CZY TO WYMAGA TYLKO POZWOLENIA
        boolean requiresPermitOnly = reason.contains("WYMAGA") &&
                reason.contains("POZWOLENIA") &&
                !reason.contains("BRAK");

        if (requiresPermitOnly) {
            result.put("notices", List.of(Map.of(
                    "code", "requiresPermit",
                    "title", "Wymaga pozwolenia",
                    "message", reason,
                    "severity", "warning"
            )));
            result.put("validationSource", "HERE_MAPS_PERMIT_REQUIRED");
            result.put("routeBlocked", false); // ✅ NIE BLOKUJ!
            result.put("requiresPermit", true);
            log.info("⚠️ Trasa wymaga pozwolenia (nie blokujemy): {}", reason);
        } else {
            result.put("notices", List.of(Map.of(
                    "code", "routeBlocked",
                    "title", "Trasa zablokowana",
                    "message", reason,
                    "severity", "critical"
            )));
            result.put("validationSource", "HERE_MAPS_BLOCKED");
            result.put("routeBlocked", true);
            result.put("blockReason", reason);
            log.error("❌ Trasa zablokowana: {}", reason);
        }

        return result;
    }

    private UriComponentsBuilder buildRouteRequest(
            double startLat, double startLng,
            double endLat, double endLng,
            int weightKg, int heightCm) {

        return UriComponentsBuilder
                .fromHttpUrl("https://router.hereapi.com/v8/routes")
                .queryParam("apikey", getApiKey())
                .queryParam("transportMode", "truck")
                .queryParam("origin", startLat + "," + startLng)
                .queryParam("destination", endLat + "," + endLng)
                .queryParam("return", "polyline,summary,actions,instructions,travelSummary")
                .queryParam("lang", "pl")
                .queryParam("units", "metric")
                .queryParam("truck[grossWeight]", weightKg)
                .queryParam("truck[height]", heightCm / 100.0)
                .queryParam("truck[width]", 3.7)
                .queryParam("truck[length]", 18.5)
                .queryParam("avoid[features]", "ferry")
                .queryParam("routingMode", "fast");
    }

    private void logHereRouteDetails(Map<String, Object> response) {
        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes == null || routes.isEmpty()) return;

            Map<String, Object> route = routes.get(0);
            List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");
            if (sections == null || sections.isEmpty()) return;

            log.info("╔═══════════════════════════════════════════════════════════");
            log.info("║ 📊 SZCZEGÓŁY TRASY Z HERE MAPS");
            log.info("╠═══════════════════════════════════════════════════════════");

            double totalDistance = 0;
            int totalDuration = 0;

            for (int i = 0; i < sections.size(); i++) {
                Map<String, Object> section = sections.get(i);
                Map<String, Object> summary = (Map<String, Object>) section.get("summary");

                if (summary != null) {
                    Integer length = (Integer) summary.get("length");
                    Integer duration = (Integer) summary.get("duration");

                    if (length != null) totalDistance += length;
                    if (duration != null) totalDuration += duration;

                    log.info("║  Sekcja #{}: ", i + 1);
                    log.info("║    ├─ Długość:  {} km", length != null ? String.format("%.1f", length / 1000.0) : "?");
                    log.info("║    ├─ Czas:     {} min", duration != null ? duration / 60 : "?");
                }

                Object noticesObj = section.get("notices");
                if (noticesObj instanceof List) {
                    List<Map<String, Object>> notices = (List<Map<String, Object>>) noticesObj;
                    if (!notices.isEmpty()) {
                        log.info("║    ├─ Ostrzeżenia: {}", notices.size());
                        for (Map<String, Object> notice : notices) {
                            String code = (String) notice.get("code");
                            String title = (String) notice.get("title");
                            String severity = (String) notice.get("severity");

                            log.info("║    │  {} [{}]: {}",
                                    getSeverityIcon(severity), code, title != null ? title : "Brak opisu");
                        }
                    }
                }
            }

            log.info("║");
            log.info("║  📍 PODSUMOWANIE TRASY:");
            log.info("║    ├─ Całkowita długość:  {} km", String.format("%.1f", totalDistance / 1000.0));
            log.info("║    ├─ Całkowity czas:     {} h {} min",
                    totalDuration / 3600, (totalDuration % 3600) / 60);
            log.info("║    └─ Liczba sekcji:      {}", sections.size());
            log.info("╚═══════════════════════════════════════════════════════════");

        } catch (Exception e) {
            log.error("❌ Błąd logowania szczegółów HERE: {}", e.getMessage(), e);
        }
    }

    private void logViolations(Map<String, Object> response) {
        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes == null || routes.isEmpty()) return;

            Map<String, Object> route = routes.get(0);
            Object violationsObj = route.get("violations");

            if (violationsObj instanceof List) {
                List<Map<String, Object>> violations = (List<Map<String, Object>>) violationsObj;

                if (violations != null && !violations.isEmpty()) {
                    log.warn("╔═══════════════════════════════════════════════════════════");
                    log.warn("║ ⚠️ WYKRYTE NARUSZENIA OGRANICZEŃ");
                    log.warn("╠═══════════════════════════════════════════════════════════");

                    for (int i = 0; i < violations.size(); i++) {
                        Map<String, Object> violation = violations.get(i);
                        String type = (String) violation.get("type");
                        String severity = (String) violation.get("severity");
                        Integer offset = (Integer) violation.get("offset");

                        log.warn("║  Naruszenie #{}: ", i + 1);
                        log.warn("║    ├─ Typ:      {}", type);
                        log.warn("║    ├─ Waga:     {}", severity);
                        log.warn("║    └─ Offset:   {} m", offset);
                    }

                    log.warn("╚═══════════════════════════════════════════════════════════");
                } else {
                    log.info("✅ Brak wykrytych naruszeń ograniczeń");
                }
            } else {
                log.info("✅ Brak wykrytych naruszeń ograniczeń");
            }
        } catch (Exception e) {
            log.error("❌ Błąd logowania violations: {}", e.getMessage());
        }
    }

    private void compareTransportWithRoute(int weightKg, int heightCm, Map<String, Object> response) {
        log.info("╔═══════════════════════════════════════════════════════════");
        log.info("║ ⚖️ PORÓWNANIE: TRANSPORT vs TRASA");
        log.info("╠═══════════════════════════════════════════════════════════");
        log.info("║  🚛 Parametry transportu:");
        log.info("║    ├─ Waga całkowita:  {} t", String.format("%.1f", weightKg / 1000.0));
        log.info("║    ├─ Wysokość:        {} m", String.format("%.2f", heightCm / 100.0));
        log.info("║    ├─ Szerokość:       3.7 m");
        log.info("║    └─ Długość:         18.5 m");
        log.info("║");

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> route = routes.get(0);
                Object violationsObj = route.get("violations");

                if (violationsObj instanceof List) {
                    List<Map<String, Object>> violations = (List<Map<String, Object>>) violationsObj;

                    if (violations != null && !violations.isEmpty()) {
                        log.warn("║  ❌ WYKRYTO {} NARUSZENIA(Ń)!", violations.size());

                        Map<String, Long> violationsByType = violations.stream()
                                .collect(Collectors.groupingBy(
                                        v -> (String) v.get("type"),
                                        Collectors.counting()
                                ));

                        violationsByType.forEach((type, count) ->
                                log.warn("║    • {}: {}", type, count)
                        );
                    } else {
                        log.info("║  ✅ Brak wykrytych naruszeń");
                    }
                } else {
                    log.info("║  ✅ Brak wykrytych naruszeń");
                }
            }
        } catch (Exception e) {
            log.error("║  ⚠️ Błąd sprawdzania violations");
        }

        log.info("╚═══════════════════════════════════════════════════════════");
    }

    private void analyzeNotices(List<Map<String, Object>> notices) {
        if (notices == null || notices.isEmpty()) {
            log.info("✅ Brak ostrzeżeń HERE Maps");
            return;
        }

        Map<String, Integer> noticesByType = new HashMap<>();
        Map<String, Integer> noticesBySeverity = new HashMap<>();

        for (Map<String, Object> notice : notices) {
            String code = (String) notice.get("code");
            String severity = (String) notice.get("severity");

            noticesByType.merge(code != null ? code : "unknown", 1, Integer::sum);
            noticesBySeverity.merge(severity != null ? severity : "unknown", 1, Integer::sum);
        }

        log.info("╔═══════════════════════════════════════════════════════════");
        log.info("║ 📊 ANALIZA OSTRZEŻEŃ HERE MAPS");
        log.info("╠═══════════════════════════════════════════════════════════");
        log.info("║  Całkowita liczba: {}", notices.size());
        log.info("║");
        log.info("║  Według wagi:");
        noticesBySeverity.forEach((severity, count) ->
                log.info("║    {} {}: {}", getSeverityIcon(severity), severity, count)
        );
        log.info("║");
        log.info("║  Według typu (top 10):");
        noticesByType.entrySet().stream()
                .sorted((a, b) -> b.getValue().compareTo(a.getValue()))
                .limit(10)
                .forEach(entry ->
                        log.info("║    • {}: {}", entry.getKey(), entry.getValue())
                );
        log.info("╚═══════════════════════════════════════════════════════════");
    }

    private String getSeverityIcon(String severity) {
        if (severity == null) return "ℹ️";
        return switch (severity.toLowerCase()) {
            case "critical" -> "🚨";
            case "error" -> "❌";
            case "warning" -> "⚠️";
            case "info" -> "ℹ️";
            default -> "📌";
        };
    }

    private List<Map<String, Object>> extractNotices(Map<String, Object> response) {
        List<Map<String, Object>> allNotices = new ArrayList<>();

        try {
            Object topLevelNoticesObj = response.get("notices");

            if (topLevelNoticesObj instanceof List) {
                List<Map<String, Object>> topLevelNotices = (List<Map<String, Object>>) topLevelNoticesObj;

                if (topLevelNotices != null) {
                    for (Map<String, Object> notice : topLevelNotices) {
                        String code = (String) notice.get("code");
                        String severity = (String) notice.get("severity");

                        if ("unknownParameter".equals(code) && "info".equals(severity)) {
                            continue;
                        }

                        allNotices.add(notice);
                    }
                }
            }

            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> route = routes.get(0);
                List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");

                if (sections != null) {
                    for (Map<String, Object> section : sections) {
                        Object sectionNoticesObj = section.get("notices");

                        if (sectionNoticesObj instanceof List) {
                            List<Map<String, Object>> sectionNotices = (List<Map<String, Object>>) sectionNoticesObj;

                            if (sectionNotices != null) {
                                allNotices.addAll(sectionNotices);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Błąd wyciągania notices", e);
        }

        return allNotices;
    }

    private String extractPolyline(Map<String, Object> response) {
        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes == null || routes.isEmpty()) return null;

            Map<String, Object> route = routes.get(0);
            List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");
            if (sections == null || sections.isEmpty()) return null;

            Map<String, Object> section = sections.get(0);
            return (String) section.get("polyline");

        } catch (Exception e) {
            log.error("Błąd wyciągania polyline", e);
            return null;
        }
    }

    private List<ExcludedPoint> findExcludedPointsOnRoute(
            Map<String, Object> response,
            Set<String> excludedNames) {

        List<ExcludedPoint> excludedPoints = new ArrayList<>();

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes == null || routes.isEmpty()) return excludedPoints;

            Map<String, Object> route = routes.get(0);
            List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");
            if (sections == null || sections.isEmpty()) return excludedPoints;

            for (Map<String, Object> section : sections) {
                Object actionsObj = section.get("actions");
                if (!(actionsObj instanceof List)) continue;

                List<Map<String, Object>> actions = (List<Map<String, Object>>) actionsObj;
                if (actions == null) continue;

                for (Map<String, Object> action : actions) {
                    String instruction = (String) action.get("instruction");
                    Integer offset = (Integer) action.get("offset");
                    Integer length = (Integer) action.get("length");

                    if (instruction == null) continue;

                    for (String excludedName : excludedNames) {
                        if (instruction.toLowerCase().contains(excludedName.toLowerCase())) {

                            ExcludedPoint point = new ExcludedPoint();
                            point.name = excludedName;
                            point.instruction = instruction;
                            point.offset = offset != null ? offset : 0;
                            point.length = length != null ? length : 100;

                            excludedPoints.add(point);

                            log.debug("🚫 Wykluczony: '{}' @ offset {}m, długość {}m",
                                    excludedName, point.offset, point.length);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Błąd znajdowania wykluczonych punktów", e);
        }

        return excludedPoints;
    }

    private Map<String, Object> recalculateWithExcludedPoints(
            double startLat, double startLng,
            double endLat, double endLng,
            int weightKg, int heightCm,
            List<ExcludedPoint> excludedPoints,
            Map<String, Object> originalResponse) {

        log.info("🔄 PUNKTOWY OBJAZD: Omijam {} konkretnych obiektów", excludedPoints.size());

        String polyline = extractPolyline(originalResponse);
        if (polyline == null) {
            log.error("❌ Brak polyline");
            return buildBlockedResult("Brak polyline do analizy");
        }

        List<double[]> routePoints;
        try {
            routePoints = decodeHereFlexiblePolyline(polyline);
        } catch (Exception e) {
            log.error("❌ Błąd dekodowania polyline: {}", e.getMessage());
            return buildBlockedResult("Błąd dekodowania polyline");
        }

        if (routePoints.isEmpty()) {
            log.error("❌ Nie udało się zdekodować polyline");
            return buildBlockedResult("Błąd dekodowania polyline");
        }

        if (routePoints.size() > 0) {
            double[] firstPoint = routePoints.get(0);
            if (Math.abs(firstPoint[1]) < 0.01) {
                log.debug("⚠️ Naprawiam pierwszy punkt: lng={} -> lng={}", firstPoint[1], startLng);
                routePoints.set(0, new double[]{startLat, startLng});
            }
        }

        log.info("📍 Zdekodowano {} punktów z polyline", routePoints.size());

        List<double[]> excludedLocations = new ArrayList<>();

        for (ExcludedPoint point : excludedPoints) {
            int index = Math.min(point.offset, routePoints.size() - 1);

            if (index >= 0 && index < routePoints.size()) {
                double[] location = routePoints.get(index);

                if (location[0] >= -90 && location[0] <= 90 &&
                        location[1] >= -180 && location[1] <= 180 &&
                        Math.abs(location[1]) > 0.1) {

                    excludedLocations.add(location);
                    log.info("📍 Lokalizacja obiektu '{}': [{}, {}]",
                            point.name, location[0], location[1]);
                }
            }
        }

        if (excludedLocations.isEmpty()) {
            log.error("❌ Nie udało się znaleźć lokalizacji wykluczonych obiektów");
            return buildBlockedResult("Brak lokalizacji wykluczonych obiektów");
        }

        return calculateRouteWithExcludedAreas(
                startLat, startLng, endLat, endLng,
                weightKg, heightCm, excludedLocations, excludedPoints
        );
    }

    private Map<String, Object> calculateRouteWithExcludedAreas(
            double startLat, double startLng,
            double endLat, double endLng,
            int weightKg, int heightCm,
            List<double[]> excludedLocations,
            List<ExcludedPoint> excludedPoints) {

        log.info("🚀 Obliczam trasę z wykluczeniem {} obszarów", excludedLocations.size());

        UriComponentsBuilder builder = buildRouteRequest(startLat, startLng, endLat, endLng, weightKg, heightCm);

        for (double[] location : excludedLocations) {
            double latOffset = EXCLUSION_RADIUS_KM / 111.0;
            double lngOffset = EXCLUSION_RADIUS_KM / (111.0 * Math.cos(Math.toRadians(location[0])));

            double minLat = location[0] - latOffset;
            double maxLat = location[0] + latOffset;
            double minLng = location[1] - lngOffset;
            double maxLng = location[1] + lngOffset;

            String avoidArea = String.format("bbox:%.6f,%.6f,%.6f,%.6f", minLng, minLat, maxLng, maxLat);
            builder.queryParam("avoid[areas]", avoidArea);

            log.debug("🚫 Wykluczam obszar {} wokół [{}, {}]", EXCLUSION_RADIUS_KM + "km", location[0], location[1]);
        }

        String url = builder.toUriString();

        try {
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response == null || !response.containsKey("routes")) {
                log.error("❌ HERE Maps nie zwróciło trasy z wykluczeniem obszarów");
                return buildBlockedResult("Brak trasy z HERE Maps");
            }

            List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");
            if (routes == null || routes.isEmpty()) {
                log.error("❌ Pusta lista tras - brak alternatywnej drogi");
                return buildBlockedResult("Brak alternatywnej trasy omijającej wykluczone obiekty");
            }

            log.info("📊 TRASA OBJAZDOWA:");
            logHereRouteDetails(response);
            logViolations(response);

            Set<String> excludedNames = excludedPoints.stream()
                    .map(p -> p.name)
                    .collect(Collectors.toSet());

            List<ExcludedPoint> stillExcluded = findExcludedPointsOnRoute(response, excludedNames);

            if (!stillExcluded.isEmpty()) {
                log.warn("⚠️ Trasa nadal przechodzi przez {} wykluczonych obiektów (brak alternatywy)",
                        stillExcluded.size());

                for (ExcludedPoint point : stillExcluded) {
                    log.warn("   ⚠️ Nadal na trasie: {}", point.name);
                }

                // ✅ ZWRÓĆ BŁĄD - brak alternatywy
                return buildBlockedResult(
                        "Wszystkie możliwe trasy przechodzą przez zablokowane obiekty: " +
                                stillExcluded.stream().map(p -> p.name).collect(Collectors.joining(", "))
                );
            } else {
                log.info("✅ SUKCES: Trasa omija wszystkie wykluczone obiekty!");
            }

            String polyline = extractPolyline(response);
            List<Map<String, Object>> notices = extractNotices(response);

            Map<String, Object> result = new HashMap<>();
            result.put("polyline", polyline);
            result.put("herePolyline", polyline);
            result.put("violations", 0);
            result.put("restrictions", 0);
            result.put("warnings", notices.size());
            result.put("notices", notices);
            result.put("detour_used", true);
            result.put("excluded_objects_avoided", excludedLocations.size());
            result.put("excluded_objects_remaining", 0);
            result.put("validationSource", "HERE_MAPS_POINT_DETOUR");
            result.put("routeBlocked", false);

            return result;

        } catch (Exception e) {
            log.error("❌ Błąd podczas obliczania trasy objazdowej", e);
            return buildBlockedResult("Błąd API: " + e.getMessage());
        }
    }

    private List<double[]> decodeHereFlexiblePolyline(String encoded) {
        try {
            List<FlexiblePolyline.LatLng> decoded = FlexiblePolyline.decode(encoded);

            List<double[]> coordinates = new ArrayList<>();

            for (FlexiblePolyline.LatLng point : decoded) {
                if (point.lat >= -90 && point.lat <= 90 &&
                        point.lng >= -180 && point.lng <= 180) {
                    coordinates.add(new double[]{point.lat, point.lng});
                }
            }

            log.info("✅ Zdekodowano {} punktów", coordinates.size());

            if (!coordinates.isEmpty()) {
                log.debug("📍 Start: [{}, {}]", coordinates.get(0)[0], coordinates.get(0)[1]);
                log.debug("📍 Koniec: [{}, {}]",
                        coordinates.get(coordinates.size() - 1)[0],
                        coordinates.get(coordinates.size() - 1)[1]);
            }

            return coordinates;

        } catch (Exception e) {
            log.error("❌ Błąd dekodowania: {}", e.getMessage());
            return new ArrayList<>();
        }
    }

    public Map<String, Object> validateRoute(
            double startLat, double startLng,
            double endLat, double endLng,
            int weightKg, int heightCm) {

        return validateRoute(startLat, startLng, endLat, endLng, weightKg, heightCm, null);
    }

    public List<Map<String, Object>> getAlternativeRoutes(
            double startLat, double startLng,
            double endLat, double endLng,
            TransportSet transportSet) {

        log.info("🔍 Szukanie alternatywnych tras...");

        List<Map<String, Object>> alternatives = new ArrayList<>();

        try {
            UriComponentsBuilder builder = buildRouteRequest(
                    startLat, startLng, endLat, endLng,
                    transportSet.getTotalWeightKg(),
                    transportSet.getTotalHeightCm()
            );

            builder.queryParam("alternatives", 3);

            String url = builder.toUriString();
            Map<String, Object> response = restTemplate.getForObject(url, Map.class);

            if (response != null) {
                List<Map<String, Object>> routes = (List<Map<String, Object>>) response.get("routes");

                if (routes != null) {
                    for (int i = 0; i < routes.size(); i++) {
                        Map<String, Object> route = routes.get(i);
                        Map<String, Object> alternative = new HashMap<>();

                        alternative.put("routeIndex", i);
                        alternative.put("routeType", i == 0 ? "fastest" : "alternative_" + i);

                        List<Map<String, Object>> sections = (List<Map<String, Object>>) route.get("sections");
                        if (sections != null && !sections.isEmpty()) {
                            Map<String, Object> section = sections.get(0);
                            Map<String, Object> summary = (Map<String, Object>) section.get("summary");

                            if (summary != null) {
                                alternative.put("distance_meters", summary.get("length"));
                                alternative.put("duration_seconds", summary.get("duration"));
                                alternative.put("distance_km",
                                        ((Number) summary.get("length")).doubleValue() / 1000.0);
                                alternative.put("duration_minutes",
                                        ((Number) summary.get("duration")).intValue() / 60);
                            }

                            alternative.put("polyline", section.get("polyline"));
                        }

                        alternatives.add(alternative);
                    }

                    log.info("✅ Znaleziono {} alternatywnych tras", alternatives.size());
                }
            }

        } catch (Exception e) {
            log.error("❌ Błąd podczas pobierania alternatywnych tras", e);
        }

        return alternatives;
    }

    private static class ExcludedPoint {
        String name;
        String instruction;
        int offset;
        int length;
    }
}