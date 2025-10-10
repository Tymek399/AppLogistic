package com.military.applogistic.service;

import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.util.FlexiblePolyline;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RouteService {

    private final RouteRepository routeRepository;
    private final TransportSetRepository transportSetRepository;
    private final GoogleMapsService googleMapsService;
    private final HereMapsService hereMapsService;
    private final MilitaryRoadPermissions militaryRoadPermissions;
    private final ObjectMapper objectMapper = new ObjectMapper();

    private static final int MAX_ROUTE_ATTEMPTS = 10;
    private static final int LIGHT_VEHICLE_THRESHOLD_KG = 5000;

    /**
     * ✅ GŁÓWNA METODA TWORZENIA TRASY - Z WALIDACJĄ PRZED ZAPISEM
     */
    public RouteResponse createRoute(CreateRouteRequest request, String createdByUsername) {
        log.info("========================================");
        log.info("ROZPOCZĘCIE PLANOWANIA TRASY");
        log.info("========================================");

        TransportSet transportSet = transportSetRepository.findById(request.getTransportSetId())
                .orElseThrow(() -> new RuntimeException("Nie znaleziono zestawu transportowego"));

        logTransportSetParameters(transportSet);
        validateTransportSet(transportSet);

        // ✅ LEKKIE POJAZDY - szybka ścieżka bez walidacji
        if (transportSet.getTotalWeightKg() <= LIGHT_VEHICLE_THRESHOLD_KG) {
            log.info("🚗 LEKKI POJAZD (≤5t) - POMIJAM WALIDACJĘ MOSTÓW");
            return createLightVehicleRoute(request, transportSet, createdByUsername);
        }

        // ✅ CIĘŻKIE POJAZDY - PEŁNA WALIDACJA PRZED ZAPISEM
        return createHeavyVehicleRouteWithValidation(request, transportSet, createdByUsername);
    }

    /**
     * ✅ WALIDACJA ZESTAWU TRANSPORTOWEGO
     */
    private void validateTransportSet(TransportSet transportSet) {
        List<String> errors = new ArrayList<>();

        if (transportSet.getTotalWeightKg() == null || transportSet.getTotalWeightKg() <= 0) {
            errors.add("Nieprawidłowa masa zestawu");
        }

        if (transportSet.getTotalHeightCm() == null || transportSet.getTotalHeightCm() <= 0) {
            errors.add("Nieprawidłowa wysokość zestawu");
        }

        if (transportSet.getTotalWeightKg() != null && transportSet.getTotalWeightKg() > 150000) {
            errors.add("Masa zestawu przekracza maksymalną dopuszczalną (150t)");
        }

        if (transportSet.getTotalHeightCm() != null && transportSet.getTotalHeightCm() > 600) {
            errors.add("Wysokość zestawu przekracza maksymalną dopuszczalną (6m)");
        }

        if (!errors.isEmpty()) {
            throw new RuntimeException("Błędy walidacji zestawu: " + String.join(", ", errors));
        }
    }

    /**
     * ✅ TWORZENIE TRASY DLA LEKKIEGO POJAZDU
     */
    private RouteResponse createLightVehicleRoute(CreateRouteRequest request,
                                                  TransportSet transportSet,
                                                  String createdByUsername) {
        try {
            Map<String, Object> routeData = googleMapsService.getRoute(
                    request.getStartAddress(),
                    request.getEndAddress(),
                    transportSet,
                    new HashSet<>()
            );

            routeData.put("lightVehicle", true);
            routeData.put("validationSkipped", true);
            routeData.put("reason", "Pojazd ≤5t - walidacja mostów pominięta");
            routeData.put("searchAttempts", 1);
            routeData.put("successfulAttempt", 1);
            routeData.put("createdAt", LocalDateTime.now().toString());

            Route route = buildRouteEntity(request, transportSet, createdByUsername, routeData);
            Route savedRoute = routeRepository.save(route);

            log.info("✅ Trasa dla lekkiego pojazdu utworzona bez walidacji (ID: {})", savedRoute.getId());

            return convertToResponse(savedRoute, routeData);

        } catch (Exception e) {
            log.error("❌ Błąd tworzenia trasy lekkiego pojazdu", e);
            throw new RuntimeException("Nie udało się utworzyć trasy: " + e.getMessage());
        }
    }

    /**
     * ✅ NOWA METODA - TWORZENIE TRASY Z PEŁNĄ WALIDACJĄ PRZED ZAPISEM
     */
    private RouteResponse createHeavyVehicleRouteWithValidation(
            CreateRouteRequest request,
            TransportSet transportSet,
            String createdByUsername) {

        List<RouteAttemptReport> allAttempts = new ArrayList<>();
        Set<String> excludedBridges = new HashSet<>();
        Map<String, Object> validatedRouteData = null;

        for (int attempt = 1; attempt <= MAX_ROUTE_ATTEMPTS; attempt++) {
            log.info("╔═══════════════════════════════════════╗");
            log.info("PRÓBA #{} - Szukanie trasy...", attempt);

            if (!excludedBridges.isEmpty()) {
                log.info("🚫 Wykluczam {} mostów: {}", excludedBridges.size(), excludedBridges);
            }

            try {
                // ✅ KROK 1: Pobierz trasę z Google Maps
                Map<String, Object> routeData = googleMapsService.getRoute(
                        request.getStartAddress(),
                        request.getEndAddress(),
                        transportSet,
                        excludedBridges
                );

                // ✅ KROK 2: Waliduj trasę PRZED ZAPISEM
                RouteAttemptReport attemptReport = validateRouteBeforeSaving(
                        routeData, attempt, excludedBridges, transportSet
                );
                allAttempts.add(attemptReport);

                // ✅ KROK 3: Jeśli trasa przejezdna - zapisz i zakończ
                if (attemptReport.isFullyPassable()) {
                    log.info("🎉 SUKCES! Znaleziono przejezdną trasę w próbie #{}", attempt);

                    // Dodaj raporty walidacji do danych trasy
                    routeData.put("searchAttempts", attempt);
                    routeData.put("attemptReports", allAttempts);
                    routeData.put("validationCompleted", true);
                    routeData.put("createdAt", LocalDateTime.now().toString());

                    validatedRouteData = routeData;
                    break;
                }

                // ✅ KROK 4: Jeśli trasa zablokowana - dodaj mosty do wykluczeń
                List<String> criticalBridges = attemptReport.getCriticalBridges();
                if (criticalBridges.isEmpty()) {
                    log.warn("⚠️ Brak możliwych tras dalej - wszystkie opcje wyczerpane");
                    break;
                }

                // Dodaj krytyczne mosty do wykluczeń
                int bridgesToExclude = Math.min(3, criticalBridges.size());
                for (int i = 0; i < bridgesToExclude; i++) {
                    excludedBridges.add(criticalBridges.get(i));
                }

            } catch (Exception e) {
                log.error("❌ Błąd w próbie #{}: {}", attempt, e.getMessage());
                RouteAttemptReport errorReport = new RouteAttemptReport();
                errorReport.setAttemptNumber(attempt);
                errorReport.setError(e.getMessage());
                allAttempts.add(errorReport);
            }
        }

        // ✅ KROK 5: Jeśli znaleziono trasę - zapisz do bazy
        if (validatedRouteData != null) {
            return saveValidatedRoute(request, transportSet, createdByUsername, validatedRouteData);
        }

        // ✅ KROK 6: Jeśli nie znaleziono trasy - zwróć błąd
        log.error("💥 BRAK FIZYCZNIE MOŻLIWEJ TRASY po {} próbach", allAttempts.size());
        throw buildNoRouteException(transportSet, allAttempts);
    }

    /**
     * ✅ NOWA METODA: WALIDACJA TRASY PRZED ZAPISEM
     */
    private RouteAttemptReport validateRouteBeforeSaving(
            Map<String, Object> routeData,
            int attemptNumber,
            Set<String> excludedBridges,
            TransportSet transportSet) {

        RouteAttemptReport report = new RouteAttemptReport();
        report.setAttemptNumber(attemptNumber);
        report.setExcludedBridges(new ArrayList<>(excludedBridges));

        // ✅ SPRAWDŹ CZY TRASA ZOSTAŁA ZABLOKOWANA
        if (Boolean.TRUE.equals(routeData.get("routeBlocked"))) {
            String blockReason = (String) routeData.get("blockReason");
            log.error("🚨 Trasa zablokowana w próbie #{}: {}", attemptNumber, blockReason);

            report.setPassable(false);
            report.setViolations(List.of(blockReason));
            report.setBlockedBridges(999); // Specjalna wartość oznaczająca całkowitą blokadę
            report.setCriticalBridges(List.of()); // Brak sensu szukać alternatyw
            report.setSuccessScore(0);

            return report;
        }

        // ✅ SPRAWDŹ CZY WYMAGA POZWOLENIA (NIE BLOKUJ!)
        if (Boolean.TRUE.equals(routeData.get("requiresPermit"))) {
            List<String> permits = (List<String>) routeData.getOrDefault("permits", new ArrayList<>());
            log.info("⚠️ Trasa wymaga pozwolenia w próbie #{}: {}", attemptNumber, permits);

            report.setPassable(true); // ✅ PRZEJEZDNA, TYLKO WYMAGA POZWOLENIA
            report.setRequiresPermit(true);
            report.setPermits(permits);
            report.setBlockedBridges(0);
            report.setCriticalBridges(new ArrayList<>());
            report.setSuccessScore(90); // Wysoki wynik, bo trasa OK

            return report;
        }

        // Pobierz dane z walidacji
        List<String> violations = (List<String>) routeData.getOrDefault("violations", new ArrayList<>());
        List<String> restrictions = (List<String>) routeData.getOrDefault("restrictions", new ArrayList<>());
        List<String> permits = (List<String>) routeData.getOrDefault("permits", new ArrayList<>());
        List<Map<String, Object>> infrastructure =
                (List<Map<String, Object>>) routeData.getOrDefault("infrastructureDetails", new ArrayList<>());

        report.setViolations(violations);
        report.setRestrictions(restrictions);
        report.setPermits(permits);
        report.setTotalInfrastructureChecked(infrastructure.size());

        // ✅ SPRAWDŹ CZY WSZYSTKIE MOSTY/TUNELE SĄ PRZEJEZDNE
        long blockedBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .count();

        report.setBlockedBridges((int) blockedBridges);
        report.setPassable(violations.isEmpty() && blockedBridges == 0);
        report.setRequiresPermit(!permits.isEmpty());

        // Zbierz nazwy zablokowanych obiektów
        List<String> criticalBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .map(i -> (String) i.get("name"))
                .limit(100)
                .collect(Collectors.toList());

        report.setCriticalBridges(criticalBridges);

        // Oblicz wynik próby
        double successScore = calculateAttemptScore(report, transportSet);
        report.setSuccessScore(successScore);

        // ✅ LOGUJ SZCZEGÓŁY WALIDACJI
        log.info("📊 WALIDACJA PRÓBY #{}:", attemptNumber);
        log.info("   ✓ Sprawdzono obiektów: {}", infrastructure.size());
        log.info("   ✓ Zablokowanych: {}", blockedBridges);
        log.info("   ✓ Naruszeń: {}", violations.size());
        log.info("   ✓ Pozwoleń wymaganych: {}", permits.size());
        log.info("   ✓ Przejezdna: {}", report.isPassable() ? "TAK" : "NIE");

        if (!criticalBridges.isEmpty()) {
            log.warn("   🚫 Zablokowane obiekty:");
            criticalBridges.forEach(b -> log.warn("      - {}", b));
        }

        if (!permits.isEmpty()) {
            log.info("   ⚠️ Wymagane pozwolenia:");
            permits.forEach(p -> log.info("      - {}", p));
        }

        return report;
    }

    /**
     * ✅ OBLICZA WYNIK PRÓBY (0-100)
     */
    private double calculateAttemptScore(RouteAttemptReport report, TransportSet transportSet) {
        double score = 100.0;

        // Kary za naruszenia
        score -= report.getViolations().size() * 30;
        score -= report.getBlockedBridges() * 25;
        score -= report.getRestrictions().size() * 10;

        // Mniejsza kara za pozwolenia (tylko 5 punktów)
        score -= report.getPermits().size() * 5;

        // Bonus za sprawdzenie wielu obiektów (dokładność)
        score += Math.min(report.getTotalInfrastructureChecked() * 0.5, 10);

        return Math.max(0, Math.min(100, score));
    }

    /**
     * ✅ ZAPISUJE ZWALIDOWANĄ TRASĘ DO BAZY
     */
    private RouteResponse saveValidatedRoute(
            CreateRouteRequest request,
            TransportSet transportSet,
            String createdByUsername,
            Map<String, Object> routeData) {

        // Dodaj podsumowanie walidacji
        Map<String, Object> validationSummary = new HashMap<>();
        validationSummary.put("totalAttempts", routeData.get("searchAttempts"));
        validationSummary.put("validationCompleted", true);
        validationSummary.put("validatedAt", LocalDateTime.now().toString());

        // ✅ Dodaj info o pozwoleniach
        if (routeData.containsKey("permits") && !((List) routeData.get("permits")).isEmpty()) {
            validationSummary.put("requiresPermits", true);
            validationSummary.put("permits", routeData.get("permits"));
        }

        routeData.put("validationSummary", validationSummary);

        Route route = buildRouteEntity(request, transportSet, createdByUsername, routeData);
        Route savedRoute = routeRepository.save(route);

        log.info("✅ Trasa #{} utworzona pomyślnie po walidacji", savedRoute.getId());

        return convertToResponse(savedRoute, routeData);
    }

    /**
     * ✅ BUDUJE WYJĄTEK BRAKU TRASY
     */
    private RuntimeException buildNoRouteException(TransportSet transportSet, List<RouteAttemptReport> allAttempts) {
        StringBuilder message = new StringBuilder();
        message.append("❌ BRAK FIZYCZNIE MOŻLIWEJ TRASY\n\n");

        // ✅ Sprawdź czy była całkowita blokada
        boolean totalBlockage = allAttempts.stream()
                .anyMatch(r -> r.getBlockedBridges() >= 999);

        if (totalBlockage) {
            message.append("🚨 CAŁKOWITA BLOKADA TRASY\n");
            message.append("═══════════════════════════════════════════════\n");
            RouteAttemptReport blockedAttempt = allAttempts.stream()
                    .filter(r -> r.getBlockedBridges() >= 999)
                    .findFirst()
                    .orElse(null);

            if (blockedAttempt != null && !blockedAttempt.getViolations().isEmpty()) {
                message.append("\nPowód blokady:\n");
                blockedAttempt.getViolations().forEach(v ->
                        message.append("  • ").append(v).append("\n")
                );
            }

            message.append("\n💡 Rozwiązania:\n");
            message.append("───────────────────────────────────────────────────\n");
            message.append("1. Zmień zestaw transportowy na lżejszy/niższy\n");
            message.append("2. Rozważ transport na kilka mniejszych zestawów\n");
            message.append("3. Skontaktuj się z zarządem dróg w sprawie pozwoleń specjalnych\n");

        } else {
            message.append("📊 Podsumowanie walidacji:\n");
            message.append("═══════════════════════════════════════════════\n");
            message.append(String.format("• Sprawdzono %d alternatywnych tras\n", allAttempts.size()));
            message.append(String.format("• Waga zestawu: %.1f ton\n", transportSet.getTotalWeightKg() / 1000.0));
            message.append(String.format("• Wysokość zestawu: %.2f m\n", transportSet.getTotalHeightCm() / 100.0));

            int totalInfrastructure = allAttempts.stream()
                    .mapToInt(RouteAttemptReport::getTotalInfrastructureChecked)
                    .sum();
            int totalBlocked = allAttempts.stream()
                    .mapToInt(RouteAttemptReport::getBlockedBridges)
                    .sum();

            message.append(String.format("• Sprawdzono %d obiektów infrastruktury\n", totalInfrastructure));
            message.append(String.format("• Zablokowanych obiektów: %d\n", totalBlocked));

            message.append("\n💡 Sugerowane rozwiązania:\n");
            message.append("───────────────────────────────────────────────────\n");
            message.append("1. Zmień zestaw transportowy na lżejszy/niższy\n");
            message.append("2. Rozważ transport na kilka mniejszych zestawów\n");
            message.append("3. Skontaktuj się z operatorem tras specjalnych\n");
        }

        return new RuntimeException(message.toString());
    }

    /**
     * ✅ LOGUJE PARAMETRY ZESTAWU
     */
    private void logTransportSetParameters(TransportSet ts) {
        log.info("╔════════════════════════════════════╗");
        log.info("PARAMETRY ZESTAWU: {}", ts.getDescription());
        log.info("────────────────────────────────────");
        log.info("Waga: {} kg ({} ton)", ts.getTotalWeightKg(), ts.getTotalWeightKg() / 1000.0);
        log.info("Wysokość: {} cm ({} m)", ts.getTotalHeightCm(), ts.getTotalHeightCm() / 100.0);
        log.info("Długość: {} cm ({} m)", ts.getTotalLengthCm(), ts.getTotalLengthCm() / 100.0);
        log.info("Szerokość: {} cm ({} m)", ts.getTotalWidthCm(), ts.getTotalWidthCm() / 100.0);
        log.info("Max nacisk na oś: {} kg ({} t)", ts.getMaxAxleLoadKg(), ts.getMaxAxleLoadKg() / 1000.0);
        log.info("Typ naczepy: {}", ts.getTrailerType());
        log.info("╚════════════════════════════════════╝");
    }

    /**
     * ✅ BUDUJE ENCJĘ ROUTE
     */
    private Route buildRouteEntity(CreateRouteRequest request, TransportSet transportSet,
                                   String createdByUsername, Map<String, Object> routeData) {
        Route route = new Route();
        route.setStartAddress(request.getStartAddress());
        route.setEndAddress(request.getEndAddress());
        route.setStartLatitude(request.getStartLatitude());
        route.setStartLongitude(request.getStartLongitude());
        route.setEndLatitude(request.getEndLatitude());
        route.setEndLongitude(request.getEndLongitude());
        route.setTransportSet(transportSet);
        route.setCreatedByUsername(createdByUsername);
        route.setStatus(Route.RouteStatus.CREATED);

        try {
            route.setRouteDataJson(objectMapper.writeValueAsString(routeData));
        } catch (Exception e) {
            log.error("❌ Błąd serializacji danych trasy", e);
            route.setRouteDataJson("{}");
        }

        extractRouteMetrics(route, routeData);
        return route;
    }

    /**
     * ✅ WYCIĄGA METRYKI TRASY
     */
    private void extractRouteMetrics(Route route, Map<String, Object> routeData) {
        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> firstRoute = routes.get(0);
                List<Map<String, Object>> legs = (List<Map<String, Object>>) firstRoute.get("legs");
                if (legs != null && !legs.isEmpty()) {
                    Map<String, Object> leg = legs.get(0);

                    // Dystans
                    Object distanceObj = leg.get("distance");
                    if (distanceObj instanceof Map) {
                        Object valueObj = ((Map<String, Object>) distanceObj).get("value");
                        if (valueObj instanceof Number) {
                            route.setTotalDistanceKm(((Number) valueObj).doubleValue() / 1000.0);
                        }
                    }

                    // Czas
                    Object durationObj = leg.get("duration");
                    if (durationObj instanceof Map) {
                        Object valueObj = ((Map<String, Object>) durationObj).get("value");
                        if (valueObj instanceof Number) {
                            route.setEstimatedTimeMinutes(((Number) valueObj).intValue() / 60);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.warn("⚠️ Nie można wyciągnąć metryk: {}", e.getMessage());
        }
    }

    /**
     * ✅ KONWERTUJE DO RESPONSE
     */
    private RouteResponse convertToResponse(Route route, Map<String, Object> routeData) {
        RouteResponse response = RouteResponse.from(route);

        try {
            if (routeData != null) {
                if (routeData.containsKey("hasRestrictions")) {
                    response.setHasRestrictions((Boolean) routeData.get("hasRestrictions"));
                }
                if (routeData.containsKey("warnings")) {
                    response.setWarnings((List<String>) routeData.get("warnings"));
                }
            }
        } catch (Exception e) {
            log.warn("⚠️ Błąd konwersji: {}", e.getMessage());
        }

        return response;
    }

    // ═══════════════════════════════════════════════════════════════════
    // GENEROWANIE PLIKÓW NAWIGACYJNYCH (GPX, KML) - POPRAWIONE
    // ═══════════════════════════════════════════════════════════════════

    /**
     * ✅ POPRAWIONE GENEROWANIE PLIKU GPX/KML
     */
    public byte[] generateNavigationFile(Long routeId, String format) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
            throw new RuntimeException("Route has no navigation data");
        }

        try {
            Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

            if ("gpx".equalsIgnoreCase(format)) {
                return generateGpx(routeData, route).getBytes(StandardCharsets.UTF_8);
            } else if ("kml".equalsIgnoreCase(format)) {
                return generateKml(routeData, route).getBytes(StandardCharsets.UTF_8);
            } else {
                throw new RuntimeException("Unsupported format: " + format);
            }

        } catch (Exception e) {
            throw new RuntimeException("Error generating navigation file: " + e.getMessage(), e);
        }
    }

    /**
     * ✅ POPRAWIONE GENEROWANIE GPX - Z OBSŁUGĄ POLYLINE
     */
    private String generateGpx(Map<String, Object> routeData, Route route) {
        StringBuilder gpx = new StringBuilder();
        gpx.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        gpx.append("<gpx version=\"1.1\" creator=\"MilitaryLogisticOps\" ");
        gpx.append("xmlns=\"http://www.topografix.com/GPX/1/1\">\n");

        gpx.append("  <metadata>\n");
        gpx.append("    <name>").append(escapeXml(route.getStartAddress())).append(" → ")
                .append(escapeXml(route.getEndAddress())).append("</name>\n");
        gpx.append("    <desc>Trasa wojskowa - Transport ID: ").append(route.getTransportSet().getId()).append("</desc>\n");
        gpx.append("    <time>").append(LocalDateTime.now()).append("</time>\n");
        gpx.append("  </metadata>\n\n");

        // Punkty startowy i końcowy
        gpx.append("  <wpt lat=\"").append(route.getStartLatitude())
                .append("\" lon=\"").append(route.getStartLongitude()).append("\">\n");
        gpx.append("    <name>START</name>\n");
        gpx.append("    <desc>").append(escapeXml(route.getStartAddress())).append("</desc>\n");
        gpx.append("  </wpt>\n\n");

        gpx.append("  <wpt lat=\"").append(route.getEndLatitude())
                .append("\" lon=\"").append(route.getEndLongitude()).append("\">\n");
        gpx.append("    <name>KONIEC</name>\n");
        gpx.append("    <desc>").append(escapeXml(route.getEndAddress())).append("</desc>\n");
        gpx.append("  </wpt>\n\n");

        // ✅ PRIORYTET 1: HERE Maps polyline (najdokładniejsza)
        String herePolyline = (String) routeData.get("herePolyline");
        if (herePolyline != null && !herePolyline.isEmpty()) {
            log.info("📍 Używam HERE Maps polyline do generowania GPX");
            try {
                List<FlexiblePolyline.LatLng> coordinates = FlexiblePolyline.decode(herePolyline);
                appendHereTrackToGpx(gpx, coordinates, "Trasa HERE Maps");
                gpx.append("</gpx>");
                return gpx.toString();
            } catch (Exception e) {
                log.warn("⚠️ Błąd dekodowania HERE polyline: {}", e.getMessage());
            }
        }

        // ✅ PRIORYTET 2: Google Maps routes (POPRAWIONE PARSOWANIE)
        List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
        if (routes != null && !routes.isEmpty()) {
            Map<String, Object> firstRoute = routes.get(0);

            // ✅ POPRAWKA: overview_polyline może być obiektem
            Object polylineObj = firstRoute.get("overview_polyline");
            String encodedPolyline = null;

            if (polylineObj instanceof String) {
                encodedPolyline = (String) polylineObj;
            } else if (polylineObj instanceof Map) {
                encodedPolyline = (String) ((Map<String, Object>) polylineObj).get("points");
            }

            if (encodedPolyline != null && !encodedPolyline.isEmpty()) {
                log.info("📍 Używam Google Maps polyline do generowania GPX");
                try {
                    List<double[]> coordinates = decodeGooglePolyline(encodedPolyline);
                    appendGoogleTrackToGpx(gpx, coordinates, "Trasa Google Maps");
                    gpx.append("</gpx>");
                    return gpx.toString();
                } catch (Exception e) {
                    log.warn("⚠️ Błąd dekodowania Google polyline: {}", e.getMessage());
                }
            }
        }

        // ✅ FALLBACK: Proste połączenie start-koniec
        log.warn("⚠️ Brak polyline - używam prostej linii między punktami");
        gpx.append("  <trk>\n");
        gpx.append("    <name>Trasa podstawowa</name>\n");
        gpx.append("    <trkseg>\n");
        gpx.append("      <trkpt lat=\"").append(route.getStartLatitude())
                .append("\" lon=\"").append(route.getStartLongitude()).append("\"/>\n");
        gpx.append("      <trkpt lat=\"").append(route.getEndLatitude())
                .append("\" lon=\"").append(route.getEndLongitude()).append("\"/>\n");
        gpx.append("    </trkseg>\n");
        gpx.append("  </trk>\n");
        gpx.append("</gpx>");

        return gpx.toString();
    }

    /**
     * ✅ DODAJE TRACK DO GPX Z LISTY WSPÓŁRZĘDNYCH HERE MAPS (LatLng)
     */
    private void appendHereTrackToGpx(StringBuilder gpx, List<FlexiblePolyline.LatLng> coordinates, String trackName) {
        gpx.append("  <trk>\n");
        gpx.append("    <name>").append(escapeXml(trackName)).append("</name>\n");
        gpx.append("    <trkseg>\n");

        for (FlexiblePolyline.LatLng coord : coordinates) {
            gpx.append("      <trkpt lat=\"").append(coord.lat)
                    .append("\" lon=\"").append(coord.lng).append("\"/>\n");
        }

        gpx.append("    </trkseg>\n");
        gpx.append("  </trk>\n");
    }

    /**
     * ✅ DODAJE TRACK DO GPX Z LISTY WSPÓŁRZĘDNYCH GOOGLE MAPS (double[])
     */
    private void appendGoogleTrackToGpx(StringBuilder gpx, List<double[]> coordinates, String trackName) {
        gpx.append("  <trk>\n");
        gpx.append("    <name>").append(escapeXml(trackName)).append("</name>\n");
        gpx.append("    <trkseg>\n");

        for (double[] coord : coordinates) {
            gpx.append("      <trkpt lat=\"").append(coord[0])
                    .append("\" lon=\"").append(coord[1]).append("\"/>\n");
        }

        gpx.append("    </trkseg>\n");
        gpx.append("  </trk>\n");
    }

    /**
     * ✅ POPRAWIONE GENEROWANIE KML - Z OBSŁUGĄ POLYLINE
     */
    private String generateKml(Map<String, Object> routeData, Route route) {
        StringBuilder kml = new StringBuilder();
        kml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        kml.append("<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n");
        kml.append("  <Document>\n");
        kml.append("    <name>").append(escapeXml(route.getStartAddress())).append(" → ")
                .append(escapeXml(route.getEndAddress())).append("</name>\n");
        kml.append("    <description>Trasa wojskowa - Transport ID: ")
                .append(route.getTransportSet().getId()).append("</description>\n\n");

        // Style dla trasy
        kml.append("    <Style id=\"routeStyle\">\n");
        kml.append("      <LineStyle>\n");
        kml.append("        <color>ff0000ff</color>\n"); // Czerwony
        kml.append("        <width>4</width>\n");
        kml.append("      </LineStyle>\n");
        kml.append("    </Style>\n\n");

        // Punkt startowy
        kml.append("    <Placemark>\n");
        kml.append("      <name>START</name>\n");
        kml.append("      <description>").append(escapeXml(route.getStartAddress())).append("</description>\n");
        kml.append("      <Point>\n");
        kml.append("        <coordinates>").append(route.getStartLongitude())
                .append(",").append(route.getStartLatitude()).append(",0</coordinates>\n");
        kml.append("      </Point>\n");
        kml.append("    </Placemark>\n\n");

        // Punkt końcowy
        kml.append("    <Placemark>\n");
        kml.append("      <name>KONIEC</name>\n");
        kml.append("      <description>").append(escapeXml(route.getEndAddress())).append("</description>\n");
        kml.append("      <Point>\n");
        kml.append("        <coordinates>").append(route.getEndLongitude())
                .append(",").append(route.getEndLatitude()).append(",0</coordinates>\n");
        kml.append("      </Point>\n");
        kml.append("    </Placemark>\n\n");

        // ✅ PRIORYTET 1: HERE Maps polyline
        String herePolyline = (String) routeData.get("herePolyline");
        if (herePolyline != null && !herePolyline.isEmpty()) {
            log.info("📍 Używam HERE Maps polyline do generowania KML");
            try {
                List<FlexiblePolyline.LatLng> coordinates = FlexiblePolyline.decode(herePolyline);
                appendHereLineStringToKml(kml, coordinates, "Trasa HERE Maps");
                kml.append("  </Document>\n</kml>");
                return kml.toString();
            } catch (Exception e) {
                log.warn("⚠️ Błąd dekodowania HERE polyline: {}", e.getMessage());
            }
        }

        // ✅ PRIORYTET 2: Google Maps polyline (POPRAWIONE PARSOWANIE)
        List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
        if (routes != null && !routes.isEmpty()) {
            Map<String, Object> firstRoute = routes.get(0);

            // ✅ POPRAWKA: overview_polyline może być obiektem
            Object polylineObj = firstRoute.get("overview_polyline");
            String encodedPolyline = null;

            if (polylineObj instanceof String) {
                encodedPolyline = (String) polylineObj;
            } else if (polylineObj instanceof Map) {
                encodedPolyline = (String) ((Map<String, Object>) polylineObj).get("points");
            }

            if (encodedPolyline != null && !encodedPolyline.isEmpty()) {
                log.info("📍 Używam Google Maps polyline do generowania KML");
                try {
                    List<double[]> coordinates = decodeGooglePolyline(encodedPolyline);
                    appendGoogleLineStringToKml(kml, coordinates, "Trasa Google Maps");
                    kml.append("  </Document>\n</kml>");
                    return kml.toString();
                } catch (Exception e) {
                    log.warn("⚠️ Błąd dekodowania Google polyline: {}", e.getMessage());
                }
            }
        }

        // ✅ FALLBACK: Proste połączenie
        log.warn("⚠️ Brak polyline - używam prostej linii między punktami");
        kml.append("    <Placemark>\n");
        kml.append("      <name>Trasa podstawowa</name>\n");
        kml.append("      <styleUrl>#routeStyle</styleUrl>\n");
        kml.append("      <LineString>\n");
        kml.append("        <coordinates>\n");
        kml.append("          ").append(route.getStartLongitude()).append(",")
                .append(route.getStartLatitude()).append(",0\n");
        kml.append("          ").append(route.getEndLongitude()).append(",")
                .append(route.getEndLatitude()).append(",0\n");
        kml.append("        </coordinates>\n");
        kml.append("      </LineString>\n");
        kml.append("    </Placemark>\n");
        kml.append("  </Document>\n</kml>");

        return kml.toString();
    }

    /**
     * ✅ DODAJE LINESTRING DO KML Z LISTY WSPÓŁRZĘDNYCH HERE MAPS (LatLng)
     */
    private void appendHereLineStringToKml(StringBuilder kml, List<FlexiblePolyline.LatLng> coordinates, String name) {
        kml.append("    <Placemark>\n");
        kml.append("      <name>").append(escapeXml(name)).append("</name>\n");
        kml.append("      <styleUrl>#routeStyle</styleUrl>\n");
        kml.append("      <LineString>\n");
        kml.append("        <tessellate>1</tessellate>\n");
        kml.append("        <coordinates>\n");

        for (FlexiblePolyline.LatLng coord : coordinates) {
            kml.append("          ").append(coord.lng).append(",")
                    .append(coord.lat).append(",0\n");
        }

        kml.append("        </coordinates>\n");
        kml.append("      </LineString>\n");
        kml.append("    </Placemark>\n");
    }

    /**
     * ✅ DODAJE LINESTRING DO KML Z LISTY WSPÓŁRZĘDNYCH GOOGLE MAPS (double[])
     */
    private void appendGoogleLineStringToKml(StringBuilder kml, List<double[]> coordinates, String name) {
        kml.append("    <Placemark>\n");
        kml.append("      <name>").append(escapeXml(name)).append("</name>\n");
        kml.append("      <styleUrl>#routeStyle</styleUrl>\n");
        kml.append("      <LineString>\n");
        kml.append("        <tessellate>1</tessellate>\n");
        kml.append("        <coordinates>\n");

        for (double[] coord : coordinates) {
            kml.append("          ").append(coord[1]).append(",")
                    .append(coord[0]).append(",0\n");
        }

        kml.append("        </coordinates>\n");
        kml.append("      </LineString>\n");
        kml.append("    </Placemark>\n");
    }

    /**
     * ✅ DEKODOWANIE GOOGLE POLYLINE
     */
    private List<double[]> decodeGooglePolyline(String encoded) {
        List<double[]> coordinates = new ArrayList<>();
        int index = 0, len = encoded.length();
        int lat = 0, lng = 0;

        while (index < len) {
            int b, shift = 0, result = 0;
            do {
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20);
            int dlat = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lat += dlat;

            shift = 0;
            result = 0;
            do {
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20);
            int dlng = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lng += dlng;

            coordinates.add(new double[]{lat / 1E5, lng / 1E5});
        }

        return coordinates;
    }

    /**
     * ✅ ESCAPE XML CHARACTERS
     */
    private String escapeXml(String text) {
        if (text == null) return "";
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&apos;");
    }

    // ═══════════════════════════════════════════════════════════════════
    // ZARZĄDZANIE TRASAMI - CRUD I OPERACJE
    // ═══════════════════════════════════════════════════════════════════

    /**
     * ✅ POBIERZ SZCZEGÓŁY WALIDACJI TRASY
     */
    public Map<String, Object> getValidationDetails(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        Map<String, Object> details = new HashMap<>();
        details.put("routeId", routeId);
        details.put("status", route.getStatus());

        try {
            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

                details.put("hasRestrictions", routeData.getOrDefault("hasRestrictions", false));
                details.put("hasWarnings", routeData.getOrDefault("hasWarnings", false));
                details.put("requiresPermit", routeData.getOrDefault("requiresPermit", false));
                details.put("warnings", routeData.getOrDefault("warnings", new ArrayList<>()));
                details.put("violations", routeData.getOrDefault("violations", new ArrayList<>()));
                details.put("permits", routeData.getOrDefault("permits", new ArrayList<>()));
                details.put("infrastructureDetails", routeData.getOrDefault("infrastructureDetails", new ArrayList<>()));
                details.put("attemptReports", routeData.getOrDefault("attemptReports", new ArrayList<>()));
                details.put("searchAttempts", routeData.getOrDefault("searchAttempts", 1));
            }
        } catch (Exception e) {
            log.error("Error parsing validation details", e);
        }

        return details;
    }

    /**
     * ✅ PRZYPISZ KIEROWCĘ DO TRASY
     */
    public RouteResponse assignDriverToRoute(Long routeId, String driverUsername) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (route.getStatus() != Route.RouteStatus.CREATED) {
            throw new RuntimeException("Cannot assign driver - route is not in CREATED status");
        }

        route.setAssignedDriverUsername(driverUsername);
        route.setStatus(Route.RouteStatus.ASSIGNED);
        Route savedRoute = routeRepository.save(route);

        log.info("✅ Assigned driver {} to route #{}", driverUsername, routeId);

        return RouteResponse.from(savedRoute);
    }

    /**
     * ✅ ZMIEŃ ZESTAW TRANSPORTOWY
     */
    public RouteResponse changeTransportSet(Long routeId, Long newTransportSetId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (route.getStatus() == Route.RouteStatus.IN_PROGRESS ||
                route.getStatus() == Route.RouteStatus.COMPLETED) {
            throw new RuntimeException("Cannot change transport set - route is already in progress or completed");
        }

        TransportSet newTransportSet = transportSetRepository.findById(newTransportSetId)
                .orElseThrow(() -> new RuntimeException("Transport set not found"));

        route.setTransportSet(newTransportSet);
        Route savedRoute = routeRepository.save(route);

        log.info("✅ Changed transport set for route #{} to #{}", routeId, newTransportSetId);

        return RouteResponse.from(savedRoute);
    }

    /**
     * ✅ ROZPOCZNIJ TRASĘ
     */
    public RouteResponse startRoute(Long routeId, String driverUsername) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (!driverUsername.equals(route.getAssignedDriverUsername())) {
            throw new RuntimeException("This route is not assigned to you");
        }

        if (route.getStatus() != Route.RouteStatus.ASSIGNED) {
            throw new RuntimeException("Route must be in ASSIGNED status to start");
        }

        route.setStatus(Route.RouteStatus.IN_PROGRESS);
        route.setStartedAt(LocalDateTime.now());
        Route savedRoute = routeRepository.save(route);

        log.info("✅ Route #{} started by driver {}", routeId, driverUsername);

        return RouteResponse.from(savedRoute);
    }

    /**
     * ✅ ZAKOŃCZ TRASĘ
     */
    public RouteResponse completeRoute(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (route.getStatus() != Route.RouteStatus.IN_PROGRESS) {
            throw new RuntimeException("Route must be in progress to complete");
        }

        route.setStatus(Route.RouteStatus.COMPLETED);
        route.setCompletedAt(LocalDateTime.now());
        Route savedRoute = routeRepository.save(route);

        log.info("✅ Route #{} completed", routeId);

        return RouteResponse.from(savedRoute);
    }

    /**
     * ✅ POBIERZ TRASY KIEROWCY
     */
    public List<RouteResponse> getRoutesByDriver(String driverUsername) {
        List<Route> routes = routeRepository.findByAssignedDriverUsername(driverUsername);
        return routes.stream()
                .map(RouteResponse::from)
                .collect(Collectors.toList());
    }

    /**
     * ✅ POBIERZ WSZYSTKIE TRASY
     */
    public List<RouteResponse> getAllRoutes() {
        List<Route> routes = routeRepository.findAll();
        return routes.stream()
                .map(RouteResponse::from)
                .collect(Collectors.toList());
    }

    /**
     * ✅ POBIERZ AKTYWNE TRASY
     */
    public List<RouteResponse> getActiveRoutes() {
        List<Route> routes = routeRepository.findByStatusIn(
                Arrays.asList(Route.RouteStatus.ASSIGNED, Route.RouteStatus.IN_PROGRESS)
        );
        return routes.stream()
                .map(RouteResponse::from)
                .collect(Collectors.toList());
    }

    /**
     * ✅ POBIERZ TRASĘ PO ID
     */
    public RouteResponse getRouteById(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        return RouteResponse.from(route);
    }

    /**
     * ✅ USUŃ TRASĘ
     */
    public void deleteRoute(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (route.getStatus() == Route.RouteStatus.IN_PROGRESS) {
            throw new RuntimeException("Cannot delete route in progress");
        }

        routeRepository.delete(route);
        log.info("✅ Route #{} deleted", routeId);
    }

    /**
     * ✅ REWALIDUJ TRASĘ
     */
    public RouteResponse revalidateRoute(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        // Utwórz nowy request z danych istniejącej trasy
        CreateRouteRequest request = new CreateRouteRequest();
        request.setStartAddress(route.getStartAddress());
        request.setEndAddress(route.getEndAddress());
        request.setStartLatitude(route.getStartLatitude());
        request.setStartLongitude(route.getStartLongitude());
        request.setEndLatitude(route.getEndLatitude());
        request.setEndLongitude(route.getEndLongitude());
        request.setTransportSetId(route.getTransportSet().getId());

        log.info("♻️ Rewalidacja trasy #{}", routeId);

        // Usuń starą trasę
        routeRepository.delete(route);

        // Utwórz nową trasę z pełną walidacją
        return createRoute(request, route.getCreatedByUsername());
    }

    /**
     * ✅ POBIERZ ALTERNATYWNE TRASY
     */
    public List<Map<String, Object>> getAlternativeRoutes(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        List<Map<String, Object>> alternatives = new ArrayList<>();

        try {
            if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

                List<Map<String, Object>> attemptReports =
                        (List<Map<String, Object>>) routeData.get("attemptReports");

                if (attemptReports != null) {
                    for (Map<String, Object> attempt : attemptReports) {
                        if (Boolean.TRUE.equals(attempt.get("passable"))) {
                            alternatives.add(attempt);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error parsing alternative routes", e);
        }

        return alternatives;
    }

    /**
     * ✅ STATYSTYKI WALIDACJI
     */
    public Map<String, Object> getValidationStatistics() {
        List<Route> allRoutes = routeRepository.findAll();

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalRoutes", allRoutes.size());

        long routesWithRestrictions = 0;
        long routesWithPermits = 0;
        long routesWithViolations = 0;
        int totalSearchAttempts = 0;

        for (Route route : allRoutes) {
            try {
                if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                    Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

                    if (Boolean.TRUE.equals(routeData.get("hasRestrictions"))) {
                        routesWithRestrictions++;
                    }
                    if (Boolean.TRUE.equals(routeData.get("requiresPermit"))) {
                        routesWithPermits++;
                    }
                    if (Boolean.TRUE.equals(routeData.get("hasViolations"))) {
                        routesWithViolations++;
                    }

                    Object attempts = routeData.get("searchAttempts");
                    if (attempts instanceof Number) {
                        totalSearchAttempts += ((Number) attempts).intValue();
                    }
                }
            } catch (Exception e) {
                log.warn("Error parsing route statistics for route #{}", route.getId());
            }
        }

        stats.put("routesWithRestrictions", routesWithRestrictions);
        stats.put("routesWithPermits", routesWithPermits);
        stats.put("routesWithViolations", routesWithViolations);
        stats.put("totalSearchAttempts", totalSearchAttempts);
        stats.put("averageSearchAttempts", allRoutes.isEmpty() ? 0 : (double) totalSearchAttempts / allRoutes.size());

        return stats;
    }

    // ═══════════════════════════════════════════════════════════════════
    // KLASA POMOCNICZA - RAPORT PRÓBY WALIDACJI
    // ═══════════════════════════════════════════════════════════════════

    /**
     * ✅ RAPORT Z JEDNEJ PRÓBY WALIDACJI TRASY
     */
    @lombok.Data
    public static class RouteAttemptReport {
        private int attemptNumber;
        private boolean passable;
        private boolean requiresPermit;
        private int blockedBridges;
        private int totalInfrastructureChecked;
        private List<String> violations = new ArrayList<>();
        private List<String> restrictions = new ArrayList<>();
        private List<String> permits = new ArrayList<>();
        private List<String> excludedBridges = new ArrayList<>();
        private List<String> criticalBridges = new ArrayList<>();
        private double successScore;
        private String error;

        public boolean isFullyPassable() {
            return passable && violations.isEmpty() && blockedBridges == 0;
        }
    }
}