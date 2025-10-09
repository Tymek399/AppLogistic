package com.military.applogistic.service;

import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
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
            log.info("╔═══════════════════════════════════╗");
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

//                // ✅ KROK 3: Sprawdź czy trasa CAŁKOWICIE ZABLOKOWANA
//                if (attemptReport.getBlockedBridges() >= 999) {
//                    log.error("🚨 Trasa całkowicie zablokowana - brak sensu dalszych prób");
//                    log.error("Powód: {}", attemptReport.getViolations());
//                    break; // Przerwij szukanie - wszystkie trasy są zablokowane
//                }

                // ✅ KROK 4: Jeśli trasa przejezdna - zapisz i zakończ
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

                // ✅ KROK 5: Jeśli trasa zablokowana - dodaj mosty do wykluczeń
                List<String> criticalBridges = attemptReport.getCriticalBridges();
                if (criticalBridges.isEmpty()) {
                    log.warn("⚠️ Brak możliwych tras dalej - wszystkie opcje wyczerpane");
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

        // ✅ KROK 6: Jeśli znaleziono trasę - zapisz do bazy
        if (validatedRouteData != null) {
            return saveValidatedRoute(request, transportSet, createdByUsername, validatedRouteData);
        }

        // ✅ KROK 7: Jeśli nie znaleziono trasy - zwróć błąd
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

        // Pobierz dane z walidacji
        List<String> violations = (List<String>) routeData.getOrDefault("violations", new ArrayList<>());
        List<String> restrictions = (List<String>) routeData.getOrDefault("restrictions", new ArrayList<>());
        List<Map<String, Object>> infrastructure =
                (List<Map<String, Object>>) routeData.getOrDefault("infrastructureDetails", new ArrayList<>());

        report.setViolations(violations);
        report.setRestrictions(restrictions);
        report.setTotalInfrastructureChecked(infrastructure.size());

        // ✅ SPRAWDŹ CZY WSZYSTKIE MOSTY/TUNELE SĄ PRZEJEZDNE
        long blockedBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .count();

        report.setBlockedBridges((int) blockedBridges);
        report.setPassable(violations.isEmpty() && blockedBridges == 0);

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
        log.info("   ✓ Przejezdna: {}", report.isPassable() ? "TAK" : "NIE");

        if (!criticalBridges.isEmpty()) {
            log.warn("   🚫 Zablokowane obiekty:");
            criticalBridges.forEach(b -> log.warn("      - {}", b));
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
            message.append("═══════════════════════════════════════════════════\n");
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
            message.append("─────────────────────────────────────────────────────\n");
            message.append("1. Zmień zestaw transportowy na lżejszy/niższy\n");
            message.append("2. Rozważ transport na kilka mniejszych zestawów\n");
            message.append("3. Skontaktuj się z zarządem dróg w sprawie pozwoleń specjalnych\n");

        } else {
            message.append("📊 Podsumowanie walidacji:\n");
            message.append("═══════════════════════════════════════════════════\n");
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
            message.append("─────────────────────────────────────────────────────\n");
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
        log.info("╔═══════════════════════════════════╗");
        log.info("PARAMETRY ZESTAWU: {}", ts.getDescription());
        log.info("────────────────────────────────────");
        log.info("Waga: {} kg ({} ton)", ts.getTotalWeightKg(), ts.getTotalWeightKg() / 1000.0);
        log.info("Wysokość: {} cm ({} m)", ts.getTotalHeightCm(), ts.getTotalHeightCm() / 100.0);
        log.info("Długość: {} cm ({} m)", ts.getTotalLengthCm(), ts.getTotalLengthCm() / 100.0);
        log.info("Szerokość: {} cm ({} m)", ts.getTotalWidthCm(), ts.getTotalWidthCm() / 100.0);
        log.info("Max nacisk na oś: {} kg ({} t)", ts.getMaxAxleLoadKg(), ts.getMaxAxleLoadKg() / 1000.0);
        log.info("Typ naczepy: {}", ts.getTrailerType());
        log.info("╚═══════════════════════════════════╝");
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

    // ═══════════════════════════════════════════════════════════════════════
    // POZOSTAŁE METODY (bez zmian)
    // ═══════════════════════════════════════════════════════════════════════

    /**
     * ✅ GENEROWANIE PLIKU GPX
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

    private String generateGpx(Map<String, Object> routeData, Route route) {
        StringBuilder gpx = new StringBuilder();
        gpx.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        gpx.append("<gpx version=\"1.1\" creator=\"MilitaryLogisticOps\" ");
        gpx.append("xmlns=\"http://www.topografix.com/GPX/1/1\" ");
        gpx.append("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ");
        gpx.append("xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd\">\n");
        gpx.append("  <metadata>\n");
        gpx.append("    <name>Military Route #").append(route.getId()).append("</name>\n");
        gpx.append("    <desc>").append(route.getStartAddress()).append(" → ").append(route.getEndAddress()).append("</desc>\n");
        gpx.append("    <time>").append(java.time.Instant.now().toString()).append("</time>\n");
        gpx.append("  </metadata>\n");
        gpx.append("  <trk>\n");
        gpx.append("    <name>Transport Route</name>\n");
        gpx.append("    <trkseg>\n");

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> routeMap = routes.get(0);
                Map<String, Object> overviewPolyline = (Map<String, Object>) routeMap.get("overview_polyline");

                if (overviewPolyline != null) {
                    String encodedPolyline = (String) overviewPolyline.get("points");
                    if (encodedPolyline != null) {
                        List<double[]> coordinates = decodePolyline(encodedPolyline);

                        for (double[] coord : coordinates) {
                            gpx.append(String.format(Locale.US,
                                    "      <trkpt lat=\"%.6f\" lon=\"%.6f\"></trkpt>\n",
                                    coord[0], coord[1]));
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error generating GPX coordinates: {}", e.getMessage());
        }

        gpx.append("    </trkseg>\n");
        gpx.append("  </trk>\n");
        gpx.append("</gpx>");

        return gpx.toString();
    }

    private String generateKml(Map<String, Object> routeData, Route route) {
        StringBuilder kml = new StringBuilder();
        kml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        kml.append("<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n");
        kml.append("  <Document>\n");
        kml.append("    <name>Military Route #").append(route.getId()).append("</name>\n");
        kml.append("    <description>").append(route.getStartAddress()).append(" → ").append(route.getEndAddress()).append("</description>\n");
        kml.append("    <Style id=\"routeStyle\">\n");
        kml.append("      <LineStyle>\n");
        kml.append("        <color>ff0000ff</color>\n");
        kml.append("        <width>4</width>\n");
        kml.append("      </LineStyle>\n");
        kml.append("    </Style>\n");
        kml.append("    <Placemark>\n");
        kml.append("      <name>Transport Route</name>\n");
        kml.append("      <styleUrl>#routeStyle</styleUrl>\n");
        kml.append("      <LineString>\n");
        kml.append("        <tessellate>1</tessellate>\n");
        kml.append("        <coordinates>\n");

        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> routeMap = routes.get(0);
                Map<String, Object> overviewPolyline = (Map<String, Object>) routeMap.get("overview_polyline");

                if (overviewPolyline != null) {
                    String encodedPolyline = (String) overviewPolyline.get("points");
                    if (encodedPolyline != null) {
                        List<double[]> coordinates = decodePolyline(encodedPolyline);

                        for (double[] coord : coordinates) {
                            kml.append(String.format(Locale.US,
                                    "          %.6f,%.6f,0\n", coord[1], coord[0]));
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error generating KML coordinates: {}", e.getMessage());
        }

        kml.append("        </coordinates>\n");
        kml.append("      </LineString>\n");
        kml.append("    </Placemark>\n");
        kml.append("  </Document>\n");
        kml.append("</kml>");

        return kml.toString();
    }

    private List<double[]> decodePolyline(String encoded) {
        List<double[]> path = new ArrayList<>();
        int index = 0;
        int len = encoded.length();
        int lat = 0;
        int lng = 0;

        while (index < len) {
            int b;
            int shift = 0;
            int result = 0;
            do {
                if (index >= len) break;
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20 && index < len);

            int dlat = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lat += dlat;

            shift = 0;
            result = 0;
            do {
                if (index >= len) break;
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20 && index < len);

            int dlng = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lng += dlng;

            path.add(new double[]{lat / 1E5, lng / 1E5});
        }

        return path;
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getRoutesByDriver(String driverUsername) {
        return routeRepository.findByAssignedDriverUsername(driverUsername).stream()
                .map(r -> convertToResponse(r, null))
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getAllRoutes() {
        return routeRepository.findAll().stream()
                .map(r -> convertToResponse(r, null))
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<RouteResponse> getActiveRoutes() {
        return routeRepository.findActiveRoutes().stream()
                .map(r -> convertToResponse(r, null))
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public RouteResponse getRouteById(Long id) {
        Route route = routeRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        return convertToResponse(route, null);
    }

    public RouteResponse assignDriverToRoute(Long routeId, String driverUsername) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        route.setAssignedDriverUsername(driverUsername);
        route.setStatus(Route.RouteStatus.ASSIGNED);
        route.setAssignedAt(LocalDateTime.now());
        return convertToResponse(routeRepository.save(route), null);
    }

    public RouteResponse startRoute(Long routeId, String driverUsername) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        if (!driverUsername.equals(route.getAssignedDriverUsername())) {
            throw new RuntimeException("Driver not assigned to this route");
        }
        route.setStatus(Route.RouteStatus.ACTIVE);
        route.setStartedAt(LocalDateTime.now());
        return convertToResponse(routeRepository.save(route), null);
    }

    public RouteResponse completeRoute(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        route.setStatus(Route.RouteStatus.COMPLETED);
        route.setCompletedAt(LocalDateTime.now());
        return convertToResponse(routeRepository.save(route), null);
    }

    public void deleteRoute(Long routeId) {
        routeRepository.deleteById(routeId);
    }

    public RouteResponse changeTransportSet(Long routeId, Long newTransportSetId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (route.getStatus() != Route.RouteStatus.CREATED) {
            throw new RuntimeException("Can only change transport set for CREATED routes");
        }

        TransportSet newTransportSet = transportSetRepository.findById(newTransportSetId)
                .orElseThrow(() -> new RuntimeException("Transport set not found"));

        route.setTransportSet(newTransportSet);
        return convertToResponse(routeRepository.save(route), null);
    }

    public RouteResponse revalidateRoute(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        Map<String, Object> newValidation = hereMapsService.validateRoute(
                route.getStartLatitude(),
                route.getStartLongitude(),
                route.getEndLatitude(),
                route.getEndLongitude(),
                route.getTransportSet().getTotalWeightKg(),
                route.getTransportSet().getTotalHeightCm(),
                null
        );

        try {
            Map<String, Object> routeData = route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}") ?
                    objectMapper.readValue(route.getRouteDataJson(), Map.class) : new HashMap<>();

            routeData.putAll(newValidation);
            routeData.put("last_validation", LocalDateTime.now().toString());
            routeData.put("revalidated", true);

            route.setRouteDataJson(objectMapper.writeValueAsString(routeData));
            routeRepository.save(route);

            return convertToResponse(route, routeData);

        } catch (Exception e) {
            throw new RuntimeException("Failed to update route validation");
        }
    }

    public Map<String, Object> getValidationDetails(Long routeId) {
        try {
            Route route = routeRepository.findById(routeId)
                    .orElseThrow(() -> new RuntimeException("Route not found"));

            if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
                Map<String, Object> noData = new HashMap<>();
                noData.put("routeId", routeId);
                noData.put("validationAvailable", false);
                noData.put("message", "Brak danych walidacji dla tej trasy");
                return noData;
            }

            Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

            Map<String, Object> validationDetails = new HashMap<>();
            validationDetails.put("routeId", routeId);
            validationDetails.put("validationAvailable", true);
            validationDetails.put("validationSource", routeData.getOrDefault("validation_source", "unknown"));
            validationDetails.put("hasRestrictions", routeData.getOrDefault("hasRestrictions", false));
            validationDetails.put("hasWarnings", routeData.getOrDefault("hasWarnings", false));
            validationDetails.put("hasViolations", routeData.getOrDefault("hasViolations", false));
            validationDetails.put("warnings", routeData.getOrDefault("warnings", Collections.emptyList()));
            validationDetails.put("restrictions", routeData.getOrDefault("restrictions", Collections.emptyList()));
            validationDetails.put("violations", routeData.getOrDefault("violations", Collections.emptyList()));
            validationDetails.put("validationDetails", routeData.getOrDefault("validationDetails", Collections.emptyList()));
            validationDetails.put("transportSetInfo", routeData.getOrDefault("transportSet", Collections.emptyMap()));
            validationDetails.put("routeJustification", routeData.getOrDefault("routeJustification", Collections.emptyList()));
            validationDetails.put("searchAttempts", routeData.getOrDefault("searchAttempts", null));
            validationDetails.put("successfulAttempt", routeData.getOrDefault("successfulAttempt", null));
            validationDetails.put("validationSummary", routeData.getOrDefault("validationSummary", Collections.emptyMap()));

            return validationDetails;

        } catch (Exception e) {
            log.error("Błąd pobierania szczegółów walidacji: {}", e.getMessage());
            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("routeId", routeId);
            errorResponse.put("validationAvailable", false);
            errorResponse.put("error", "Błąd podczas odczytu danych walidacji: " + e.getMessage());
            return errorResponse;
        }
    }

    public List<Map<String, Object>> getAlternativeRoutes(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        return hereMapsService.getAlternativeRoutes(
                route.getStartLatitude(), route.getStartLongitude(),
                route.getEndLatitude(), route.getEndLongitude(),
                route.getTransportSet()
        );
    }

    public Map<String, Object> getValidationStatistics() {
        List<Route> allRoutes = routeRepository.findAll();
        long totalRoutes = allRoutes.size();
        long routesWithRestrictions = 0;
        long routesWithWarnings = 0;
        long routesWithViolations = 0;
        long lightVehicleRoutes = 0;

        for (Route route : allRoutes) {
            try {
                if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                    Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
                    if (Boolean.TRUE.equals(routeData.get("hasRestrictions"))) routesWithRestrictions++;
                    if (Boolean.TRUE.equals(routeData.get("hasWarnings"))) routesWithWarnings++;
                    if (Boolean.TRUE.equals(routeData.get("hasViolations"))) routesWithViolations++;
                    if (Boolean.TRUE.equals(routeData.get("lightVehicle"))) lightVehicleRoutes++;
                }
            } catch (Exception e) {
                log.warn("Cannot parse route data for stats");
            }
        }

        Map<String, Object> stats = new HashMap<>();
        stats.put("total_routes", totalRoutes);
        stats.put("routes_with_restrictions", routesWithRestrictions);
        stats.put("routes_with_warnings", routesWithWarnings);
        stats.put("routes_with_violations", routesWithViolations);
        stats.put("light_vehicle_routes", lightVehicleRoutes);
        stats.put("heavy_vehicle_routes", totalRoutes - lightVehicleRoutes);

        return stats;
    }

    /**
     * ✅ KLASA WEWNĘTRZNA - RAPORT PRÓBY
     */
    @lombok.Data
    static class RouteAttemptReport {
        private int attemptNumber;
        private List<String> excludedBridges = new ArrayList<>();
        private int totalInfrastructureChecked;
        private int blockedBridges;
        private List<String> violations = new ArrayList<>();
        private List<String> restrictions = new ArrayList<>();
        private List<String> criticalBridges = new ArrayList<>();
        private boolean passable;
        private String error;
        private double successScore;

        public boolean isFullyPassable() {
            return passable && violations.isEmpty() && blockedBridges == 0;
        }
    }
}