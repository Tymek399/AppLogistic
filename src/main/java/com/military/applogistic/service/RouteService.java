package com.military.applogistic.service;

import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.util.FlexiblePolyline;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    private final GeocodingService geocodingService;

    private static final int MAX_ROUTE_ATTEMPTS = 10;
    private static final int LIGHT_VEHICLE_THRESHOLD_KG = 5000;

    /**
     * ✅ GŁÓWNA METODA TWORZENIA TRASY
     */
    public RouteResponse createRoute(CreateRouteRequest request, String createdByUsername) {
        log.info("========================================");
        log.info("ROZPOCZĘCIE PLANOWANIA TRASY");
        log.info("========================================");

        TransportSet transportSet = transportSetRepository.findById(request.getTransportSetId())
                .orElseThrow(() -> new RuntimeException("Nie znaleziono zestawu transportowego"));

        logTransportSetParameters(transportSet);
        validateTransportSet(transportSet);

        // Najpierw pokazujemy początkową trasę Google na mapie
        Map<String, Object> initialGoogleRoute = getInitialGoogleRoute(
                request.getStartAddress(),
                request.getEndAddress()
        );

        // LEKKIE POJAZDY - szybka ścieżka bez walidacji
        if (transportSet.getTotalWeightKg() <= LIGHT_VEHICLE_THRESHOLD_KG) {
            log.info("🚗 LEKKI POJAZD (≤5t) - POMIJAM WALIDACJĘ MOSTÓW");
            return createLightVehicleRoute(request, transportSet, createdByUsername, initialGoogleRoute);
        }

        // CIĘŻKIE POJAZDY - PEŁNA WALIDACJA
        return createHeavyVehicleRouteWithValidation(request, transportSet, createdByUsername, initialGoogleRoute);
    }

    /**
     * ✅ NOWA METODA - Pobiera początkową trasę Google do wyświetlenia na mapie
     */
    private Map<String, Object> getInitialGoogleRoute(String startAddress, String endAddress) {
        try {
            log.info("🔍 Pobieram początkową trasę Google Maps do podglądu");
            Map<String, Object> googleRoute = googleMapsService.getBasicRoute(startAddress, endAddress);
            googleRoute.put("isPreview", true);
            googleRoute.put("timestamp", LocalDateTime.now().toString());
            return googleRoute;
        } catch (Exception e) {
            log.error("Błąd pobierania trasy podglądu: {}", e.getMessage());
            return new HashMap<>();
        }
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
                                                  String createdByUsername,
                                                  Map<String, Object> initialGoogleRoute) {
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
            routeData.put("initialGoogleRoute", initialGoogleRoute);

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
     * ✅✅✅ NOWA GŁÓWNA METODA - INTELIGENTNE WYSZUKIWANIE TRASY ✅✅✅
     *
     * ALGORYTM:
     * 1. Próba #1: Autostrady i drogi główne (optymalna trasa)
     * 2. Próby #2-N: Wyszukiwanie alternatyw z omijaniem przeszkód
     * 3. Ostateczność: Draft dla operatora
     */
    private RouteResponse createHeavyVehicleRouteWithValidation(
            CreateRouteRequest request,
            TransportSet transportSet,
            String createdByUsername,
            Map<String, Object> initialGoogleRoute) {

        log.info("╔════════════════════════════════════════════════════════════╗");
        log.info("║     🎯 INTELIGENTNE WYSZUKIWANIE TRASY DLA POJAZDU >5T    ║");
        log.info("╚════════════════════════════════════════════════════════════╝");

        List<RouteAttemptReport> allAttempts = new ArrayList<>();
        Set<String> excludedInfrastructure = new HashSet<>();
        Map<String, Object> validatedRouteData = null;

        // ============================================================================
        // KROK 1: PRÓBA #1 - TRASA OPTYMALNA (AUTOSTRADY PREFEROWANE)
        // ============================================================================

        log.info("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        log.info("🛣️  PRÓBA #1: TRASA OPTYMALNA (PREFEROWANE AUTOSTRADY)");
        log.info("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

        try {
            Map<String, Object> routeData = googleMapsService.getRoute(
                    request.getStartAddress(),
                    request.getEndAddress(),
                    transportSet,
                    new HashSet<>(), // Brak wykluczeń
                    true // ✅ PREFERUJ AUTOSTRADY
            );

            RouteAttemptReport attemptReport = validateRouteBeforeSaving(
                    routeData, 1, new HashSet<>(), transportSet
            );
            attemptReport.setPreferredHighways(true);
            allAttempts.add(attemptReport);

            if (attemptReport.isFullyPassable()) {
                log.info("╔════════════════════════════════════════════════════════════╗");
                log.info("║  🎉 SUKCES! ZNALEZIONO OPTYMALNĄ TRASĘ (AUTOSTRADY)       ║");
                log.info("║  ✅ Próba #1 zakończona sukcesem                          ║");
                log.info("╚════════════════════════════════════════════════════════════╝");

                routeData.put("searchAttempts", 1);
                routeData.put("attemptReports", allAttempts);
                routeData.put("validationCompleted", true);
                routeData.put("createdAt", LocalDateTime.now().toString());
                routeData.put("initialGoogleRoute", initialGoogleRoute);
                routeData.put("routeType", "OPTIMAL_HIGHWAY");

                return saveValidatedRoute(request, transportSet, createdByUsername, routeData);
            }

            // Trasa autostradowa jest zablokowana
            log.warn("⚠️  Trasa autostradowa ma przeszkody - rozpoczynam szukanie alternatyw");

            List<String> criticalBridges = attemptReport.getCriticalBridges();
            if (!criticalBridges.isEmpty()) {
                excludedInfrastructure.addAll(criticalBridges);
                log.info("🚫 Wykluczam {} obiektów z próby #1", criticalBridges.size());
            }

        } catch (Exception e) {
            log.error("❌ Błąd w próbie #1: {}", e.getMessage());
            RouteAttemptReport errorReport = new RouteAttemptReport();
            errorReport.setAttemptNumber(1);
            errorReport.setError(e.getMessage());
            errorReport.setPreferredHighways(true);
            allAttempts.add(errorReport);
        }

        // ============================================================================
        // KROK 2: PĘTLA WYSZUKIWANIA ALTERNATYW (PRÓBY #2 DO #N)
        // ============================================================================

        log.info("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        log.info("🔄 ROZPOCZYNAM WYSZUKIWANIE TRAS ALTERNATYWNYCH");
        log.info("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

        for (int attempt = 2; attempt <= MAX_ROUTE_ATTEMPTS; attempt++) {
            log.info("┌────────────────────────────────────────────────────────────┐");
            log.info("│ 🔍 PRÓBA #{} - Szukanie trasy alternatywnej                │", attempt);
            log.info("│ 🚫 Wykluczonych obiektów: {}                               │", excludedInfrastructure.size());
            log.info("└────────────────────────────────────────────────────────────┘");

            try {
                Map<String, Object> routeData = googleMapsService.getRoute(
                        request.getStartAddress(),
                        request.getEndAddress(),
                        transportSet,
                        excludedInfrastructure, // Wykluczamy znalezione przeszkody
                        false // ✅ NIE preferuj autostrad - szukaj JAKIEJKOLWIEK drogi
                );

                RouteAttemptReport attemptReport = validateRouteBeforeSaving(
                        routeData, attempt, excludedInfrastructure, transportSet
                );
                attemptReport.setPreferredHighways(false);
                allAttempts.add(attemptReport);

                if (attemptReport.isFullyPassable()) {
                    log.info("╔════════════════════════════════════════════════════════════╗");
                    log.info("║  🎉 SUKCES! ZNALEZIONO BEZPIECZNĄ TRASĘ ALTERNATYWNĄ      ║");
                    log.info("║  ✅ Próba #{} zakończona sukcesem                         ║", attempt);
                    log.info("╚════════════════════════════════════════════════════════════╝");

                    routeData.put("searchAttempts", attempt);
                    routeData.put("attemptReports", allAttempts);
                    routeData.put("validationCompleted", true);
                    routeData.put("createdAt", LocalDateTime.now().toString());
                    routeData.put("initialGoogleRoute", initialGoogleRoute);
                    routeData.put("routeType", "ALTERNATIVE_SAFE");

                    return saveValidatedRoute(request, transportSet, createdByUsername, routeData);
                }

                // Trasa nadal ma przeszkody
                List<String> newCriticalBridges = attemptReport.getCriticalBridges();

                if (newCriticalBridges.isEmpty()) {
                    log.warn("⚠️  Google Maps nie znalazł więcej tras - przerywam pętlę");
                    break;
                }

                int beforeSize = excludedInfrastructure.size();
                excludedInfrastructure.addAll(newCriticalBridges);
                int addedCount = excludedInfrastructure.size() - beforeSize;

                log.info("🚫 Wykluczam dodatkowo {} nowych obiektów", addedCount);

            } catch (Exception e) {
                log.error("❌ Błąd w próbie #{}: {}", attempt, e.getMessage());
                RouteAttemptReport errorReport = new RouteAttemptReport();
                errorReport.setAttemptNumber(attempt);
                errorReport.setError(e.getMessage());
                errorReport.setPreferredHighways(false);
                allAttempts.add(errorReport);
            }
        }

        // ... wewnątrz metody createHeavyVehicleRouteWithValidation ...

        // ============================================================================
        // KROK 3: OSTATECZNA PORAŻKA - DRAFT DLA OPERATORA
        // ============================================================================

        log.info("╔════════════════════════════════════════════════════════════╗");
        log.info("║  ⚠️  WYCZERPANO WSZYSTKIE PRÓBY AUTOMATYCZNEGO WYSZUKIWANIA║");
        log.info("║  📋 Tworzę trasę DRAFT - wymaga decyzji operatora         ║");
        log.info("╚════════════════════════════════════════════════════════════╝");

        log.warn("⚠️  Nie znaleziono w pełni przejezdnej trasy po {} próbach", allAttempts.size());
        log.warn("🚫 Łączna liczba zablokowanych obiektów: {}", excludedInfrastructure.size());

        // Zbierz wszystkie unikalne punkty problematyczne
        Set<String> allRejectedPointsSet = new HashSet<>();
        List<Map<String, Object>> rejectedPointsDetails = new ArrayList<>();

        // ✅ POPRAWKA: Zbieramy punkty problematyczne ze WSZYSTKICH prób
        for (RouteAttemptReport report : allAttempts) {
            // 1. Zbieramy BLOKUJĄCE MOSTY (jeśli są)
            if (report.getCriticalBridges() != null && !report.getCriticalBridges().isEmpty()) {
                for (String bridge : report.getCriticalBridges()) {
                    if (allRejectedPointsSet.add(bridge)) { // <-- Tylko nazwy mostów
                        Map<String, Object> rejectedPoint = new HashMap<>();
                        rejectedPoint.put("name", bridge);
                        rejectedPoint.put("firstSeenAttempt", report.getAttemptNumber());
                        List<String> reasons = report.getViolations() != null && !report.getViolations().isEmpty() ?
                                report.getViolations() : List.of("Przekroczone parametry mostu");
                        rejectedPoint.put("reason", reasons);
                        rejectedPoint.put("canBeAccepted", true); // Mosty można akceptować
                        rejectedPointsDetails.add(rejectedPoint);
                    }
                }
            }
            // 2. Zbieramy BŁĘDY WALIDACJI (np. z HERE), jeśli nie ma mostów
            else if (report.getViolations() != null && !report.getViolations().isEmpty()) {
                // To jest błąd na poziomie trasy, nie punktu (np. HERE zablokował)
                String reasonKey = String.join(", ", report.getViolations());
                if (allRejectedPointsSet.add(reasonKey)) { // <-- Użyj błędu jako klucza
                    Map<String, Object> rejectedPoint = new HashMap<>();
                    rejectedPoint.put("name", "Błąd walidacji trasy (np. HERE Maps)");
                    rejectedPoint.put("firstSeenAttempt", report.getAttemptNumber());
                    rejectedPoint.put("reason", report.getViolations());
                    // Błędu walidacji całej trasy nie można "zaakceptować" punktowo
                    // Operator musi albo zaakceptować całą trasę mimo wszystko, albo ją odrzucić.
                    rejectedPoint.put("canBeAccepted", true);
                    rejectedPointsDetails.add(rejectedPoint);
                }
            }
        }

        log.info("📊 Znaleziono {} unikalnych punktów problematycznych", rejectedPointsDetails.size());

        return saveDraftRouteWithProblems(
                request, transportSet, createdByUsername,
                initialGoogleRoute, allAttempts, rejectedPointsDetails
        );
    }

    /**
     * ✅ NOWA METODA - Zapisuje trasę jako DRAFT z problemami
     */
    private RouteResponse saveDraftRouteWithProblems(
            CreateRouteRequest request,
            TransportSet transportSet,
            String createdByUsername,
            Map<String, Object> initialGoogleRoute,
            List<RouteAttemptReport> allAttempts,
            List<Map<String, Object>> rejectedPoints) {

        Map<String, Object> routeData = new HashMap<>(initialGoogleRoute);

        routeData.put("isDraft", true);
        routeData.put("hasValidationProblems", true);
        routeData.put("attemptReports", allAttempts);
        routeData.put("rejectedPoints", rejectedPoints);
        routeData.put("requiresOperatorDecision", true);
        routeData.put("routeType", "DRAFT_REQUIRES_APPROVAL");

        List<String> operatorMessages = new ArrayList<>();
        operatorMessages.add("⚠️  TRASA NIEPRZEJEZDNA - wymaga decyzji operatora");
        operatorMessages.add(String.format("System wykonał %d prób znalezienia bezpiecznej trasy", allAttempts.size()));
        operatorMessages.add(String.format("Znaleziono %d punktów problematycznych", rejectedPoints.size()));

        if (!rejectedPoints.isEmpty()) {
            operatorMessages.add("");
            operatorMessages.add("📋 PUNKTY DO PRZEGLĄDU:");
            for (Map<String, Object> point : rejectedPoints) {
                operatorMessages.add(String.format("  • %s (próba #%d)",
                        point.get("name"),
                        point.get("firstSeenAttempt")));
            }
        }

        operatorMessages.add("");
        operatorMessages.add("💡 OPCJE OPERATORA:");
        operatorMessages.add("  1️⃣  Zaakceptuj trasę mimo problemów (na własną odpowiedzialność)");
        operatorMessages.add("  2️⃣  Zmień zestaw transportowy na lżejszy");
        operatorMessages.add("  3️⃣  Wybierz inną trasę start/koniec");
        operatorMessages.add("  4️⃣  Odrzuć i usuń tę trasę");

        routeData.put("operatorMessages", operatorMessages);

        Route route = buildRouteEntity(request, transportSet, createdByUsername, routeData);
        route.setStatus(Route.RouteStatus.VALIDATION_REQUIRED);
        route.setIsDraft(true);
        route.setHasValidationProblems(true);

        try {
            String rejectedPointsJson = objectMapper.writeValueAsString(rejectedPoints);
            route.setRejectedPointsJson(rejectedPointsJson);
        } catch (Exception e) {
            log.error("Błąd serializacji rejected points", e);
        }

        Route savedRoute = routeRepository.save(route);

        log.info("╔════════════════════════════════════════════════════════════╗");
        log.info("║  📝 Trasa #{} zapisana jako DRAFT                         ║", savedRoute.getId());
        log.info("║  ⚠️  Status: VALIDATION_REQUIRED                          ║");
        log.info("║  👤 Wymaga akceptacji operatora                          ║");
        log.info("╚════════════════════════════════════════════════════════════╝");

        return convertToResponse(savedRoute, routeData);
    }
    /**
     * ✅ NOWA METODA - Akceptacja trasy przez operatora
     */
    public RouteResponse acceptRouteWithProblems(Long routeId, String operatorUsername,
                                                 String comment, List<String> acceptedPoints) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Nie znaleziono trasy"));

        if (!route.getIsDraft() || !route.getHasValidationProblems()) {
            throw new RuntimeException("Ta trasa nie wymaga akceptacji operatora");
        }

        route.setOperatorAccepted(true);
        route.setOperatorAcceptedBy(operatorUsername);
        route.setOperatorAcceptedAt(LocalDateTime.now());
        route.setOperatorComment(comment);
        route.setStatus(Route.RouteStatus.CREATED);
        route.setIsDraft(false);

        try {
            Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
            routeData.put("operatorAccepted", true);
            routeData.put("operatorAcceptedBy", operatorUsername);
            routeData.put("operatorAcceptedAt", LocalDateTime.now().toString());
            routeData.put("operatorComment", comment);
            routeData.put("acceptedPoints", acceptedPoints);

            List<String> driverWarnings = new ArrayList<>();
            driverWarnings.add("⚠️  UWAGA: Trasa zaakceptowana przez operatora mimo ograniczeń");
            driverWarnings.add("Operator: " + operatorUsername);
            driverWarnings.add("Komentarz: " + comment);
            driverWarnings.add("Zachowaj szczególną ostrożność podczas przejazdu");
            routeData.put("driverWarnings", driverWarnings);

            route.setRouteDataJson(objectMapper.writeValueAsString(routeData));
        } catch (Exception e) {
            log.error("Błąd aktualizacji danych trasy", e);
        }

        Route savedRoute = routeRepository.save(route);
        log.info("✅ Trasa #{} zaakceptowana przez operatora {} mimo problemów", routeId, operatorUsername);

        return convertToResponseFromEntity(savedRoute);
    }

    /**
     * ✅ NOWA METODA - Pobieranie tras wymagających akceptacji
     */
    public List<RouteResponse> getRoutesRequiringAcceptance() {
        List<Route> draftRoutes = routeRepository.findByStatusAndIsDraft(
                Route.RouteStatus.VALIDATION_REQUIRED, true);

        return draftRoutes.stream()
                .map(this::convertToResponseFromEntity)
                .collect(Collectors.toList());
    }

    /**
     * ✅ WALIDACJA TRASY PRZED ZAPISEM
     */
    private RouteAttemptReport validateRouteBeforeSaving(
            Map<String, Object> routeData,
            int attemptNumber,
            Set<String> excludedBridges,
            TransportSet transportSet) {

        RouteAttemptReport report = new RouteAttemptReport();
        report.setAttemptNumber(attemptNumber);
        report.setExcludedBridges(new ArrayList<>(excludedBridges));

        if (Boolean.TRUE.equals(routeData.get("routeBlocked"))) {
            String blockReason = (String) routeData.get("blockReason");
            log.error("🚨 Trasa zablokowana w próbie #{}: {}", attemptNumber, blockReason);
            report.setPassable(false);
            report.setViolations(List.of(blockReason));
            report.setBlockedBridges(999);
            report.setCriticalBridges(List.of());
            report.setSuccessScore(0);
            return report;
        }

        if (Boolean.TRUE.equals(routeData.get("requiresPermit"))) {
            List<String> permits = (List<String>) routeData.getOrDefault("permits", new ArrayList<>());
            log.info("⚠️  Trasa wymaga pozwolenia w próbie #{}: {}", attemptNumber, permits);
            report.setPassable(true);
            report.setRequiresPermit(true);
            report.setPermits(permits);
            report.setBlockedBridges(0);
            report.setCriticalBridges(new ArrayList<>());
            report.setSuccessScore(90);
            return report;
        }

        List<String> violations = (List<String>) routeData.getOrDefault("violations", new ArrayList<>());
        List<String> restrictions = (List<String>) routeData.getOrDefault("restrictions", new ArrayList<>());
        List<String> permits = (List<String>) routeData.getOrDefault("permits", new ArrayList<>());
        List<Map<String, Object>> infrastructure =
                (List<Map<String, Object>>) routeData.getOrDefault("infrastructureDetails", new ArrayList<>());

        report.setViolations(violations);
        report.setRestrictions(restrictions);
        report.setPermits(permits);
        report.setTotalInfrastructureChecked(infrastructure.size());

        long blockedBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .count();

        report.setBlockedBridges((int) blockedBridges);
        report.setPassable(violations.isEmpty() && blockedBridges == 0);
        report.setRequiresPermit(!permits.isEmpty());

        List<String> criticalBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .map(i -> (String) i.get("name"))
                .limit(100)
                .collect(Collectors.toList());
        report.setCriticalBridges(criticalBridges);

        double successScore = calculateAttemptScore(report, transportSet);
        report.setSuccessScore(successScore);

        log.info("📊 WALIDACJA PRÓBY #{}:", attemptNumber);
        log.info("   ✓ Obiektów: {}, Zablokowanych: {}, Naruszeń: {}, Pozwoleń: {}, Przejezdna: {}",
                infrastructure.size(), blockedBridges, violations.size(), permits.size(), report.isPassable() ? "TAK" : "NIE");
        if (!criticalBridges.isEmpty()) {
            criticalBridges.forEach(b -> log.warn("      - 🚫 Zablokowany: {}", b));
        }

        return report;
    }

    /**
     * ✅ OBLICZA WYNIK PRÓBY (0-100)
     */
    private double calculateAttemptScore(RouteAttemptReport report, TransportSet transportSet) {
        double score = 100.0;
        score -= report.getViolations().size() * 30;
        score -= report.getBlockedBridges() * 25;
        score -= report.getRestrictions().size() * 10;
        score -= report.getPermits().size() * 5;
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

        Map<String, Object> validationSummary = new HashMap<>();
        validationSummary.put("totalAttempts", routeData.get("searchAttempts"));
        validationSummary.put("validationCompleted", true);
        validationSummary.put("validatedAt", LocalDateTime.now().toString());
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
     * ✅ LOGUJE PARAMETRY ZESTAWU
     */
    private void logTransportSetParameters(TransportSet ts) {
        log.info("╔══════════════════════════════════════╗");
        log.info("PARAMETRY ZESTAWU: {}", ts.getDescription());
        log.info("────────────────────────────────────────");
        log.info("Waga: {} kg ({} ton)", ts.getTotalWeightKg(), ts.getTotalWeightKg() / 1000.0);
        log.info("Wysokość: {} cm ({} m)", ts.getTotalHeightCm(), ts.getTotalHeightCm() / 100.0);
        log.info("Długość: {} cm ({} m)", ts.getTotalLengthCm(), ts.getTotalLengthCm() / 100.0);
        log.info("Szerokość: {} cm ({} m)", ts.getTotalWidthCm(), ts.getTotalWidthCm() / 100.0);
        log.info("Max nacisk na oś: {} kg", ts.getMaxAxleLoadKg());
        log.info("Typ naczepy: {}", ts.getTrailerType());
        log.info("╚══════════════════════════════════════╝");
    }

    /**
     * ✅ BUDUJE ENCJĘ ROUTE
     */
    private Route buildRouteEntity(CreateRouteRequest request, TransportSet transportSet,
                                   String createdByUsername, Map<String, Object> routeData) {
        Route route = new Route();
        // ✅ FIX: Geokoduj adresy aby uniknąć błędów typu "ul. Łódzka, Gdańsk" -> "Łódź"
        GeocodingService.GeocodingResult startGeo = geocodingService.geocodeAddress(request.getStartAddress());
        GeocodingService.GeocodingResult endGeo = geocodingService.geocodeAddress(request.getEndAddress());

        String finalStartAddress = (startGeo != null && startGeo.getFormattedAddress() != null)
                ? startGeo.getFormattedAddress()
                : request.getStartAddress();
        String finalEndAddress = (endGeo != null && endGeo.getFormattedAddress() != null)
                ? endGeo.getFormattedAddress()
                : request.getEndAddress();

        log.info("✅ Adresy zweryfikowane:");
        log.info("   Start: '{}' → '{}'", request.getStartAddress(), finalStartAddress);
        log.info("   End: '{}' → '{}'", request.getEndAddress(), finalEndAddress);

        route.setStartAddress(finalStartAddress);
        route.setEndAddress(finalEndAddress);
        route.setStartLatitude(request.getStartLatitude());
        route.setStartLongitude(request.getStartLongitude());
        route.setEndLatitude(request.getEndLatitude());
        route.setEndLongitude(request.getEndLongitude());
        route.setTransportSet(transportSet);
        route.setCreatedByUsername(createdByUsername);
        route.setStatus(Route.RouteStatus.CREATED);

        Boolean hasProblems = (Boolean) routeData.getOrDefault("hasValidationProblems", false);
        route.setHasValidationProblems(hasProblems);
        route.setIsDraft((Boolean) routeData.getOrDefault("isDraft", false));

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

                    Object distanceObj = leg.get("distance");
                    if (distanceObj instanceof Map) {
                        Object valueObj = ((Map<String, Object>) distanceObj).get("value");
                        if (valueObj instanceof Number) {
                            route.setTotalDistanceKm(((Number) valueObj).doubleValue() / 1000.0);
                        }
                    }

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
            log.warn("⚠️  Nie można wyciągnąć metryk: {}", e.getMessage());
        }
    }

    /**
     * ✅ KONWERTUJE DO RESPONSE
     */
    private RouteResponse convertToResponse(Route route, Map<String, Object> routeData) {
        RouteResponse response = new RouteResponse();
        response.setId(route.getId());
        response.setStartAddress(route.getStartAddress());
        response.setEndAddress(route.getEndAddress());
        response.setStatus(route.getStatus().toString());
        response.setDistance(route.getTotalDistanceKm());
        response.setEstimatedTime(route.getEstimatedTimeMinutes());
        response.setTransportSetId(route.getTransportSet().getId());
        response.setCreatedBy(route.getCreatedByUsername());
        response.setCreatedAt(route.getCreatedAt());

        response.setIsDraft(route.getIsDraft());
        response.setHasValidationProblems(route.getHasValidationProblems());
        response.setOperatorAccepted(route.getOperatorAccepted());
        response.setOperatorMessages((List<String>) routeData.get("operatorMessages"));
        response.setRejectedPoints((List<Map<String, Object>>) routeData.get("rejectedPoints"));

        if (routeData != null) {
            response.setValidation((Map<String, Object>) routeData.get("validation"));
            response.setRouteData(routeData);
        }

        return response;
    }

    /**
     * ✅ KONWERTUJE Z ENCJI DO RESPONSE
     */
    private RouteResponse convertToResponseFromEntity(Route route) {
        RouteResponse response = new RouteResponse();
        response.setId(route.getId());
        response.setStartAddress(route.getStartAddress());
        response.setEndAddress(route.getEndAddress());
        response.setStatus(route.getStatus().toString());
        response.setDistance(route.getTotalDistanceKm());
        response.setEstimatedTime(route.getEstimatedTimeMinutes());
        response.setTransportSetId(route.getTransportSet().getId());
        response.setCreatedBy(route.getCreatedByUsername());
        response.setCreatedAt(route.getCreatedAt());

        response.setIsDraft(route.getIsDraft());
        response.setHasValidationProblems(route.getHasValidationProblems());
        response.setOperatorAccepted(route.getOperatorAccepted());

        try {
            if (route.getRouteDataJson() != null) {
                Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
                response.setValidation((Map<String, Object>) routeData.get("validation"));
                response.setRouteData(routeData);
                response.setOperatorMessages((List<String>) routeData.get("operatorMessages"));
            }
            if (route.getRejectedPointsJson() != null) {
                List<Map<String, Object>> rejectedPoints = objectMapper.readValue(
                        route.getRejectedPointsJson(), List.class);
                response.setRejectedPoints(rejectedPoints);
            }
        } catch (Exception e) {
            log.error("Błąd parsowania danych JSON", e);
        }

        return response;
    }// ═══════════════════════════════════════════════════════════════════════════
    // GENEROWANIE PLIKÓW NAWIGACYJNYCH (GPX/KML)
    // ═══════════════════════════════════════════════════════════════════════════

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
        gpx.append("<gpx version=\"1.1\" creator=\"MilitaryLogisticOps\" xmlns=\"http://www.topografix.com/GPX/1/1\">\n");
        gpx.append("  <metadata>\n");
        gpx.append("    <name>").append(escapeXml(route.getStartAddress())).append(" → ").append(escapeXml(route.getEndAddress())).append("</name>\n");
        gpx.append("    <desc>Trasa wojskowa - Transport ID: ").append(route.getTransportSet().getId()).append("</desc>\n");
        gpx.append("    <time>").append(LocalDateTime.now()).append("</time>\n");
        gpx.append("  </metadata>\n\n");
        gpx.append("  <wpt lat=\"").append(route.getStartLatitude()).append("\" lon=\"").append(route.getStartLongitude()).append("\">\n");
        gpx.append("    <name>START</name>\n<desc>").append(escapeXml(route.getStartAddress())).append("</desc>\n</wpt>\n\n");
        gpx.append("  <wpt lat=\"").append(route.getEndLatitude()).append("\" lon=\"").append(route.getEndLongitude()).append("\">\n");
        gpx.append("    <name>KONIEC</name>\n<desc>").append(escapeXml(route.getEndAddress())).append("</desc>\n</wpt>\n\n");

        String herePolyline = (String) routeData.get("herePolyline");
        if (herePolyline != null && !herePolyline.isEmpty()) {
            try {
                List<FlexiblePolyline.LatLng> coordinates = FlexiblePolyline.decode(herePolyline);
                appendHereTrackToGpx(gpx, coordinates, "Trasa HERE Maps");
                gpx.append("</gpx>");
                return gpx.toString();
            } catch (Exception e) {
                log.warn("⚠️ Błąd dekodowania HERE polyline: {}", e.getMessage());
            }
        }

        List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
        if (routes != null && !routes.isEmpty()) {
            Object polylineObj = routes.get(0).get("overview_polyline");
            String encodedPolyline = null;
            if (polylineObj instanceof String) {
                encodedPolyline = (String) polylineObj;
            } else if (polylineObj instanceof Map) {
                encodedPolyline = (String) ((Map<String, Object>) polylineObj).get("points");
            }
            if (encodedPolyline != null && !encodedPolyline.isEmpty()) {
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

        log.warn("⚠️ Brak polyline - używam prostej linii między punktami");
        gpx.append("  <trk>\n<name>Trasa podstawowa</name>\n<trkseg>\n");
        gpx.append("      <trkpt lat=\"").append(route.getStartLatitude()).append("\" lon=\"").append(route.getStartLongitude()).append("\"/>\n");
        gpx.append("      <trkpt lat=\"").append(route.getEndLatitude()).append("\" lon=\"").append(route.getEndLongitude()).append("\"/>\n");
        gpx.append("    </trkseg>\n</trk>\n</gpx>");
        return gpx.toString();
    }

    private void appendHereTrackToGpx(StringBuilder gpx, List<FlexiblePolyline.LatLng> coordinates, String trackName) {
        gpx.append("  <trk>\n<name>").append(escapeXml(trackName)).append("</name>\n<trkseg>\n");
        for (FlexiblePolyline.LatLng coord : coordinates) {
            gpx.append("      <trkpt lat=\"").append(coord.lat).append("\" lon=\"").append(coord.lng).append("\"/>\n");
        }
        gpx.append("    </trkseg>\n</trk>\n");
    }

    private void appendGoogleTrackToGpx(StringBuilder gpx, List<double[]> coordinates, String trackName) {
        gpx.append("  <trk>\n<name>").append(escapeXml(trackName)).append("</name>\n<trkseg>\n");
        for (double[] coord : coordinates) {
            gpx.append("      <trkpt lat=\"").append(coord[0]).append("\" lon=\"").append(coord[1]).append("\"/>\n");
        }
        gpx.append("    </trkseg>\n</trk>\n");
    }

    private String generateKml(Map<String, Object> routeData, Route route) {
        StringBuilder kml = new StringBuilder();
        kml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n  <Document>\n");
        kml.append("    <name>").append(escapeXml(route.getStartAddress())).append(" → ").append(escapeXml(route.getEndAddress())).append("</name>\n");
        kml.append("    <description>Trasa wojskowa - Transport ID: ").append(route.getTransportSet().getId()).append("</description>\n\n");
        kml.append("    <Style id=\"routeStyle\"><LineStyle><color>ff0000ff</color><width>4</width></LineStyle></Style>\n\n");
        kml.append("    <Placemark>\n<name>START</name>\n<description>").append(escapeXml(route.getStartAddress())).append("</description>\n");
        kml.append("      <Point>\n<coordinates>").append(route.getStartLongitude()).append(",").append(route.getStartLatitude()).append(",0</coordinates>\n</Point>\n</Placemark>\n\n");
        kml.append("    <Placemark>\n<name>KONIEC</name>\n<description>").append(escapeXml(route.getEndAddress())).append("</description>\n");
        kml.append("      <Point>\n<coordinates>").append(route.getEndLongitude()).append(",").append(route.getEndLatitude()).append(",0</coordinates>\n</Point>\n</Placemark>\n\n");

        String herePolyline = (String) routeData.get("herePolyline");
        if (herePolyline != null && !herePolyline.isEmpty()) {
            try {
                List<FlexiblePolyline.LatLng> coordinates = FlexiblePolyline.decode(herePolyline);
                appendHereLineStringToKml(kml, coordinates, "Trasa HERE Maps");
                kml.append("  </Document>\n</kml>");
                return kml.toString();
            } catch (Exception e) {
                log.warn("⚠️ Błąd dekodowania HERE polyline: {}", e.getMessage());
            }
        }

        List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
        if (routes != null && !routes.isEmpty()) {
            Object polylineObj = routes.get(0).get("overview_polyline");
            String encodedPolyline = null;
            if (polylineObj instanceof String) encodedPolyline = (String) polylineObj;
            else if (polylineObj instanceof Map) encodedPolyline = (String) ((Map<String, Object>) polylineObj).get("points");

            if (encodedPolyline != null && !encodedPolyline.isEmpty()) {
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

        log.warn("⚠️ Brak polyline - używam prostej linii między punktami");
        kml.append("    <Placemark>\n<name>Trasa podstawowa</name>\n<styleUrl>#routeStyle</styleUrl>\n<LineString>\n<coordinates>\n");
        kml.append("          ").append(route.getStartLongitude()).append(",").append(route.getStartLatitude()).append(",0\n");
        kml.append("          ").append(route.getEndLongitude()).append(",").append(route.getEndLatitude()).append(",0\n");
        kml.append("        </coordinates>\n</LineString>\n</Placemark>\n  </Document>\n</kml>");
        return kml.toString();
    }

    private void appendHereLineStringToKml(StringBuilder kml, List<FlexiblePolyline.LatLng> coordinates, String name) {
        kml.append("    <Placemark>\n<name>").append(escapeXml(name)).append("</name>\n<styleUrl>#routeStyle</styleUrl>\n<LineString>\n<tessellate>1</tessellate>\n<coordinates>\n");
        for (FlexiblePolyline.LatLng coord : coordinates) {
            kml.append("          ").append(coord.lng).append(",").append(coord.lat).append(",0\n");
        }
        kml.append("        </coordinates>\n</LineString>\n</Placemark>\n");
    }

    private void appendGoogleLineStringToKml(StringBuilder kml, List<double[]> coordinates, String name) {
        kml.append("    <Placemark>\n<name>").append(escapeXml(name)).append("</name>\n<styleUrl>#routeStyle</styleUrl>\n<LineString>\n<tessellate>1</tessellate>\n<coordinates>\n");
        for (double[] coord : coordinates) {
            kml.append("          ").append(coord[1]).append(",").append(coord[0]).append(",0\n");
        }
        kml.append("        </coordinates>\n</LineString>\n</Placemark>\n");
    }

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

    private String escapeXml(String text) {
        if (text == null) return "";
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&apos;");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // ZARZĄDZANIE TRASAMI - CRUD I OPERACJE
    // ═══════════════════════════════════════════════════════════════════════════

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

    public RouteResponse assignDriverToRoute(Long routeId, String driverUsername) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        if (route.getStatus() != Route.RouteStatus.CREATED) {
            throw new RuntimeException("Cannot assign driver - route is not in CREATED status");
        }
        if (route.getIsDraft()) {
            throw new RuntimeException("Cannot assign driver - route is a DRAFT and requires acceptance");
        }
        route.setAssignedDriverUsername(driverUsername);
        route.setStatus(Route.RouteStatus.ASSIGNED);
        Route savedRoute = routeRepository.save(route);
        log.info("✅ Assigned driver {} to route #{}", driverUsername, routeId);
        return convertToResponseFromEntity(savedRoute);
    }

    public RouteResponse changeTransportSet(Long routeId, Long newTransportSetId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        if (route.getStatus() == Route.RouteStatus.IN_PROGRESS || route.getStatus() == Route.RouteStatus.COMPLETED) {
            throw new RuntimeException("Cannot change transport set - route is already in progress or completed");
        }
        TransportSet newTransportSet = transportSetRepository.findById(newTransportSetId)
                .orElseThrow(() -> new RuntimeException("Transport set not found"));
        route.setTransportSet(newTransportSet);
        Route savedRoute = routeRepository.save(route);
        log.info("✅ Changed transport set for route #{} to #{}", routeId, newTransportSetId);
        return convertToResponseFromEntity(savedRoute);
    }

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
        return convertToResponseFromEntity(savedRoute);
    }

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
        return convertToResponseFromEntity(savedRoute);
    }

    public List<RouteResponse> getRoutesByDriver(String driverUsername) {
        List<Route> routes = routeRepository.findByAssignedDriverUsername(driverUsername);
        return routes.stream()
                .map(this::convertToResponseFromEntity)
                .collect(Collectors.toList());
    }

    public List<RouteResponse> getAllRoutes() {
        List<Route> routes = routeRepository.findAll();
        return routes.stream()
                .map(this::convertToResponseFromEntity)
                .collect(Collectors.toList());
    }

    public List<RouteResponse> getActiveRoutes() {
        List<Route> routes = routeRepository.findByStatusIn(
                Arrays.asList(Route.RouteStatus.ASSIGNED, Route.RouteStatus.IN_PROGRESS)
        );
        return routes.stream()
                .map(this::convertToResponseFromEntity)
                .collect(Collectors.toList());
    }

    public RouteResponse getRouteById(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        return convertToResponseFromEntity(route);
    }

    public void deleteRoute(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        if (route.getStatus() == Route.RouteStatus.IN_PROGRESS) {
            throw new RuntimeException("Cannot delete route in progress");
        }
        routeRepository.delete(route);
        log.info("✅ Route #{} deleted", routeId);
    }

    public RouteResponse revalidateRoute(Long routeId) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));
        CreateRouteRequest request = new CreateRouteRequest();
        request.setStartAddress(route.getStartAddress());
        request.setEndAddress(route.getEndAddress());
        request.setStartLatitude(route.getStartLatitude());
        request.setStartLongitude(route.getStartLongitude());
        request.setEndLatitude(route.getEndLatitude());
        request.setEndLongitude(route.getEndLongitude());
        request.setTransportSetId(route.getTransportSet().getId());
        log.info("♻️ Rewalidacja trasy #{}", routeId);
        routeRepository.delete(route);
        return createRoute(request, route.getCreatedByUsername());
    }

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
                    if (Boolean.TRUE.equals(routeData.get("hasRestrictions"))) routesWithRestrictions++;
                    if (Boolean.TRUE.equals(routeData.get("requiresPermit"))) routesWithPermits++;
                    if (Boolean.TRUE.equals(routeData.get("hasViolations"))) routesWithViolations++;
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

    // ═══════════════════════════════════════════════════════════════════════════
    // KLASA POMOCNICZA - RAPORT PRÓBY WALIDACJI
    // ═══════════════════════════════════════════════════════════════════════════

    public static class RouteAttemptReport {
        private int attemptNumber;
        private List<String> excludedBridges = new ArrayList<>();
        private boolean passable;
        private boolean requiresPermit;
        private List<String> violations = new ArrayList<>();
        private List<String> restrictions = new ArrayList<>();
        private List<String> permits = new ArrayList<>();
        private int blockedBridges;
        private List<String> criticalBridges = new ArrayList<>();
        private int totalInfrastructureChecked;
        private double successScore;
        private String error;
        private boolean preferredHighways; // ✅ NOWE POLE

        public boolean isFullyPassable() {
            return passable && violations.isEmpty();
        }


        // Gettery i Settery
        public int getAttemptNumber() { return attemptNumber; }
        public void setAttemptNumber(int attemptNumber) { this.attemptNumber = attemptNumber; }
        public List<String> getExcludedBridges() { return excludedBridges; }
        public void setExcludedBridges(List<String> excludedBridges) { this.excludedBridges = excludedBridges; }
        public boolean isPassable() { return passable; }
        public void setPassable(boolean passable) { this.passable = passable; }
        public boolean isRequiresPermit() { return requiresPermit; }
        public void setRequiresPermit(boolean requiresPermit) { this.requiresPermit = requiresPermit; }
        public List<String> getViolations() { return violations; }
        public void setViolations(List<String> violations) { this.violations = violations; }
        public List<String> getRestrictions() { return restrictions; }
        public void setRestrictions(List<String> restrictions) { this.restrictions = restrictions; }
        public List<String> getPermits() { return permits; }
        public void setPermits(List<String> permits) { this.permits = permits; }
        public int getBlockedBridges() { return blockedBridges; }
        public void setBlockedBridges(int blockedBridges) { this.blockedBridges = blockedBridges; }
        public List<String> getCriticalBridges() { return criticalBridges; }
        public void setCriticalBridges(List<String> criticalBridges) { this.criticalBridges = criticalBridges; }
        public int getTotalInfrastructureChecked() { return totalInfrastructureChecked; }
        public void setTotalInfrastructureChecked(int totalInfrastructureChecked) { this.totalInfrastructureChecked = totalInfrastructureChecked; }
        public double getSuccessScore() { return successScore; }
        public void setSuccessScore(double successScore) { this.successScore = successScore; }
        public String getError() { return error; }
        public void setError(String error) { this.error = error; }
        public boolean isPreferredHighways() { return preferredHighways; }
        public void setPreferredHighways(boolean preferredHighways) { this.preferredHighways = preferredHighways; }
    }
    // ═══════════════════════════════════════════════════════════════════════════
    // ✅ NOWE METODY - PUNKTOWA AKCEPTACJA I REWALIDACJA
    // ═══════════════════════════════════════════════════════════════════════════

    /**
     * ✅ NOWA METODA: Przegląd punktów problematycznych przez operatora
     *
     * FLOW:
     * 1. Operator przegląda każdy punkt osobno i decyduje: ACCEPT lub REJECT
     * 2. Jeśli wszystkie ACCEPT → zaakceptuj trasę
     * 3. Jeśli są REJECT → uruchom rewalidację z wykluczeniami
     */
    public RouteResponse reviewRejectedPointsByOperator(Long routeId,
                                                        List<PointDecisionDto> decisions,
                                                        String operatorUsername) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Nie znaleziono trasy"));

        if (!route.getIsDraft() || !route.getHasValidationProblems()) {
            throw new RuntimeException("Ta trasa nie wymaga przeglądu punktów");
        }

        log.info("╔════════════════════════════════════════════════════════════╗");
        log.info("║  Operator {} przegląda {} punktów dla trasy {}           ",
                operatorUsername, decisions.size(), routeId);
        log.info("╚════════════════════════════════════════════════════════════╝");

        // Pobierz rejected points z trasy
        List<Map<String, Object>> rejectedPoints = new ArrayList<>();
        try {
            if (route.getRejectedPointsJson() != null && !route.getRejectedPointsJson().isEmpty()) {
                rejectedPoints = objectMapper.readValue(
                        route.getRejectedPointsJson(),
                        objectMapper.getTypeFactory().constructCollectionType(List.class, Map.class)
                );
            }
        } catch (Exception e) {
            log.error("Błąd parsowania rejected points", e);
        }

        if (rejectedPoints.isEmpty()) {
            throw new RuntimeException("Brak punktów problematycznych do przeglądu");
        }

        // Utwórz mapę decyzji operatora
        Map<String, PointDecisionDto> decisionMap = new HashMap<>();
        for (PointDecisionDto decision : decisions) {
            decisionMap.put(decision.getPointName(), decision);
        }

        // Przetwórz każdy punkt
        List<String> acceptedPoints = new ArrayList<>();
        List<String> rejectedPointsList = new ArrayList<>();

        for (Map<String, Object> point : rejectedPoints) {
            String pointName = (String) point.get("name");
            PointDecisionDto decision = decisionMap.get(pointName);

            if (decision == null) {
                log.warn("⚠️  Brak decyzji dla punktu: {}", pointName);
                continue;
            }

            if ("ACCEPTED".equals(decision.getDecision())) {
                point.put("operatorDecision", "ACCEPTED");
                point.put("operatorDecisionBy", operatorUsername);
                point.put("operatorDecisionAt", LocalDateTime.now().toString());
                point.put("operatorComment", decision.getComment());
                acceptedPoints.add(pointName);
                log.info("✅ Punkt '{}' ZAAKCEPTOWANY przez {}", pointName, operatorUsername);

            } else if ("REJECTED".equals(decision.getDecision())) {
                point.put("operatorDecision", "REJECTED");
                point.put("operatorDecisionBy", operatorUsername);
                point.put("operatorDecisionAt", LocalDateTime.now().toString());
                point.put("operatorComment", decision.getComment());
                rejectedPointsList.add(pointName);
                log.info("❌ Punkt '{}' ODRZUCONY przez {}", pointName, operatorUsername);
            }
        }

        // Zapisz zaktualizowane punkty
        try {
            route.setRejectedPointsJson(objectMapper.writeValueAsString(rejectedPoints));
        } catch (Exception e) {
            log.error("Błąd zapisu rejected points", e);
        }

        log.info("📊 PODSUMOWANIE:");
        log.info("   ✅ Zaakceptowane: {}", acceptedPoints.size());
        log.info("   ❌ Odrzucone: {}", rejectedPointsList.size());

        // Decyzja o dalszych krokach
        if (rejectedPointsList.isEmpty()) {
            // SCENARIUSZ 1: Wszystkie punkty zaakceptowane
            log.info("🎉 WSZYSTKIE PUNKTY ZAAKCEPTOWANE - akceptuję trasę");
            return acceptRouteWithProblems(
                    routeId,
                    operatorUsername,
                    "Wszystkie punkty zaakceptowane przez operatora",
                    acceptedPoints
            );

        } else {
            // SCENARIUSZ 2: Są punkty odrzucone - uruchom rewalidację
            log.info("🔄 ROZPOCZYNAM REWALIDACJĘ - {} punktów odrzuconych", rejectedPointsList.size());
            return revalidateRouteWithExclusions(route, rejectedPointsList, acceptedPoints, operatorUsername);
        }
    }

    /**
     * ✅ NOWA METODA: Rewalidacja trasy z wykluczeniem odrzuconych punktów
     *
     * Szuka nowej trasy omijającej punkty odrzucone przez operatora
     */
    private RouteResponse revalidateRouteWithExclusions(Route route,
                                                        List<String> excludedPointNames,
                                                        List<String> acceptedPointNames,
                                                        String operatorUsername) {
        log.info("╔════════════════════════════════════════════════════════════╗");
        log.info("║  🔄 REWALIDACJA TRASY {} Z WYKLUCZENIAMI                  ", route.getId());
        log.info("║  🚫 Wykluczonych punktów: {}                              ", excludedPointNames.size());
        log.info("║  ✅ Zaakceptowanych punktów: {}                           ", acceptedPointNames.size());
        log.info("╚════════════════════════════════════════════════════════════╝");

        // Konwertuj nazwy punktów na set do wykluczenia
        Set<String> exclusions = new HashSet<>(excludedPointNames);

        try {
            // Pobierz nową trasę z Google Maps z wykluczeniami
            log.info("🔍 Szukam nowej trasy omijającej odrzucone punkty...");

            Map<String, Object> newRouteData = googleMapsService.getRoute(
                    route.getStartAddress(),
                    route.getEndAddress(),
                    route.getTransportSet(),
                    exclusions,  // Przekaż wykluczenia
                    false        // Nie preferuj autostrad - szukaj DOWOLNEJ trasy
            );

            // Waliduj nową trasę
            RouteAttemptReport validationReport = validateRouteBeforeSaving(
                    newRouteData,
                    100, // Numer próby (specjalny dla rewalidacji)
                    exclusions,
                    route.getTransportSet()
            );

            if (validationReport.isFullyPassable()) {
                // ✅ SUKCES: Nowa trasa jest bezpieczna
                log.info("╔════════════════════════════════════════════════════════════╗");
                log.info("║  ✅ SUKCES! Znaleziono bezpieczną trasę alternatywną      ║");
                log.info("╚════════════════════════════════════════════════════════════╝");

                newRouteData.put("revalidated", true);
                newRouteData.put("revalidatedBy", operatorUsername);
                newRouteData.put("excludedPoints", excludedPointNames);
                newRouteData.put("acceptedPoints", acceptedPointNames);
                newRouteData.put("revalidatedAt", LocalDateTime.now().toString());
                newRouteData.put("originalRouteId", route.getId());

                route.setRouteDataJson(objectMapper.writeValueAsString(newRouteData));
                route.setStatus(Route.RouteStatus.CREATED);
                route.setIsDraft(false);
                route.setHasValidationProblems(false);
                route.setOperatorAccepted(true);
                route.setOperatorAcceptedBy(operatorUsername);
                route.setOperatorAcceptedAt(LocalDateTime.now());
                route.setOperatorComment("Trasa zrewalidowana - odrzucone punkty ominięte");

                Route savedRoute = routeRepository.save(route);
                log.info("✅ Trasa #{} zaakceptowana po rewalidacji", route.getId());

                return convertToResponseFromEntity(savedRoute);

            } else {
                // ⚠️ PROBLEM: Nowa trasa też ma problemy
                log.warn("╔════════════════════════════════════════════════════════════╗");
                log.warn("║  ⚠️  UWAGA! Rewalidacja znalazła NOWE problemy            ║");
                log.warn("║  📋 Wymaga ponownego przeglądu przez operatora            ║");
                log.warn("╚════════════════════════════════════════════════════════════╝");

                List<Map<String, Object>> newRejectedPoints = new ArrayList<>();
                for (String criticalBridge : validationReport.getCriticalBridges()) {
                    Map<String, Object> point = new HashMap<>();
                    point.put("name", criticalBridge);
                    point.put("reason", "Znaleziony podczas rewalidacji");
                    point.put("foundDuringRevalidation", true);
                    point.put("requiresReview", true);
                    newRejectedPoints.add(point);
                    log.warn("   🚫 Nowy problem: {}", criticalBridge);
                }

                route.setRejectedPointsJson(objectMapper.writeValueAsString(newRejectedPoints));
                route.setStatus(Route.RouteStatus.VALIDATION_REQUIRED);
                route.setOperatorComment("Rewalidacja znalazła nowe problemy - wymaga ponownego przeglądu");

                Route savedRoute = routeRepository.save(route);
                return convertToResponseFromEntity(savedRoute);
            }

        } catch (Exception e) {
            log.error("❌ Błąd rewalidacji", e);
            throw new RuntimeException("Błąd rewalidacji: " + e.getMessage());
        }
    }

    /**
     * ✅ KLASA DTO - Decyzja operatora dla pojedynczego punktu
     */
    public static class PointDecisionDto {
        private String pointName;
        private String decision; // "ACCEPTED" lub "REJECTED"
        private String comment;

        // Konstruktory
        public PointDecisionDto() {}

        public PointDecisionDto(String pointName, String decision, String comment) {
            this.pointName = pointName;
            this.decision = decision;
            this.comment = comment;
        }

        // Gettery i Settery
        public String getPointName() { return pointName; }
        public void setPointName(String pointName) { this.pointName = pointName; }

        public String getDecision() { return decision; }
        public void setDecision(String decision) { this.decision = decision; }

        public String getComment() { return comment; }
        public void setComment(String comment) { this.comment = comment; }
    }
}
