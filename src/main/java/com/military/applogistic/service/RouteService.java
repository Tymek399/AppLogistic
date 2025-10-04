package com.military.applogistic.service;

import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.dto.request.CreateRouteRequest;
import com.military.applogistic.dto.response.RouteResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static org.springframework.data.jpa.domain.AbstractPersistable_.id;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RouteService {

    private final RouteRepository routeRepository;
    private final TransportSetRepository transportSetRepository;
    private final GoogleMapsService googleMapsService;
    private final HereMapsService hereMapsService;
    private final MilitaryLoadCalculator loadCalculator;
    private final ObjectMapper objectMapper = new ObjectMapper();

    private static final int MAX_ROUTE_ATTEMPTS = 10;
    private static final int LIGHT_VEHICLE_THRESHOLD_KG = 5000; // 5 ton

    public RouteResponse createRoute(CreateRouteRequest request, String createdByUsername) {
        log.info("========================================");
        log.info("ROZPOCZĘCIE PLANOWANIA TRASY");
        log.info("========================================");

        TransportSet transportSet = transportSetRepository.findById(request.getTransportSetId())
                .orElseThrow(() -> new RuntimeException("Nie znaleziono zestawu transportowego"));

        logTransportSetParameters(transportSet);

        // ✅ AUTO-AKCEPTACJA dla lekkich pojazdów (≤5t)
        if (transportSet.getTotalWeightKg() <= LIGHT_VEHICLE_THRESHOLD_KG) {
            log.info("🚗 LEKKI POJAZD (≤5t) - POMIJAM WALIDACJĘ MOSTÓW");
            return createLightVehicleRoute(request, transportSet, createdByUsername);
        }

        // STANDARDOWY PROCES dla ciężkich pojazdów
        return createHeavyVehicleRoute(request, transportSet, createdByUsername);
    }

    // ✅ NOWA METODA: Szybka trasa bez walidacji dla lekkich pojazdów
    private RouteResponse createLightVehicleRoute(CreateRouteRequest request,
                                                  TransportSet transportSet,
                                                  String createdByUsername) {
        try {
            // TYLKO JEDNA PROSTA TRASA - bez sprawdzania mostów
            Map<String, Object> routeData = googleMapsService.getRoute(
                    request.getStartAddress(),
                    request.getEndAddress(),
                    transportSet,
                    new HashSet<>() // Brak wykluczanych mostów
            );

            // Dodaj informację o pominięciu walidacji
            routeData.put("lightVehicle", true);
            routeData.put("validationSkipped", true);
            routeData.put("reason", "Pojazd ≤5t - walidacja mostów pominięta");
            routeData.put("searchAttempts", 1);
            routeData.put("successfulAttempt", 1);

            Route route = buildRouteEntity(request, transportSet, createdByUsername, routeData);
            Route savedRoute = routeRepository.save(route);

            log.info("✅ Trasa dla lekkiego pojazdu utworzona bez walidacji (ID: {})", savedRoute.getId());

            return convertToResponse(savedRoute, routeData);

        } catch (Exception e) {
            log.error("Błąd tworzenia trasy lekkiego pojazdu", e);
            throw new RuntimeException("Nie udało się utworzyć trasy: " + e.getMessage());
        }
    }

    // ISTNIEJĄCA METODA dla ciężkich pojazdów (bez zmian)
    private RouteResponse createHeavyVehicleRoute(CreateRouteRequest request,
                                                  TransportSet transportSet,
                                                  String createdByUsername) {
        List<RouteAttemptReport> allAttempts = new ArrayList<>();
        Set<String> excludedBridges = new HashSet<>();

        for (int attempt = 1; attempt <= MAX_ROUTE_ATTEMPTS; attempt++) {
            log.info("╔═══════════════════════════════════════════════╗");
            log.info("PRÓBA #{} - Szukanie trasy...", attempt);

            if (!excludedBridges.isEmpty()) {
                log.info("Wykluczam {} mostów", excludedBridges.size());
            }

            try {
                Map<String, Object> routeData = googleMapsService.getRoute(
                        request.getStartAddress(),
                        request.getEndAddress(),
                        transportSet,
                        excludedBridges
                );

                RouteAttemptReport attemptReport = analyzeRouteAttempt(
                        routeData, attempt, excludedBridges, transportSet
                );
                allAttempts.add(attemptReport);

                if (attemptReport.isFullyPassable()) {
                    log.info("SUKCES! Znaleziono przejezdną trasę w próbie #{}", attempt);
                    return buildSuccessResponse(request, transportSet, createdByUsername,
                            routeData, allAttempts, attempt);
                }

                List<String> criticalBridges = attemptReport.getCriticalBridges();
                if (criticalBridges.isEmpty()) {
                    log.warn("Brak możliwych tras dalej - wszystkie opcje wyczerpane");
                    break;
                }

                int bridgesToExclude = Math.min(3, criticalBridges.size());
                for (int i = 0; i < bridgesToExclude; i++) {
                    excludedBridges.add(criticalBridges.get(i));
                }

            } catch (Exception e) {
                log.error("Błąd w próbie #{}: {}", attempt, e.getMessage());
                RouteAttemptReport errorReport = new RouteAttemptReport();
                errorReport.setAttemptNumber(attempt);
                errorReport.setError(e.getMessage());
                allAttempts.add(errorReport);
            }
        }

        log.error("BRAK FIZYCZNIE MOŻLIWEJ TRASY po {} próbach", allAttempts.size());
        return buildFailureResponse(transportSet, allAttempts);
    }

    private RouteAttemptReport analyzeRouteAttempt(
            Map<String, Object> routeData,
            int attemptNumber,
            Set<String> excludedBridges,
            TransportSet transportSet) {

        RouteAttemptReport report = new RouteAttemptReport();
        report.setAttemptNumber(attemptNumber);
        report.setExcludedBridges(new ArrayList<>(excludedBridges));

        List<String> violations = (List<String>) routeData.getOrDefault("violations", new ArrayList<>());
        List<String> restrictions = (List<String>) routeData.getOrDefault("restrictions", new ArrayList<>());
        List<Map<String, Object>> infrastructure =
                (List<Map<String, Object>>) routeData.getOrDefault("infrastructureDetails", new ArrayList<>());

        report.setViolations(violations);
        report.setRestrictions(restrictions);
        report.setTotalInfrastructureChecked(infrastructure.size());

        long blockedBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .count();

        report.setBlockedBridges((int) blockedBridges);
        report.setPassable(violations.isEmpty() && blockedBridges == 0);

        List<String> criticalBridges = infrastructure.stream()
                .filter(i -> Boolean.FALSE.equals(i.get("canPass")))
                .map(i -> (String) i.get("name"))
                .limit(5)
                .collect(Collectors.toList());

        report.setCriticalBridges(criticalBridges);

        return report;
    }

    private RouteResponse buildSuccessResponse(
            CreateRouteRequest request,
            TransportSet transportSet,
            String createdByUsername,
            Map<String, Object> routeData,
            List<RouteAttemptReport> allAttempts,
            int successfulAttempt) {

        routeData.put("searchAttempts", allAttempts.size());
        routeData.put("successfulAttempt", successfulAttempt);
        routeData.put("attemptReports", allAttempts);

        Route route = buildRouteEntity(request, transportSet, createdByUsername, routeData);
        Route savedRoute = routeRepository.save(route);

        log.info("Trasa #{} utworzona pomyślnie po {} próbach",
                savedRoute.getId(), allAttempts.size());

        return convertToResponse(savedRoute, routeData);
    }

    private RouteResponse buildFailureResponse(
            TransportSet transportSet,
            List<RouteAttemptReport> allAttempts) {

        StringBuilder message = new StringBuilder();
        message.append("BRAK FIZYCZNIE MOŻLIWEJ TRASY\n\n");
        message.append("Sprawdzono ").append(allAttempts.size()).append(" alternatywnych tras\n");
        message.append("Waga zestawu: ").append(transportSet.getTotalWeightKg() / 1000.0).append(" ton\n");

        throw new RuntimeException(message.toString());
    }

    private void logTransportSetParameters(TransportSet ts) {
        log.info("╔════════════════════════════════════════════════╗");
        log.info("PARAMETRY ZESTAWU: {}", ts.getDescription());
        log.info("Waga: {} kg ({} ton)", ts.getTotalWeightKg(), ts.getTotalWeightKg() / 1000.0);
        log.info("Wysokość: {} cm", ts.getTotalHeightCm());
        log.info("╚════════════════════════════════════════════════╝");
    }

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
            route.setRouteDataJson("{}");
        }

        extractRouteMetrics(route, routeData);
        return route;
    }

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
            log.warn("Nie można wyciągnąć metryk: {}", e.getMessage());
        }
    }

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
            log.warn("Błąd konwersji: {}", e.getMessage());
        }

        return response;
    }

    // POZOSTAŁE METODY BEZ ZMIAN
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

        Map<String, Object> newValidation = hereMapsService.validateRouteRestrictions(
                route.getStartLatitude(), route.getStartLongitude(),
                route.getEndLatitude(), route.getEndLongitude(),
                route.getTransportSet()
        );

        try {
            Map<String, Object> routeData = route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}") ?
                    objectMapper.readValue(route.getRouteDataJson(), Map.class) : new HashMap<>();

            routeData.putAll(newValidation);
            routeData.put("last_validation", LocalDateTime.now().toString());

            route.setRouteDataJson(objectMapper.writeValueAsString(routeData));
            routeRepository.save(route);

            return convertToResponse(route, routeData);

        } catch (Exception e) {
            throw new RuntimeException("Failed to update route validation");
        }
    }
    // RouteService
    public Map<String, Object> getValidationDetails(Long routeId) {
        try {
            Route route = routeRepository.findById(routeId)
                    .orElseThrow(() -> new RuntimeException("Route not found"));

            if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
                Map<String, Object> noDataResponse = new HashMap<>();
                noDataResponse.put("routeId", routeId);
                noDataResponse.put("validationAvailable", false);
                noDataResponse.put("message", "Brak danych walidacji dla tej trasy");
                return noDataResponse;
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

    public byte[] generateNavigationFile(Long routeId, String format) {
        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
            throw new RuntimeException("Route has no navigation data");
        }

        try {
            Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);

            // Tu możesz rozwinąć logikę → np. konwersja do GPX/KML
            if ("gpx".equalsIgnoreCase(format)) {
                return generateGpx(routeData).getBytes(StandardCharsets.UTF_8);
            } else if ("kml".equalsIgnoreCase(format)) {
                return generateKml(routeData).getBytes(StandardCharsets.UTF_8);
            } else {
                throw new RuntimeException("Unsupported format: " + format);
            }

        } catch (Exception e) {
            throw new RuntimeException("Error generating navigation file: " + e.getMessage(), e);
        }
    }

    private String generateGpx(Map<String, Object> routeData) {
        return """
            <?xml version="1.0" encoding="UTF-8"?>
            <gpx version="1.1" creator="MilitaryApp">
              <trk>
                <name>Military Route</name>
                <trkseg>
                  <!-- TODO: tu pętlą wygeneruj <trkpt lat="..." lon="..."/> -->
                </trkseg>
              </trk>
            </gpx>
            """;
    }

    private String generateKml(Map<String, Object> routeData) {
        return """
            <?xml version="1.0" encoding="UTF-8"?>
            <kml xmlns="http://www.opengis.net/kml/2.2">
              <Document>
                <name>Military Route</name>
                <Placemark>
                  <LineString>
                    <coordinates>
                      <!-- TODO: tu pętlą wygeneruj współrzędne -->
                    </coordinates>
                  </LineString>
                </Placemark>
              </Document>
            </kml>
            """;
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

    public boolean hasRestrictiveViolations(Long routeId) {
        try {
            Route route = routeRepository.findById(routeId).orElse(null);
            if (route == null || route.getRouteDataJson() == null) {
                return false;
            }
            Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
            Boolean hasViolations = (Boolean) routeData.get("hasViolations");
            return Boolean.TRUE.equals(hasViolations);
        } catch (Exception e) {
            return false;
        }
    }

    public Map<String, Object> getValidationStatistics() {
        List<Route> allRoutes = routeRepository.findAll();
        long totalRoutes = allRoutes.size();
        long routesWithRestrictions = 0;
        long routesWithWarnings = 0;
        long routesWithViolations = 0;

        for (Route route : allRoutes) {
            try {
                if (route.getRouteDataJson() != null && !route.getRouteDataJson().equals("{}")) {
                    Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
                    if (Boolean.TRUE.equals(routeData.get("hasRestrictions"))) routesWithRestrictions++;
                    if (Boolean.TRUE.equals(routeData.get("hasWarnings"))) routesWithWarnings++;
                    if (Boolean.TRUE.equals(routeData.get("hasViolations"))) routesWithViolations++;
                }
            } catch (Exception e) {
                // ignore
            }
        }

        Map<String, Object> stats = new HashMap<>();
        stats.put("total_routes", totalRoutes);
        stats.put("routes_with_restrictions", routesWithRestrictions);
        stats.put("routes_with_warnings", routesWithWarnings);
        stats.put("routes_with_violations", routesWithViolations);
        return stats;
    }
}

@lombok.Data
class RouteAttemptReport {
    private int attemptNumber;
    private List<String> excludedBridges = new ArrayList<>();
    private int totalInfrastructureChecked;
    private int blockedBridges;
    private List<String> violations = new ArrayList<>();
    private List<String> restrictions = new ArrayList<>();
    private List<String> criticalBridges = new ArrayList<>();
    private boolean passable;
    private String error;

    public boolean isFullyPassable() {
        return passable && violations.isEmpty() && blockedBridges == 0;
    }
}