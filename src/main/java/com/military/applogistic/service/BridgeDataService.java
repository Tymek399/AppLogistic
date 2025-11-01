package com.military.applogistic.service;

import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.OverpassService.InfrastructurePoint;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;

/**
 * ✅ POPRAWIONA KLASA - PRIORYTET HEURYSTYKI DLA AUTOSTRAD I DRÓG EKSPRESOWYCH
 *
 * KRYTYCZNA POPRAWKA:
 * - Dla autostrad i dróg ekspresowych ZAWSZE używamy heurystyki (gwarantowany limit 120T/100T)
 * - Dane z OSM używane tylko dla dróg niższej kategorii
 * - Eliminuje problem: "autostrada bez limitu masy"
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class BridgeDataService {

    private final OverpassService overpassService;

    /**
     * ✅ POPRAWIONA METODA - Priorytet heurystyki dla autostrad/ekspresówek
     */
    public MilitaryLoadCalculator.BridgeSpecification enrichBridgeData(
            InfrastructurePoint point,
            TransportSet transportSet) {

        String pointName = point.getName() != null ? point.getName() : "Nienazwany";
        String pointType = point.getType() != null ? point.getType().getPolish() : "Nieznany";

        log.debug("🔍 Analiza infrastruktury: {} (typ: {})", pointName, pointType);

        // ✅ NOWY PRIORYTET 1: Sprawdź czy to autostrada/droga ekspresowa
        // Dla tych dróg ZAWSZE używamy heurystyki (gwarantowane limity wojskowe)
        if (isMotorwayOrExpressway(point.getRoadName())) {
            log.debug("🛣️ Wykryto autostradę/drogę ekspresową - użycie heurystyki (gwarantowany limit)");
            MilitaryLoadCalculator.BridgeSpecification spec = estimateFromRoadType(point);
            if (spec != null) {
                logHeuristicData(pointName, spec, point);
                return spec;
            }
        }

        // ✅ PRIORYTET 2: DANE Z OSM (tylko dla dróg niższej kategorii)
        if (point.getMaxWeightTons() != null || point.getMaxHeightMeters() != null) {
            log.debug("✅ Rzeczywiste dane z OSM dostępne (droga niższej kategorii)");
            logOsmData(pointName, pointType, point);

            return MilitaryLoadCalculator.BridgeSpecification.builder()
                    .name(pointName)
                    .location(point.getRoadName())
                    .city(extractCityFromTags(point))
                    .maxWeight(point.getMaxWeightTons() != null ?
                            BigDecimal.valueOf(point.getMaxWeightTons()) : null)
                    .maxHeight(point.getMaxHeightMeters() != null ?
                            BigDecimal.valueOf(point.getMaxHeightMeters()) : null)
                    .maxWidth(new BigDecimal("4.0"))
                    .bridgeType(point.getType().name().toLowerCase() + "_osm_verified")
                    .condition("osm_data_available")
                    .build();
        }

        // ✅ PRIORYTET 3: HEURYSTYKA (dla pozostałych dróg)
        MilitaryLoadCalculator.BridgeSpecification spec = estimateFromRoadType(point);
        if (spec != null) {
            log.debug("📊 Zastosowano heurystykę dla drogi: {}", point.getRoadName());
            logHeuristicData(pointName, spec, point);
            return spec;
        }

        // ✅ PRIORYTET 4: WARTOŚCI DOMYŚLNE
        log.warn("⚠️ Użyto wartości domyślnych - brak danych OSM i nie rozpoznano typu drogi");
        MilitaryLoadCalculator.BridgeSpecification defaultSpec =
                MilitaryLoadCalculator.BridgeSpecification.createMilitaryDefault(pointName);
        logDefaultData(pointName, defaultSpec);

        return defaultSpec;
    }

    /**
     * ✅ NOWA METODA - Sprawdza czy droga to autostrada lub droga ekspresowa
     */
    private boolean isMotorwayOrExpressway(String roadName) {
        if (roadName == null) return false;

        String road = roadName.toLowerCase().trim();

        // Autostrady: A1, A2, A4, "autostrada"
        if (road.matches("a\\d+") || road.contains("autostrada")) {
            log.debug("   ✅ Wykryto AUTOSTRADĘ: {}", roadName);
            return true;
        }
        if (road.matches("a\\d+") || road.contains("Autostrada")) {
            log.debug("   ✅ Wykryto AUTOSTRADĘ: {}", roadName);
            return true;
        }

        // Drogi ekspresowe: S7, S8, S19, "ekspresowa"
        if (road.matches("s\\d+") || road.contains("ekspresowa")) {
            log.debug("   ✅ Wykryto DROGĘ EKSPRESOWĄ: {}", roadName);
            return true;
        }

        return false;
    }

    private String extractCityFromTags(InfrastructurePoint point) {
        if (point.getTags() == null) return null;

        String city = point.getTags().get("addr:city");
        if (city != null && !city.isEmpty()) return city;

        city = point.getTags().get("is_in:city");
        if (city != null && !city.isEmpty()) return city;

        city = point.getTags().get("addr:suburb");
        if (city != null && !city.isEmpty()) return city;

        String name = point.getName();
        if (name != null && name.contains(",")) {
            String[] parts = name.split(",");
            if (parts.length > 1) {
                return parts[1].trim();
            }
        }

        return null;
    }

    private void logOsmData(String pointName, String pointType, InfrastructurePoint point) {
        log.info("┌─────────────────────────────────────────────");
        log.info("│ 📊 DANE OSM: {}", pointName);
        log.info("├─────────────────────────────────────────────");

        if (point.getMaxWeightTons() != null) {
            log.info("│  🔸 Nośność:  {} ton (RZECZYWISTE DANE OSM)", point.getMaxWeightTons());
        } else {
            log.info("│  🔸 Nośność:  BRAK DANYCH W OSM");
        }

        if (point.getMaxHeightMeters() != null) {
            log.info("│  🔸 Wysokość: {} m (RZECZYWISTE DANE OSM)", point.getMaxHeightMeters());
        } else {
            log.info("│  🔸 Wysokość: BRAK DANYCH W OSM");
        }

        log.info("│  🔸 Droga:    {}", point.getRoadName() != null ? point.getRoadName() : "Nieznana");
        log.info("│  🔸 Typ:      {}", pointType);
        log.info("│  🔸 Źródło:   OpenStreetMap (zweryfikowane)");

        String city = extractCityFromTags(point);
        if (city != null) {
            log.info("│  🔸 Miasto:   {}", city);
        }

        log.info("└─────────────────────────────────────────────");
    }

    private void logHeuristicData(String pointName, MilitaryLoadCalculator.BridgeSpecification spec, InfrastructurePoint point) {
        log.info("┌─────────────────────────────────────────────");
        log.info("│ 📊 HEURYSTYKA: {}", pointName);
        log.info("├─────────────────────────────────────────────");
        log.info("│  🔸 Nośność:  {} ton (GWARANTOWANY limit wojskowy)", spec.getMaxWeight());
        log.info("│  🔸 Wysokość: {} m (szacunek na podstawie typu drogi)", spec.getMaxHeight());
        log.info("│  🔸 Droga:    {}", point.getRoadName());
        log.info("│  🔸 Typ:      {}", spec.getBridgeType());
        log.info("│  🔸 Źródło:   Normy polskich dróg + limity wojskowe");
        log.info("└─────────────────────────────────────────────");
    }

    private void logDefaultData(String pointName, MilitaryLoadCalculator.BridgeSpecification spec) {
        log.warn("┌─────────────────────────────────────────────");
        log.warn("│ ⚠️  WARTOŚCI DOMYŚLNE: {}", pointName);
        log.warn("├─────────────────────────────────────────────");
        log.warn("│  🔸 Nośność:  {} ton (domyślna konserwatywna)", spec.getMaxWeight());
        log.warn("│  🔸 Wysokość: {} m (domyślna konserwatywna)", spec.getMaxHeight());
        log.warn("│  🔸 Uwaga:    Brak danych OSM");
        log.warn("│  🔸 Uwaga:    Nie rozpoznano typu drogi");
        log.warn("│  🔸 Źródło:   Wartości konserwatywne (militarne)");
        log.warn("└─────────────────────────────────────────────");
    }

    /**
     * ✅ HEURYSTYKA - ZWIĘKSZONE LIMITY WOJSKOWE
     */
    private MilitaryLoadCalculator.BridgeSpecification estimateFromRoadType(
            InfrastructurePoint point) {

        String roadName = point.getRoadName();
        if (roadName == null) {
            return null;
        }

        roadName = roadName.toLowerCase().trim();

        BigDecimal maxWeight;
        BigDecimal maxHeight;
        BigDecimal maxAxleLoad;
        String bridgeType;
        String roadClass;

        boolean isTunnel = point.getType() == OverpassService.InfrastructureType.TUNNEL;

        // ✅ AUTOSTRADY (A) - LIMIT 120T
        if (roadName.matches("a\\d+") || roadName.contains("autostrada")) {
            maxWeight = new BigDecimal("120.0");   // ✅ ZWIĘKSZONO z 100T do 120T
            maxHeight = new BigDecimal(isTunnel ? "4.7" : "5.5");
            maxAxleLoad = new BigDecimal("16.0");  // ✅ Zwiększono z 15T do 16T
            bridgeType = isTunnel ? "tunel_autostrada" : "most_autostrada";
            roadClass = "Autostrada (A) - wzmocniona wojskowo 120T";

            log.debug("   🛣️ AUTOSTRADA - Gwarantowany limit wojskowy: 120T");

            // ✅ DROGI EKSPRESOWE (S) - ZRÓŻNICOWANE LIMITY
        } else if (roadName.matches("s\\d+") || roadName.contains("ekspresowa")) {

            // Główne drogi ekspresowe (S1-S19) - 100T
            if (roadName.matches("s[1-9]\\b") || roadName.matches("s1[0-9]\\b")) {
                maxWeight = new BigDecimal("100.0");   // ✅ ZWIĘKSZONO z 85T do 100T
                maxHeight = new BigDecimal(isTunnel ? "4.6" : "5.4");
                maxAxleLoad = new BigDecimal("15.0");
                roadClass = "Droga ekspresowa (S) - główna 100T";
                log.debug("   🛣️ DROGA EKSPRESOWA GŁÓWNA ({}) - Gwarantowany limit: 100T", roadName);

                // Pozostałe drogi ekspresowe (S20+) - 95T
            } else if (roadName.matches("s[2-9]\\d")) {
                maxWeight = new BigDecimal("95.0");    // ✅ ZWIĘKSZONO z 85T do 95T
                maxHeight = new BigDecimal(isTunnel ? "4.5" : "5.3");
                maxAxleLoad = new BigDecimal("14.5");
                roadClass = "Droga ekspresowa (S) - standard 95T";
                log.debug("   🛣️ DROGA EKSPRESOWA STANDARD ({}) - Gwarantowany limit: 95T", roadName);

            } else {
                // Domyślnie dla nierozpoznanych S
                maxWeight = new BigDecimal("90.0");
                maxHeight = new BigDecimal(isTunnel ? "4.5" : "5.3");
                maxAxleLoad = new BigDecimal("14.0");
                roadClass = "Droga ekspresowa (S) - podstawowa 90T";
                log.debug("   🛣️ DROGA EKSPRESOWA ({}) - Limit domyślny: 90T", roadName);
            }

            bridgeType = isTunnel ? "tunel_ekspresowa" : "most_ekspresowa";

            // DROGI KRAJOWE (DK) - bez zmian
        } else if (roadName.matches("\\d+") || roadName.matches("dk\\d+") ||
                roadName.contains("krajowa") || roadName.matches("\\d{1,3}")) {
            maxWeight = new BigDecimal("75.0");
            maxHeight = new BigDecimal(isTunnel ? "4.2" : "5.0");
            maxAxleLoad = new BigDecimal("13.0");
            bridgeType = isTunnel ? "tunel_krajowa" : "most_krajowa";
            roadClass = "Droga krajowa (DK) 75T";

            // DROGI WOJEWÓDZKIE (DW) - bez zmian
        } else if (roadName.matches("dw\\d+") || roadName.contains("wojewódzka")) {
            maxWeight = new BigDecimal("60.0");
            maxHeight = new BigDecimal(isTunnel ? "4.0" : "4.8");
            maxAxleLoad = new BigDecimal("12.0");
            bridgeType = isTunnel ? "tunel_wojewódzka" : "most_wojewódzka";
            roadClass = "Droga wojewódzka (DW) 60T";

            // DROGI LOKALNE - bez zmian
        } else {
            maxWeight = new BigDecimal("50.0");
            maxHeight = new BigDecimal(isTunnel ? "3.8" : "4.5");
            maxAxleLoad = new BigDecimal("11.0");
            bridgeType = isTunnel ? "tunel_lokalna" : "most_lokalny";
            roadClass = "Droga lokalna/powiatowa 50T";
        }

        log.debug("   🔹 Rozpoznano: {} → Klasa: {}", roadName, roadClass);

        return MilitaryLoadCalculator.BridgeSpecification.builder()
                .name(point.getName())
                .location(point.getRoadName())
                .city(extractCityFromTags(point))
                .maxWeight(maxWeight)
                .maxHeight(maxHeight)
                .maxWidth(new BigDecimal("4.0"))
                .maxAxleLoad(maxAxleLoad)
                .bridgeType(bridgeType + "_estimated")
                .condition("military_standard_" + roadClass)
                .build();
    }

    public void logDataSourceStatistics(List<InfrastructurePoint> points) {
        if (points == null || points.isEmpty()) {
            log.info("📊 Brak obiektów infrastruktury do analizy");
            return;
        }

        int osmData = 0;
        int heuristic = 0;
        int motorwayHeuristic = 0;  // ✅ NOWE - licznik dla autostrad/ekspresówek
        int defaults = 0;
        int withWeight = 0;
        int withHeight = 0;

        for (InfrastructurePoint point : points) {
            // Sprawdź czy to autostrada/ekspresówka (priorytet heurystyki)
            if (isMotorwayOrExpressway(point.getRoadName())) {
                motorwayHeuristic++;
            } else if (point.getMaxWeightTons() != null || point.getMaxHeightMeters() != null) {
                osmData++;
                if (point.getMaxWeightTons() != null) withWeight++;
                if (point.getMaxHeightMeters() != null) withHeight++;
            } else if (point.getRoadName() != null &&
                    estimateFromRoadType(point) != null) {
                heuristic++;
            } else {
                defaults++;
            }
        }

        int total = points.size();

        log.info("╔═══════════════════════════════════════════════════════");
        log.info("║ 📊 STATYSTYKI ŹRÓDEŁ DANYCH INFRASTRUKTURY");
        log.info("╠═══════════════════════════════════════════════════════");
        log.info("║  📌 Całkowita liczba obiektów: {}", total);
        log.info("║");
        log.info("║  🛣️  Autostrady/Ekspresówki: {} ({} %)",
                motorwayHeuristic, total > 0 ? (motorwayHeuristic * 100 / total) : 0);
        log.info("║     └─ Gwarantowane limity wojskowe (120T/100T/95T)");
        log.info("║");
        log.info("║  ✅ Dane z OSM:          {} ({} %)",
                osmData, total > 0 ? (osmData * 100 / total) : 0);
        log.info("║     ├─ Z nośnością:     {}", withWeight);
        log.info("║     └─ Z wysokością:    {}", withHeight);
        log.info("║");
        log.info("║  📊 Heurystyka:          {} ({} %)",
                heuristic, total > 0 ? (heuristic * 100 / total) : 0);
        log.info("║     └─ Na podstawie typu drogi");
        log.info("║");
        log.info("║  ⚠️  Wartości domyślne:  {} ({} %)",
                defaults, total > 0 ? (defaults * 100 / total) : 0);
        log.info("║     └─ Brak danych do analizy");
        log.info("╚═══════════════════════════════════════════════════════");
    }

    public void debugInfrastructurePoint(InfrastructurePoint point) {
        log.info("╔═══════════════════════════════════════════════════════");
        log.info("║ 🔍 DEBUG OBIEKTU INFRASTRUKTURY");
        log.info("╠═══════════════════════════════════════════════════════");
        log.info("║  Nazwa:           {}", point.getName());
        log.info("║  Typ:             {}", point.getType().getPolish());
        log.info("║  Droga:           {}", point.getRoadName());
        log.info("║  Nośność (OSM):   {} ton",
                point.getMaxWeightTons() != null ? point.getMaxWeightTons() : "BRAK");
        log.info("║  Wysokość (OSM):  {} m",
                point.getMaxHeightMeters() != null ? point.getMaxHeightMeters() : "BRAK");

        String city = extractCityFromTags(point);
        if (city != null) {
            log.info("║  Miasto:          {}", city);
        }

        // ✅ NOWE - informacja czy to autostrada/ekspresówka
        if (isMotorwayOrExpressway(point.getRoadName())) {
            log.info("║  🛣️  AUTOSTRADA/EKSPRESÓWKA - użycie heurystyki (gwarantowany limit)");
        }

        log.info("╚═══════════════════════════════════════════════════════");
    }

    public String compareTransportWithInfrastructure(
            TransportSet transportSet,
            MilitaryLoadCalculator.BridgeSpecification spec) {

        StringBuilder result = new StringBuilder();
        result.append("\n┌─────────────────────────────────────────────\n");
        result.append("│ ⚖️  PORÓWNANIE: ").append(spec.getName()).append("\n");
        result.append("├─────────────────────────────────────────────\n");

        if (spec.getMaxWeight() != null) {
            double transportWeight = transportSet.getTotalWeightKg() / 1000.0;
            double bridgeWeight = spec.getMaxWeight().doubleValue();
            boolean weightOk = transportWeight <= bridgeWeight;

            result.append(String.format("│  Nośność:  %.1ft / %.1ft  %s\n",
                    transportWeight, bridgeWeight,
                    weightOk ? "✅" : "❌"));
        }

        if (spec.getMaxHeight() != null) {
            double transportHeight = transportSet.getTotalHeightCm() / 100.0;
            double bridgeHeight = spec.getMaxHeight().doubleValue();
            boolean heightOk = transportHeight <= bridgeHeight;

            result.append(String.format("│  Wysokość: %.2fm / %.2fm  %s\n",
                    transportHeight, bridgeHeight,
                    heightOk ? "✅" : "❌"));
        }

        result.append("└─────────────────────────────────────────────");

        return result.toString();
    }
}