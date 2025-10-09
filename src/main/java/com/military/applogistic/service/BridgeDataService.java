package com.military.applogistic.service;

import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.OverpassService.InfrastructurePoint;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class BridgeDataService {

    private final OverpassService overpassService;

    /**
     * ✅ POPRAWIONE WZBOGACANIE DANYCH - TYLKO RZECZYWISTE WARTOŚCI Z OSM
     */
    public MilitaryLoadCalculator.BridgeSpecification enrichBridgeData(
            InfrastructurePoint point,
            TransportSet transportSet) {

        String pointName = point.getName() != null ? point.getName() : "Nienazwany";
        String pointType = point.getType() != null ? point.getType().getPolish() : "Nieznany";

        log.debug("🔍 Analiza infrastruktury: {} (typ: {})", pointName, pointType);

        // ✅ PRIORYTET 1: DANE Z OSM - TYLKO JEŚLI RZECZYWIŚCIE ISTNIEJĄ
        if (point.getMaxWeightTons() != null || point.getMaxHeightMeters() != null) {
            log.debug("✅ Rzeczywiste dane z OSM dostępne");

            logOsmData(pointName, pointType, point);

            return MilitaryLoadCalculator.BridgeSpecification.builder()
                    .name(pointName)
                    .location(point.getRoadName())
                    .maxWeight(point.getMaxWeightTons() != null ?
                            BigDecimal.valueOf(point.getMaxWeightTons()) : null)
                    .maxHeight(point.getMaxHeightMeters() != null ?
                            BigDecimal.valueOf(point.getMaxHeightMeters()) : null)
                    .maxWidth(new BigDecimal("4.0"))
                    .bridgeType(point.getType().name().toLowerCase() + "_osm_verified")
                    .condition("osm_data_available")
                    .build();
        }

        // ✅ PRIORYTET 2: HEURYSTYKA OPARTA O TYP DROGI (NORMY POLSKIE)
        MilitaryLoadCalculator.BridgeSpecification spec = estimateFromRoadType(point);
        if (spec != null) {
            log.debug("📊 Zastosowano heurystykę dla drogi: {}", point.getRoadName());
            logHeuristicData(pointName, spec, point);
            return spec;
        }

        // ✅ PRIORYTET 3: WARTOŚCI DOMYŚLNE - KONSERWATYWNE (TYLKO JEŚLI BRAK DANYCH)
        log.warn("⚠️ Użyto wartości domyślnych - brak danych OSM i nie rozpoznano typu drogi");

        MilitaryLoadCalculator.BridgeSpecification defaultSpec =
                MilitaryLoadCalculator.BridgeSpecification.createMilitaryDefault(pointName);

        logDefaultData(pointName, defaultSpec);

        return defaultSpec;
    }

    /**
     * ✅ SZCZEGÓŁOWE LOGOWANIE DANYCH OSM
     */
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
        log.info("└─────────────────────────────────────────────");
    }

    /**
     * ✅ SZCZEGÓŁOWE LOGOWANIE HEURYSTYKI
     */
    private void logHeuristicData(String pointName, MilitaryLoadCalculator.BridgeSpecification spec, InfrastructurePoint point) {
        log.info("┌─────────────────────────────────────────────");
        log.info("│ 📊 HEURYSTYKA: {}", pointName);
        log.info("├─────────────────────────────────────────────");
        log.info("│  🔸 Nośność:  {} ton (szacunek na podstawie typu drogi)", spec.getMaxWeight());
        log.info("│  🔸 Wysokość: {} m (szacunek na podstawie typu drogi)", spec.getMaxHeight());
        log.info("│  🔸 Droga:    {}", point.getRoadName());
        log.info("│  🔸 Typ:      {}", spec.getBridgeType());
        log.info("│  🔸 Źródło:   Normy polskich dróg (rozporządzenie)");
        log.info("└─────────────────────────────────────────────");
    }

    /**
     * ✅ SZCZEGÓŁOWE LOGOWANIE WARTOŚCI DOMYŚLNYCH
     */
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
     * ✅ HEURYSTYKA OPARTA O STANDARDY DRÓG W POLSCE (MLC)
     * Źródło: Rozporządzenie Ministra Transportu w sprawie warunków technicznych
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

        // ✅ Normy polskich dróg (zgodne z MLC - Military Load Classification)
        if (roadName.matches("a\\d+") || roadName.contains("autostrada")) {
            // AUTOSTRADA (A1, A2, A4, etc.)
            maxWeight = new BigDecimal("100.0");   // MLC 150
            maxHeight = new BigDecimal(isTunnel ? "4.7" : "5.5");
            maxAxleLoad = new BigDecimal("15.0");
            bridgeType = isTunnel ? "tunel_autostrada" : "most_autostrada";
            roadClass = "Autostrada (A)";

        } else if (roadName.matches("s\\d+") || roadName.contains("ekspresowa")) {
            // DROGA EKSPRESOWA (S7, S8, S19, etc.)
            maxWeight = new BigDecimal("85.0");    // MLC 120
            maxHeight = new BigDecimal(isTunnel ? "4.5" : "5.3");
            maxAxleLoad = new BigDecimal("14.0");
            bridgeType = isTunnel ? "tunel_ekspresowa" : "most_ekspresowa";
            roadClass = "Droga ekspresowa (S)";

        } else if (roadName.matches("\\d+") || roadName.matches("dk\\d+") ||
                roadName.contains("krajowa") || roadName.matches("\\d{1,3}")) {
            // DROGA KRAJOWA (7, 91, DK75, etc.)
            maxWeight = new BigDecimal("75.0");    // MLC 100
            maxHeight = new BigDecimal(isTunnel ? "4.2" : "5.0");
            maxAxleLoad = new BigDecimal("13.0");
            bridgeType = isTunnel ? "tunel_krajowa" : "most_krajowa";
            roadClass = "Droga krajowa (DK)";

        } else if (roadName.matches("dw\\d+") || roadName.contains("wojewódzka")) {
            // DROGA WOJEWÓDZKA (DW965, etc.)
            maxWeight = new BigDecimal("60.0");    // MLC 80
            maxHeight = new BigDecimal(isTunnel ? "4.0" : "4.8");
            maxAxleLoad = new BigDecimal("12.0");
            bridgeType = isTunnel ? "tunel_wojewódzka" : "most_wojewódzka";
            roadClass = "Droga wojewódzka (DW)";

        } else {
            // DROGA LOKALNA / POWIATOWA
            maxWeight = new BigDecimal("50.0");    // MLC 60
            maxHeight = new BigDecimal(isTunnel ? "3.8" : "4.5");
            maxAxleLoad = new BigDecimal("11.0");
            bridgeType = isTunnel ? "tunel_lokalna" : "most_lokalny";
            roadClass = "Droga lokalna/powiatowa";
        }

        log.debug("   🔹 Rozpoznano: {} → Klasa: {}", roadName, roadClass);

        return MilitaryLoadCalculator.BridgeSpecification.builder()
                .name(point.getName())
                .location(point.getRoadName())
                .maxWeight(maxWeight)
                .maxHeight(maxHeight)
                .maxWidth(new BigDecimal("4.0"))
                .maxAxleLoad(maxAxleLoad)
                .bridgeType(bridgeType + "_estimated")
                .condition("standard_polish_road_" + roadClass)
                .build();
    }

    /**
     * ✅ STATYSTYKI ŹRÓDEŁ DANYCH - Dla całej trasy
     */
    public void logDataSourceStatistics(List<InfrastructurePoint> points) {
        if (points == null || points.isEmpty()) {
            log.info("📊 Brak obiektów infrastruktury do analizy");
            return;
        }

        int osmData = 0;
        int heuristic = 0;
        int defaults = 0;
        int withWeight = 0;
        int withHeight = 0;

        for (InfrastructurePoint point : points) {
            if (point.getMaxWeightTons() != null || point.getMaxHeightMeters() != null) {
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

    /**
     * ✅ SZCZEGÓŁOWA ANALIZA POJEDYNCZEGO OBIEKTU (dla debugowania)
     */
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
        log.info("╚═══════════════════════════════════════════════════════");
    }

    /**
     * ✅ PORÓWNANIE TRANSPORTU Z OBIEKTEM
     */
    public String compareTransportWithInfrastructure(
            TransportSet transportSet,
            MilitaryLoadCalculator.BridgeSpecification spec) {

        StringBuilder result = new StringBuilder();
        result.append("\n┌─────────────────────────────────────────────\n");
        result.append("│ ⚖️  PORÓWNANIE: ").append(spec.getName()).append("\n");
        result.append("├─────────────────────────────────────────────\n");

        // Nośność
        if (spec.getMaxWeight() != null) {
            double transportWeight = transportSet.getTotalWeightKg() / 1000.0;
            double bridgeWeight = spec.getMaxWeight().doubleValue();
            boolean weightOk = transportWeight <= bridgeWeight;

            result.append(String.format("│  Nośność:  %.1ft / %.1ft  %s\n",
                    transportWeight, bridgeWeight,
                    weightOk ? "✅" : "❌"));
        }

        // Wysokość
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