package com.military.applogistic.service;

import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.OverpassService.InfrastructurePoint;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class BridgeDataService {

    private final OverpassService overpassService;
    private final HereMapsService hereMapsService;
    private final TomTomService tomTomService;

    public MilitaryLoadCalculator.BridgeSpecification enrichBridgeData(
            InfrastructurePoint point,
            TransportSet transportSet) {

        log.info("🔍 Analiza infrastruktury: {} (typ: {})",
                point.getName(), point.getType().getPolish());

        // 1. Sprawdź Overpass (OSM)
        MilitaryLoadCalculator.BridgeSpecification spec = fetchFromOverpass(point);
        if (spec != null) {
            log.info("✅ Dane pobrane z Overpass (OSM)");
            return spec;
        }

        // 2. ✅ OPTYMALIZACJA - nie pytaj HERE Maps dla KAŻDEGO mostu osobno
        // Zamiast tego użyj heurystyki
        spec = estimateFromRoadTypeForMilitary(point);
        if (spec != null) {
            log.info("📊 Zastosowano heurystykę MLC dla typu drogi");
            return spec;
        }

        // 3. Domyślne wartości
        log.warn("⚠️ Brak danych – użyto konserwatywnych wartości domyślnych");
        return MilitaryLoadCalculator.BridgeSpecification.createMilitaryDefault(point.getName());
    }

    private MilitaryLoadCalculator.BridgeSpecification fetchFromOverpass(InfrastructurePoint point) {
        try {
            if (point.getMaxWeightTons() != null || point.getMaxHeightMeters() != null) {
                return MilitaryLoadCalculator.BridgeSpecification.builder()
                        .name(point.getName())
                        .location(point.getRoadName())
                        .maxWeight(point.getMaxWeightTons() != null ?
                                BigDecimal.valueOf(point.getMaxWeightTons()) : null)
                        .maxHeight(point.getMaxHeightMeters() != null ?
                                BigDecimal.valueOf(point.getMaxHeightMeters()) : null)
                        .maxWidth(new BigDecimal("4.0"))
                        .bridgeType(point.getType().name().toLowerCase())
                        .condition("osm_data")
                        .build();
            }
        } catch (Exception e) {
            log.debug("Błąd pobierania z Overpass: {}", e.getMessage());
        }
        return null;
    }

    private MilitaryLoadCalculator.BridgeSpecification estimateFromRoadTypeForMilitary(
            InfrastructurePoint point) {

        String roadName = point.getRoadName();
        if (roadName == null) {
            return null;
        }

        roadName = roadName.toLowerCase();

        BigDecimal maxWeight;
        BigDecimal maxHeight;
        BigDecimal maxAxleLoad;
        String bridgeType;

        // ✅ ROZRÓŻNIENIE: MOST vs TUNEL
        boolean isTunnel = point.getType() == OverpassService.InfrastructureType.TUNNEL;

        if (roadName.matches("a\\d+") || roadName.contains("autostrada")) {
            maxWeight = new BigDecimal("100.0");
            maxHeight = new BigDecimal(isTunnel ? "4.7" : "5.5"); // Tunele niższe!
            maxAxleLoad = new BigDecimal("15.0");
            bridgeType = isTunnel ? "tunel_autostrada_mlc150" : "autostrada_mlc150";
        } else if (roadName.matches("s\\d+") || roadName.contains("ekspresowa")) {
            maxWeight = new BigDecimal("80.0");
            maxHeight = new BigDecimal(isTunnel ? "4.5" : "5.3");
            maxAxleLoad = new BigDecimal("14.0");
            bridgeType = isTunnel ? "tunel_ekspresowa_mlc120" : "ekspresowa_mlc120";
        } else if (roadName.matches("dk\\d+") || roadName.contains("krajowa")) {
            maxWeight = new BigDecimal("70.0");
            maxHeight = new BigDecimal(isTunnel ? "4.2" : "5.0");
            maxAxleLoad = new BigDecimal("13.0");
            bridgeType = isTunnel ? "tunel_krajowa_mlc100" : "droga_krajowa_mlc100";
        } else if (roadName.matches("dw\\d+") || roadName.contains("wojewódzka")) {
            maxWeight = new BigDecimal("60.0");
            maxHeight = new BigDecimal(isTunnel ? "4.0" : "4.8");
            maxAxleLoad = new BigDecimal("12.0");
            bridgeType = isTunnel ? "tunel_wojewódzka_mlc80" : "droga_wojewódzka_mlc80";
        } else {
            maxWeight = new BigDecimal("50.0");
            maxHeight = new BigDecimal(isTunnel ? "3.8" : "4.5");
            maxAxleLoad = new BigDecimal("11.0");
            bridgeType = isTunnel ? "tunel_lokalna_mlc60" : "droga_lokalna_mlc60";
        }

        return MilitaryLoadCalculator.BridgeSpecification.builder()
                .name(point.getName())
                .location(point.getRoadName())
                .maxWeight(maxWeight)
                .maxHeight(maxHeight)
                .maxWidth(new BigDecimal("4.0"))
                .maxAxleLoad(maxAxleLoad)
                .bridgeType(bridgeType + "_military_estimated")
                .condition("assumed_military_standard")
                .build();
    }
}