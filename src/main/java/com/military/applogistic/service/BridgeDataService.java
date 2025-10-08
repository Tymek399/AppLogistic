package com.military.applogistic.service;

import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.api.OverpassService;
import com.military.applogistic.service.api.OverpassService.InfrastructurePoint;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Slf4j
@Component
@RequiredArgsConstructor
public class BridgeDataService {

    private final OverpassService overpassService;

    /**
     * Wzbogacanie danych mostu/tunelu - TYLKO HEURYSTYKA (bez API)
     */
    public MilitaryLoadCalculator.BridgeSpecification enrichBridgeData(
            InfrastructurePoint point,
            TransportSet transportSet) {

        log.debug("Analiza infrastruktury: {} (typ: {})",
                point.getName(), point.getType().getPolish());

        // 1. Sprawdź Overpass (OSM) - jeśli ma dane
        if (point.getMaxWeightTons() != null || point.getMaxHeightMeters() != null) {
            log.debug("✅ Dane z OSM dostępne");
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

        // 2. HEURYSTYKA - zamiast pytać API
        MilitaryLoadCalculator.BridgeSpecification spec = estimateFromRoadType(point);
        if (spec != null) {
            log.debug("📊 Zastosowano heurystykę dla drogi: {}", point.getRoadName());
            return spec;
        }

        // 3. Domyślne wartości konserwatywne
        log.debug("⚠️ Użyto wartości domyślnych");
        return MilitaryLoadCalculator.BridgeSpecification.createMilitaryDefault(point.getName());
    }

    /**
     * Heurystyka oparta o standardy dróg w Polsce (MLC)
     */
    private MilitaryLoadCalculator.BridgeSpecification estimateFromRoadType(
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

        boolean isTunnel = point.getType() == OverpassService.InfrastructureType.TUNNEL;

        // Normy polskich dróg
        if (roadName.matches("a\\d+") || roadName.contains("autostrada")) {
            maxWeight = new BigDecimal("100.0");   // MLC 150
            maxHeight = new BigDecimal(isTunnel ? "4.7" : "5.5");
            maxAxleLoad = new BigDecimal("15.0");
            bridgeType = isTunnel ? "tunel_autostrada" : "autostrada";
        } else if (roadName.matches("s\\d+") || roadName.contains("ekspresowa")) {
            maxWeight = new BigDecimal("85.0");    // MLC 120
            maxHeight = new BigDecimal(isTunnel ? "4.5" : "5.3");
            maxAxleLoad = new BigDecimal("14.0");
            bridgeType = isTunnel ? "tunel_ekspresowa" : "ekspresowa";
        } else if (roadName.matches("dk\\d+") || roadName.contains("krajowa")) {
            maxWeight = new BigDecimal("75.0");    // MLC 100
            maxHeight = new BigDecimal(isTunnel ? "4.2" : "5.0");
            maxAxleLoad = new BigDecimal("13.0");
            bridgeType = isTunnel ? "tunel_krajowa" : "droga_krajowa";
        } else if (roadName.matches("dw\\d+") || roadName.contains("wojewódzka")) {
            maxWeight = new BigDecimal("60.0");    // MLC 80
            maxHeight = new BigDecimal(isTunnel ? "4.0" : "4.8");
            maxAxleLoad = new BigDecimal("12.0");
            bridgeType = isTunnel ? "tunel_wojewódzka" : "droga_wojewódzka";
        } else {
            maxWeight = new BigDecimal("50.0");    // MLC 60
            maxHeight = new BigDecimal(isTunnel ? "3.8" : "4.5");
            maxAxleLoad = new BigDecimal("11.0");
            bridgeType = isTunnel ? "tunel_lokalna" : "droga_lokalna";
        }

        return MilitaryLoadCalculator.BridgeSpecification.builder()
                .name(point.getName())
                .location(point.getRoadName())
                .maxWeight(maxWeight)
                .maxHeight(maxHeight)
                .maxWidth(new BigDecimal("4.0"))
                .maxAxleLoad(maxAxleLoad)
                .bridgeType(bridgeType + "_estimated")
                .condition("standard_polish_road")
                .build();
    }
}