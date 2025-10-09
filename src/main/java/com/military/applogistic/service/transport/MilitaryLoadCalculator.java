package com.military.applogistic.service.transport;

import lombok.Data;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Kalkulator obciążeń dla transportu wojskowego
 * Zgodny z normami NATO STANAG 2021 i MLC (Military Load Classification)
 */
@Slf4j
@Component
public class MilitaryLoadCalculator {

    private static final BigDecimal GRAVITY = new BigDecimal("9.81");
    private static final BigDecimal SAFETY_FACTOR = new BigDecimal("1.1");  // Zmniejszony dla wojska

    // LIMITY WOJSKOWE (wyższe niż cywilne)
    private static final BigDecimal MAX_AXLE_LOAD_MILITARY = new BigDecimal("15.0");  // 15t
    private static final BigDecimal MAX_TOTAL_WEIGHT_MILITARY = new BigDecimal("100.0"); // 100t

    public AxleLoadResult calculateAxleLoad(VehicleConfiguration config) {
        log.debug("Kalkulacja nacisku na oś dla: {}", config.getVehicleName());

        BigDecimal totalWeight = config.getTotalWeight();
        BigDecimal frontAxleRatio = config.getFrontAxleRatio();
        BigDecimal rearAxleRatio = BigDecimal.ONE.subtract(frontAxleRatio);

        BigDecimal frontAxleLoad = totalWeight
                .multiply(frontAxleRatio)
                .divide(BigDecimal.valueOf(config.getFrontAxleCount()), 2, RoundingMode.HALF_UP);

        BigDecimal rearAxleLoad = totalWeight
                .multiply(rearAxleRatio)
                .divide(BigDecimal.valueOf(config.getRearAxleCount()), 2, RoundingMode.HALF_UP);

        BigDecimal maxAxleLoad = frontAxleLoad.max(rearAxleLoad);
        BigDecimal effectiveAxleLoad = maxAxleLoad.multiply(SAFETY_FACTOR);

        return AxleLoadResult.builder()
                .frontAxleLoad(frontAxleLoad)
                .rearAxleLoad(rearAxleLoad)
                .maxAxleLoad(maxAxleLoad)
                .effectiveAxleLoad(effectiveAxleLoad)
                .exceedsMilitaryLimit(effectiveAxleLoad.compareTo(MAX_AXLE_LOAD_MILITARY) > 0)
                .build();
    }

    public BridgeCrossabilityResult checkBridgeCrossability(
            VehicleConfiguration vehicle,
            BridgeSpecification bridge) {

        log.debug("Sprawdzanie przepustowości mostu {} dla {}",
                bridge.getName(), vehicle.getVehicleName());

        AxleLoadResult axleLoad = calculateAxleLoad(vehicle);

        boolean weightOk = vehicle.getTotalWeight().compareTo(bridge.getMaxWeight()) <= 0;
        boolean heightOk = vehicle.getHeight().compareTo(bridge.getMaxHeight()) <= 0;
        boolean widthOk = vehicle.getWidth().compareTo(bridge.getMaxWidth()) <= 0;

        boolean lengthOk = true;
        if (bridge.getLength() != null) {
            lengthOk = vehicle.getLength()
                    .compareTo(bridge.getLength().multiply(new BigDecimal("0.75"))) <= 0;
        }

        boolean axleLoadOk = axleLoad.getEffectiveAxleLoad()
                .compareTo(bridge.getMaxAxleLoad()) <= 0;

        boolean canCross = weightOk && heightOk && widthOk && lengthOk && axleLoadOk;

        int mlcRating = calculateMLCRating(bridge);
        int vehicleMLC = calculateVehicleMLC(vehicle);

        return BridgeCrossabilityResult.builder()
                .canCross(canCross)
                .weightOk(weightOk)
                .heightOk(heightOk)
                .widthOk(widthOk)
                .lengthOk(lengthOk)
                .axleLoadOk(axleLoadOk)
                .bridgeMLC(mlcRating)
                .vehicleMLC(vehicleMLC)
                .mlcCompatible(vehicleMLC <= mlcRating)
                .weightMargin(bridge.getMaxWeight().subtract(vehicle.getTotalWeight()))
                .heightMargin(bridge.getMaxHeight().subtract(vehicle.getHeight()))
                .reasons(buildFailureReasons(weightOk, heightOk, widthOk, lengthOk, axleLoadOk,
                        vehicle, bridge, axleLoad))
                .build();
    }

    private int calculateMLCRating(BridgeSpecification bridge) {
        // MLC rating: weight in tons / 0.9 dla gąsienicowych, / 1.13 dla kołowych
        return bridge.getMaxWeight()
                .divide(new BigDecimal("1.0"), 0, RoundingMode.DOWN)
                .intValue();
    }

    private int calculateVehicleMLC(VehicleConfiguration vehicle) {
        BigDecimal mlcDivisor = vehicle.isTracked() ?
                new BigDecimal("0.9") : new BigDecimal("1.13");

        return vehicle.getTotalWeight()
                .divide(mlcDivisor, 0, RoundingMode.UP)
                .intValue();
    }

    private String buildFailureReasons(boolean weightOk, boolean heightOk,
                                       boolean widthOk, boolean lengthOk, boolean axleLoadOk,
                                       VehicleConfiguration vehicle, BridgeSpecification bridge,
                                       AxleLoadResult axleLoad) {

        StringBuilder reasons = new StringBuilder();

        if (!weightOk) {
            reasons.append(String.format("PRZEKROCZONA NOŚNOŚĆ: limit %.1ft, pojazd %.1ft; ",
                    bridge.getMaxWeight(), vehicle.getTotalWeight()));
        }
        if (!heightOk) {
            reasons.append(String.format("ZA WYSOKI: limit %.2fm, pojazd %.2fm; ",
                    bridge.getMaxHeight(), vehicle.getHeight()));
        }
        if (!widthOk) {
            reasons.append(String.format("ZA SZEROKI: limit %.2fm, pojazd %.2fm; ",
                    bridge.getMaxWidth(), vehicle.getWidth()));
        }
        if (!lengthOk) {
            reasons.append(String.format("ZA DŁUGI dla mostu o długości %.1fm; ",
                    bridge.getLength()));
        }
        if (!axleLoadOk) {
            reasons.append(String.format("PRZEKROCZONY NACISK NA OŚ: limit %.2ft, pojazd %.2ft; ",
                    bridge.getMaxAxleLoad(), axleLoad.getEffectiveAxleLoad()));
        }

        return reasons.toString();
    }

    @Data
    @Builder
    public static class VehicleConfiguration {
        private String vehicleName;
        private BigDecimal totalWeight;
        private BigDecimal height;
        private BigDecimal width;
        private BigDecimal length;
        private int numberOfAxles;
        private int frontAxleCount;
        private int rearAxleCount;
        private BigDecimal frontAxleRatio;
        private boolean tracked;
    }

    @Data
    @Builder
    public static class BridgeSpecification {
        private String name;
        private String location;
        private BigDecimal maxWeight;
        private BigDecimal maxHeight;
        private BigDecimal maxWidth;
        private BigDecimal length;
        private BigDecimal maxAxleLoad;
        private String bridgeType;
        private Integer yearBuilt;
        private String condition;

        /**
         * NOWA: Domyślne wartości dla transportów WOJSKOWYCH
         */
        public static BridgeSpecification createMilitaryDefault(String name) {
            return BridgeSpecification.builder()
                    .name(name)
                    .maxWeight(new BigDecimal("70.0"))   // 70t - typowy most MLC 100
                    .maxHeight(new BigDecimal("5.0"))     // 5.0m
                    .maxWidth(new BigDecimal("4.0"))      // 4.0m
                    .maxAxleLoad(new BigDecimal("13.0"))  // 13t
                    .bridgeType("military_default_mlc100")
                    .condition("assumed_military_standard")
                    .build();
        }
    }

    @Data
    @Builder
    public static class AxleLoadResult {
        private BigDecimal frontAxleLoad;
        private BigDecimal rearAxleLoad;
        private BigDecimal maxAxleLoad;
        private BigDecimal effectiveAxleLoad;
        private boolean exceedsMilitaryLimit;
    }

    @Data
    @Builder
    public static class BridgeCrossabilityResult {
        private boolean canCross;
        private boolean weightOk;
        private boolean heightOk;
        private boolean widthOk;
        private boolean lengthOk;
        private boolean axleLoadOk;
        private int bridgeMLC;
        private int vehicleMLC;
        private boolean mlcCompatible;
        private BigDecimal weightMargin;
        private BigDecimal heightMargin;
        private String reasons;
    }
}