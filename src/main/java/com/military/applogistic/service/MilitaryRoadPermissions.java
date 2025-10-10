package com.military.applogistic.service;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class MilitaryRoadPermissions {

    // Marginesy tolerancji
    private static final double WEIGHT_TOLERANCE_PERCENT = 15.0; // 15% dla masy
    private static final double HEIGHT_TOLERANCE_CM = 0.0; // 0cm dla wysokości (bezwzględne)

    // Specjalne limity wojskowe
    private static final double MILITARY_MOTORWAY_LIMIT_TONS = 100.0; // Autostrady z pozwoleniem
    private static final double CIVILIAN_MOTORWAY_LIMIT_TONS = 60.0; // Autostrady standardowe

    @Data
    public static class ValidationResult {
        private boolean canPass;
        private boolean requiresPermit;
        private String reason;
        private double actualWeight;
        private double actualHeight;
        private double limitWeight;
        private double limitHeight;
        private double weightExcess;
        private double heightExcess;
        private String permitType;
        private String infrastructureName; // ✅ NOWE
        private String city; // ✅ NOWE - miasto
        private String roadName; // ✅ NOWE - droga
    }

    /**
     * ✅ GŁÓWNA METODA - Walidacja z marginesami i uprawnieniami
     */
    public ValidationResult validatePassage(
            double transportWeightTons,
            double transportHeightMeters,
            Double limitWeightTons,
            Double limitHeightMeters,
            String roadType) {

        ValidationResult result = new ValidationResult();
        result.setActualWeight(transportWeightTons);
        result.setActualHeight(transportHeightMeters);
        result.setLimitWeight(limitWeightTons != null ? limitWeightTons : 0);
        result.setLimitHeight(limitHeightMeters != null ? limitHeightMeters : 0);

        // ✅ WALIDACJA WYSOKOŚCI - BEZWZGLĘDNA (brak tolerancji)
        if (limitHeightMeters != null && transportHeightMeters > limitHeightMeters) {
            result.setCanPass(false);
            result.setHeightExcess(transportHeightMeters - limitHeightMeters);
            result.setReason(String.format(
                    "Przekroczenie wysokości: %.2fm > %.2fm (BRAK MOŻLIWOŚCI PRZEJAZDU)",
                    transportHeightMeters, limitHeightMeters
            ));
            log.warn("❌ Wysokość: {}m > {}m - BLOKADA", transportHeightMeters, limitHeightMeters);
            return result;
        }

        // ✅ WALIDACJA MASY - Z MARGINESEM I UPRAWNIENIAMI
        if (limitWeightTons != null) {
            double tolerance = limitWeightTons * (WEIGHT_TOLERANCE_PERCENT / 100.0);
            double effectiveLimit = limitWeightTons + tolerance;
            double weightExcess = transportWeightTons - limitWeightTons;

            result.setWeightExcess(weightExcess);

            // ✅ PRZYPADEK 1: W limicie + tolerancji (15%)
            if (transportWeightTons <= effectiveLimit) {
                result.setCanPass(true);

                if (transportWeightTons > limitWeightTons) {
                    result.setRequiresPermit(true);
                    result.setPermitType("STANDARD");
                    result.setReason(String.format(
                            "Przekroczenie o %.1f%% (%.1ft > %.1ft) - wymaga standardowego pozwolenia",
                            (weightExcess / limitWeightTons) * 100,
                            transportWeightTons, limitWeightTons
                    ));
                    log.info("⚠️ Masa: {}t > {}t (+{}%) - WYMAGA POZWOLENIA",
                            transportWeightTons, limitWeightTons,
                            String.format("%.1f", (weightExcess / limitWeightTons) * 100));
                } else {
                    result.setReason("Przejazd bez ograniczeń");
                    log.debug("✅ Masa OK: {}t <= {}t", transportWeightTons, limitWeightTons);
                }
                return result;
            }

            // ✅ PRZYPADEK 2: Autostrada - sprawdź specjalne uprawnienia wojskowe
            if (isMotorway(roadType)) {
                if (transportWeightTons <= MILITARY_MOTORWAY_LIMIT_TONS) {
                    result.setCanPass(true);
                    result.setRequiresPermit(true);
                    result.setPermitType("MILITARY_MOTORWAY");
                    result.setReason(String.format(
                            "Autostrada: %.1ft (limit cywilny: %.1ft, limit wojskowy: %.1ft) - WYMAGA POZWOLENIA WOJSKOWEGO",
                            transportWeightTons, CIVILIAN_MOTORWAY_LIMIT_TONS, MILITARY_MOTORWAY_LIMIT_TONS
                    ));
                    log.info("⚠️ Autostrada: {}t (cywilny: {}t, wojskowy: {}t) - POZWOLENIE WOJSKOWE",
                            transportWeightTons, CIVILIAN_MOTORWAY_LIMIT_TONS, MILITARY_MOTORWAY_LIMIT_TONS);
                    return result;
                }

                // Przekroczenie nawet limitu wojskowego > 100%
                if (transportWeightTons > MILITARY_MOTORWAY_LIMIT_TONS * 2) {
                    result.setCanPass(false);
                    result.setReason(String.format(
                            "Autostrada: %.1ft > %.1ft (przekroczenie powyżej 100%% limitu wojskowego) - BRAK PRZEJAZDU",
                            transportWeightTons, MILITARY_MOTORWAY_LIMIT_TONS
                    ));
                    log.error("❌ Autostrada: {}t > {}t (>100% limitu) - BLOKADA",
                            transportWeightTons, MILITARY_MOTORWAY_LIMIT_TONS);
                    return result;
                }
            }

            // ✅ PRZYPADEK 3: Przekroczenie powyżej 15% (inne drogi)
            result.setCanPass(false);
            result.setReason(String.format(
                    "Przekroczenie o %.1f%% (%.1ft > %.1ft + 15%% tolerancji) - BRAK PRZEJAZDU",
                    (weightExcess / limitWeightTons) * 100,
                    transportWeightTons, effectiveLimit
            ));
            log.error("❌ Masa: {}t > {}t (+15% = {}t) - BLOKADA",
                    transportWeightTons, limitWeightTons, effectiveLimit);
            return result;
        }

        // ✅ Brak limitów - przejazd OK
        result.setCanPass(true);
        result.setReason("Brak danych o ograniczeniach");
        log.debug("ℹ️ Brak limitów - przejazd OK");
        return result;
    }

    private boolean isMotorway(String roadType) {
        if (roadType == null) return false;
        String type = roadType.toLowerCase();
        return type.contains("autostrada") ||
                type.contains("motorway") ||
                type.contains("highway") ||
                type.contains("a1") || type.contains("a2") || type.contains("a4");
    }

    /**
     * ✅ NOWA METODA - formatowanie komunikatu z miastem
     */
    public String getPermitDescriptionWithLocation(ValidationResult result) {
        if (!result.isRequiresPermit()) {
            return "Brak wymaganych pozwoleń";
        }

        StringBuilder description = new StringBuilder();

        // Nazwa obiektu (most/tunel/droga)
        if (result.getInfrastructureName() != null) {
            description.append(result.getInfrastructureName());
        }

        // Miasto
        if (result.getCity() != null) {
            description.append(" (").append(result.getCity()).append(")");
        }

        // Droga
        if (result.getRoadName() != null) {
            description.append(" - droga: ").append(result.getRoadName());
        }

        description.append(": ");

        // Typ pozwolenia
        switch (result.getPermitType()) {
            case "STANDARD":
                description.append("Wymaga standardowego pozwolenia na przejazd (przekroczenie do 15%)");
                break;
            case "MILITARY_MOTORWAY":
                description.append("Wymaga pozwolenia wojskowego na przejazd autostradą (60-100t)");
                break;
            default:
                description.append("Wymaga specjalnego pozwolenia");
        }

        return description.toString();
    }

    /**
     * ✅ Metoda pomocnicza - formatowanie komunikatu dla użytkownika (stara wersja - kompatybilność)
     */
    public String getPermitDescription(ValidationResult result) {
        if (!result.isRequiresPermit()) {
            return "Brak wymaganych pozwoleń";
        }

        switch (result.getPermitType()) {
            case "STANDARD":
                return "Wymaga standardowego pozwolenia na przejazd (przekroczenie do 15%)";
            case "MILITARY_MOTORWAY":
                return "Wymaga pozwolenia wojskowego na przejazd autostradą (60-100t)";
            default:
                return "Wymaga specjalnego pozwolenia";
        }
    }
}