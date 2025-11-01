package com.military.applogistic.service;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * ✅ ZAKTUALIZOWANA KLASA - ZWIĘKSZONE LIMITY WOJSKOWE DLA AUTOSTRAD
 *
 * ZMIANY:
 * 1. Limit wojskowy dla autostrad: 120T (poprzednio 100T)
 * 2. Zwiększona tolerancja dla autostrad: 20% (poprzednio 15%)
 * 3. Preferowanie autostrad dla pojazdów >60T
 */
@Slf4j
@Component
public class MilitaryRoadPermissions {

    // Marginesy tolerancji
    private static final double WEIGHT_TOLERANCE_PERCENT = 15.0; // 15% dla masy (drogi zwykłe)
    private static final double MOTORWAY_WEIGHT_TOLERANCE_PERCENT = 20.0; // ✅ 20% dla autostrad
    private static final double HEIGHT_TOLERANCE_CM = 0.0; // 0cm dla wysokości (bezwzględne)

    // ✅ ZAKTUALIZOWANE LIMITY WOJSKOWE
    private static final double MILITARY_MOTORWAY_LIMIT_TONS = 120.0; // ✅ ZWIĘKSZONO z 100T do 120T
    private static final double CIVILIAN_MOTORWAY_LIMIT_TONS = 100.0; // Limit standardowy (bez pozwolenia)

    // ✅ NOWE - Próg dla preferowania autostrad
    private static final double HEAVY_VEHICLE_THRESHOLD_TONS = 60.0; // Pojazdy >60T powinny jechać autostradami

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
        private String infrastructureName;
        private String city;
        private String roadName;
        private boolean preferMotorway; // ✅ NOWE - czy preferować autostradę
    }

    /**
     * ✅ GŁÓWNA METODA - Walidacja z marginesami i uprawnieniami
     * NOWE: zwiększone limity dla autostrad i preferowanie dla ciężkich pojazdów
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

        // ✅ NOWE - Oznacz czy pojazd powinien preferować autostrady
        if (transportWeightTons > HEAVY_VEHICLE_THRESHOLD_TONS) {
            result.setPreferMotorway(true);
            log.info("🛣️ Pojazd >60T - preferowana jest trasa autostradą");
        }

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
            boolean isMotorway = isMotorway(roadType);

            // ✅ NOWE - Różne tolerancje dla autostrad i innych dróg
            double tolerancePercent = isMotorway ? MOTORWAY_WEIGHT_TOLERANCE_PERCENT : WEIGHT_TOLERANCE_PERCENT;
            double tolerance = limitWeightTons * (tolerancePercent / 100.0);
            double effectiveLimit = limitWeightTons + tolerance;
            double weightExcess = transportWeightTons - limitWeightTons;

            result.setWeightExcess(weightExcess);

            // ✅ PRZYPADEK 1: W limicie + tolerancji
            if (transportWeightTons <= effectiveLimit) {
                result.setCanPass(true);

                if (transportWeightTons > limitWeightTons) {
                    result.setRequiresPermit(true);
                    result.setPermitType("STANDARD");
                    result.setReason(String.format(
                            "Przekroczenie o %.1f%% (%.1ft > %.1ft) - wymaga standardowego pozwolenia (tolerancja %.0f%%)",
                            (weightExcess / limitWeightTons) * 100,
                            transportWeightTons, limitWeightTons,
                            tolerancePercent
                    ));
                    log.info("⚠️ Masa: {}t > {}t (+{}%) - WYMAGA POZWOLENIA (tolerancja {}%)",
                            transportWeightTons, limitWeightTons,
                            String.format("%.1f", (weightExcess / limitWeightTons) * 100),
                            (int)tolerancePercent);
                } else {
                    result.setReason("Przejazd bez ograniczeń");
                    log.debug("✅ Masa OK: {}t <= {}t", transportWeightTons, limitWeightTons);
                }
                return result;
            }

            // ✅ PRZYPADEK 2: Autostrada - sprawdź specjalne uprawnienia wojskowe (do 120T)
            if (isMotorway) {
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

                // ✅ Przekroczenie limitu wojskowego autostrad (>120T)
                // Dla autostrad dopuszczamy większe przekroczenia z pozwoleniem specjalnym
                if (transportWeightTons <= MILITARY_MOTORWAY_LIMIT_TONS * 1.2) {
                    result.setCanPass(true);
                    result.setRequiresPermit(true);
                    result.setPermitType("MILITARY_SPECIAL");
                    result.setReason(String.format(
                            "Autostrada: %.1ft > %.1ft (przekroczenie limitu wojskowego) - WYMAGA SPECJALNEGO POZWOLENIA WOJSKOWEGO + ESKORTĘ",
                            transportWeightTons, MILITARY_MOTORWAY_LIMIT_TONS
                    ));
                    log.warn("🚨 Autostrada: {}t > {}t (+{}%) - POZWOLENIE SPECJALNE + ESKORTA",
                            transportWeightTons, MILITARY_MOTORWAY_LIMIT_TONS,
                            String.format("%.1f", ((transportWeightTons - MILITARY_MOTORWAY_LIMIT_TONS) / MILITARY_MOTORWAY_LIMIT_TONS) * 100));
                    return result;
                }

                // Przekroczenie >20% limitu wojskowego - BLOKADA
                result.setCanPass(false);
                result.setReason(String.format(
                        "Autostrada: %.1ft > %.1ft (przekroczenie >20%% limitu wojskowego) - BRAK MOŻLIWOŚCI PRZEJAZDU",
                        transportWeightTons, MILITARY_MOTORWAY_LIMIT_TONS * 1.2
                ));
                log.error("❌ Autostrada: {}t > {}t (+20% = {}t) - BLOKADA",
                        transportWeightTons, MILITARY_MOTORWAY_LIMIT_TONS, MILITARY_MOTORWAY_LIMIT_TONS * 1.2);
                return result;
            }

            // ✅ PRZYPADEK 3: Przekroczenie powyżej tolerancji (inne drogi)
            result.setCanPass(false);
            result.setReason(String.format(
                    "Przekroczenie o %.1f%% (%.1ft > %.1ft + %.0f%% tolerancji) - BRAK PRZEJAZDU",
                    (weightExcess / limitWeightTons) * 100,
                    transportWeightTons, effectiveLimit, tolerancePercent
            ));
            log.error("❌ Masa: {}t > {}t (+{}% = {}t) - BLOKADA",
                    transportWeightTons, limitWeightTons, (int)tolerancePercent, effectiveLimit);
            return result;
        }

        // ✅ Brak limitów - przejazd OK
        result.setCanPass(true);
        result.setReason("Brak danych o ograniczeniach");
        log.debug("ℹ️ Brak limitów - przejazd OK");
        return result;
    }

    /**
     * ✅ ZAKTUALIZOWANA METODA - rozszerzona lista autostrad
     */
    private boolean isMotorway(String roadType) {
        if (roadType == null) return false;
        String type = roadType.toLowerCase();
        return type.contains("autostrada") ||
                type.contains("motorway") ||
                type.contains("highway") ||
                type.matches(".*\\ba\\d+.*");  // Dopasowanie A1, A2, A4, etc.
    }

    /**
     * ✅ NOWA METODA - sprawdza czy pojazd powinien preferować autostrady
     */
    public boolean shouldPreferMotorway(double transportWeightTons) {
        return transportWeightTons > HEAVY_VEHICLE_THRESHOLD_TONS;
    }

    /**
     * ✅ NOWA METODA - formatowanie komunikatu z miastem
     */
    public String getPermitDescriptionWithLocation(ValidationResult result) {
        if (!result.isRequiresPermit()) {
            return "Brak wymaganych pozwoleń";
        }

        StringBuilder description = new StringBuilder();

        // Typ pozwolenia
        switch (result.getPermitType()) {
            case "STANDARD":
                description.append("📋 Pozwolenie standardowe");
                break;
            case "MILITARY_MOTORWAY":
                description.append("🎖️ Pozwolenie wojskowe (autostrada do 120T)");
                break;
            case "MILITARY_SPECIAL":
                description.append("🚨 Pozwolenie specjalne wojskowe + eskorta");
                break;
            default:
                description.append("📋 Pozwolenie wymagane");
        }

        // Nazwa obiektu
        if (result.getInfrastructureName() != null) {
            description.append(" - ").append(result.getInfrastructureName());
        }

        // Miasto
        if (result.getCity() != null) {
            description.append(" (").append(result.getCity()).append(")");
        }

        // Droga
        if (result.getRoadName() != null) {
            description.append(" - droga: ").append(result.getRoadName());
        }

        // Szczegóły przekroczenia
        if (result.getWeightExcess() > 0) {
            description.append(String.format(" | Przekroczenie: +%.1ft (%.1f%%)",
                    result.getWeightExcess(),
                    (result.getWeightExcess() / result.getLimitWeight()) * 100));
        }

        return description.toString();
    }

    /**
     * ✅ NOWA METODA - zwraca zalecany typ drogi dla danego pojazdu
     */
    public String getRecommendedRoadType(double transportWeightTons) {
        if (transportWeightTons > HEAVY_VEHICLE_THRESHOLD_TONS) {
            return "MOTORWAY_PREFERRED";
        } else if (transportWeightTons > 40.0) {
            return "MOTORWAY_RECOMMENDED";
        } else {
            return "ANY_SUITABLE";
        }
    }

    /**
     * ✅ NOWA METODA - zwraca opis zaleceń dotyczących trasy
     */
    public String getRouteRecommendation(double transportWeightTons) {
        if (transportWeightTons > HEAVY_VEHICLE_THRESHOLD_TONS) {
            return String.format(
                    "⚠️ POJAZD CIĘŻKI (%.1ft) - SILNIE ZALECANA TRASA AUTOSTRADĄ. " +
                            "Autostrady mają limit do 120T z pozwoleniem wojskowym i są najbezpieczniejsze dla tego typu transportu.",
                    transportWeightTons
            );
        } else if (transportWeightTons > 40.0) {
            return String.format(
                    "ℹ️ Transport średni (%.1ft) - zalecana trasa autostradą gdy dostępna. " +
                            "Możliwa trasa drogami ekspresowymi (limit do 100T).",
                    transportWeightTons
            );
        } else {
            return String.format(
                    "✅ Transport lekki (%.1ft) - możliwa trasa dowolnymi drogami zgodnie z przepisami.",
                    transportWeightTons
            );
        }
    }
}