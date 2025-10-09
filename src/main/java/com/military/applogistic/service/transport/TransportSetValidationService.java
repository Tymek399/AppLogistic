package com.military.applogistic.service.transport;

import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.service.api.OverpassService.InfrastructurePoint;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * Serwis odpowiedzialny za walidację parametrów zestawu transportowego
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TransportSetValidationService {

    /**
     * Waliduje czy wszystkie parametry zestawu są poprawne
     */
    public ValidationResult validateTransportSet(TransportSet transportSet) {
        ValidationResult result = new ValidationResult();

        if (transportSet == null) {
            result.addError("Brak zestawu transportowego");
            return result;
        }

        // Walidacja masy
        if (transportSet.getTotalWeightKg() == null || transportSet.getTotalWeightKg() <= 0) {
            result.addError("Nieprawidłowa masa zestawu: " + transportSet.getTotalWeightKg());
        }

        // Walidacja wysokości
        if (transportSet.getTotalHeightCm() == null || transportSet.getTotalHeightCm() <= 0) {
            result.addError("Nieprawidłowa wysokość zestawu: " + transportSet.getTotalHeightCm());
        }

        // Walidacja długości
        if (transportSet.getTotalLengthCm() == null || transportSet.getTotalLengthCm() <= 0) {
            result.addError("Nieprawidłowa długość zestawu: " + transportSet.getTotalLengthCm());
        }

        // Walidacja szerokości
        if (transportSet.getTotalWidthCm() == null || transportSet.getTotalWidthCm() <= 0) {
            result.addError("Nieprawidłowa szerokość zestawu: " + transportSet.getTotalWidthCm());
        }

        // Walidacja nacisku na oś
        if (transportSet.getMaxAxleLoadKg() == null || transportSet.getMaxAxleLoadKg() <= 0) {
            result.addError("Nieprawidłowy nacisk na oś: " + transportSet.getMaxAxleLoadKg());
        }

        // Walidacja wysokości naczepy vs ładunku
        if (transportSet.getTrailerHeightCm() != null &&
                transportSet.getCargo() != null &&
                transportSet.getCargo().getHeightCm() != null) {

            int calculatedHeight = transportSet.getTrailerHeightCm() + transportSet.getCargo().getHeightCm();
            if (Math.abs(calculatedHeight - transportSet.getTotalHeightCm()) > 10) {
                result.addWarning(String.format(
                        "Rozbieżność wysokości: naczepa %dcm + ładunek %dcm = %dcm, ale total=%dcm",
                        transportSet.getTrailerHeightCm(),
                        transportSet.getCargo().getHeightCm(),
                        calculatedHeight,
                        transportSet.getTotalHeightCm()
                ));
            }
        }

        log.info("Walidacja zestawu {}: {}",
                transportSet.getId(),
                result.isValid() ? "OK" : "BŁĘDY");

        return result;
    }

    /**
     * Waliduje czy zestaw może przejechać przez infrastrukturę
     */
    public InfrastructureValidationResult validateInfrastructure(
            TransportSet transportSet,
            InfrastructurePoint point) {

        InfrastructureValidationResult result = new InfrastructureValidationResult();
        result.setInfrastructureName(point.getName());
        result.setInfrastructureType(point.getType().getPolish());

        // Walidacja wagi
        if (point.getMaxWeightTons() != null) {
            double transportWeightTons = transportSet.getTotalWeightKg() / 1000.0;
            double margin = point.getMaxWeightTons() - transportWeightTons;

            if (margin < 0) {
                result.setCanPass(false);
                result.addViolation(String.format(
                        "%s - PRZEKROCZONA NOŚNOŚĆ: limit %.1ft, twój zestaw %.1ft",
                        point.getName(),
                        point.getMaxWeightTons(),
                        transportWeightTons
                ));
            } else if (margin < 5) {
                result.addWarning(String.format(
                        "%s - mały margines nośności: %.1ft",
                        point.getName(),
                        margin
                ));
            } else {
                result.addSuccess(String.format(
                        "%s - nośność OK: limit %.1ft, twój zestaw %.1ft (zapas %.1ft)",
                        point.getName(),
                        point.getMaxWeightTons(),
                        transportWeightTons,
                        margin
                ));
            }
        }

        // Walidacja wysokości
        if (point.getMaxHeightMeters() != null) {
            double transportHeightMeters = transportSet.getTotalHeightCm() / 100.0;
            double margin = point.getMaxHeightMeters() - transportHeightMeters;

            if (margin < 0) {
                result.setCanPass(false);
                result.addViolation(String.format(
                        "%s - ZA NISKI PRZEJAZD: limit %.2fm, twój zestaw %.2fm",
                        point.getName(),
                        point.getMaxHeightMeters(),
                        transportHeightMeters
                ));
            } else if (margin < 0.3) {
                result.addWarning(String.format(
                        "%s - minimalny zapas wysokości: %.0fcm",
                        point.getName(),
                        margin * 100
                ));
            } else {
                result.addSuccess(String.format(
                        "%s - wysokość OK: limit %.2fm, twój zestaw %.2fm (zapas %.0fcm)",
                        point.getName(),
                        point.getMaxHeightMeters(),
                        transportHeightMeters,
                        margin * 100
                ));
            }
        }

        return result;
    }

    // ================== KLASY POMOCNICZE ==================

    public static class ValidationResult {
        private final List<String> errors = new ArrayList<>();
        private final List<String> warnings = new ArrayList<>();

        public void addError(String error) {
            errors.add(error);
        }

        public void addWarning(String warning) {
            warnings.add(warning);
        }

        public boolean isValid() {
            return errors.isEmpty();
        }

        public List<String> getErrors() {
            return new ArrayList<>(errors);
        }

        public List<String> getWarnings() {
            return new ArrayList<>(warnings);
        }
    }

    public static class InfrastructureValidationResult {
        private String infrastructureName;
        private String infrastructureType;
        private boolean canPass = true;
        private final List<String> violations = new ArrayList<>();
        private final List<String> warnings = new ArrayList<>();
        private final List<String> successes = new ArrayList<>();

        public void setInfrastructureName(String name) {
            this.infrastructureName = name;
        }

        public void setInfrastructureType(String type) {
            this.infrastructureType = type;
        }

        public void setCanPass(boolean canPass) {
            this.canPass = canPass;
        }

        public void addViolation(String violation) {
            violations.add(violation);
        }

        public void addWarning(String warning) {
            warnings.add(warning);
        }

        public void addSuccess(String success) {
            successes.add(success);
        }

        public String getInfrastructureName() {
            return infrastructureName;
        }

        public String getInfrastructureType() {
            return infrastructureType;
        }

        public boolean canPass() {
            return canPass;
        }

        public List<String> getViolations() {
            return new ArrayList<>(violations);
        }

        public List<String> getWarnings() {
            return new ArrayList<>(warnings);
        }

        public List<String> getSuccesses() {
            return new ArrayList<>(successes);
        }
    }
}