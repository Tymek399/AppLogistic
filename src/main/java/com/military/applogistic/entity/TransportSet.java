package com.military.applogistic.entity;

import jakarta.persistence.*;
import lombok.Data;

@Entity
@Table(name = "transport_sets")
@Data
public class TransportSet {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "transporter_id")
    private Vehicle transporter;

    @ManyToOne
    @JoinColumn(name = "cargo_id")
    private Vehicle cargo;

    // ═══════════════════════════════════════════════════════════════════════════
    // NOWE POLE - Odniesienie do konkretnej naczepy
    // ═══════════════════════════════════════════════════════════════════════════
    @ManyToOne
    @JoinColumn(name = "trailer_id")
    private Trailer trailer;

    private String description;

    private Integer totalHeightCm;
    private Integer totalWeightKg;
    private Integer maxAxleLoadKg;
    private Integer totalLengthCm;
    private Integer totalWidthCm;
    private Integer trailerHeightCm;

    // ✅ NOWE POLE - wymuszenie jazdy na własnych kołach
    private Boolean forceSelfDriving = false;

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * GŁÓWNA METODA OBLICZANIA PARAMETRÓW ZESTAWU TRANSPORTOWEGO
     *
     * Uwzględnia trzy źródła danych:
     * 1. Transporter (ciągnik) - Vehicle
     * 2. Cargo (ładunek) - Vehicle
     * 3. Trailer (naczepa) - Trailer (NOWE!)
     * ═══════════════════════════════════════════════════════════════════════════
     */
    public void calculateTransportParameters() {
        if (transporter == null || cargo == null) {
            return;
        }

        // ✅ PRIORYTET 1: Jeśli operator wymusił self-driving
        if (Boolean.TRUE.equals(forceSelfDriving) && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            calculateSelfDrivingParameters();
            return;
        }

        // ✅ PRIORYTET 2: Automatyczna decyzja czy pojazd powinien jechać na lawecie
        boolean isOnTrailer = shouldBeOnTrailer();

        if (!isOnTrailer && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            calculateSelfDrivingParameters();
        } else {
            calculateTrailerParameters();
        }
    }

    /**
     * Obliczenia dla pojazdu jadącego samodzielnie (bez naczepy)
     */
    private void calculateSelfDrivingParameters() {
        // STANDALONE: Lekki pojazd jedzie sam (Humvee, Sprinter)
        this.totalWeightKg = cargo.getTotalWeightKg();
        this.totalHeightCm = cargo.getHeightCm();
        this.totalLengthCm = estimateStandaloneLength();
        this.totalWidthCm = estimateCargoWidth();
        this.maxAxleLoadKg = cargo.getMaxAxleLoadKg();
        this.trailerHeightCm = 0;
    }

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * ZMODYFIKOWANA METODA - Obliczenia dla zestawu z naczepą
     *
     * Jeśli naczepa jest przypisana, używa jej RZECZYWISTYCH parametrów.
     * W przeciwnym razie używa estymacji (dla kompatybilności wstecznej).
     * ═══════════════════════════════════════════════════════════════════════════
     */
    private void calculateTrailerParameters() {
        // ═══════════════════════════════════════════════════════════════════════
        // WAGA CAŁKOWITA ZESTAWU
        // W_całkowita = W_ciągnik_pusty + W_naczepa_pusta + W_ładunek + W_wyposażenie
        // ═══════════════════════════════════════════════════════════════════════

        // Masa pustego ciągnika (szacowana jako 40% DMC)
        int tractorWeight = (int) (transporter.getTotalWeightKg() * 0.4);

        // Masa ładunku
        int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;

        // Masa naczepy - używamy rzeczywistych danych jeśli dostępne
        int trailerWeight;
        if (trailer != null && trailer.getEmptyWeight() != null) {
            trailerWeight = trailer.getEmptyWeight();
        } else {
            trailerWeight = estimateSemiTrailerWeight(cargoWeight);
        }

        // Dodatkowe wyposażenie wojskowe
        int militaryEquipmentWeight = estimateMilitaryEquipment(cargo.getModel());

        this.totalWeightKg = tractorWeight + trailerWeight + cargoWeight + militaryEquipmentWeight;

        // ═══════════════════════════════════════════════════════════════════════
        // WYSOKOŚĆ ZESTAWU
        // H_zestawu = max(H_ciągnik, H_naczepa_pusta + H_ładunek)
        // ═══════════════════════════════════════════════════════════════════════

        int trailerUnloadedHeightCm;
        if (trailer != null && trailer.getUnloadedHeightCm() != null) {
            trailerUnloadedHeightCm = trailer.getUnloadedHeightCm();
        } else {
            trailerUnloadedHeightCm = estimateTrailerHeight(cargoWeight);
        }
        this.trailerHeightCm = trailerUnloadedHeightCm;

        int cargoHeightCm = cargo.getHeightCm() != null ? cargo.getHeightCm() : 0;
        int transporterHeightCm = transporter.getHeightCm() != null ? transporter.getHeightCm() : 0;

        // Wysokość zestawu = max(wysokość ciągnika, wysokość naczepy + ładunek)
        int heightWithCargo = trailerUnloadedHeightCm + cargoHeightCm;
        this.totalHeightCm = Math.max(transporterHeightCm, heightWithCargo);

        // ═══════════════════════════════════════════════════════════════════════
        // DŁUGOŚĆ ZESTAWU
        // L_zestawu = L_ciągnik + L_naczepa (lub estymacja)
        // ═══════════════════════════════════════════════════════════════════════

        if (trailer != null && trailer.getLengthCm() != null) {
            int truckLength = 650; // Standardowa długość ciągnika 6x4 (6.5m)
            this.totalLengthCm = truckLength + trailer.getLengthCm();
        } else {
            this.totalLengthCm = estimateTotalLength();
        }

        // ═══════════════════════════════════════════════════════════════════════
        // SZEROKOŚĆ ZESTAWU
        // W_zestawu = max(W_ciągnik, W_naczepa, W_ładunek)
        // ═══════════════════════════════════════════════════════════════════════

        int trailerWidthCm = 255; // Domyślna szerokość naczepy
        if (trailer != null && trailer.getWidthCm() != null) {
            trailerWidthCm = trailer.getWidthCm();
        }

        this.totalWidthCm = Math.max(
                Math.max(estimateTransporterWidth(), trailerWidthCm),
                estimateCargoWidth()
        );

        // ═══════════════════════════════════════════════════════════════════════
        // NACISK NA OSIE
        // Używamy rzeczywistej liczby osi naczepy jeśli dostępna
        // ═══════════════════════════════════════════════════════════════════════

        this.maxAxleLoadKg = calculateMaxAxleLoad(this.totalWeightKg, getAxleCount());
    }

    /**
     * ✅ KLUCZOWA METODA: Czy pojazd powinien jechać na lawecie?
     */
    private boolean shouldBeOnTrailer() {
        String model = cargo.getModel().toLowerCase();
        int weight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;

        // Pojazdy >5t ZAWSZE na lawecie (oszczędność paliwa + zużycia)
        if (weight > 5000) {
            return true;
        }

        // Pojazdy wojskowe gąsienicowe/ciężkie ZAWSZE na lawecie
        if (model.contains("rosomak") ||
                model.contains("leopard") ||
                model.contains("krab") ||
                model.contains("bwp") ||
                model.contains("tank")) {
            return true;
        }

        // Lekkie pojazdy kołowe mogą jechać same
        return false;
    }

    private int estimateMilitaryEquipment(String model) {
        if (model == null) return 0;

        String m = model.toLowerCase();

        if (m.contains("rosomak")) {
            return 3000; // Załoga + amunicja + prowiant
        }

        if (m.contains("krab")) {
            return 5000; // Załoga + amunicja artyleryjska
        }

        if (m.contains("leopard")) {
            return 8000; // Czołg bojowy z pełnym wyposażeniem
        }

        return 0;
    }

    private int estimateStandaloneLength() {
        String model = cargo.getModel().toLowerCase();
        if (model.contains("rosomak")) return 730;
        if (model.contains("leopard")) return 1020;
        if (model.contains("krab")) return 920;
        if (model.contains("humvee")) return 490;
        if (model.contains("sprinter")) return 620;
        return 600;
    }

    private int estimateTrailerHeight(Integer cargoWeight) {
        if (cargoWeight == null) return 120;
        if (cargoWeight > 45000) return 80;  // Niskopodwoziowa
        if (cargoWeight > 25000) return 120; // Standardowa
        return 140; // Lekka
    }

    private int estimateSemiTrailerWeight(int cargoWeight) {
        if (cargoWeight > 50000) return 15000; // Wzmocniona dla czołgów
        if (cargoWeight > 30000) return 12000; // Ciężka
        if (cargoWeight > 15000) return 10000; // Standardowa
        return 8000; // Lekka
    }

    private int calculateMaxAxleLoad(int totalWeight, int axleCount) {
        if (axleCount == 0) return 0;
        int baseAxleLoad = totalWeight / axleCount;
        return (int) (baseAxleLoad * 1.2);
    }

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * ZMODYFIKOWANA METODA - Liczba osi zestawu
     * Używa rzeczywistej liczby osi naczepy jeśli dostępna
     * ═══════════════════════════════════════════════════════════════════════════
     */
    private int getAxleCount() {
        int truckAxles = 3; // Typowo 6x4 ciągnik
        int trailerAxles;

        // Używamy rzeczywistej liczby osi naczepy jeśli dostępna
        if (trailer != null && trailer.getNumberOfAxles() != null) {
            trailerAxles = trailer.getNumberOfAxles();
        } else {
            // Estymacja na podstawie wagi ładunku
            int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;
            if (cargoWeight > 50000) {
                trailerAxles = 4; // 4-osiowa dla czołgów
            } else if (cargoWeight > 30000) {
                trailerAxles = 3; // 3-osiowa
            } else {
                trailerAxles = 2; // Standardowa
            }
        }

        return truckAxles + trailerAxles;
    }

    private int estimateTotalLength() {
        int truckLength = 650; // Ciągnik 6x4
        String cargoModel = cargo.getModel().toLowerCase();
        int cargoLength = 1350;

        if (cargoModel.contains("leopard")) cargoLength = 1200;
        else if (cargoModel.contains("krab")) cargoLength = 1400;
        else if (cargoModel.contains("rosomak")) cargoLength = 1000;

        return truckLength + cargoLength;
    }

    private int estimateTransporterWidth() {
        return 255; // 2.55m
    }

    private int estimateCargoWidth() {
        String model = cargo.getModel().toLowerCase();
        if (model.contains("leopard")) return 370;
        if (model.contains("rosomak")) return 290;
        if (model.contains("krab")) return 340;
        if (model.contains("radar")) return 320;
        if (model.contains("humvee")) return 220;
        if (model.contains("sprinter")) return 200;
        return 255;
    }

    public Integer getTotalHeightCm() {
        if (totalHeightCm == null) calculateTransportParameters();
        return totalHeightCm;
    }

    public Integer getTrailerHeightCm() {
        if (trailerHeightCm == null) calculateTransportParameters();
        return trailerHeightCm;
    }

    public Integer getTotalWeightKg() {
        if (totalWeightKg == null) calculateTransportParameters();
        return totalWeightKg;
    }

    public Integer getMaxAxleLoadKg() {
        if (maxAxleLoadKg == null) calculateTransportParameters();
        return maxAxleLoadKg;
    }

    public Integer getTotalLengthCm() {
        if (totalLengthCm == null) calculateTransportParameters();
        return totalLengthCm;
    }

    public Integer getTotalWidthCm() {
        if (totalWidthCm == null) calculateTransportParameters();
        return totalWidthCm;
    }

    public boolean isLowLoader() {
        String cargoModel = cargo.getModel().toLowerCase();
        return cargoModel.contains("leopard") ||
                cargoModel.contains("tank") ||
                cargo.getTotalWeightKg() > 45000;
    }

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * ZMODYFIKOWANA METODA - Typ naczepy z uwzględnieniem rzeczywistej naczepy
     * ═══════════════════════════════════════════════════════════════════════════
     */
    public String getTrailerType() {
        if (Boolean.TRUE.equals(forceSelfDriving)) {
            return "Pojazd jedzie na własnych kołach (wymuszony)";
        }

        if (!shouldBeOnTrailer() && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            return "Pojazd samojezdny (bez naczepy)";
        }

        // Jeśli mamy przypisaną rzeczywistą naczepę, użyj jej typu
        if (trailer != null && trailer.getType() != null) {
            return "Naczepa: " + trailer.getType() + " (" + trailer.getRegistrationNumber() + ")";
        }

        // Fallback do estymacji
        if (isLowLoader()) {
            return "Naczepa niskopodwoziowa";
        }
        if (cargo.getTotalWeightKg() > 30000) {
            return "Naczepa wzmocniona";
        }
        return "Naczepa standardowa";
    }

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * ZMODYFIKOWANA METODA - Szczegółowy opis wysokości
     * ═══════════════════════════════════════════════════════════════════════════
     */
    public String getHeightBreakdown() {
        if (Boolean.TRUE.equals(forceSelfDriving) ||
                (!shouldBeOnTrailer() && Boolean.TRUE.equals(cargo.getCanDriveAlone()))) {
            return String.format("Wysokość pojazdu: %dcm (jedzie samodzielnie)", getTotalHeightCm());
        }

        String trailerInfo = "";
        if (trailer != null) {
            trailerInfo = String.format(" [%s]", trailer.getRegistrationNumber());
        }

        return String.format("Wysokość całkowita: %dcm (naczepa %dcm + ładunek %dcm)%s",
                getTotalHeightCm(),
                getTrailerHeightCm(),
                cargo.getHeightCm(),
                trailerInfo);
    }

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * NOWA METODA - Szczegółowy opis wagi zestawu
     * ═══════════════════════════════════════════════════════════════════════════
     */
    public String getWeightBreakdown() {
        if (Boolean.TRUE.equals(forceSelfDriving) ||
                (!shouldBeOnTrailer() && Boolean.TRUE.equals(cargo.getCanDriveAlone()))) {
            return String.format("Masa pojazdu: %dkg (jedzie samodzielnie)", getTotalWeightKg());
        }

        int tractorWeight = (int) (transporter.getTotalWeightKg() * 0.4);
        int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;
        int trailerWeight;
        String trailerInfo;

        if (trailer != null && trailer.getEmptyWeight() != null) {
            trailerWeight = trailer.getEmptyWeight();
            trailerInfo = String.format(" [%s]", trailer.getRegistrationNumber());
        } else {
            trailerWeight = estimateSemiTrailerWeight(cargoWeight);
            trailerInfo = " [estymacja]";
        }

        int equipmentWeight = estimateMilitaryEquipment(cargo.getModel());

        return String.format("Masa całkowita: %dkg (ciągnik ~%dkg + naczepa %dkg%s + ładunek %dkg + wyposażenie %dkg)",
                getTotalWeightKg(),
                tractorWeight,
                trailerWeight,
                trailerInfo,
                cargoWeight,
                equipmentWeight);
    }

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * NOWA METODA - Szczegółowy opis osi zestawu
     * ═══════════════════════════════════════════════════════════════════════════
     */
    public String getAxleBreakdown() {
        int truckAxles = 3;
        int trailerAxles;
        String trailerInfo;

        if (trailer != null && trailer.getNumberOfAxles() != null) {
            trailerAxles = trailer.getNumberOfAxles();
            trailerInfo = String.format(" [%s]", trailer.getRegistrationNumber());
        } else {
            int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;
            if (cargoWeight > 50000) {
                trailerAxles = 4;
            } else if (cargoWeight > 30000) {
                trailerAxles = 3;
            } else {
                trailerAxles = 2;
            }
            trailerInfo = " [estymacja]";
        }

        return String.format("Osie: %d (ciągnik %d + naczepa %d%s), Max nacisk: %dkg/oś",
                truckAxles + trailerAxles,
                truckAxles,
                trailerAxles,
                trailerInfo,
                getMaxAxleLoadKg());
    }
}