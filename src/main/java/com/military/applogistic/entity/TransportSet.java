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

    private String description;

    private Integer totalHeightCm;
    private Integer totalWeightKg;
    private Integer maxAxleLoadKg;
    private Integer totalLengthCm;
    private Integer totalWidthCm;
    private Integer trailerHeightCm;

    public void calculateTransportParameters() {
        if (transporter == null || cargo == null) {
            return;
        }

        // ✅ NOWA LOGIKA: Rozróżnienie między "może jechać sam" a "jedzie na lawecie"
        boolean isOnTrailer = shouldBeOnTrailer();

        if (!isOnTrailer && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            // STANDALONE: Lekki pojazd jedzie sam (Humvee, Sprinter)
            this.totalWeightKg = cargo.getTotalWeightKg();
            this.totalHeightCm = cargo.getHeightCm();
            this.totalLengthCm = estimateStandaloneLength();
            this.totalWidthCm = estimateCargoWidth();
            this.maxAxleLoadKg = cargo.getMaxAxleLoadKg();
            this.trailerHeightCm = 0;
            return;
        }

        // ZESTAW Z NACZEPĄ (ciężkie pojazdy lub transport długodystansowy)
        this.trailerHeightCm = estimateTrailerHeight(cargo.getTotalWeightKg());
        this.totalHeightCm = this.trailerHeightCm + (cargo.getHeightCm() != null ? cargo.getHeightCm() : 0);

        // Składowe masy
        int tractorWeight = (int) (transporter.getTotalWeightKg() * 0.4); // Pusty ciągnik
        int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;
        int semiTrailerWeight = estimateSemiTrailerWeight(cargoWeight);

        // ✅ POPRAWKA: Dodaj wagę ekwipunku bojowego dla pojazdów wojskowych
        int militaryEquipmentWeight = estimateMilitaryEquipment(cargo.getModel());

        this.totalWeightKg = tractorWeight + semiTrailerWeight + cargoWeight + militaryEquipmentWeight;

        this.maxAxleLoadKg = calculateMaxAxleLoad(this.totalWeightKg, getAxleCount());
        this.totalLengthCm = estimateTotalLength();
        this.totalWidthCm = Math.max(estimateTransporterWidth(), estimateCargoWidth());
    }

    /**
     * ✅ NOWA METODA: Czy pojazd powinien jechać na lawecie?
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

    /**
     * ✅ NOWA METODA: Szacuj wagę ekwipunku bojowego
     */
    private int estimateMilitaryEquipment(String model) {
        if (model == null) return 0;

        String m = model.toLowerCase();

        if (m.contains("rosomak")) {
            // Załoga (8-10 osób) + amunicja + prowiant + paliwo dodatkowe
            return 3000; // ~3t
        }

        if (m.contains("krab")) {
            // Załoga + amunicja artyleryjska
            return 5000; // ~5t
        }

        if (m.contains("leopard")) {
            // Załoga + amunicja pancerna
            return 8000; // ~8t (czołg bojowy)
        }

        return 0; // Pojazdy cywilne
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

    private int getAxleCount() {
        int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;
        int truckAxles = 3; // Typowo 6x4 ciągnik
        int trailerAxles;

        if (cargoWeight > 50000) {
            trailerAxles = 4; // 4-osiowa dla czołgów
        } else if (cargoWeight > 30000) {
            trailerAxles = 3; // 3-osiowa
        } else {
            trailerAxles = 2; // Standardowa
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

    public String getTrailerType() {
        if (!shouldBeOnTrailer() && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            return "Pojazd samojezdny (bez naczepy)";
        }
        if (isLowLoader()) {
            return "Naczepa niskopodwoziowa";
        }
        if (cargo.getTotalWeightKg() > 30000) {
            return "Naczepa wzmocniona";
        }
        return "Naczepa standardowa";
    }

    public String getHeightBreakdown() {
        if (!shouldBeOnTrailer() && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            return String.format("Wysokość pojazdu: %dcm (jedzie samodzielnie)", getTotalHeightCm());
        }
        return String.format("Wysokość całkowita: %dcm (naczepa %dcm + ładunek %dcm)",
                getTotalHeightCm(),
                getTrailerHeightCm(),
                cargo.getHeightCm());
    }
}