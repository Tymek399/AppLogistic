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

        // ✅ STANDALONE: Pojazd jedzie sam bez naczepy
        if (Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            this.totalWeightKg = cargo.getTotalWeightKg();
            this.totalHeightCm = cargo.getHeightCm();
            this.totalLengthCm = estimateStandaloneLength();
            this.totalWidthCm = estimateCargoWidth();
            this.maxAxleLoadKg = cargo.getMaxAxleLoadKg();
            this.trailerHeightCm = 0; // Brak naczepy
            return;
        }

        // STANDARDOWY ZESTAW Z NACZEPĄ
        this.trailerHeightCm = estimateTrailerHeight(cargo.getTotalWeightKg());
        this.totalHeightCm = this.trailerHeightCm + (cargo.getHeightCm() != null ? cargo.getHeightCm() : 0);

        int transporterWeight = transporter.getTotalWeightKg() != null ? transporter.getTotalWeightKg() : 0;
        int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;
        int semiTrailerWeight = estimateSemiTrailerWeight(cargoWeight);
        this.totalWeightKg = transporterWeight + semiTrailerWeight + cargoWeight;

        this.maxAxleLoadKg = calculateMaxAxleLoad(this.totalWeightKg, getAxleCount());
        this.totalLengthCm = estimateTotalLength();
        this.totalWidthCm = Math.max(estimateTransporterWidth(), estimateCargoWidth());
    }

    private int estimateStandaloneLength() {
        String model = cargo.getModel().toLowerCase();
        if (model.contains("rosomak")) return 730;
        if (model.contains("leopard")) return 1020;
        if (model.contains("krab")) return 920;
        return 600; // domyślnie 6m
    }

    private int estimateTrailerHeight(Integer cargoWeight) {
        if (cargoWeight == null) return 120;
        if (cargoWeight > 45000) return 80;
        if (cargoWeight > 25000) return 120;
        return 140;
    }

    private int estimateSemiTrailerWeight(int cargoWeight) {
        if (cargoWeight > 50000) return 15000;
        if (cargoWeight > 30000) return 12000;
        if (cargoWeight > 15000) return 10000;
        return 8000;
    }

    private int calculateMaxAxleLoad(int totalWeight, int axleCount) {
        if (axleCount == 0) return 0;
        int baseAxleLoad = totalWeight / axleCount;
        return (int) (baseAxleLoad * 1.2);
    }

    private int getAxleCount() {
        int cargoWeight = cargo.getTotalWeightKg() != null ? cargo.getTotalWeightKg() : 0;
        int truckAxles = 3;
        int trailerAxles;

        if (cargoWeight > 50000) {
            trailerAxles = 4;
        } else if (cargoWeight > 30000) {
            trailerAxles = 3;
        } else {
            trailerAxles = 2;
        }

        return truckAxles + trailerAxles;
    }

    private int estimateTotalLength() {
        int truckLength = 650;
        String cargoModel = cargo.getModel().toLowerCase();
        int cargoLength = 1350;

        if (cargoModel.contains("leopard")) cargoLength = 1200;
        else if (cargoModel.contains("krab")) cargoLength = 1400;
        else if (cargoModel.contains("rosomak")) cargoLength = 1000;

        return truckLength + cargoLength;
    }

    private int estimateTransporterWidth() {
        return 255;
    }

    private int estimateCargoWidth() {
        String model = cargo.getModel().toLowerCase();
        if (model.contains("leopard")) return 370;
        if (model.contains("rosomak")) return 290;
        if (model.contains("krab")) return 340;
        if (model.contains("radar")) return 320;
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
        if (Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
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
        if (Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            return String.format("Wysokość pojazdu: %dcm (jedzie samodzielnie)", getTotalHeightCm());
        }
        return String.format("Wysokość całkowita: %dcm (naczepa %dcm + ładunek %dcm)",
                getTotalHeightCm(),
                getTrailerHeightCm(),
                cargo.getHeightCm());
    }
}