package com.military.applogistic.service;

import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.entity.Vehicle;
import org.springframework.stereotype.Component;

@Component
public class TransportSetCalculator {

    public void calculateTransportParameters(TransportSet transportSet) {
        Vehicle transporter = transportSet.getTransporter();
        Vehicle cargo = transportSet.getCargo();
        if (transporter == null || cargo == null) {
            return;
        }

        // ✅ PRIORYTET 1: Jeśli operator wymusił self-driving
        if (Boolean.TRUE.equals(transportSet.getForceSelfDriving()) && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            calculateSelfDrivingParameters(transportSet);
            return;
        }

        // ✅ PRIORYTET 2: Automatyczna decyzja czy pojazd powinien jechać na lawecie
        boolean isOnTrailer = shouldBeOnTrailer(cargo);

        if (!isOnTrailer && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            calculateSelfDrivingParameters(transportSet);
        } else {
            calculateTrailerParameters(transportSet);
        }
    }

    private void calculateSelfDrivingParameters(TransportSet transportSet) {
        Vehicle cargo = transportSet.getCargo();
        // STANDALONE: Lekki pojazd jedzie sam (Humvee, Sprinter)
        transportSet.setTotalWeightKg(cargo.getTotalWeightKg());
        transportSet.setTotalHeightCm(cargo.getHeightCm());
        transportSet.setTotalLengthCm(estimateStandaloneLength(cargo));
        transportSet.setTotalWidthCm(estimateCargoWidth(cargo));
        transportSet.setMaxAxleLoadKg(cargo.getMaxAxleLoadKg());
        transportSet.setTrailerHeightCm(0);
    }

    private void calculateTrailerParameters(TransportSet transportSet) {
        Vehicle transporter = transportSet.getTransporter();
        Vehicle cargo = transportSet.getCargo();
        // ZESTAW Z NACZEPĄ (ciężkie pojazdy lub transport długodystansowy)
        Integer cargoWeight = cargo.getTotalWeightKg();
        int trailerHeightCm = estimateTrailerHeight(cargoWeight);
        transportSet.setTrailerHeightCm(trailerHeightCm);
        transportSet.setTotalHeightCm(trailerHeightCm + (cargo.getHeightCm() != null ? cargo.getHeightCm() : 0));

        // Składowe masy
        int tractorWeight = (int) (transporter.getTotalWeightKg() * 0.4); // Pusty ciągnik
        int cargoWeightVal = cargoWeight != null ? cargoWeight : 0;
        int semiTrailerWeight = estimateSemiTrailerWeight(cargoWeightVal);
        int militaryEquipmentWeight = estimateMilitaryEquipment(cargo.getModel());

        transportSet.setTotalWeightKg(tractorWeight + semiTrailerWeight + cargoWeightVal + militaryEquipmentWeight);
        transportSet.setMaxAxleLoadKg(calculateMaxAxleLoad(transportSet.getTotalWeightKg(), getAxleCount(cargo)));
        transportSet.setTotalLengthCm(estimateTotalLength(transporter, cargo));
        transportSet.setTotalWidthCm(Math.max(estimateTransporterWidth(transporter), estimateCargoWidth(cargo)));
    }

    private boolean shouldBeOnTrailer(Vehicle cargo) {
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

    private int estimateStandaloneLength(Vehicle cargo) {
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

    public int estimateSemiTrailerWeight(int cargoWeight) {
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

    private int getAxleCount(Vehicle cargo) {
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

    private int estimateTotalLength(Vehicle transporter, Vehicle cargo) {
        int truckLength = 650; // Ciągnik 6x4
        String cargoModel = cargo.getModel().toLowerCase();
        int cargoLength = 1350;

        if (cargoModel.contains("leopard")) cargoLength = 1200;
        else if (cargoModel.contains("krab")) cargoLength = 1400;
        else if (cargoModel.contains("rosomak")) cargoLength = 1000;

        return truckLength + cargoLength;
    }

    private int estimateTransporterWidth(Vehicle transporter) {
        return 255; // 2.55m
    }

    private int estimateCargoWidth(Vehicle cargo) {
        String model = cargo.getModel().toLowerCase();
        if (model.contains("leopard")) return 370;
        if (model.contains("rosomak")) return 290;
        if (model.contains("krab")) return 340;
        if (model.contains("radar")) return 320;
        if (model.contains("humvee")) return 220;
        if (model.contains("sprinter")) return 200;
        return 255;
    }

    public boolean isLowLoader(TransportSet transportSet) {
        return isLowLoader(transportSet.getCargo());
    }

    private boolean isLowLoader(Vehicle cargo) {
        String cargoModel = cargo.getModel().toLowerCase();
        return cargoModel.contains("leopard") ||
                cargoModel.contains("tank") ||
                (cargo.getTotalWeightKg() != null && cargo.getTotalWeightKg() > 45000);
    }

    public String getTrailerType(TransportSet transportSet) {
        Vehicle cargo = transportSet.getCargo();
        if (Boolean.TRUE.equals(transportSet.getForceSelfDriving())) {
            return "Pojazd jedzie na własnych kołach (wymuszony)";
        }

        if (!shouldBeOnTrailer(cargo) && Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
            return "Pojazd samojezdny (bez naczepy)";
        }
        if (isLowLoader(cargo)) {
            return "Naczepa niskopodwoziowa";
        }
        if (cargo.getTotalWeightKg() != null && cargo.getTotalWeightKg() > 30000) {
            return "Naczepa wzmocniona";
        }
        return "Naczepa standardowa";
    }

    public String getHeightBreakdown(TransportSet transportSet) {
        Vehicle cargo = transportSet.getCargo();
        if (Boolean.TRUE.equals(transportSet.getForceSelfDriving()) ||
                (!shouldBeOnTrailer(cargo) && Boolean.TRUE.equals(cargo.getCanDriveAlone()))) {
            return String.format("Wysokość pojazdu: %dcm (jedzie samodzielnie)", transportSet.getTotalHeightCm());
        }
        return String.format("Wysokość całkowita: %dcm (naczepa %dcm + ładunek %dcm)",
                transportSet.getTotalHeightCm(),
                transportSet.getTrailerHeightCm(),
                cargo.getHeightCm());
    }
}