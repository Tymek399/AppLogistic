package com.military.applogistic.mapper;

import com.military.applogistic.dto.request.CreateVehicleRequest;
import com.military.applogistic.entity.Vehicle;
import org.springframework.stereotype.Component;

@Component
public class VehicleMapper {

    public Vehicle toTransporter(CreateVehicleRequest request) {
        Vehicle vehicle = new Vehicle();
        vehicle.setReferenceNumber(request.getReferenceNumber());
        vehicle.setModel(request.getModel());
        vehicle.setType(Vehicle.VehicleType.TRANSPORTER);
        vehicle.setTotalWeightKg(request.getTotalWeightKg());
        vehicle.setHeightCm(request.getHeightCm());
        vehicle.setMaxAxleLoadKg(request.getMaxAxleLoadKg());
        vehicle.setActive(true);
        vehicle.setCanDriveAlone(false);
        vehicle.setVehicleCategory("TRUCK");
        return vehicle;
    }

    public Vehicle toCargo(CreateVehicleRequest request) {
        Vehicle vehicle = new Vehicle();
        vehicle.setReferenceNumber(request.getReferenceNumber());
        vehicle.setModel(request.getModel());
        vehicle.setType(Vehicle.VehicleType.CARGO);
        vehicle.setTotalWeightKg(request.getTotalWeightKg());
        vehicle.setHeightCm(request.getHeightCm());
        vehicle.setMaxAxleLoadKg(request.getMaxAxleLoadKg());
        vehicle.setActive(true);
        vehicle.setCanDriveAlone(request.getTotalWeightKg() != null && request.getTotalWeightKg() <= 5000);
        vehicle.setVehicleCategory(vehicle.getCanDriveAlone() ? "STANDALONE" : "MILITARY_VEHICLE");
        return vehicle;
    }
}