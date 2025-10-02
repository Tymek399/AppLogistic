package com.military.applogistic.dto.request;

import lombok.Data;

@Data
public class CreateVehicleRequest {
    private String model;
    private Integer totalWeightKg;
    private Integer heightCm;
    private Integer maxAxleLoadKg;
    private Boolean canDriveAlone; // ✅ Nowe pole opcjonalne
    private String vehicleCategory; // ✅ Nowe pole opcjonalne
}