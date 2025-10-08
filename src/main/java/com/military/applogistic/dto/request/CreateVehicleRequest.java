package com.military.applogistic.dto.request;

import lombok.Data;

@Data
public class CreateVehicleRequest {
    private String referenceNumber;
    private String model;
    private Integer totalWeightKg;
    private Integer heightCm;
    private Integer maxAxleLoadKg;
}