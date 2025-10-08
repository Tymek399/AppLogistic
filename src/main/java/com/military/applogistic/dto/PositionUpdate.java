package com.military.applogistic.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@Data
public class PositionUpdate {
    private Double latitude;
    private Double longitude;
    private Double speedKmh;
    private Double accuracy;
    private Double batteryLevel;
    private LocalDateTime timestamp;
    private String driverUsername;
}

