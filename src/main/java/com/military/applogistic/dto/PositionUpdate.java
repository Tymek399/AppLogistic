package com.military.applogistic.dto;

import lombok.Data;

@Data
public class PositionUpdate {
    private Double latitude;
    private Double longitude;
    private Double speedKmh;
    private Integer accuracy;
    private Integer batteryLevel;
    private String timestamp;
}
