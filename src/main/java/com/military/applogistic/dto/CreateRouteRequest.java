package com.military.applogistic.dto;

import lombok.Data;

@Data
public class CreateRouteRequest {
    private String startAddress;
    private String endAddress;
    private Double startLatitude;
    private Double startLongitude;
    private Double endLatitude;
    private Double endLongitude;
    private Long transportSetId;
}
