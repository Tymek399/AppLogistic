package com.military.applogistic.dto;

import lombok.Builder;
import lombok.Data;
import java.time.LocalDateTime;

@Data
@Builder
public class LiveDriverInfo {
    private String driverUsername;
    private Long routeId;
    private String routeDescription;
    private Double latitude;
    private Double longitude;
    private Double speedKmh;
    private LocalDateTime lastUpdate;
    private Boolean isOnline;
}