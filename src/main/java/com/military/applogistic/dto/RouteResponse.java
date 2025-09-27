package com.military.applogistic.dto;

import lombok.Data;
import com.military.applogistic.entity.Route;
import java.util.List;

@Data
public class RouteResponse {
    private Long id;
    private String startAddress;
    private String endAddress;
    private String status;
    private Double totalDistanceKm;
    private Integer estimatedTimeMinutes;
    private String assignedDriver;
    private boolean hasRestrictions;
    private List<String> warnings;

    public static RouteResponse from(Route route) {
        RouteResponse response = new RouteResponse();
        response.setId(route.getId());
        response.setStartAddress(route.getStartAddress());
        response.setEndAddress(route.getEndAddress());
        response.setStatus(route.getStatus().toString());
        response.setTotalDistanceKm(route.getTotalDistanceKm());
        response.setEstimatedTimeMinutes(route.getEstimatedTimeMinutes());
        response.setAssignedDriver(route.getAssignedDriverUsername());
        return response;
    }
}