package com.military.applogistic.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;


@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GpxRouteData {

    private List<GpxPoint> points;
    private Double totalDistanceKm;
    private Integer estimatedTimeMinutes;
    private String routeName;
    private String routeDescription;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class GpxPoint {
        private Double latitude;
        private Double longitude;
        private Double elevation;
        private Integer sequenceNumber;

        public boolean isValid() {
            return latitude != null && longitude != null &&
                    latitude >= -90 && latitude <= 90 &&
                    longitude >= -180 && longitude <= 180;
        }
    }

    public boolean isValid() {
        return points != null && !points.isEmpty() &&
                points.stream().allMatch(GpxPoint::isValid);
    }

    public List<GpxPoint> getPoints() {
        if (points == null) {
            points = new ArrayList<>();
        }
        return points;
    }
}