package com.military.applogistic.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * DTO dla odpowiedzi zawierającej informacje o trasie
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RouteResponse {

    private Long id;

    private String startAddress;

    private String endAddress;

    private String status;

    private DistanceInfo distance;

    private TimeInfo time;

    private String assignedDriver;

    private boolean hasRestrictions;

    private List<RouteWarning> warnings;

    /**
     * Informacje o dystansie
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DistanceInfo {
        private Double kilometers;
        private Double meters;
        private Double miles;

        public static DistanceInfo fromKilometers(Double km) {
            if (km == null) return null;
            return DistanceInfo.builder()
                    .kilometers(km)
                    .meters(km * 1000)
                    .miles(km * 0.621371)
                    .build();
        }
    }

    /**
     * Informacje o czasie
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class TimeInfo {
        private Integer totalMinutes;
        private Integer hours;
        private Integer minutes;

        public static TimeInfo fromMinutes(Integer totalMin) {
            if (totalMin == null) return null;
            return TimeInfo.builder()
                    .totalMinutes(totalMin)
                    .hours(totalMin / 60)
                    .minutes(totalMin % 60)
                    .build();
        }
    }

    /**
     * Ostrzeżenie dotyczące trasy
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RouteWarning {
        private String description;
        private WarningSeverity severity;
        private String location;

        public enum WarningSeverity {
            INFO, WARNING, CRITICAL
        }
    }

    // ==================== LEGACY SUPPORT ====================
    // Dla zachowania wstecznej kompatybilności

    public Double getTotalDistanceKm() {
        return distance != null ? distance.getKilometers() : null;
    }

    public void setTotalDistanceKm(Double km) {
        this.distance = DistanceInfo.fromKilometers(km);
    }

    public Integer getEstimatedTimeMinutes() {
        return time != null ? time.getTotalMinutes() : null;
    }

    public void setEstimatedTimeMinutes(Integer minutes) {
        this.time = TimeInfo.fromMinutes(minutes);
    }
}