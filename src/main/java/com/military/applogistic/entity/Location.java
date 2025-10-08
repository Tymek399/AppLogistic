package com.military.applogistic.entity;

import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Reprezentuje lokalizację geograficzną (współrzędne GPS)
 */
@Embeddable
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Location {

    private Double latitude;

    private Double longitude;

    /**
     * Sprawdza czy lokalizacja jest poprawna
     */
    public boolean isValid() {
        return latitude != null && longitude != null &&
                latitude >= -90 && latitude <= 90 &&
                longitude >= -180 && longitude <= 180;
    }

    /**
     * Oblicza odległość do innej lokalizacji (w km)
     * Używa wzoru Haversine
     */
    public double distanceTo(Location other) {
        if (!this.isValid() || !other.isValid()) {
            throw new IllegalStateException("Invalid coordinates");
        }

        final double R = 6371; // Promień Ziemi w km

        double lat1Rad = Math.toRadians(this.latitude);
        double lat2Rad = Math.toRadians(other.latitude);
        double deltaLat = Math.toRadians(other.latitude - this.latitude);
        double deltaLon = Math.toRadians(other.longitude - this.longitude);

        double a = Math.sin(deltaLat / 2) * Math.sin(deltaLat / 2) +
                Math.cos(lat1Rad) * Math.cos(lat2Rad) *
                        Math.sin(deltaLon / 2) * Math.sin(deltaLon / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return R * c;
    }

    @Override
    public String toString() {
        return String.format("%.6f, %.6f", latitude, longitude);
    }
}