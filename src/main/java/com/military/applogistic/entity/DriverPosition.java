package com.military.applogistic.entity;

import jakarta.persistence.*;
import lombok.Data;
import java.time.LocalDateTime;

@Entity
@Table(name = "driver_positions")
@Data
public class DriverPosition {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "route_id")
    private Route route;

    private String driverUsername;
    private Double latitude;
    private Double longitude;
    private Double speedKmh;
    private Integer accuracy; // GPS accuracy in meters
    private Integer batteryLevel;
    private Boolean isOnline = true;
    private LocalDateTime lastUpdateTime = LocalDateTime.now();
}
