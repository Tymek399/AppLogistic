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

    //todo jak nazwa ma wiecej niz jeden wyraz to robimy adnocajre z podlaga
    //todo uzywaj referaceNumber
    @Column(name = "driver_username")
    private String driverUsername;

    //todo latniej bedzie stworzyc obiekt ktory bedzie reprezentowal lokalizacje niz dodawac wszystkie pola osobno
    private Double latitude;
    private Double longitude;
    private Double speedKmh;
    private Double accuracy; // GPS accuracy in meters
    private Double batteryLevel;
    private Boolean isOnline = true;
    private LocalDateTime lastUpdateTime = LocalDateTime.now();
}
