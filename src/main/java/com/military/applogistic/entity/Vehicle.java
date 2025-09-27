package com.military.applogistic.entity;

import jakarta.persistence.*;
import lombok.Data;

@Entity
@Table(name = "vehicles")
@Data
public class Vehicle {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String model;

    @Enumerated(EnumType.STRING)
    private VehicleType type;

    private Integer totalWeightKg;
    private Integer heightCm;
    private Integer maxAxleLoadKg;
    private Boolean active = true;

    public enum VehicleType {
        TRANSPORTER, CARGO
    }
}
