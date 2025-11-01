package com.military.applogistic.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Entity
@Table(name = "vehicles")
@Data
public class Vehicle {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    @NotNull
    private String model;

    @Enumerated(EnumType.STRING)
    private VehicleType type;

    private Integer totalWeightKg;
    private Integer heightCm;
    private Integer maxAxleLoadKg;
    private Boolean active = true;


    private Boolean canDriveAlone = false;
    private String vehicleCategory;

    public enum VehicleType {
        TRANSPORTER, CARGO
    }
}