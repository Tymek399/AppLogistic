package com.military.applogistic.entity;

import jakarta.persistence.*;
import lombok.Data;

@Entity
@Table(name = "transport_sets")
@Data
public class TransportSet {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "transporter_id")
    private Vehicle transporter;

    @ManyToOne
    @JoinColumn(name = "cargo_id")
    private Vehicle cargo;

    private String description;

    // Calculated automatically
    private Integer totalHeightCm;
    private Integer totalWeightKg;
    private Integer maxAxleLoadKg;
}