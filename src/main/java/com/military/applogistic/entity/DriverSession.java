package com.military.applogistic.entity;


import jakarta.persistence.*;
import lombok.Data;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "driver_sessions")
@Data
public class DriverSession {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String driverUsername;

    @ManyToOne
    @JoinColumn(name = "route_id")
    private Route activeRoute;

    private String sessionToken = UUID.randomUUID().toString();
    private LocalDateTime loginTime = LocalDateTime.now();
    private LocalDateTime lastActivityTime = LocalDateTime.now();
    private Boolean isActive = true;
}