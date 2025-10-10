package com.military.applogistic.entity;

import jakarta.persistence.*;
import lombok.Data;
import java.time.LocalDateTime;

@Entity
@Table(name = "routes")
@Data
public class Route {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String startAddress;
    private String endAddress;
    private Double startLatitude;
    private Double startLongitude;
    private Double endLatitude;
    private Double endLongitude;

    @Lob
    private String routeDataJson; // Google Maps response

    private Double totalDistanceKm;
    private Integer estimatedTimeMinutes;

    @ManyToOne
    @JoinColumn(name = "transport_set_id")
    private TransportSet transportSet;

    private String assignedDriverUsername;
    private String createdByUsername;

    @Enumerated(EnumType.STRING)
    private RouteStatus status = RouteStatus.CREATED;

    private LocalDateTime createdAt = LocalDateTime.now();
    private LocalDateTime assignedAt;
    private LocalDateTime startedAt;
    private LocalDateTime completedAt;

    public enum RouteStatus {
        CREATED, ASSIGNED,IN_PROGRESS, ACTIVE, COMPLETED
    }
}