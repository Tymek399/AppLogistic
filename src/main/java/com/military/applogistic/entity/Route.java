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

    @Lob
    private String rejectedPointsJson; // NOWE - JSON z odrzuconymi punktami

    private Double totalDistanceKm;
    private Integer estimatedTimeMinutes;

    @ManyToOne
    @JoinColumn(name = "transport_set_id")
    private TransportSet transportSet;

    private String assignedDriverUsername;
    private String createdByUsername;

    @Enumerated(EnumType.STRING)
    private RouteStatus status = RouteStatus.CREATED;

    // NOWE POLA
    private Boolean isDraft = false; // czy trasa jest wersją roboczą
    private Boolean hasValidationProblems = false; // czy ma problemy z walidacją
    private Boolean operatorAccepted = false; // czy operator zaakceptował mimo problemów
    private String operatorAcceptedBy; // kto zaakceptował
    private LocalDateTime operatorAcceptedAt; // kiedy zaakceptowano
    private String operatorComment; // komentarz operatora przy akceptacji

    private LocalDateTime createdAt = LocalDateTime.now();
    private LocalDateTime assignedAt;
    private LocalDateTime startedAt;
    private LocalDateTime completedAt;

    public enum RouteStatus {
        CREATED,
        DRAFT,
        VALIDATION_REQUIRED,
        REVALIDATING,         // ✅ DODAJ TĘ LINIĘ
        ASSIGNED,
        IN_PROGRESS,
        ACTIVE,
        COMPLETED
    }
}