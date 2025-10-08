package com.military.applogistic.entity;

import jakarta.persistence.*;
import lombok.*;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;

/**
 * Encja reprezentująca trasę transportową
 */
@Entity
@Table(name = "routes")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EntityListeners(AuditingEntityListener.class)
public class Route {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String startAddress;

    @Column(nullable = false)
    private String endAddress;

    @Embedded
    @AttributeOverrides({
            @AttributeOverride(name = "latitude", column = @Column(name = "start_latitude")),
            @AttributeOverride(name = "longitude", column = @Column(name = "start_longitude"))
    })
    private Location startLocation;

    @Embedded
    @AttributeOverrides({
            @AttributeOverride(name = "latitude", column = @Column(name = "end_latitude")),
            @AttributeOverride(name = "longitude", column = @Column(name = "end_longitude"))
    })
    private Location endLocation;

    @Lob
    @Column(name = "route_data_json")
    private String routeDataJson;

    @Column(name = "total_distance_km")
    private Double totalDistanceKm;

    @Column(name = "estimated_time_minutes")
    private Integer estimatedTimeMinutes;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "transport_set_id", nullable = false)
    private TransportSet transportSet;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "assigned_driver_id")
    private User assignedDriver;

    @Column(name = "assigned_driver_username")
    private String assignedDriverUsername;

    @CreatedBy
    @Column(name = "created_by_username", updatable = false)
    private String createdByUsername;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    @Builder.Default
    private RouteStatus status = RouteStatus.CREATED;

    @CreatedDate
    @Column(name = "created_at", updatable = false)
    private LocalDateTime createdAt;

    @LastModifiedDate
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @Column(name = "assigned_at")
    private LocalDateTime assignedAt;

    @Column(name = "started_at")
    private LocalDateTime startedAt;

    @Column(name = "completed_at")
    private LocalDateTime completedAt;

    // ==================== LEGACY COMPATIBILITY ====================

    public Double getStartLatitude() {
        return startLocation != null ? startLocation.getLatitude() : null;
    }

    public void setStartLatitude(Double latitude) {
        if (this.startLocation == null) {
            this.startLocation = new Location();
        }
        this.startLocation.setLatitude(latitude);
    }

    public Double getStartLongitude() {
        return startLocation != null ? startLocation.getLongitude() : null;
    }

    public void setStartLongitude(Double longitude) {
        if (this.startLocation == null) {
            this.startLocation = new Location();
        }
        this.startLocation.setLongitude(longitude);
    }

    public Double getEndLatitude() {
        return endLocation != null ? endLocation.getLatitude() : null;
    }

    public void setEndLatitude(Double latitude) {
        if (this.endLocation == null) {
            this.endLocation = new Location();
        }
        this.endLocation.setLatitude(latitude);
    }

    public Double getEndLongitude() {
        return endLocation != null ? endLocation.getLongitude() : null;
    }

    public void setEndLongitude(Double longitude) {
        if (this.endLocation == null) {
            this.endLocation = new Location();
        }
        this.endLocation.setLongitude(longitude);
    }

    // ==================== BUSINESS METHODS ====================

    public boolean canBeModified() {
        return status == RouteStatus.CREATED;
    }

    public boolean isActive() {
        return status == RouteStatus.ACTIVE;
    }

    public boolean isCompleted() {
        return status == RouteStatus.COMPLETED;
    }
}