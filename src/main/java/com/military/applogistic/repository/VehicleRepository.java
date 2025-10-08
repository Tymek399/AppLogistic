package com.military.applogistic.repository;

import com.military.applogistic.entity.Vehicle;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleRepository extends JpaRepository<Vehicle, Long> {
    List<Vehicle> findByTypeAndActive(Vehicle.VehicleType type, boolean active);

    Optional<Vehicle> findByReferenceNumberAndActive(String referenceNumber, boolean active);
}