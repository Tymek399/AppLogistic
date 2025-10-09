package com.military.applogistic.controller;

import com.military.applogistic.dto.request.CreateTransportSetRequest;
import com.military.applogistic.dto.request.CreateVehicleRequest;
import com.military.applogistic.entity.Vehicle;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.repository.VehicleRepository;
import com.military.applogistic.repository.TransportSetRepository;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.util.List;

@RestController
@RequestMapping("/api/vehicles")
@RequiredArgsConstructor
@Slf4j
public class VehicleController {

    private final VehicleRepository vehicleRepository;
    private final TransportSetRepository transportSetRepository;

    @GetMapping("/transporters")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<Vehicle>> getTransporters() {
        List<Vehicle> transporters = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.TRANSPORTER, true);
        return ResponseEntity.ok(transporters);
    }

    @GetMapping("/cargo")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<Vehicle>> getCargo() {
        List<Vehicle> cargo = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.CARGO, true);
        return ResponseEntity.ok(cargo);
    }

    @PostMapping("/transporters")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Vehicle> createTransporter(@RequestBody CreateVehicleRequest request) {
        log.info("Creating new transporter: {}", request.getModel());

        Vehicle vehicle = new Vehicle();
        vehicle.setModel(request.getModel());
        vehicle.setType(Vehicle.VehicleType.TRANSPORTER);
        vehicle.setTotalWeightKg(request.getTotalWeightKg());
        vehicle.setHeightCm(request.getHeightCm());
        vehicle.setMaxAxleLoadKg(request.getMaxAxleLoadKg());
        vehicle.setActive(true);
        vehicle.setCanDriveAlone(false);
        vehicle.setVehicleCategory("TRUCK");

        Vehicle saved = vehicleRepository.save(vehicle);
        log.info("Transporter created with ID: {}", saved.getId());

        return ResponseEntity.ok(saved);
    }

    @PostMapping("/cargo")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Vehicle> createCargo(@RequestBody CreateVehicleRequest request) {
        log.info("Creating new cargo: {}", request.getModel());

        Vehicle vehicle = new Vehicle();
        vehicle.setModel(request.getModel());
        vehicle.setType(Vehicle.VehicleType.CARGO);
        vehicle.setTotalWeightKg(request.getTotalWeightKg());
        vehicle.setHeightCm(request.getHeightCm());
        vehicle.setMaxAxleLoadKg(request.getMaxAxleLoadKg());
        vehicle.setActive(true);

        // Domyślnie: jeśli ≤5t to może jechać samo
        vehicle.setCanDriveAlone(request.getTotalWeightKg() != null && request.getTotalWeightKg() <= 5000);
        vehicle.setVehicleCategory(vehicle.getCanDriveAlone() ? "STANDALONE" : "MILITARY_VEHICLE");

        Vehicle saved = vehicleRepository.save(vehicle);
        log.info("Cargo created with ID: {} (canDriveAlone: {})", saved.getId(), saved.getCanDriveAlone());

        return ResponseEntity.ok(saved);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Void> deleteVehicle(@PathVariable Long id) {
        log.info("Deleting vehicle: {}", id);

        Vehicle vehicle = vehicleRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Vehicle not found"));

        vehicle.setActive(false);
        vehicleRepository.save(vehicle);

        log.info("Vehicle {} deactivated", id);
        return ResponseEntity.ok().build();
    }

    @GetMapping("/transport-sets")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<TransportSet>> getTransportSets() {
        List<TransportSet> sets = transportSetRepository.findAll();
        return ResponseEntity.ok(sets);
    }

    @PostMapping("/transport-sets")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<TransportSet> createTransportSet(@RequestBody CreateTransportSetRequest request) {
        log.info("Creating transport set: transporter={}, cargo={}, mode={}",
                request.getTransporterId(), request.getCargoId(), request.getTransportMode());

        Vehicle transporter = vehicleRepository.findById(request.getTransporterId())
                .orElseThrow(() -> new RuntimeException("Transporter not found"));
        Vehicle cargo = vehicleRepository.findById(request.getCargoId())
                .orElseThrow(() -> new RuntimeException("Cargo not found"));

        TransportSet transportSet = new TransportSet();
        transportSet.setTransporter(transporter);
        transportSet.setCargo(cargo);
        transportSet.setDescription(request.getDescription());

        // Ustawienie wymuszenia jazdy na własnych kołach
        if (request.isForceSelfDriving()) {
            if (Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
                transportSet.setForceSelfDriving(true);
                log.info("Operator wymusił jazdę na własnych kołach dla: {}", cargo.getModel());
            } else {
                log.warn("Pojazd {} nie może jechać sam - ignoruję wybór operatora", cargo.getModel());
                transportSet.setForceSelfDriving(false);
            }
        }

        transportSet.calculateTransportParameters();

        TransportSet saved = transportSetRepository.save(transportSet);
        log.info("Transport set created with ID: {} (type: {})",
                saved.getId(), saved.getTrailerType());

        return ResponseEntity.ok(saved);
    }

    @DeleteMapping("/transport-sets/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Void> deleteTransportSet(@PathVariable Long id) {
        log.info("Deleting transport set: {}", id);
        transportSetRepository.deleteById(id);
        return ResponseEntity.ok().build();
    }
}