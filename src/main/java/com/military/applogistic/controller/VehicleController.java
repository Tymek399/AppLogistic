package com.military.applogistic.controller;

import com.military.applogistic.dto.request.CreateTransportSetRequest;
import com.military.applogistic.dto.request.CreateVehicleRequest;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.entity.Vehicle;
import com.military.applogistic.service.VehicleService;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import lombok.RequiredArgsConstructor;

import java.util.List;

@RestController
@RequestMapping("/api/vehicles")
@RequiredArgsConstructor
public class VehicleController {

    private final VehicleService vehicleService;

    @GetMapping("/transporters")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<Vehicle>> getTransporters() {
        return ResponseEntity.ok(vehicleService.getTransporters());
    }

    @GetMapping("/cargo")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<Vehicle>> getCargo() {
        return ResponseEntity.ok(vehicleService.getCargo());
    }

    @PostMapping("/transporters")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Vehicle> createTransporter(@RequestBody CreateVehicleRequest request) {
        return ResponseEntity.ok(vehicleService.createTransporter(request));
    }

    @PostMapping("/cargo")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<Vehicle> createCargo(@RequestBody CreateVehicleRequest request) {
        return ResponseEntity.ok(vehicleService.createCargo(request));
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Void> deleteVehicle(@PathVariable Long id) {
        vehicleService.deleteVehicle(id);
        return ResponseEntity.ok().build();
    }

    @GetMapping("/transport-sets")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<List<TransportSet>> getTransportSets() {
        return ResponseEntity.ok(vehicleService.getTransportSets());
    }

    @PostMapping("/transport-sets")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<TransportSet> createTransportSet(@RequestBody CreateTransportSetRequest request) {
        return ResponseEntity.ok(vehicleService.createTransportSet(request));
    }

    @DeleteMapping("/transport-sets/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Void> deleteTransportSet(@PathVariable Long id) {
        vehicleService.deleteTransportSet(id);
        return ResponseEntity.ok().build();
    }
}