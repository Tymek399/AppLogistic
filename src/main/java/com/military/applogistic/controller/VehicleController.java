package com.military.applogistic.controller;

import com.military.applogistic.dto.CreateTransportSetRequest;
import com.military.applogistic.entity.Vehicle;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.repository.VehicleRepository;
import com.military.applogistic.repository.TransportSetRepository;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import lombok.RequiredArgsConstructor;
import java.util.List;

@RestController
@RequestMapping("/api/vehicles")
@RequiredArgsConstructor
public class VehicleController {

    private final VehicleRepository vehicleRepository;
    private final TransportSetRepository transportSetRepository;

    @GetMapping("/transporters")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<List<Vehicle>> getTransporters() {
        List<Vehicle> transporters = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.TRANSPORTER, true);
        return ResponseEntity.ok(transporters);
    }

    @GetMapping("/cargo")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<List<Vehicle>> getCargo() {
        List<Vehicle> cargo = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.CARGO, true);
        return ResponseEntity.ok(cargo);
    }
    @PostMapping ("/cargo")
    public ResponseEntity<Vehicle> createCargo(@RequestBody CreateTransportSetRequest createTransportSetRequest) {
        Vehicle vehicle = new Vehicle();
        return ResponseEntity.ok(vehicleRepository.save(vehicle));
    }

    @GetMapping("/transport-sets")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<List<TransportSet>> getTransportSets() {
        List<TransportSet> sets = transportSetRepository.findAll();
        return ResponseEntity.ok(sets);
    }

    @PostMapping("/transport-sets")
    @PreAuthorize("hasRole('OPERATOR')")
    public ResponseEntity<TransportSet> createTransportSet(@RequestBody CreateTransportSetRequest request) {
        Vehicle transporter = vehicleRepository.findById(request.getTransporterId())
                .orElseThrow(() -> new RuntimeException("Transporter not found"));
        Vehicle cargo = vehicleRepository.findById(request.getCargoId())
                .orElseThrow(() -> new RuntimeException("Cargo not found"));

        TransportSet transportSet = new TransportSet();
        transportSet.setTransporter(transporter);
        transportSet.setCargo(cargo);
        transportSet.setDescription(request.getDescription());

        // Calculate combined parameters
        transportSet.setTotalHeightCm(Math.max(transporter.getHeightCm(), cargo.getHeightCm()));
        transportSet.setTotalWeightKg(transporter.getTotalWeightKg() + cargo.getTotalWeightKg());
        transportSet.setMaxAxleLoadKg(Math.max(transporter.getMaxAxleLoadKg(), cargo.getMaxAxleLoadKg()));

        TransportSet saved = transportSetRepository.save(transportSet);
        return ResponseEntity.ok(saved);
    }
}
