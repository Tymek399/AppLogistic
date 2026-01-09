package com.military.applogistic.controller;

import com.military.applogistic.dto.request.CreateTransportSetRequest;
import com.military.applogistic.dto.request.CreateVehicleRequest;
import com.military.applogistic.entity.Vehicle;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.entity.Trailer;
import com.military.applogistic.repository.VehicleRepository;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.repository.TrailerRepository;
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
    // ═══════════════════════════════════════════════════════════════════════════
    // NOWE - Wstrzyknięcie TrailerRepository
    // ═══════════════════════════════════════════════════════════════════════════
    private final TrailerRepository trailerRepository;

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

    /**
     * ═══════════════════════════════════════════════════════════════════════════
     * ZMODYFIKOWANA METODA - Tworzenie zestawu transportowego z obsługą naczepy
     *
     * Teraz obsługuje:
     * - transporterId (ciągnik)
     * - cargoId (ładunek)
     * - trailerId (naczepa) - NOWE!
     * - transportMode / forceSelfDriving
     * ═══════════════════════════════════════════════════════════════════════════
     */
    @PostMapping("/transport-sets")
    @PreAuthorize("hasAnyRole('OPERATOR', 'ADMIN')")
    public ResponseEntity<TransportSet> createTransportSet(@RequestBody CreateTransportSetRequest request) {
        log.info("════════════════════════════════════════════════════════════");
        log.info("Creating transport set: transporter={}, cargo={}, trailer={}, mode={}",
                request.getTransporterId(),
                request.getCargoId(),
                request.getTrailerId(),
                request.getTransportMode());
        log.info("════════════════════════════════════════════════════════════");

        // Pobierz ciągnik
        Vehicle transporter = vehicleRepository.findById(request.getTransporterId())
                .orElseThrow(() -> new RuntimeException("Transporter not found"));

        // Pobierz ładunek
        Vehicle cargo = vehicleRepository.findById(request.getCargoId())
                .orElseThrow(() -> new RuntimeException("Cargo not found"));

        TransportSet transportSet = new TransportSet();
        transportSet.setTransporter(transporter);
        transportSet.setCargo(cargo);
        transportSet.setDescription(request.getDescription());

        // ═══════════════════════════════════════════════════════════════════════
        // NOWE - Obsługa naczepy
        // ═══════════════════════════════════════════════════════════════════════
        if (request.getTrailerId() != null && !request.isForceSelfDriving()) {
            Trailer trailer = trailerRepository.findById(request.getTrailerId())
                    .orElseThrow(() -> new RuntimeException("Trailer not found with ID: " + request.getTrailerId()));

            transportSet.setTrailer(trailer);
            log.info("✅ Przypisano naczepę: {} ({})",
                    trailer.getRegistrationNumber(),
                    trailer.getType());

            // Logowanie parametrów naczepy
            log.info("   Parametry naczepy:");
            log.info("   - Masa własna: {} kg", trailer.getEmptyWeight());
            log.info("   - Wysokość bez ładunku: {} m", trailer.getUnloadedHeight());
            log.info("   - Liczba osi: {}", trailer.getNumberOfAxles());
            log.info("   - Wymiary (DxSxW): {}m x {}m x {}m",
                    trailer.getLength(),
                    trailer.getWidth(),
                    trailer.getHeight());
            log.info("   - Max ładowność: {} kg", trailer.getMaxPayload());
        }

        // Ustawienie wymuszenia jazdy na własnych kołach
        if (request.isForceSelfDriving()) {
            if (Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
                transportSet.setForceSelfDriving(true);
                transportSet.setTrailer(null); // Brak naczepy przy self-driving
                log.info("✅ Operator wymusił jazdę na własnych kołach dla: {}", cargo.getModel());
            } else {
                log.warn("⚠️ Pojazd {} nie może jechać sam - ignoruję wybór operatora", cargo.getModel());
                transportSet.setForceSelfDriving(false);
            }
        }

        // ═══════════════════════════════════════════════════════════════════════
        // Oblicz parametry zestawu (z uwzględnieniem naczepy jeśli przypisana)
        // ═══════════════════════════════════════════════════════════════════════
        transportSet.calculateTransportParameters();

        // Logowanie obliczonych parametrów
        log.info("════════════════════════════════════════════════════════════");
        log.info("📊 OBLICZONE PARAMETRY ZESTAWU TRANSPORTOWEGO:");
        log.info("   {}", transportSet.getWeightBreakdown());
        log.info("   {}", transportSet.getHeightBreakdown());
        log.info("   {}", transportSet.getAxleBreakdown());
        log.info("   Długość całkowita: {} cm", transportSet.getTotalLengthCm());
        log.info("   Szerokość całkowita: {} cm", transportSet.getTotalWidthCm());
        log.info("   Typ: {}", transportSet.getTrailerType());
        log.info("════════════════════════════════════════════════════════════");

        TransportSet saved = transportSetRepository.save(transportSet);
        log.info("✅ Transport set created with ID: {} (type: {})",
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