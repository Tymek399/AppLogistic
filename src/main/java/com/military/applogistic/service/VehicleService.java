package com.military.applogistic.service;

import com.military.applogistic.dto.request.CreateTransportSetRequest;
import com.military.applogistic.dto.request.CreateVehicleRequest;
import com.military.applogistic.dto.request.TransportMode;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.entity.Vehicle;
import com.military.applogistic.mapper.TransportSetMapper;
import com.military.applogistic.mapper.VehicleMapper;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.repository.VehicleRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class VehicleService {

    private final VehicleRepository vehicleRepository;
    private final TransportSetRepository transportSetRepository;
    private final VehicleMapper vehicleMapper;
    private final TransportSetMapper transportSetMapper;
    private final TransportSetCalculator transportSetCalculator;

    public List<Vehicle> getTransporters() {
        return vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.TRANSPORTER, true);
    }

    public List<Vehicle> getCargo() {
        return vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.CARGO, true);
    }

    public Vehicle createTransporter(CreateVehicleRequest request) {
        log.info("Creating new transporter: {}", request.getModel());

        Vehicle vehicle = vehicleMapper.toTransporter(request);
        Vehicle saved = vehicleRepository.save(vehicle);
        log.info("Transporter created with ID: {} and reference: {}", saved.getId(), saved.getReferenceNumber());

        return saved;
    }

    public Vehicle createCargo(CreateVehicleRequest request) {
        log.info("Creating new cargo: {}", request.getModel());

        Vehicle vehicle = vehicleMapper.toCargo(request);
        Vehicle saved = vehicleRepository.save(vehicle);
        log.info("Cargo created with ID: {} (canDriveAlone: {}) and reference: {}", saved.getId(), saved.getCanDriveAlone(), saved.getReferenceNumber());

        return saved;
    }

    public void deleteVehicle(Long id) {
        log.info("Deleting vehicle: {}", id);

        Vehicle vehicle = vehicleRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Vehicle not found"));

        vehicle.setActive(false);
        vehicleRepository.save(vehicle);

        log.info("Vehicle {} deactivated", id);
    }

    public List<TransportSet> getTransportSets() {
        return transportSetRepository.findAll();
    }

    public TransportSet createTransportSet(CreateTransportSetRequest request) {
        log.info("Creating transport set: transporter={}, cargo={}, mode={}",
                request.getTransporterReferenceNumber(), request.getCargoReferenceNumber(), request.getTransportMode());

        Vehicle transporter = vehicleRepository.findByReferenceNumberAndActive(request.getTransporterReferenceNumber(), true)
                .orElseThrow(() -> new RuntimeException("Transporter not found"));
        Vehicle cargo = vehicleRepository.findByReferenceNumberAndActive(request.getCargoReferenceNumber(), true)
                .orElseThrow(() -> new RuntimeException("Cargo not found"));

        TransportSet transportSet = transportSetMapper.toEntity(request, transporter, cargo);

        transportSetCalculator.calculateTransportParameters(transportSet);

        TransportSet saved = transportSetRepository.save(transportSet);
        log.info("Transport set created with ID: {} (type: {})",
                saved.getId(), transportSetCalculator.getTrailerType(transportSet));

        return saved;
    }

    public void deleteTransportSet(Long id) {
        log.info("Deleting transport set: {}", id);
        transportSetRepository.deleteById(id);
    }
}