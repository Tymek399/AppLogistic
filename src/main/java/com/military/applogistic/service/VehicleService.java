package com.military.applogistic.service;

import com.military.applogistic.dto.request.CreateTransportSetRequest;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.entity.Vehicle;
import com.military.applogistic.repository.TransportSetRepository;
import com.military.applogistic.repository.VehicleRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class VehicleService {

    private final VehicleRepository vehicleRepository;
    private final TransportSetRepository transportSetRepository;



    public VehicleService(VehicleRepository vehicleRepository, TransportSetRepository transportSetRepository) {
        this.vehicleRepository = vehicleRepository;
        this.transportSetRepository = transportSetRepository;
    }

    public TransportSet createTransportSet(CreateTransportSetRequest request) {
        Vehicle transporter = vehicleRepository.findById(request.getTransporterId())
                .orElseThrow(() -> new RuntimeException("Transporter not found"));
        Vehicle cargo = vehicleRepository.findById(request.getCargoId())
                .orElseThrow(() -> new RuntimeException("Cargo not found"));

        TransportSet transportSet = new TransportSet();
        transportSet.setTransporter(transporter);
        transportSet.setCargo(cargo);
        transportSet.setDescription(request.getDescription());
        transportSet.calculateTransportParameters();

        return transportSetRepository.save(transportSet);
    }
}
