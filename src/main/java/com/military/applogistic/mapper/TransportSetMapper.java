package com.military.applogistic.mapper;

import com.military.applogistic.dto.request.CreateTransportSetRequest;
import com.military.applogistic.dto.request.TransportMode;
import com.military.applogistic.entity.TransportSet;
import com.military.applogistic.entity.Vehicle;
import org.springframework.stereotype.Component;

@Component
public class TransportSetMapper {

    public TransportSet toEntity(CreateTransportSetRequest request, Vehicle transporter, Vehicle cargo) {
        TransportSet transportSet = new TransportSet();
        transportSet.setTransporter(transporter);
        transportSet.setCargo(cargo);
        transportSet.setDescription(request.getDescription());
        boolean forceSelfDriving = request.getTransportMode() == TransportMode.SELF;
        if (forceSelfDriving) {
            if (Boolean.TRUE.equals(cargo.getCanDriveAlone())) {
                transportSet.setForceSelfDriving(true);
            } else {
                transportSet.setForceSelfDriving(false);
            }
        } else {
            transportSet.setForceSelfDriving(false);
        }
        return transportSet;
    }
}