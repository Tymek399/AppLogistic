package com.military.applogistic.mapper;

import com.military.applogistic.dto.PositionUpdate;
import com.military.applogistic.entity.DriverPosition;
import org.springframework.stereotype.Component;

@Component
public class DriverPositionMapper {

    public DriverPosition fromPositionUpdate(String driverUsername, DriverPosition position, PositionUpdate update) {
        position.setDriverUsername(driverUsername);
        position.setLatitude(update.getLatitude());
        position.setLongitude(update.getLongitude());
        position.setSpeedKmh(update.getSpeedKmh());
        position.setAccuracy(update.getAccuracy());
        position.setBatteryLevel(update.getBatteryLevel());
        position.setLastUpdateTime(update.getTimestamp() != null ? update.getTimestamp() : java.time.LocalDateTime.now());
        return position;
    }
}
