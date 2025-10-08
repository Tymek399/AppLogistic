package com.military.applogistic.mapper;

import com.military.applogistic.dto.LiveDriverInfo;
import com.military.applogistic.entity.DriverPosition;
import com.military.applogistic.entity.DriverSession;
import org.springframework.stereotype.Component;

@Component
public class LiveTrackingMapper {

    public LiveDriverInfo toLiveDriverInfo(DriverSession session, DriverPosition position, boolean isOnline) {
        return LiveDriverInfo.builder()
                .driverUsername(session.getDriverUsername())
                .routeId(session.getActiveRoute().getId())
                .routeDescription(session.getActiveRoute().getStartAddress() + " → " +
                        session.getActiveRoute().getEndAddress())
                .latitude(position != null ? position.getLatitude() : null)
                .longitude(position != null ? position.getLongitude() : null)
                .speedKmh(position != null ? position.getSpeedKmh() : null)
                .lastUpdate(position != null ? position.getLastUpdateTime() : null)
                .isOnline(isOnline)
                .build();
    }
}
