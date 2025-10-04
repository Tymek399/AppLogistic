package com.military.applogistic.service;

import com.military.applogistic.entity.DriverPosition;
import com.military.applogistic.entity.DriverSession;
import com.military.applogistic.entity.Route;
import com.military.applogistic.repository.DriverSessionRepository;
import com.military.applogistic.repository.DriverPositionRepository;
import com.military.applogistic.repository.RouteRepository;
import com.military.applogistic.dto.PositionUpdate;
import com.military.applogistic.dto.LiveDriverInfo;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class LiveTrackingService {

    private final DriverSessionRepository sessionRepository;
    private final DriverPositionRepository positionRepository;
    private final RouteRepository routeRepository;
    private final SimpMessagingTemplate messagingTemplate;  // Added for broadcasting updates

    public void startDriverSession(String driverUsername, Long routeId) {
        // End any existing sessions for this driver
        sessionRepository.findByDriverUsernameAndIsActive(driverUsername, true)
                .forEach(session -> {
                    session.setIsActive(false);
                    sessionRepository.save(session);
                });

        Route route = routeRepository.findById(routeId)
                .orElseThrow(() -> new RuntimeException("Route not found"));

        DriverSession newSession = new DriverSession();
        newSession.setDriverUsername(driverUsername);
        newSession.setActiveRoute(route);
        sessionRepository.save(newSession);

        // Update route status
        route.setStatus(Route.RouteStatus.ACTIVE);
        route.setStartedAt(LocalDateTime.now());
        routeRepository.save(route);

        log.info("Driver {} started navigation session for route {}", driverUsername, routeId);
    }

    public void updateDriverPosition(String driverUsername, PositionUpdate position) {
        DriverSession session = sessionRepository
                .findByDriverUsernameAndIsActive(driverUsername, true)
                .stream().findFirst()
                .orElseThrow(() -> new RuntimeException("No active session"));

        session.setLastActivityTime(LocalDateTime.now());
        sessionRepository.save(session);

        DriverPosition driverPos = new DriverPosition();
        driverPos.setRoute(session.getActiveRoute());
        driverPos.setDriverUsername(driverUsername);
        driverPos.setLatitude(position.getLatitude());
        driverPos.setLongitude(position.getLongitude());
        driverPos.setSpeedKmh(position.getSpeedKmh());
        driverPos.setAccuracy(position.getAccuracy());
        driverPos.setBatteryLevel(position.getBatteryLevel());
        positionRepository.save(driverPos);

        // Check if driver reached destination (within 100m)
        Route route = session.getActiveRoute();
        double distance = calculateDistance(
                position.getLatitude(), position.getLongitude(),
                route.getEndLatitude(), route.getEndLongitude()
        );

        if (distance < 0.1) { // 100m in km
            route.setStatus(Route.RouteStatus.COMPLETED);
            route.setCompletedAt(LocalDateTime.now());
            routeRepository.save(route);
            endDriverSession(driverUsername);
        }
    }

    public List<LiveDriverInfo> getAllActiveDrivers() {
        return sessionRepository.findByIsActive(true).stream()
                .map(session -> {
                    DriverPosition lastPos = positionRepository
                            .findTopByDriverUsernameOrderByLastUpdateTimeDesc(session.getDriverUsername())
                            .orElse(null);

                    return LiveDriverInfo.builder()
                            .driverUsername(session.getDriverUsername())
                            .routeId(session.getActiveRoute().getId())
                            .routeDescription(session.getActiveRoute().getStartAddress() + " → " +
                                    session.getActiveRoute().getEndAddress())
                            .latitude(lastPos != null ? lastPos.getLatitude() : null)
                            .longitude(lastPos != null ? lastPos.getLongitude() : null)
                            .speedKmh(lastPos != null ? lastPos.getSpeedKmh() : null)
                            .lastUpdate(lastPos != null ? lastPos.getLastUpdateTime() : null)
                            .isOnline(isDriverOnline(lastPos))
                            .build();
                })
                .collect(Collectors.toList());
    }

    public LiveDriverInfo getDriverStatus(String driverUsername) {
        DriverSession session = sessionRepository
                .findByDriverUsernameAndIsActive(driverUsername, true)
                .stream().findFirst()
                .orElse(null);

        if (session == null) {
            return LiveDriverInfo.builder()
                    .driverUsername(driverUsername)
                    .isOnline(false)
                    .build();
        }

        DriverPosition lastPos = positionRepository
                .findTopByDriverUsernameOrderByLastUpdateTimeDesc(driverUsername)
                .orElse(null);

        return LiveDriverInfo.builder()
                .driverUsername(driverUsername)
                .routeId(session.getActiveRoute().getId())
                .routeDescription(session.getActiveRoute().getStartAddress() + " → " +
                        session.getActiveRoute().getEndAddress())
                .latitude(lastPos != null ? lastPos.getLatitude() : null)
                .longitude(lastPos != null ? lastPos.getLongitude() : null)
                .speedKmh(lastPos != null ? lastPos.getSpeedKmh() : null)
                .lastUpdate(lastPos != null ? lastPos.getLastUpdateTime() : null)
                .isOnline(isDriverOnline(lastPos))
                .build();
    }

    public void endDriverSession(String driverUsername) {
        sessionRepository.findByDriverUsernameAndIsActive(driverUsername, true)
                .forEach(session -> {
                    session.setIsActive(false);
                    sessionRepository.save(session);
                });
        log.info("Driver {} ended navigation session", driverUsername);
    }

    private boolean isDriverOnline(DriverPosition pos) {
        if (pos == null) return false;
        return pos.getLastUpdateTime().isAfter(LocalDateTime.now().minusMinutes(1));
    }

    private double calculateDistance(double lat1, double lon1, double lat2, double lon2) {
        double R = 6371; // Earth radius in km
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);
        double a = Math.sin(dLat/2) * Math.sin(dLat/2) +
                Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
                        Math.sin(dLon/2) * Math.sin(dLon/2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
        return R * c;
    }


    public LiveDriverInfo getUpdatedDriverInfo(PositionUpdate position) {

        String driverUsername = "placeholder_username";  // TODO: Implement proper username retrieval

        updateDriverPosition(driverUsername, position);
        LiveDriverInfo updatedInfo = getDriverStatus(driverUsername);

        messagingTemplate.convertAndSend("/topic/driver-routes", updatedInfo);

        return updatedInfo;
    }
}