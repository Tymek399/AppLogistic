package com.military.applogistic.service;

import com.military.applogistic.dto.LiveDriverInfo;
import com.military.applogistic.dto.PositionUpdate;
import com.military.applogistic.entity.DriverPosition;
import com.military.applogistic.entity.DriverSession;
import com.military.applogistic.entity.Route;
import com.military.applogistic.entity.RouteStatus;
import com.military.applogistic.mapper.DriverPositionMapper;
import com.military.applogistic.mapper.LiveTrackingMapper;
import com.military.applogistic.repository.DriverPositionRepository;
import com.military.applogistic.repository.DriverSessionRepository;
import com.military.applogistic.repository.RouteRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class LiveTrackingService {

    private final DriverSessionRepository sessionRepository;
    private final DriverPositionRepository positionRepository;
    private final RouteRepository routeRepository;
    private final SimpMessagingTemplate messagingTemplate;
    private final LiveTrackingMapper liveTrackingMapper;
    private final DriverPositionMapper driverPositionMapper;

    public ResponseEntity<String> startDriverSessionResponse(String driverUsername, Long routeId) {
        try {
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
            newSession.setIsActive(true);
            newSession.setLastActivityTime(LocalDateTime.now());
            sessionRepository.save(newSession);

            route.setStatus(RouteStatus.ACTIVE);
            route.setStartedAt(LocalDateTime.now());
            routeRepository.save(route);

            log.info("Driver '{}' started navigation session for route {}", driverUsername, routeId);
            return ResponseEntity.ok("Navigation session started");
        } catch (Exception e) {
            log.error("Error starting session for driver {}: {}", driverUsername, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body("Failed to start session: " + e.getMessage());
        }
    }

    public ResponseEntity<Void> updateDriverPositionResponse(String driverUsername, PositionUpdate position) {
        try {
            updateDriverPosition(driverUsername, position);
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            log.error("Error updating position for driver {}: {}", driverUsername, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
        }
    }

    public ResponseEntity<Void> endDriverSessionResponse(String driverUsername) {
        try {
            endDriverSession(driverUsername);
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            log.error("Error ending session for driver {}: {}", driverUsername, e.getMessage());
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
        }
    }

    public ResponseEntity<List<LiveDriverInfo>> getAllActiveDriversResponse() {
        try {
            List<LiveDriverInfo> drivers = sessionRepository.findByIsActive(true).stream()
                    .map(session -> {
                        DriverPosition lastPos = positionRepository
                                .findTopByDriverUsernameOrderByLastUpdateTimeDesc(session.getDriverUsername())
                                .orElse(null);
                        return liveTrackingMapper.toLiveDriverInfo(session, lastPos, isDriverOnline(lastPos));
                    })
                    .collect(Collectors.toList());
            return ResponseEntity.ok(drivers);
        } catch (Exception e) {
            log.error("Error fetching active drivers: {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    public ResponseEntity<LiveDriverInfo> getDriverStatusResponse(String driverUsername) {
        try {
            DriverSession session = sessionRepository
                    .findByDriverUsernameAndIsActive(driverUsername, true)
                    .stream().findFirst()
                    .orElse(null);

            if (session == null) {
                return ResponseEntity.ok(LiveDriverInfo.builder()
                        .driverUsername(driverUsername)
                        .isOnline(false)
                        .build());
            }

            DriverPosition lastPos = positionRepository
                    .findTopByDriverUsernameOrderByLastUpdateTimeDesc(driverUsername)
                    .orElse(null);

            return ResponseEntity.ok(liveTrackingMapper.toLiveDriverInfo(session, lastPos, isDriverOnline(lastPos)));
        } catch (Exception e) {
            log.error("Error fetching driver {} status: {}", driverUsername, e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

    public LiveDriverInfo getUpdatedDriverInfo(PositionUpdate position) {
        String driverUsername = position.getDriverUsername();
        updateDriverPosition(driverUsername, position);
        LiveDriverInfo updatedInfo = getDriverStatusResponse(driverUsername).getBody();
        messagingTemplate.convertAndSend("/topic/driver-routes", updatedInfo);
        return updatedInfo;
    }

    private void updateDriverPosition(String driverUsername, PositionUpdate position) {
        DriverSession session = sessionRepository
                .findByDriverUsernameAndIsActive(driverUsername, true)
                .stream().findFirst()
                .orElseThrow(() -> new RuntimeException("No active session for driver: " + driverUsername));

        session.setLastActivityTime(LocalDateTime.now());
        sessionRepository.save(session);

        DriverPosition driverPos = driverPositionMapper.fromPositionUpdate(driverUsername, new DriverPosition(), position);
        driverPos.setRoute(session.getActiveRoute());
        positionRepository.save(driverPos);

        double distance = calculateDistance(
                position.getLatitude(), position.getLongitude(),
                session.getActiveRoute().getEndLatitude(), session.getActiveRoute().getEndLongitude()
        );

        if (distance < 0.1) {
            Route route = session.getActiveRoute();
            route.setStatus(RouteStatus.COMPLETED);
            route.setCompletedAt(LocalDateTime.now());
            routeRepository.save(route);
            endDriverSession(driverUsername);
            log.info("Driver '{}' completed route {}", driverUsername, route.getId());
        }
    }

    private void endDriverSession(String driverUsername) {
        sessionRepository.findByDriverUsernameAndIsActive(driverUsername, true)
                .forEach(session -> {
                    session.setIsActive(false);
                    sessionRepository.save(session);
                });
        log.info("Driver '{}' ended navigation session", driverUsername);
    }

    private boolean isDriverOnline(DriverPosition pos) {
        return pos != null && pos.getLastUpdateTime().isAfter(LocalDateTime.now().minusMinutes(1));
    }

    private double calculateDistance(double lat1, double lon1, double lat2, double lon2) {
        double R = 6371;
        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);
        double a = Math.sin(dLat / 2) * Math.sin(dLat / 2)
                + Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2))
                * Math.sin(dLon / 2) * Math.sin(dLon / 2);
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        return R * c;
    }
}
