package com.military.applogistic.repository.user;

import com.military.applogistic.entity.DriverSession;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import java.util.List;

@Repository
public interface DriverSessionRepository extends JpaRepository<DriverSession, Long> {
    List<DriverSession> findByDriverUsernameAndIsActive(String driverUsername, Boolean isActive);
    List<DriverSession> findByIsActive(Boolean isActive);
}