package com.military.applogistic.repository;

import com.military.applogistic.entity.DriverPosition;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import java.util.Optional;
import java.util.List;

@Repository
public interface DriverPositionRepository extends JpaRepository<DriverPosition, Long> {
    Optional<DriverPosition> findTopByDriverUsernameOrderByLastUpdateTimeDesc(String driverUsername);
    List<DriverPosition> findByDriverUsernameOrderByLastUpdateTimeDesc(String driverUsername);
}