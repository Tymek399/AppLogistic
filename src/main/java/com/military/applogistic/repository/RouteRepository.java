package com.military.applogistic.repository;


import com.military.applogistic.entity.Route;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import java.util.List;

@Repository
public interface RouteRepository extends JpaRepository<Route, Long> {
    List<Route> findByAssignedDriverUsername(String username);
    List<Route> findByStatus(Route.RouteStatus status);

    @Query("SELECT r FROM Route r WHERE r.status = 'ACTIVE'")
    List<Route> findActiveRoutes();
}
