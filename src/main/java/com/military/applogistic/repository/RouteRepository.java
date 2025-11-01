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
    List<Route> findByStatusIn(List<Route.RouteStatus> statuses);

    // NOWE METODY
    @Query("SELECT r FROM Route r WHERE r.status = ?1 AND r.isDraft = ?2")
    List<Route> findByStatusAndIsDraft(Route.RouteStatus status, Boolean isDraft);

    @Query("SELECT r FROM Route r WHERE r.hasValidationProblems = true AND r.operatorAccepted = false")
    List<Route> findRoutesRequiringAcceptance();

    @Query("SELECT r FROM Route r WHERE r.operatorAcceptedBy = ?1")
    List<Route> findRoutesAcceptedByOperator(String operatorUsername);
}