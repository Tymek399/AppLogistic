package com.military.applogistic.repository;

import com.military.applogistic.entity.TransportSet;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TransportSetRepository extends JpaRepository<TransportSet, Long> {
}