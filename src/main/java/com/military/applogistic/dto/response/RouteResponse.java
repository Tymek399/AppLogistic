package com.military.applogistic.dto.response;// RouteResponse.java - rozszerzona o nowe pola


import lombok.Data;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Data
public class RouteResponse {
    private Long id;
    private String startAddress;
    private String endAddress;
    private String status;
    private Double distance;
    private Integer estimatedTime;
    private Long transportSetId;
    private String createdBy;
    private LocalDateTime createdAt;

    // NOWE POLA
    private Boolean isDraft;
    private Boolean hasValidationProblems;
    private Boolean operatorAccepted;
    private List<String> operatorMessages;
    private List<Map<String, Object>> rejectedPoints;
    private Map<String, Object> validation;
    private Map<String, Object> routeData;
}