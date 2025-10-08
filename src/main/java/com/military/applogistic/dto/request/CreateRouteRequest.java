package com.military.applogistic.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

/**
 * Request DTO dla tworzenia nowej trasy
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CreateRouteRequest {

    @NotBlank(message = "Start address is required")
    private String startAddress;

    @NotBlank(message = "End address is required")
    private String endAddress;

    @NotNull(message = "Start latitude is required")
    private Double startLatitude;

    @NotNull(message = "Start longitude is required")
    private Double startLongitude;

    @NotNull(message = "End latitude is required")
    private Double endLatitude;

    @NotNull(message = "End longitude is required")
    private Double endLongitude;

    @NotNull(message = "Transport set ID is required")
    private Long transportSetId;
}