package com.military.applogistic.dto.request;

import lombok.Data;

@Data
public class CreateTransportSetRequest {
    private Long transporterId;
    private Long cargoId;
    private String description;

    // ═══════════════════════════════════════════════════════════════════════════
    // NOWE POLE - ID naczepy do przypisania do zestawu transportowego
    // ═══════════════════════════════════════════════════════════════════════════
    private Long trailerId;

    // Nowe pole - typ transportu
    private String transportMode; // "trailer" lub "self"

    /**
     * Czy operator wymusił jazdę na własnych kołach?
     * true = pojazd jedzie sam (mimo że mógłby być na lawecie)
     * false/null = automatyczna decyzja systemu
     */
    public boolean isForceSelfDriving() {
        return "self".equalsIgnoreCase(transportMode);
    }
}