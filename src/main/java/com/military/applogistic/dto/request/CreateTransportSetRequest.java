package com.military.applogistic.dto.request;

import lombok.Data;

@Data
public class CreateTransportSetRequest {
    private String transporterReferenceNumber;
    private String cargoReferenceNumber;
    private String description;

    private TransportMode transportMode; // TRAILER or SELF
}