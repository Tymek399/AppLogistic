package com.military.applogistic.dto.request;

import lombok.Data;

@Data
public class CreateTransportSetRequest {
    private Long transporterId;
    private Long cargoId;
    private String description;
}