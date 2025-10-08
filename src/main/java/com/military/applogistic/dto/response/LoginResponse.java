package com.military.applogistic.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class LoginResponse {
    private String token;
    //todo powinien byc token i refresh token bez username chyba ze chcesz zeby ci ktos to zajebal i wlamal sie do systmemu
    private String username;
}