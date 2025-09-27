package com.military.applogistic.config;

import com.military.applogistic.security.JwtAuthenticationFilter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import lombok.RequiredArgsConstructor;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
@RequiredArgsConstructor
public class SecurityConfig {

    private final JwtAuthenticationFilter jwtAuthenticationFilter;

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                .csrf(csrf -> csrf.disable())
                .sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .authorizeHttpRequests(auth -> auth
                        // Strony HTML - PUBLICZNE (bo autoryzacja jest w JS)
                        .requestMatchers("/", "/index.html", "/login.html").permitAll()
                        .requestMatchers("/driver-dashboard.html", "/operator-dashboard.html").permitAll()
                        .requestMatchers("/navigation.html").permitAll()

                        // Zasoby statyczne
                        .requestMatchers("/css/**", "/js/**", "/images/**").permitAll()
                        .requestMatchers("/favicon.ico").permitAll()

                        // H2 Console
                        .requestMatchers("/h2-console/**").permitAll()

                        // API Auth
                        .requestMatchers("/api/auth/**").permitAll()

                        // Navigation endpoint - WAŻNE!
                        .requestMatchers("/navigation/*").permitAll()  // Tymczasowo publiczne

                        // API Config endpoints
                        .requestMatchers("/api/config/**").authenticated()

                        // API z tokenem
                        .requestMatchers("/api/**").authenticated()

                        // Reszta
                        .anyRequest().authenticated()
                )
                .headers(headers -> headers.frameOptions(frame -> frame.disable()))
                .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);

        return http.build();
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
        return config.getAuthenticationManager();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }
}