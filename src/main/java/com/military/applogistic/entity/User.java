package com.military.applogistic.entity;

import jakarta.persistence.*;
import lombok.Data;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "users")
@Data
//TODO keycloak od razu
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

 //   @Column(unique = true, nullable = false)
  //  @GeneratedValue(strategy = GenerationType.IDENTITY)
  //  private UUID referencjeNumber;

    @Column(unique = true, nullable = false)
    private String username;

    @Column(nullable = false)
    private String password;

    @Column(nullable = false)
    private String email;

    @Enumerated(EnumType.STRING)
    private Role role;

    private String firstName;
    private String lastName;
    //todo co to kurwa jest za true
    private boolean active = true;
    //todo auditabl
    private LocalDateTime createdAt = LocalDateTime.now();

    //todo oddzielny plik
    public enum Role {
        OPERATOR, DRIVER, ADMIN
    }
}