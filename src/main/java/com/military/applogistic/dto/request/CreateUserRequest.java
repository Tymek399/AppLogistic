package com.military.applogistic.dto.request;

import com.military.applogistic.entity.User;
import lombok.Data;
import org.springframework.security.crypto.password.PasswordEncoder;

@Data
public class CreateUserRequest {
    private String username;
    private String password;
    private String email;
    private String role;
    private String firstName;
    private String lastName;

    // Metoda mapująca DTO na encję User
    public User toUser(PasswordEncoder passwordEncoder) {
        User user = new User();
        user.setUsername(this.username);
        user.setPassword(passwordEncoder.encode(this.password));
        user.setEmail(this.email);
        user.setRole(User.Role.valueOf(this.role.toUpperCase()));
        user.setFirstName(this.firstName);
        user.setLastName(this.lastName);
        user.setActive(true);
        return user;
    }
}
