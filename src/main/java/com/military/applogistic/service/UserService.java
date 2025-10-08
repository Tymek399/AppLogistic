package com.military.applogistic.service;

import com.military.applogistic.dto.request.CreateUserRequest;
import com.military.applogistic.dto.response.UserResponse;
import com.military.applogistic.entity.User;
import com.military.applogistic.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;

    public UserResponse createUser(CreateUserRequest request) {
        log.info("Creating new user: {}", request.getUsername());

        if (userRepository.findByUsername(request.getUsername()).isPresent()) {
            throw new RuntimeException("User already exists: " + request.getUsername());
        }

        User user = request.toUser(passwordEncoder);
        User savedUser = userRepository.save(user);

        log.info("User created successfully: {}", savedUser.getUsername());
        return UserResponse.from(savedUser);
    }

    public List<UserResponse> getAllUsers() {
        log.info("Retrieving all users");
        return userRepository.findAll().stream()
                .map(UserResponse::from)
                .toList();
    }

    public List<UserResponse> getDrivers() {
        return userRepository.findAll().stream()
                .filter(user ->  user.getRole() == User.Role.DRIVER)
                .map(UserResponse::from)
                .toList();
    }
    public void deleteUser(String username) {
        log.info("Deleting user: {}", username);
        User user = userRepository.findByUsername(username).orElse(null);
        if (user == null) {
            throw new RuntimeException("User not found: " + username);
        }else{
            userRepository.delete(user);
        }
    }

}
