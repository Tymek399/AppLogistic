package com.military.applogistic.dto.response;

import com.military.applogistic.controller.AdminController;
import com.military.applogistic.entity.User;
import lombok.AllArgsConstructor;
import lombok.Data;


@Data
    @AllArgsConstructor
    public  class UserResponse {
        private Long id;
        private String username;
        private String email;
        private String role;
        private String firstName;
        private String lastName;
        private boolean active;

        //toto wyjeb to do mapperow i zrob buildera
        public static UserResponse from(User user) {
            return new UserResponse(
                    user.getId(),
                    user.getUsername(),
                    user.getEmail(),
                    user.getRole().name(),
                    user.getFirstName(),
                    user.getLastName(),
                    user.isActive()
            );
        }
    }
