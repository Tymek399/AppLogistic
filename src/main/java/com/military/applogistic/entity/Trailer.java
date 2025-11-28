package com.military.applogistic.entity;


import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;

@Entity
public class Trailer {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // Dane z formularza
    private String registrationNumber;
    private String type;
    private String vin;

    // Specyfikacje (używamy typów liczbowych pasujących do JS i bazy)
    private Integer maxPayload; // Waga (kg)
    private Double length;      // Długość (m)
    private Double width;       // Szerokość (m)
    private Double height;      // Wysokość (m)

    // Konstruktory (wymagany jest domyślny dla JPA)
    public Trailer() {}

    // Konstruktor do tworzenia obiektu
    public Trailer(String registrationNumber, String type, String vin, Integer maxPayload, Double length, Double width, Double height) {
        this.registrationNumber = registrationNumber;
        this.type = type;
        this.vin = vin;
        this.maxPayload = maxPayload;
        this.length = length;
        this.width = width;
        this.height = height;
    }

    // --- GETTERY I SETTERY (Wymagane przez Spring/Jackson) ---

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getRegistrationNumber() {
        return registrationNumber;
    }

    public void setRegistrationNumber(String registrationNumber) {
        this.registrationNumber = registrationNumber;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getVin() {
        return vin;
    }

    public void setVin(String vin) {
        this.vin = vin;
    }

    public Integer getMaxPayload() {
        return maxPayload;
    }

    public void setMaxPayload(Integer maxPayload) {
        this.maxPayload = maxPayload;
    }

    public Double getLength() {
        return length;
    }

    public void setLength(Double length) {
        this.length = length;
    }

    public Double getWidth() {
        return width;
    }

    public void setWidth(Double width) {
        this.width = width;
    }

    public Double getHeight() {
        return height;
    }

    public void setHeight(Double height) {
        this.height = height;
    }
}
