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
    private Double height;      // Wysokość (m) - wysokość z ładunkiem

    // ═══════════════════════════════════════════════════════════════════════════
    // NOWE POLA - wymagane dla pełnej walidacji zestawu transportowego
    // ═══════════════════════════════════════════════════════════════════════════

    /**
     * Liczba osi naczepy - używana do obliczania nacisków na osie
     */
    private Integer numberOfAxles;

    /**
     * Wysokość naczepy bez ładunku (w metrach) - używana do walidacji tuneli
     * Wysokość całkowita zestawu = max(wysokość_ciągnika, wysokość_naczepy_pusta + wysokość_ładunku)
     */
    private Double unloadedHeight;

    /**
     * Masa własna naczepy (w kg) - używana do obliczania całkowitej masy zestawu
     */
    private Integer emptyWeight;

    // Konstruktory
    public Trailer() {}

    // Konstruktor podstawowy (bez nowych pól - dla kompatybilności wstecznej)
    public Trailer(String registrationNumber, String type, String vin, Integer maxPayload, Double length, Double width, Double height) {
        this.registrationNumber = registrationNumber;
        this.type = type;
        this.vin = vin;
        this.maxPayload = maxPayload;
        this.length = length;
        this.width = width;
        this.height = height;
    }

    // Konstruktor pełny (z nowymi polami)
    public Trailer(String registrationNumber, String type, String vin, Integer maxPayload,
                   Double length, Double width, Double height,
                   Integer numberOfAxles, Double unloadedHeight, Integer emptyWeight) {
        this.registrationNumber = registrationNumber;
        this.type = type;
        this.vin = vin;
        this.maxPayload = maxPayload;
        this.length = length;
        this.width = width;
        this.height = height;
        this.numberOfAxles = numberOfAxles;
        this.unloadedHeight = unloadedHeight;
        this.emptyWeight = emptyWeight;
    }

    // --- GETTERY I SETTERY ---

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

    // ═══════════════════════════════════════════════════════════════════════════
    // GETTERY I SETTERY DLA NOWYCH PÓL
    // ═══════════════════════════════════════════════════════════════════════════

    public Integer getNumberOfAxles() {
        return numberOfAxles;
    }

    public void setNumberOfAxles(Integer numberOfAxles) {
        this.numberOfAxles = numberOfAxles;
    }

    public Double getUnloadedHeight() {
        return unloadedHeight;
    }

    public void setUnloadedHeight(Double unloadedHeight) {
        this.unloadedHeight = unloadedHeight;
    }

    public Integer getEmptyWeight() {
        return emptyWeight;
    }

    public void setEmptyWeight(Integer emptyWeight) {
        this.emptyWeight = emptyWeight;
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // METODY POMOCNICZE
    // ═══════════════════════════════════════════════════════════════════════════

    /**
     * Zwraca wysokość naczepy bez ładunku w centymetrach (do użycia w TransportSet)
     */
    public Integer getUnloadedHeightCm() {
        if (unloadedHeight == null) {
            return null;
        }
        return (int) (unloadedHeight * 100);
    }

    /**
     * Zwraca długość naczepy w centymetrach
     */
    public Integer getLengthCm() {
        if (length == null) {
            return null;
        }
        return (int) (length * 100);
    }

    /**
     * Zwraca szerokość naczepy w centymetrach
     */
    public Integer getWidthCm() {
        if (width == null) {
            return null;
        }
        return (int) (width * 100);
    }

    /**
     * Zwraca wysokość naczepy (z ładunkiem) w centymetrach
     */
    public Integer getHeightCm() {
        if (height == null) {
            return null;
        }
        return (int) (height * 100);
    }
}