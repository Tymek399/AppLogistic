package com.military.applogistic.entity;

/**
 * Status trasy w systemie
 */
public enum RouteStatus {

    /**
     * Trasa utworzona, oczekuje na przypisanie kierowcy
     */
    CREATED("Utworzona"),

    /**
     * Trasa przypisana do kierowcy
     */
    ASSIGNED("Przypisana"),

    /**
     * Trasa w trakcie realizacji
     */
    ACTIVE("Aktywna"),

    /**
     * Trasa zakończona
     */
    COMPLETED("Zakończona");

    private final String displayName;

    RouteStatus(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    /**
     * Sprawdza czy można przypisać kierowcę
     */
    public boolean canAssignDriver() {
        return this == CREATED;
    }

    /**
     * Sprawdza czy można rozpocząć trasę
     */
    public boolean canStart() {
        return this == ASSIGNED;
    }

    /**
     * Sprawdza czy można zakończyć trasę
     */
    public boolean canComplete() {
        return this == ACTIVE;
    }

    /**
     * Sprawdza czy trasa jest w trakcie realizacji
     */
    public boolean isInProgress() {
        return this == ACTIVE;
    }

    /**
     * Sprawdza czy trasa jest zakończona
     */
    public boolean isFinished() {
        return this == COMPLETED;
    }
}