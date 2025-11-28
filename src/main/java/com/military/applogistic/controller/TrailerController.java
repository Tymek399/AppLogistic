package com.military.applogistic.controller;

import com.military.applogistic.entity.Trailer;
import com.military.applogistic.repository.TrailerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/trailers") // Adres bazowy: /api/trailers
public class TrailerController {

    @Autowired
    private TrailerRepository trailerRepository;

    // Obsługa żądania POST: /api/trailers
    // Służy do dodawania nowej naczepy
    @PostMapping
    public ResponseEntity<Trailer> createTrailer(@RequestBody Trailer newTrailer) {
        try {
            // Walidacja podstawowa, np. czy numer rejestracyjny nie jest pusty
            if (newTrailer.getRegistrationNumber() == null || newTrailer.getRegistrationNumber().isEmpty()) {
                // Zwracamy 400 Bad Request
                return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
            }

            // Zapis obiektu do bazy danych
            Trailer savedTrailer = trailerRepository.save(newTrailer);

            // Zwracamy status 201 CREATED (utworzono)
            return new ResponseEntity<>(savedTrailer, HttpStatus.CREATED);
        } catch (Exception e) {
            // W przypadku błędu serwera/bazy danych
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    // Obsługa żądania GET: /api/trailers
    // Służy do pobierania listy naczep
    @GetMapping
    public ResponseEntity<List<Trailer>> getAllTrailers() {
        try {
            List<Trailer> trailers = trailerRepository.findAll();

            // Zwracamy status 200 OK
            return new ResponseEntity<>(trailers, HttpStatus.OK);
        } catch (Exception e) {
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}