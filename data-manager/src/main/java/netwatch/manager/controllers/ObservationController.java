package netwatch.manager.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import netwatch.manager.entities.Observation;
import netwatch.manager.services.ObservationService;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

// This class is a REST controller that handles HTTP requests related to the {@link Observation} entity.
// It maps utilities to perform CRUD operations on the {@link Observation} entity in the "/api/observations" context path.
// @author Antonio Scardace
// @version 1.0

@RestController
@RequestMapping("/api/observations")
public class ObservationController {
    
    private final ObservationService observationService;

    @Autowired
    public ObservationController(ObservationService observationService) {
        this.observationService = observationService;
    }

    @GetMapping(path="/")
    public List<Observation> getAllAssociations() {
        return this.observationService.getAllAssociations();
    }

    @GetMapping(path="/filter/{env}")
    public List<Observation> getAllAssociationsByEnvironment(@PathVariable("env") String environmentName) {
        return this.observationService.getAllAssociationsByEnvironment(environmentName);
    }

    @GetMapping(path="/check")
    public Boolean checkIfReferentObservesUtility(@RequestParam("address") String utilityAddress, @RequestParam("contact") String referentContact) {
        return this.observationService.checkIfReferentObservesUtility(utilityAddress, referentContact);
    }

    @PostMapping(path="/{env}")
    public ResponseEntity<Void> insertObservation(@RequestParam("address") String utilityAddress, @RequestParam("contact") String contact, @PathVariable("env") String environmentName) {
        return Boolean.TRUE.equals(this.observationService.insertObservation(utilityAddress, contact, environmentName))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @DeleteMapping(path="/{env}")
    public ResponseEntity<Void> deleteObservation(@RequestParam("address") String utilityAddress, @RequestParam("contact") String contact, @PathVariable("env") String environmentName) {
        return Boolean.TRUE.equals(this.observationService.deleteObservation(utilityAddress, contact, environmentName))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }
}