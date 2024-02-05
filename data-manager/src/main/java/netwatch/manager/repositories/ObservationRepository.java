package netwatch.manager.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import netwatch.manager.entities.Observation;
import netwatch.manager.entities.ObservationId;

import java.util.List;

// This interface defines the "ObservationRepository", which extends "JpaRepository" interface.
// JpaRepository is a Spring Data interface that provides CRUD operations for the {@link Observation} entity.
// It helps in defining auto-generated SQL queries by the names of the methods.
// @author Antonio Scardace
// @version 1.0

public interface ObservationRepository extends JpaRepository<Observation, ObservationId> {
    List<Observation> findByEnvironment(String environmentName);
    Observation findByObservationIdAddressAndObservationIdContact(String address, String referentContract);
    Boolean existsByObservationIdAddressAndObservationIdContact(String address, String referentContract);
}