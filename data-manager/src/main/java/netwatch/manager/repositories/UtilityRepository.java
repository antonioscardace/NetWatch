package netwatch.manager.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import netwatch.manager.entities.Utility;

// This interface defines the "UtilityRepository", which extends "JpaRepository" interface.
// JpaRepository is a Spring Data interface that provides CRUD operations for the {@link Utility} entity.
// It helps in defining auto-generated SQL queries by the names of the methods.
// @author Antonio Scardace
// @version 1.0

public interface UtilityRepository extends JpaRepository<Utility, String> {
    Utility findByAddress(String address);
    Boolean existsByAddress(String address);
}