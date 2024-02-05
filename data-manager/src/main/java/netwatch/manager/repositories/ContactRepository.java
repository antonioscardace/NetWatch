package netwatch.manager.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import netwatch.manager.entities.Contact;

// This interface defines the "ContactRepository", which extends "JpaRepository" interface.
// JpaRepository is a Spring Data interface that provides CRUD operations for the {@link Contact} entity.
// It helps in defining auto-generated SQL queries by the names of the methods.
// @author Antonio Scardace
// @version 1.0

public interface ContactRepository extends JpaRepository<Contact, String> {
    Contact findByValue(String contactValue);
    Boolean existsByValue(String contactValue);
}