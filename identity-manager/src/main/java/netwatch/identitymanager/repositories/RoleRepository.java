package netwatch.identitymanager.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import netwatch.identitymanager.entities.Role;

// This interface defines the "RoleRepository", which extends "JpaRepository" interface.
// JpaRepository is a Spring Data interface that provides CRUD operations for the {@link Role} entity.
// It helps in defining auto-generated SQL queries by the names of the methods.
// @author Antonio Scardace
// @version 1.0

public interface RoleRepository extends JpaRepository<Role, String> {
    Role findByName(String name);
    Boolean existsByName(String name);
}