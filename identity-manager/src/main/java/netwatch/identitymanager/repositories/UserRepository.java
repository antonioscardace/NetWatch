package netwatch.identitymanager.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import netwatch.identitymanager.entities.User;

// This interface defines the "UserRepository", which extends "JpaRepository" interface.
// JpaRepository is a Spring Data interface that provides CRUD operations for the {@link User} entity.
// It helps in defining auto-generated SQL queries by the names of the methods.
// @author Antonio Scardace
// @version 1.0

public interface UserRepository extends JpaRepository<User, String> {
    User findByUsername(String username);
    Boolean existsByUsername(String username);
}