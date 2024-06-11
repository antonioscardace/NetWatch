package netwatch.identitymanager.repositories;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import netwatch.identitymanager.entities.Permission;
import netwatch.identitymanager.entities.PermissionId;

// This interface defines the "PermissionRepository", which extends "JpaRepository" interface.
// JpaRepository is a Spring Data interface that provides CRUD operations for the {@link Permission} entity.
// It helps in defining auto-generated SQL queries by the names of the methods.
// @author Antonio Scardace
// @version 1.0

public interface PermissionRepository extends JpaRepository<Permission, PermissionId> {
    List<Permission> findByIdRole(String role);
    Permission findByIdRoleAndIdMethodAndIdObject(String role, String method, String object);
    Boolean existsByIdRoleAndIdMethodAndIdObject(String role, String method, String object);
}