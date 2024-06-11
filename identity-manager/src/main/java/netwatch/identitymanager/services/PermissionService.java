package netwatch.identitymanager.services;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import netwatch.identitymanager.entities.Permission;
import netwatch.identitymanager.entities.PermissionId;
import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.repositories.PermissionRepository;

// This class is a service component responsible for managing operations related to role permissions.
// The update operation is not provided because all three fields are primary keys.
// @author Antonio Scardace
// @version 1.0

@Service
public class PermissionService {
    
    private final PermissionRepository permissionRepository;
    private final RoleService roleService;

    @Autowired
    public PermissionService(PermissionRepository permissionRepository, RoleService roleService) {
        this.permissionRepository = permissionRepository;
        this.roleService = roleService;
    }

    public List<PermissionId> getAllPermissions() {
        return this.permissionRepository.findAll().stream()
            .map(Permission::getId)
            .collect(Collectors.toList());
    }

    public List<PermissionId> getRolePermissions(String roleName) {
        return this.permissionRepository.findByIdRole(roleName).stream()
            .map(Permission::getId)
            .collect(Collectors.toList());
    }

    // Checks if a role has permission to access a specific object.
    // The given role must already exist in the database.
    // Returns true if the role has that authorization, false otherwise.

    public Boolean checkIfRoleHasPermission(String roleName, String method, String object) {
        return this.roleService.checkIfRoleExists(roleName) &&
               this.permissionRepository.existsByIdRoleAndIdMethodAndIdObject(roleName, method, object);
    }

    // Inserts the new permission in the database.
    // The given permission rule must not already exist in the database.
    // If it can be added, the method returns true, false otherwise.

    public Boolean insertPermission(String roleName, String method, String object) {
        if (Boolean.TRUE.equals(this.checkIfRoleHasPermission(roleName, method, object)))
            return false;

        PermissionId id = new PermissionId(roleName, method, object);
        Role role = this.roleService.getRole(roleName);
        Permission permission = new Permission(id, role);
        this.permissionRepository.save(permission);
        return true;
    }

    // Deletes the permission from the database.
    // The given permission rule must already exist in the database.
    // If it can be deleted, the method returns true, false otherwise.

    public Boolean deletePermission(String roleName, String method, String object) {
        if (Boolean.FALSE.equals(this.checkIfRoleHasPermission(roleName, method, object)))
            return false;

        PermissionId id = this.permissionRepository.findByIdRoleAndIdMethodAndIdObject(roleName, method, object).getId();
        this.permissionRepository.deleteById(id);
        return true;
    }
}