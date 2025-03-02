package netwatch.identitymanager.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.repositories.RoleRepository;

// This class is a service component responsible for managing operations related to user roles.
// @author Antonio Scardace
// @version 1.0

@Service
public class RoleService {
    
    private final RoleRepository roleRepository;

    @Autowired
    public RoleService(RoleRepository roleRepository) {
        this.roleRepository = roleRepository;
    }

    public List<Role> getAllRoles() {
        return this.roleRepository.findAll();
    }

    public Role getRole(String name) {
        return this.roleRepository.findByName(name);
    }

    public Boolean checkIfRoleExists(String name) {
        return this.roleRepository.existsByName(name);
    }

    // Inserts the new role in the database.
    // The given user role must not already exist in the database.
    // If it can be added, the method returns true, false otherwise.

    public Boolean insertRole(String name, String description) {
        if (Boolean.TRUE.equals(this.roleRepository.existsByName(name)))
            return false;

        Role role = new Role(name, description);
        this.roleRepository.save(role);
        return true;
    }

    // Updates the role description in the database.
    // The given role must already exist in the database.
    // If it can be updated, the method returns true, false otherwise.

    public Boolean updateRole(String name, String description) {
        if (Boolean.FALSE.equals(this.roleRepository.existsByName(name)))
            return false;

        Role role = this.roleRepository.findByName(name);
        role.setDescription(description);
        this.roleRepository.save(role);
        return true;
    }

    // Deletes the role from the database.
    // The given role must already exist in the database.
    // If it can be deleted, the method returns true, false otherwise.

    public Boolean deleteRole(String name) {
        if (Boolean.FALSE.equals(this.roleRepository.existsByName(name)))
            return false;

        Role role = this.roleRepository.findByName(name);
        this.roleRepository.delete(role);
        return true;
    }
}