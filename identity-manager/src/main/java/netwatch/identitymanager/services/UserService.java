package netwatch.identitymanager.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.entities.User;
import netwatch.identitymanager.repositories.UserRepository;

// This class is a service component responsible for managing operations related to users.
// @author Antonio Scardace
// @version 1.0

@Service
public class UserService {
    
    private final UserRepository userRepository;
    private final RoleService roleService;

    @Autowired
    public UserService(UserRepository userRepository, RoleService roleService) {
        this.userRepository = userRepository;
        this.roleService = roleService;
    }

    public List<User> getAllUsers() {
        return this.userRepository.findAll();
    }

    public User getUserByUsername(String username) {
        return this.userRepository.findByUsername(username);
    }

    public Boolean checkIfUserExists(String username) {
        return this.userRepository.existsByUsername(username);
    }

    // Checks if a user has a specific role.
    // The given user must already exist in the database.
    // Returns true if the user has that role, false otherwise.

    public Boolean checkIfUserHasRole(String username, String roleName) {
        return this.userRepository.existsByUsername(username) &&
               this.userRepository.findByUsername(username).getRole().getName().equals(roleName);
    }

    // Inserts the new user in the database.
    // The given user must not already exist in the database.
    // The given role must already exist in the database.
    // If it can be added, the method returns true, false otherwise.

    public Boolean insertUser(String username, String password, Role role) {
        if (Boolean.TRUE.equals(this.userRepository.existsByUsername(username)) ||
            Boolean.FALSE.equals(this.roleService.checkIfRoleExists(role.getName())))
            return false;

        User user = new User(username, password, role);
        this.userRepository.save(user);
        return true;
    }

    // Updates the user fields in the database.
    // The given user and role must already exist in the database.
    // If it can be updated, the method returns true, false otherwise.

    public Boolean updateUser(String username, String password, String roleName) {
        if (Boolean.FALSE.equals(this.userRepository.existsByUsername(username)) ||
            Boolean.FALSE.equals(this.roleService.checkIfRoleExists(roleName)))
            return false;

        User user = this.userRepository.findByUsername(username);
        Role role = this.roleService.getRole(roleName);
        user.setPassword(password);
        user.setRole(role);
        this.userRepository.save(user);
        return true;
    }

    // Deletes the user from the database.
    // The given user must already exist in the database.
    // If it can be deleted, the method returns true, false otherwise.

    public Boolean deleteUser(String username) {
        if (Boolean.FALSE.equals(this.userRepository.existsByUsername(username)))
            return false;

        User user = this.userRepository.findByUsername(username);
        this.userRepository.delete(user);
        return true;
    }
}