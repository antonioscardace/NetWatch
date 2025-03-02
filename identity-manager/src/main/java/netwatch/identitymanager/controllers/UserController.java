package netwatch.identitymanager.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import netwatch.identitymanager.entities.User;
import netwatch.identitymanager.services.UserService;

// This class is a REST controller that handles HTTP requests related to the {@link User} entity.
// It maps endpoints to perform CRUD operations on the {@link User} entity in the "/api/users" context path.
// @author Antonio Scardace
// @version 1.0

@RestController
@RequestMapping("/api/users")
public class UserController {
    
    private final UserService userService;

    @Autowired
    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping(path="/")
    public List<User> getAllUsers() {
        return this.userService.getAllUsers();
    }

    @GetMapping(path="/check")
    public Boolean checkIfUserExists(@RequestParam("user") String username) {
        return this.userService.checkIfUserExists(username);
    }

    @PutMapping(path="/")
    public ResponseEntity<Void> updateUser(@RequestParam("user") String username, @RequestParam("psw") String password, @RequestParam("role") String roleName) {
        return Boolean.TRUE.equals(this.userService.updateUser(username, password, roleName))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @DeleteMapping(path="/")
    public ResponseEntity<Void> deleteUser(@RequestParam("user") String username) {
        return Boolean.TRUE.equals(this.userService.deleteUser(username))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }
}