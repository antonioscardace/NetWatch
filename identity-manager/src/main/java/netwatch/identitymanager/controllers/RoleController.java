package netwatch.identitymanager.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.services.RoleService;

// This class is a REST controller that handles HTTP requests related to the {@link Role} entity.
// It maps endpoints to perform CRUD operations on the {@link Role} entity in the "/api/roles" context path.
// @author Antonio Scardace
// @version 1.0

@RestController
@RequestMapping("/api/roles")
public class RoleController {
    
    private final RoleService roleService;

    @Autowired
    public RoleController(RoleService roleService) {
        this.roleService = roleService;
    }

    @GetMapping(path="/")
    public List<Role> getAllRoles() {
        return this.roleService.getAllRoles();
    }

    @PostMapping(path="/")
    public ResponseEntity<Void> insertRole(@RequestParam("name") String name, @RequestParam("descr") String description) {
        return Boolean.TRUE.equals(this.roleService.insertRole(name, description))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @PutMapping(path="/")
    public ResponseEntity<Void> updateRole(@RequestParam("name") String name, @RequestParam("descr") String description) {
        return Boolean.TRUE.equals(this.roleService.updateRole(name, description))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @DeleteMapping(path="/")
    public ResponseEntity<Void> deleteRole(@RequestParam("name") String name) {
        return Boolean.TRUE.equals(this.roleService.deleteRole(name))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }
}