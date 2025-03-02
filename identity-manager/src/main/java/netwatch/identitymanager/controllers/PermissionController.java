package netwatch.identitymanager.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import netwatch.identitymanager.entities.PermissionId;
import netwatch.identitymanager.services.PermissionService;

// This class is a REST controller that handles authorization-related HTTP requests related to the {@link Permission} entity.
// It maps endpoints to perform CRUD operations on the {@link Permission} entity in the "/api/authorization" context path.
// @author Antonio Scardace
// @version 1.0

@RestController
@RequestMapping("/api/authorization")
public class PermissionController {
    
    private final PermissionService permissionService;

    @Autowired
    public PermissionController(PermissionService permissionService) {
        this.permissionService = permissionService;
    }

    @GetMapping(path="/")
    public List<PermissionId> getAllPermissions() {
        return this.permissionService.getAllPermissions();
    }

    @GetMapping(path="/filter")
    public List<PermissionId> getRolePermissions(@RequestParam("role") String roleName) {
        return this.permissionService.getRolePermissions(roleName);
    }

    @GetMapping(path="/check")
    public Boolean checkIfPermissionExists(@RequestParam("role") String roleName, @RequestParam("method") String method, @RequestParam("object") String object) {
        return this.permissionService.checkIfRoleHasPermission(roleName, method, object);
    }

    @PostMapping(path="/")
    public ResponseEntity<Void> insertPermission(@RequestParam("role") String roleName, @RequestParam("method") String method, @RequestParam("object") String object) {
        return Boolean.TRUE.equals(this.permissionService.insertPermission(roleName, method, object))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @DeleteMapping(path="/")
    public ResponseEntity<Void> deletePermission(@RequestParam("role") String roleName, @RequestParam("method") String method, @RequestParam("object") String object) {
        return Boolean.TRUE.equals(this.permissionService.deletePermission(roleName, method, object))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }
}