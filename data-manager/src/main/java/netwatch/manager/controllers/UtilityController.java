package netwatch.manager.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import netwatch.manager.entities.Utility;
import netwatch.manager.services.UtilityService;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

// This class is a REST controller that handles HTTP requests related to the {@link Utility} entity.
// It maps utilities to perform CRUD operations on the {@link Utility} entity in the "/api/utilities" context path.
// @author Antonio Scardace
// @version 1.0

@RestController
@RequestMapping("/api/utilities")
public class UtilityController {
    
    private final UtilityService utilityService;

    @Autowired
    public UtilityController(UtilityService utilityService) {
        this.utilityService = utilityService;
    }

    @GetMapping(path="/")
    public List<Utility> getAllUtilities() {
        return this.utilityService.getAllUtilities();
    }

    @GetMapping(path="/check")
    public Boolean checkIfUtilityExistsByAddress(@RequestParam("address") String address) {
        return this.utilityService.checkIfUtilityExistsByAddress(address);
    }

    @PostMapping(path="/")
    public ResponseEntity<Void> insertService(@RequestParam("address") String address, @RequestParam("name") String name, @RequestParam("type") String type) {
        return Boolean.TRUE.equals(this.utilityService.insertUtility(address, name, type))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @PutMapping(path="/")
    public ResponseEntity<Void> updateService(@RequestParam("address") String address, @RequestParam("name") String name) {
        return Boolean.TRUE.equals(this.utilityService.updateUtility(address, name))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @DeleteMapping(path="/")
    public ResponseEntity<Void> deleteService(@RequestParam("address") String address) {
        return Boolean.TRUE.equals(this.utilityService.deleteUtility(address))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }
}