package netwatch.manager.controllers;

import java.util.List;

import netwatch.manager.entities.Contact;
import netwatch.manager.services.ContactService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

// This class is a REST controller that handles HTTP requests related to the {@link Contact} entity.
// It maps endpoints to perform CRUD operations on the {@link Contact} entity in the "/api/contacts" context path.
// @author Antonio Scardace
// @version 1.0 

@RestController
@RequestMapping("/api/contacts")
public class ContactController {
    
    private final ContactService contactService;

    @Autowired
    public ContactController(ContactService contactService) {
        this.contactService = contactService;
    }

    @GetMapping(path="/")
    public List<Contact> getAllContacts() {
        return this.contactService.getAllContacts();
    }

    @GetMapping(path="/check")
    public Boolean checkIfContactExists(@RequestParam("value") String contactValue) {
        return this.contactService.checkIfContactExists(contactValue);
    }

    @PostMapping(path="/")
    public ResponseEntity<Void> insertContact(@RequestParam("value") String contactValue, @RequestParam("type") String type) {
        return Boolean.TRUE.equals(this.contactService.insertContact(contactValue, type))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }

    @DeleteMapping(path="/")
    public ResponseEntity<Void> deleteContact(@RequestParam("value") String contactValue) {
        return Boolean.TRUE.equals(this.contactService.deleteContact(contactValue))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }
}