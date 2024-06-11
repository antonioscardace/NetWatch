package netwatch.manager.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import netwatch.manager.entities.Contact;
import netwatch.manager.repositories.ContactRepository;

// This class is a service component responsible for managing operations related to referents contacts.
// Update operation is not provided because the contact value is the Primary Key.
// @author Antonio Scardace
// @version 1.0

@Service
public class ContactService {
    
    private final ContactRepository contactRepository;

    @Autowired
    public ContactService(ContactRepository contactRepository) {
        this.contactRepository = contactRepository;
    }

    public Boolean checkIfContactExists(String contactValue) {
        return this.contactRepository.existsByValue(contactValue);
    }

    public Contact getContactByValue(String contactValue) {
        return this.contactRepository.findByValue(contactValue);
    }

    public List<Contact> getAllContacts() {
        return this.contactRepository.findAll();
    }

    // Inserts the new contact in the database.
    // The given contact must not already exist in the database.
    // If it can be added, the method returns true, false otherwise.

    public Boolean insertContact(String contactValue, String type) {
        if (Boolean.TRUE.equals(this.checkIfContactExists(contactValue)))
            return false;

        Contact referentContact = new Contact(contactValue, type);
        this.contactRepository.save(referentContact);
        return true;
    }

    // Deletes the contact from the database.
    // The given contact must already exist in the database.
    // If it can be deleted, the method returns true, false otherwise.

    public Boolean deleteContact(String contactValue) {
        if (Boolean.FALSE.equals(this.checkIfContactExists(contactValue)))
            return false;

        Contact referentContact = this.contactRepository.findByValue(contactValue);
        this.contactRepository.delete(referentContact);
        return true;
    }
}