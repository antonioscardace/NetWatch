package netwatch.manager.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import netwatch.manager.entities.Contact;
import netwatch.manager.repositories.ContactRepository;

// Unit tests for the {@link ContactService} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

@ExtendWith(MockitoExtension.class)
class ContactServiceTest {

    @Mock
    ContactRepository contactRepository;

    @InjectMocks
    ContactService contactService;

    String contactValue = "test@example.com";
    String contactType = "email";

    @Test
    void testCheckIfContactExists_Successful() {
        when(contactRepository.existsByValue(contactValue)).thenReturn(true);
        assertTrue(contactService.checkIfContactExists(contactValue));
    }

    @Test
    void testCheckIfContactExists_Unsuccessful() {
        when(contactRepository.existsByValue(contactValue)).thenReturn(false);
        assertFalse(contactService.checkIfContactExists(contactValue));
    }
    
    @Test
    void testInsertContact_Successful() {
        when(contactRepository.existsByValue(contactValue)).thenReturn(false);
        assertTrue(contactService.insertContact(contactValue, contactType));
        verify(contactRepository).save(any());
    }

    @Test
    void testInsertContact_Unsuccessful() {
        when(contactRepository.existsByValue(contactValue)).thenReturn(true);
        assertFalse(contactService.insertContact(contactValue, contactType));
        verify(contactRepository, never()).save(any());
    }

    @Test
    void testDeleteContact_Successful() {
        Contact existingContact = new Contact(contactValue, contactType);
        when(contactRepository.existsByValue(existingContact.getValue())).thenReturn(true);
        when(contactRepository.findByValue(existingContact.getValue())).thenReturn(existingContact);
        assertTrue(contactService.deleteContact(existingContact.getValue()));
        verify(contactRepository).delete(existingContact);
    }

    @Test
    void testDeleteContact_Unsuccessful() {
        when(contactRepository.existsByValue(contactValue)).thenReturn(false);
        assertFalse(contactService.deleteContact(contactValue));
        verify(contactRepository, never()).delete(any());
    }
}