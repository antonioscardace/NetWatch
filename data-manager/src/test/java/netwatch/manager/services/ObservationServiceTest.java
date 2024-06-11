package netwatch.manager.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import netwatch.manager.entities.Contact;
import netwatch.manager.entities.Observation;
import netwatch.manager.entities.ObservationId;
import netwatch.manager.entities.Utility;
import netwatch.manager.repositories.ObservationRepository;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

// Unit tests for the {@link ObservationService} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

@ExtendWith(MockitoExtension.class)
class ObservationServiceTest {
    
    @Mock
    ObservationRepository observationRepository;

    @Mock
    UtilityService utilityService;

    @Mock
    ContactService contactService;

    @InjectMocks
    ObservationService observationService;
    
    String environment = "dev";
    Contact contact = new Contact("test@example.com", "email");
    Utility utility = new Utility("8.8.8.8", "Test", "ip");
    ObservationId id = new ObservationId("8.8.8.8", "test@example.com");
    Observation observation = new Observation(id, utility, contact, environment);

    @Test
    void testCheckIfReferentObservesUtility_Successful() {
        when(observationRepository.existsByObservationIdAddressAndObservationIdContact(utility.getAddress(), contact.getValue())).thenReturn(true);
        assertTrue(observationService.checkIfReferentObservesUtility(utility.getAddress(), contact.getValue()));
    }

    @Test
    void testCheckIfReferentObservesUtility_Unsuccessful() {
        when(observationRepository.existsByObservationIdAddressAndObservationIdContact(utility.getAddress(), contact.getValue())).thenReturn(false);
        assertFalse(observationService.checkIfReferentObservesUtility(utility.getAddress(), contact.getValue()));
    }

    @Test
    void testCheckIfObservationConcernsEnvironment_Successful() {
        when(observationService.checkIfReferentObservesUtility(utility.getAddress(), contact.getValue())).thenReturn(true);
        when(observationRepository.findByObservationIdAddressAndObservationIdContact(utility.getAddress(), contact.getValue())).thenReturn(observation);
        assertTrue(observationService.checkIfObservationConcernsEnvironment(utility.getAddress(), contact.getValue(), environment));
    }

    @Test
    void testCheckIfObservationConcernsEnvironment_Unsuccessful() {
        when(observationService.checkIfReferentObservesUtility(utility.getAddress(), contact.getValue())).thenReturn(false);
        assertFalse(observationService.checkIfObservationConcernsEnvironment(utility.getAddress(), contact.getValue(), environment));
        verify(observationRepository, never()).findByObservationIdAddressAndObservationIdContact(any(), any());
    }
    
    @Test
    void testInsertObservation_Successful() {
        when(observationRepository.existsByObservationIdAddressAndObservationIdContact(utility.getAddress(), contact.getValue())).thenReturn(false);
        when(utilityService.checkIfUtilityExistsByAddress(utility.getAddress())).thenReturn(true);
        when(contactService.checkIfContactExists(contact.getValue())).thenReturn(true);
        assertTrue(observationService.insertObservation(utility.getAddress(), contact.getValue(), observation.getEnvironment()));
        verify(observationRepository).save(any());
    }

    @Test
    void testDeleteObservation_Successful() {
        when(observationService.checkIfObservationConcernsEnvironment(utility.getAddress(), contact.getValue(), observation.getEnvironment())).thenReturn(true);
        when(observationService.checkIfReferentObservesUtility(utility.getAddress(), contact.getValue())).thenReturn(true);
        when(observationRepository.findByObservationIdAddressAndObservationIdContact(utility.getAddress(), contact.getValue())).thenReturn(observation);
        assertTrue(observationService.deleteObservation(utility.getAddress(), contact.getValue(), observation.getEnvironment()));
        verify(observationRepository).delete(any());
    }

    @ParameterizedTest
    @CsvSource({
        "false, false, true", // when the address does not exists
        "false, true, false", // when the contact does not exists
        "false, false, false" // when both address and contact do not exist
    })
    void testInsertObservation_Unsuccessful(boolean observationExists, boolean utilityExists, boolean contactExists) {
        lenient().when(observationRepository.existsByObservationIdAddressAndObservationIdContact(utility.getAddress(), contact.getValue())).thenReturn(observationExists);
        lenient().when(utilityService.checkIfUtilityExistsByAddress(utility.getAddress())).thenReturn(utilityExists);
        lenient().when(contactService.checkIfContactExists(contact.getValue())).thenReturn(contactExists);
        assertFalse(observationService.insertObservation(utility.getAddress(), contact.getValue(), "dev"));
        verify(observationRepository, never()).save(any());
    }

    @ParameterizedTest
    @CsvSource({
        "false", // when the referent does not observe the utility
        "true"   // when referent observes the utility, but the observation does not concern the authorized environment
    })
    void testDeleteObservation_Unsuccessful(boolean observationExists) {
        lenient().when(observationService.checkIfObservationConcernsEnvironment(utility.getAddress(), contact.getValue(), "admin")).thenReturn(false);
        lenient().when(observationService.checkIfReferentObservesUtility(utility.getAddress(), contact.getValue())).thenReturn(observationExists);
        lenient().when(observationRepository.findByObservationIdAddressAndObservationIdContact(utility.getAddress(), contact.getValue())).thenReturn(observation);
        assertFalse(observationService.deleteObservation(utility.getAddress(), contact.getValue(), "admin"));
        verify(observationRepository, never()).delete(any());
    }
}