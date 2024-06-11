package netwatch.manager.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import netwatch.manager.entities.Contact;
import netwatch.manager.entities.Utility;
import netwatch.manager.entities.Observation;
import netwatch.manager.entities.ObservationId;
import netwatch.manager.repositories.ObservationRepository;

// This class is a service component responsible for managing associations between contacts and utilities.
// Update operation is not provided because the contact value and the utility address are Primary Keys.
// @author Antonio Scardace
// @version 1.0

@Service
public class ObservationService {
    
    private final ContactService contactService;
    private final UtilityService utilityService;
    private final ObservationRepository observationRepository;

    @Autowired
    public ObservationService(ContactService contactService, UtilityService utilityService, ObservationRepository observationRepository) {
        this.contactService = contactService;
        this.observationRepository = observationRepository;
        this.utilityService = utilityService;
    }

    public Boolean checkIfReferentObservesUtility(String address, String contact) {
        return this.observationRepository.existsByObservationIdAddressAndObservationIdContact(address, contact);
    }

    public List<Observation> getAllAssociations() {
        return this.observationRepository.findAll();
    }

    public List<Observation> getAllAssociationsByEnvironment(String environmentName) {
        return this.observationRepository.findByEnvironment(environmentName);
    }

    // Checks if an observation concerns an environment.
    // The given observation must already exist in the database.
    // Returns true if the observation concerns the given environment, false otherwise.

    public Boolean checkIfObservationConcernsEnvironment(String address, String contact, String environmentName) {
        return this.checkIfReferentObservesUtility(address, contact) &&
               this.observationRepository.findByObservationIdAddressAndObservationIdContact(address, contact)
                   .getEnvironment()
                   .equals(environmentName);
    }

    // Inserts the new observation in the database.
    // The given observation must not already exist in the database.
    // The given utility and contact must already exist in the database.
    // If it can be added, the method returns true, false otherwise.

    public Boolean insertObservation(String address, String contact, String environmentName) {
        if (Boolean.TRUE.equals(this.checkIfReferentObservesUtility(address, contact)) ||
            Boolean.FALSE.equals(this.utilityService.checkIfUtilityExistsByAddress(address)) ||
            Boolean.FALSE.equals(this.contactService.checkIfContactExists(contact)))
            return false;
                
        ObservationId id = new ObservationId(address, contact);
        Utility utilityAddress = utilityService.getUtilityByAddress(address);
        Contact referentContact = contactService.getContactByValue(contact); 
        Observation observation = new Observation(id, utilityAddress, referentContact, environmentName);
        this.observationRepository.save(observation);
        return true;
    }

    // Deletes the observation from the database.
    // The given observation must already exist in the database.
    // The given observation must concern the given environment.
    // If it can be deleted, the method returns true, false otherwise.

    public Boolean deleteObservation(String address, String contact, String environmentName) {
        if (Boolean.FALSE.equals(this.checkIfReferentObservesUtility(address, contact)) ||
            Boolean.FALSE.equals(this.checkIfObservationConcernsEnvironment(address, contact, environmentName)))
            return false;

        Observation observation = this.observationRepository.findByObservationIdAddressAndObservationIdContact(address, contact);
        this.observationRepository.delete(observation);
        return true;
    }
}