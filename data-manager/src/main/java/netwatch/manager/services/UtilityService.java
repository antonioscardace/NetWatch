package netwatch.manager.services;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import netwatch.manager.entities.Utility;
import netwatch.manager.repositories.UtilityRepository;

// This class is a service component responsible for managing operations related to the utilities.
// @author Antonio Scardace
// @version 1.0

@Service
public class UtilityService {
    
    private final UtilityRepository utilityRepository;

    @Autowired
    public UtilityService(UtilityRepository utilityRepository) {
        this.utilityRepository = utilityRepository;
    }

    public Boolean checkIfUtilityExistsByAddress(String address) {
        return this.utilityRepository.existsByAddress(address);
    }

    public Utility getUtilityByAddress(String address) {
        return this.utilityRepository.findByAddress(address);
    }

    public List<Utility> getAllUtilities() {
        return this.utilityRepository.findAll();
    }

    // Inserts the new utility in the database.
    // The given utility must not already exist in the database.
    // If it can be added, the method returns true, false otherwise.

    public Boolean insertUtility(String address, String name, String type) {
        if (Boolean.TRUE.equals(this.checkIfUtilityExistsByAddress(address)))
            return false;

        Utility utility = new Utility(address, name, type);
        this.utilityRepository.save(utility);
        return true;
    }

    // Updates utility fields in the database.
    // The given utility must already exist in the database.
    // If it can be updated, the method returns true, false otherwise.

    public Boolean updateUtility(String address, String name) {
        if (Boolean.FALSE.equals(this.checkIfUtilityExistsByAddress(address)))
            return false;
        
        Utility utility = this.utilityRepository.findByAddress(address);
        utility.setName(name);
        this.utilityRepository.save(utility);
        return true;
    }

    // Deletes the utility from the database.
    // The given utility must already exist in the database.
    // If it can be deleted, the method returns true, false otherwise.

    public Boolean deleteUtility(String address) {
        if (Boolean.FALSE.equals(this.checkIfUtilityExistsByAddress(address)))
            return false;

        Utility utility = this.utilityRepository.findByAddress(address);
        this.utilityRepository.delete(utility);
        return true;
    }
}