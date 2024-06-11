package netwatch.manager.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import netwatch.manager.entities.Utility;
import netwatch.manager.repositories.UtilityRepository;

// Unit tests for the {@link UtilityService} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

@ExtendWith(MockitoExtension.class)
class UtilityServiceTest {

    @Mock
    UtilityRepository utilityRepository;

    @InjectMocks
    UtilityService utilityService;

    String address = "8.8.8.8";
    String name = "Test Service";
    String type = "ip";

    @Test
    void testCheckIfUtilityExistsByAddress_Successful() {
        when(utilityRepository.existsByAddress(address)).thenReturn(true);
        assertTrue(utilityService.checkIfUtilityExistsByAddress(address));
    }

    @Test
    void testCheckIfUtilityExistsByAddress_Unsuccessful() {
        when(utilityRepository.existsByAddress(address)).thenReturn(false);
        assertFalse(utilityService.checkIfUtilityExistsByAddress(address));
    }
    
    @Test
    void testInsertUtility_Successful() {
        when(utilityRepository.existsByAddress(address)).thenReturn(false);
        assertTrue(utilityService.insertUtility(address, name, type));
        verify(utilityRepository).save(any());
    }

    @Test
    void testInsertUtility_Unsuccessful() {
        when(utilityRepository.existsByAddress(address)).thenReturn(true);
        assertFalse(utilityService.insertUtility(address, name, type));
        verify(utilityRepository, never()).save(any());
    }

    @Test
    void testUpdateUtility_Successful() {
        Utility existingUtility = new Utility(address, name, type);
        when(utilityRepository.existsByAddress(existingUtility.getAddress())).thenReturn(true);
        when(utilityRepository.findByAddress(existingUtility.getAddress())).thenReturn(existingUtility);
        assertTrue(utilityService.updateUtility(existingUtility.getAddress(), "New Name"));
        verify(utilityRepository).save(any());
    }

    @Test
    void testUpdateUtility_Unsuccessful() {
        when(utilityRepository.existsByAddress(address)).thenReturn(false);
        assertFalse(utilityService.updateUtility(address, "New Name"));
        verify(utilityRepository, never()).save(any());
    }

    @Test
    void testDeleteUtility_Successful() {
        Utility existingUtility = new Utility(address, name, type);
        when(utilityRepository.existsByAddress(existingUtility.getAddress())).thenReturn(true);
        when(utilityRepository.findByAddress(existingUtility.getAddress())).thenReturn(existingUtility);
        assertTrue(utilityService.deleteUtility(existingUtility.getAddress()));
        verify(utilityRepository).delete(existingUtility);
    }

    @Test
    void testDeleteUtility_Unsuccessful() {
        when(utilityRepository.existsByAddress(address)).thenReturn(false);
        assertFalse(utilityService.deleteUtility(address));
        verify(utilityRepository, never()).delete(any());
    }
}