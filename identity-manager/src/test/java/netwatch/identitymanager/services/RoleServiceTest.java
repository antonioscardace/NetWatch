package netwatch.identitymanager.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.repositories.RoleRepository;

// Unit tests for the {@link RoleService} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

@ExtendWith(MockitoExtension.class)
class RoleServiceTest {

    @Mock
    RoleRepository roleRepository;

    @InjectMocks
    RoleService roleService;

    String name = "admin";
    String description = "System Administrator";

    @Test
    void testCheckIfRoleExists_Successful() {
        when(roleRepository.existsByName(name)).thenReturn(true);
        assertTrue(roleService.checkIfRoleExists(name));
    }

    @Test
    void testCheckIfRoleExists_Unsuccessful() {
        when(roleRepository.existsByName(name)).thenReturn(false);
        assertFalse(roleService.checkIfRoleExists(name));
    }
    
    @Test
    void testInsertRole_Successful() {
        when(roleRepository.existsByName(name)).thenReturn(false);
        assertTrue(roleService.insertRole(name, description));
        verify(roleRepository).save(any());
    }

    @Test
    void testInsertRole_Unsuccessful() {
        when(roleRepository.existsByName(name)).thenReturn(true);
        assertFalse(roleService.insertRole(name, description));
        verify(roleRepository, never()).save(any());
    }

    @Test
    void testUpdateRole_Successful() {
        when(roleRepository.existsByName(name)).thenReturn(true);
        when(roleRepository.findByName(name)).thenReturn(new Role(name, description));
        assertTrue(roleService.updateRole(name, description));
        verify(roleRepository).save(any());
    }

    @Test
    void testUpdateRole_Unsuccessful() {
        when(roleRepository.existsByName(name)).thenReturn(false);
        assertFalse(roleService.updateRole(name, description));
        verify(roleRepository, never()).save(any());
    }

    @Test
    void testDeleteRole_Successful() {
        when(roleRepository.existsByName(name)).thenReturn(true);
        assertTrue(roleService.deleteRole(name));
        verify(roleRepository).delete(any());
    }

    @Test
    void testDeleteRole_Unsuccessful() {
        when(roleRepository.existsByName(name)).thenReturn(false);
        assertFalse(roleService.deleteRole(name));
        verify(roleRepository, never()).delete(any());
    }
}