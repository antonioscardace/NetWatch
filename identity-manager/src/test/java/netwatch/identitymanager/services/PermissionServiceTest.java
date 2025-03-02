package netwatch.identitymanager.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import netwatch.identitymanager.entities.Permission;
import netwatch.identitymanager.entities.PermissionId;
import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.repositories.PermissionRepository;

// Unit tests for the {@link PermissionService} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

@ExtendWith(MockitoExtension.class)
class PermissionServiceTest {

    @Mock
    PermissionRepository permissionRepository;

    @Mock
    RoleService roleService;

    @InjectMocks
    PermissionService permissionService;

    Role role = new Role("admin", "System Administrator");
    String method = "GET";
    String object = "/users/";

    @Test
    void testCheckIfRoleHasPermission_Successful() {
        when(roleService.checkIfRoleExists(role.getName())).thenReturn(true);
        when(permissionRepository.existsByIdRoleAndIdMethodAndIdObject(role.getName(), method, object)).thenReturn(true);
        assertTrue(permissionService.checkIfRoleHasPermission(role.getName(), method, object));
    }

    @Test
    void testCheckIfRoleHasPermission_Unsuccessful() {
        when(roleService.checkIfRoleExists(role.getName())).thenReturn(false);
        assertFalse(permissionService.checkIfRoleHasPermission(role.getName(), method, object));
        verify(permissionRepository, never()).existsByIdRoleAndIdMethodAndIdObject(any(), any(), any());
    }
    
    @Test
    void testInsertPermission_Successful() {
        lenient().when(roleService.checkIfRoleExists(role.getName())).thenReturn(false);
        lenient().when(permissionRepository.existsByIdRoleAndIdMethodAndIdObject(role.getName(), method, object)).thenReturn(true);
        assertTrue(permissionService.insertPermission(role.getName(), method, object));
        verify(permissionRepository).save(any());
    }

    @Test
    void testInsertPermission_Unsuccessful() {
        when(roleService.checkIfRoleExists(role.getName())).thenReturn(true);
        when(permissionRepository.existsByIdRoleAndIdMethodAndIdObject(role.getName(), method, object)).thenReturn(true);
        assertFalse(permissionService.insertPermission(role.getName(), method, object));
        verify(permissionRepository, never()).save(any());
    }

    @Test
    void testDeletePermission_Successful() {
        PermissionId id = new PermissionId(role.getName(), method, object);
        when(roleService.checkIfRoleExists(role.getName())).thenReturn(true);
        when(permissionRepository.existsByIdRoleAndIdMethodAndIdObject(role.getName(), method, object)).thenReturn(true);
        when(permissionRepository.findByIdRoleAndIdMethodAndIdObject(role.getName(), method, object)).thenReturn(new Permission(id, role));
        assertTrue(permissionService.deletePermission(role.getName(), method, object));
        verify(permissionRepository).deleteById(any());
    }

    @Test
    void testDeletePermission_Unsuccessful() {
        when(roleService.checkIfRoleExists(role.getName())).thenReturn(false);
        assertFalse(permissionService.deletePermission(role.getName(), method, object));
        verify(permissionRepository, never()).delete(any());
    }
}