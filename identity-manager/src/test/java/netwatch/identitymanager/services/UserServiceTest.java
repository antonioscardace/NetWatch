package netwatch.identitymanager.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.entities.User;
import netwatch.identitymanager.repositories.RoleRepository;
import netwatch.identitymanager.repositories.UserRepository;

// Unit tests for the {@link UserService} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

@SpringBootTest
class UserServiceTest {

    @Mock
    UserRepository userRepository;

    @Mock
    RoleRepository roleRepository;

    @InjectMocks
    UserService userService;

    String username = "administrator";
    String password = "password";
    Role role = new Role("admin", "System Administrator");
    User user = new User(username, password, role);

    @Test
    void testCheckIfUserExists_Successful() {
        when(userRepository.existsByUsername(username)).thenReturn(true);
        assertTrue(userService.checkIfUserExists(username));
    }

    @Test
    void testCheckIfUserExists_Unsuccessful() {
        when(userRepository.existsByUsername(username)).thenReturn(false);
        assertFalse(userService.checkIfUserExists(username));
    }

    @Test
    void testCheckIfUserHasRole_Successful() {
        when(userRepository.existsByUsername(username)).thenReturn(true);
        when(userRepository.findByUsername(username)).thenReturn(user);
        assertTrue(userService.checkIfUserHasRole(username, role.getName()));
    }

    @Test
    void testCheckIfUserHasRole_Unsuccessful() {
        when(userRepository.existsByUsername(username)).thenReturn(false);
        verify(userRepository, never()).findByUsername(any());
        assertFalse(userService.checkIfUserHasRole(username, role.getName()));
    }
    
    @Test
    void testInsertUser_Successful() {
        when(userRepository.existsByUsername(username)).thenReturn(false);
        when(roleRepository.existsByName(role.getName())).thenReturn(true);
        assertTrue(userService.insertUser(username, password, role));
        verify(userRepository).save(any());
    }

    @ParameterizedTest
    @CsvSource({
        "false, false", // when both the user and the role do not exist
        "true, false", // when the role does not exist
        "true, true" // when the user already exists in the database
    })
    void testInsertUser_Unsuccessful(boolean userExists, boolean roleExists) {
        when(userRepository.existsByUsername(username)).thenReturn(userExists);
        when(roleRepository.existsByName(role.getName())).thenReturn(roleExists);
        assertFalse(userService.insertUser(username, password, role));
        verify(userRepository, never()).save(any());
    }

    @Test
    void testUpdateUser_Successful() {
        when(userRepository.existsByUsername(username)).thenReturn(true);
        when(roleRepository.existsByName("dev")).thenReturn(true);
        when(userRepository.findByUsername(username)).thenReturn(user);
        when(roleRepository.findByName(role.getName())).thenReturn(role);
        assertTrue(userService.updateUser(username, password, "dev"));
        verify(userRepository).save(any());
    }

    @ParameterizedTest
    @CsvSource({
        "false, false", // when both the user and the role do not exist
        "true, false", // when the role does not exist
        "false, true" // when the user does not exist in the database
    })
    void testUpdateUser_Unsuccessful(boolean userExists, boolean roleExists) {
        when(userRepository.existsByUsername(username)).thenReturn(userExists);
        when(roleRepository.existsByName("dev")).thenReturn(roleExists);
        assertFalse(userService.updateUser(username, password, "dev"));
        verify(userRepository, never()).save(any());
    }

    @Test
    void testDeleteUser_Successful() {
        when(userRepository.existsByUsername(username)).thenReturn(true);
        assertTrue(userService.deleteUser(username));
        verify(userRepository).delete(any());
    }

    @Test
    void testDeleteUser_Unsuccessful() {
        when(userRepository.existsByUsername(username)).thenReturn(false);
        assertFalse(userService.deleteUser(username));
        verify(userRepository, never()).delete(any());
    }
}