package netwatch.identitymanager.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.lambdaworks.crypto.SCryptUtil;

import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.entities.User;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

// Unit tests for the {@link AuthenticationService} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

@ExtendWith(MockitoExtension.class)
class AuthenticationServiceTest {

    @InjectMocks
    AuthenticationService authenticationService;

    @Mock
    RoleService roleService;

    @Mock
    UserService userService;

    String username = "administrator";
    String password = "password";
    Role role = new Role("admin", "System Administrator");
    User user = new User(username, SCryptUtil.scrypt(password, 32768, 8, 1), role);
    
    @Test
    void testSignUp_Successful() {
        when(roleService.getRole(role.getName())).thenReturn(role);
        lenient().when(roleService.checkIfRoleExists(role.getName())).thenReturn(true);
        lenient().when(userService.checkIfUserExists(username)).thenReturn(false);
        assertTrue(authenticationService.signUp(username, password, role.getName()));
        verify(userService).insertUser(any(), any(), any());
    }

    @ParameterizedTest
    @CsvSource({
        "false, false", // when both the user and the role do not exist
        "false, true", // when the role does not exist
        "true, true" // when the user already exists in the database
    })
    void testSignUp_Unsuccessful(boolean roleExists, boolean userExists) {
        lenient().when(roleService.checkIfRoleExists(role.getName())).thenReturn(roleExists);
        lenient().when(userService.checkIfUserExists(username)).thenReturn(userExists);
        assertFalse(authenticationService.signUp(username, password, role.getName()));
        verify(userService, never()).insertUser(any(), any(), any());
    }

    @Test
    void testSignIn_Unsuccessful_whenUserDoesNotExists() {
        when(userService.checkIfUserExists(username)).thenReturn(false);
        lenient().when(userService.getUserByUsername(username)).thenReturn(user);
        assertEquals(null, authenticationService.signIn(username, password));
    }

    @Test
    void testSignIn_Unsuccessful_withWrongPassword() {
        when(userService.checkIfUserExists(username)).thenReturn(true);
        lenient().when(userService.getUserByUsername(username)).thenReturn(user);
        assertEquals(null, authenticationService.signIn(username, "wrong_password"));
    }
}