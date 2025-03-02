package netwatch.identitymanager.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import netwatch.identitymanager.entities.Role;
import netwatch.identitymanager.entities.User;
import netwatch.identitymanager.token.TokenGenerator;
import netwatch.identitymanager.token.TokenStore;
import com.lambdaworks.crypto.SCryptUtil;

// This class is a service component responsible for managing operations related to authentication.
// They are for checking a token validity, for sign-in and sign-up.
// @author Antonio Scardace
// @version 1.0

@Service
public class AuthenticationService {

    private final UserService userService;
    private final RoleService roleService;

    @Autowired
    public AuthenticationService(UserService userService, RoleService roleService) {
        this.userService = userService;
        this.roleService = roleService;
    }

    public Boolean isTokenValid(String apiKey) {
        return TokenStore.isTokenPresent(apiKey);
    }

    public String getUserRole(String apiKey) {
        return Boolean.TRUE.equals(this.isTokenValid(apiKey))
            ? TokenStore.getTokenByApiKey(apiKey).role()
            : null;
    }

    // Signs up a new user by creating a new user account with the provided username, password, and role.
    // The given user must not exist.
    // The given user role must exist.
    // If it can be added, the method returns true, false otherwise.

    public Boolean signUp(String username, String password, String roleName) {
        if (Boolean.FALSE.equals(this.roleService.checkIfRoleExists(roleName)) ||
            Boolean.TRUE.equals(this.userService.checkIfUserExists(username)))
            return false;

        Role role = this.roleService.getRole(roleName);
        this.userService.insertUser(username, SCryptUtil.scrypt(password, 32768, 8, 1), role);
        return true;
    }

    // Signs in a user by validating the provided username and password.
    // The given user must exist and should not already have an active session token.
    // Returns the API Key if the user exists and credentials are correct, null otherwise.

    public String signIn(String username, String password) {
        if (Boolean.FALSE.equals(this.userService.checkIfUserExists(username)))
            return null;

        User user = this.userService.getUserByUsername(username);
        if (!SCryptUtil.check(password, user.getPassword()))
            return null;

        return Boolean.TRUE.equals(TokenStore.isUserLogged(username))
            ? TokenStore.getTokenByUsername(username).apiKey()
            : TokenGenerator.createAndStore(username, user.getRole().getName()).apiKey();
    }
}