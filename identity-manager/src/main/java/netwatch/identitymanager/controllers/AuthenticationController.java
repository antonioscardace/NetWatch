package netwatch.identitymanager.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import netwatch.identitymanager.services.AuthenticationService;

// This class is a REST controller for handling authentication-related HTTP requests.
// It maps those requests to the "/api/authentication" context path.
// @author Antonio Scardace
// @version 1.0

@RestController
@RequestMapping("/api/authentication")
public class AuthenticationController {

    private final AuthenticationService authentication;

    @Autowired
    public AuthenticationController(AuthenticationService authentication) {
        this.authentication = authentication;
    }

    @GetMapping(path="/role/{token}")
    public String getRoleByToken(@PathVariable("token") String apiKey) {
        return this.authentication.getUserRole(apiKey);
    }

    @GetMapping(path="/validate/{token}")
    public Boolean validateToken(@PathVariable("token") String apiKey) {
        return Boolean.TRUE.equals(this.authentication.isTokenValid(apiKey));
    }

    @PostMapping(path="/signin")
    public String signIn(@RequestParam("user") String username, @RequestParam("psw") String password) {
        return this.authentication.signIn(username, password);
    }

    @PostMapping(path="/signup")
    public ResponseEntity<Void> signUp(@RequestParam("user") String username, @RequestParam("psw") String password, @RequestParam("role") String roleName) {
        return Boolean.TRUE.equals(this.authentication.signUp(username, password, roleName))
            ? ResponseEntity.ok().build()
            : ResponseEntity.status(400).build();
    }
}