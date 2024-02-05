package netwatch.identitymanager.token;

// Record for defining my own Token structure.
// The API Key field is used as a Nonce.
// @author Antonio Scardace
// @version 1.0

public record Token(String username, String role, String apiKey) {
    
}