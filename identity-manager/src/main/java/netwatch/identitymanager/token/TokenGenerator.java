package netwatch.identitymanager.token;

import org.apache.commons.lang3.RandomStringUtils;

// Static class for generating new tokens by User's username and role.
// @author Antonio Scardace
// @version 1.0

public class TokenGenerator {

    private TokenGenerator() {

    }

    private static String generateNonce() {
        return RandomStringUtils.randomAlphanumeric(32);
    }

    private static Token tokenBuild(String username, String role) {
        return new Token(
            username,
            role,
            TokenGenerator.generateNonce()
        );
    }
    
    public static Token createAndStore(String username, String role) {
        Token jwt = TokenGenerator.tokenBuild(username, role);
        TokenStore.store(jwt);
        return TokenStore.getTokenByApiKey(jwt.apiKey());
    }
}