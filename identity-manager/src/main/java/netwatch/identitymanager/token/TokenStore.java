package netwatch.identitymanager.token;

import org.springframework.beans.factory.annotation.Autowired;

import netwatch.identitymanager.services.CacheService;

// Static class for storing and managing valid tokens.
// A token is identified in the Redis cache by its API Key.
// Each token has a lifetime of 3 days (4320 minutes).
// Expired tokens are automatically removed to avoid Replay Attack, thanks to always fresh API Keys.
// @author Antonio Scardace
// @version 1.0

public class TokenStore {

    private static CacheService cacheService;
    private static final long VALIDITY_MINS = 4320;
    private static final String TOKENS_CACHE_KEY = "tokens:";
    private static final String USERS_CACHE_KEY = "users:";

    private TokenStore() {
        
    }

    @Autowired
    public static void setCacheService(CacheService cacheService) {
        TokenStore.cacheService = cacheService;
    }

    public static Boolean isUserLogged(String username) {
        return TokenStore.cacheService.get(USERS_CACHE_KEY + username) != null;
    }

    public static Boolean isTokenPresent(String apiKey) {
        return TokenStore.cacheService.get(TOKENS_CACHE_KEY + apiKey) != null;
    }

    public static Token getTokenByApiKey(String apiKey) {
        return TokenStore.cacheService.get(TOKENS_CACHE_KEY + apiKey);
    }

    public static Token getTokenByUsername(String username) {
        return TokenStore.cacheService.get(USERS_CACHE_KEY + username);
    }

    public static void store(Token token) {
        TokenStore.cacheService.insert(USERS_CACHE_KEY + token.username(), token, VALIDITY_MINS);
        TokenStore.cacheService.insert(TOKENS_CACHE_KEY + token.apiKey(), token, VALIDITY_MINS);
    }
}