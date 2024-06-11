package netwatch.identitymanager.services;

import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import lombok.NonNull;
import netwatch.identitymanager.token.Token;

// This class is a service component responsible for managing the interaction with Redis for storing and retrieving data.
// @author Antonio Scardace
// @version 1.0

@Service
public class CacheService {

    private final RedisTemplate<String, Token> redisTemplate;

    @Autowired
    public CacheService(RedisTemplate<String, Token> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    public Boolean delete(@NonNull String key) {
        return this.redisTemplate.delete(key);
    }

    public Token get(@NonNull String key) {
        return this.redisTemplate.opsForValue().get(key);
    }

    public void insert(@NonNull String key, @NonNull Token value, long validity) {
        this.redisTemplate.opsForValue().set(key, value);
        this.redisTemplate.expire(key, validity, TimeUnit.MINUTES);
    }
}