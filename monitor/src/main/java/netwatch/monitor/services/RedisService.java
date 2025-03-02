package netwatch.monitor.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import netwatch.monitor.entities.ObservationId;

import java.util.Set;

// This class is a service component responsible for managing the interaction with Redis for storing and retrieving data.
// The Redis data type handled by the class is the Set.
// @author Antonio Scardace
// @version 1.0

@Service
public class RedisService {

    private RedisTemplate<String, ObservationId> redisTemplate;

    @Autowired
    public RedisService(RedisTemplate<String, ObservationId> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    public Boolean contains(String setName, ObservationId value) {
        return this.redisTemplate.opsForSet().isMember(setName, value);
    }

    public Boolean insert(String setName, ObservationId value) {
        return this.redisTemplate.opsForSet().add(setName, value) != null;
    }

    public Boolean delete(String setName, ObservationId value) {
        return this.redisTemplate.opsForSet().remove(setName, value) != null;
    }

    public Set<ObservationId> getAllByName(String setName) {
        return this.redisTemplate.opsForSet().members(setName);
    }
}