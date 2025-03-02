package netwatch.monitor.services;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import lombok.extern.java.Log;
import netwatch.monitor.entities.Observation;
import netwatch.monitor.entities.ObservationId;
import netwatch.monitor.entities.Utility;
import netwatch.monitor.requests.IRequest;
import netwatch.monitor.requests.RequestCreator;
import netwatch.monitor.utils.AlertMessage;

// This class is responsible for monitoring the state of utilities.
// It scans periodically (each second) a set of utilities and, based on detected events, sends notifications.
// Notifications are sent to the interested microservices by RabbitMQ.
// @author Antonio Scardace
// @version 1.0

@Log
@Service
public class MonitorService {

    private static final String SET_NAME = "offline-utilities";

    private final RestTemplate restTemplate;
    private final RabbitMqProducerService rabbitManager;
    private final RedisService redisService;

    @Autowired
    public MonitorService(RestTemplate restTemplate, RabbitMqProducerService rabbitManager, RedisService redisService) {
        this.restTemplate = restTemplate;
        this.rabbitManager = rabbitManager;
        this.redisService = redisService;
    }

    private Boolean checkUtility(Utility utility) {
        IRequest req = RequestCreator.getRequest(utility.getType());
        return req.request(utility.getAddress());
    }

    private void updateOfflineUtilities(ObservationId observation, Boolean backOnline) {
        if (Boolean.TRUE.equals(backOnline)) this.redisService.delete(SET_NAME, observation);
        else this.redisService.insert(SET_NAME, observation);
    }

    // Fetches utility observations from the external API specified by the MANAGER_API_URL environment variable.
    // Logs a warning if the API endpoint cannot be reached and return an empty list.
    // Returns the list of observations, if any.

    private List<Observation> fetchUtilities() {
        try {
            String urlApi = System.getenv("MANAGER_API_URL");
            ResponseEntity<Observation[]> responseEntity = restTemplate.getForEntity(urlApi, Observation[].class);
            Observation[] observationsArray = responseEntity.getBody();
            return observationsArray == null ? Collections.emptyList() : Arrays.asList(observationsArray);
        }
        catch (RestClientException e) {
            log.warning("API endpoint cannot be reached.");
            return Collections.emptyList();
        }
    }

    // Performs a single scan for a given utility observation, checking its connectivity status.
    // If the utility is online and was offline in the previous scans, a Solve Message is send.
    // If the utility is offline and was not offline in the previous scans, a Resolution Message is send.

    private void singleScan(Observation observation) {
        Boolean isOnline = this.checkUtility(observation.getUtility());
        Boolean wasOffline = this.redisService.contains(SET_NAME, observation.getObservationId());

        if (Boolean.compare(isOnline, wasOffline) == 0) {
            this.rabbitManager.send(AlertMessage.generate(observation, isOnline));
            this.updateOfflineUtilities(observation.getObservationId(), wasOffline);
        }
    }

    // Scheduled method to periodically scan utility observations for connectivity status.
    // Fetches utility observations and performs a single scan for each observation concurrently.
    
    @Scheduled(fixedDelay = 1000)
    public void scanUtilities() {
        CompletableFuture<?>[] futures = this.fetchUtilities().stream()
            .map(utility -> CompletableFuture.runAsync(() -> singleScan(utility)))
            .toArray(CompletableFuture[]::new);
        CompletableFuture.allOf(futures).join();
    }
}