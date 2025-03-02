package netwatch.monitor.services;

import org.springframework.amqp.core.AmqpTemplate;
import org.springframework.amqp.core.FanoutExchange;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import lombok.extern.java.Log;

// This class is a simple RabbitMQ Producer (Publisher) to a Fanout Exchange for broadcasting to all bound queues.
// @author Antonio Scardace
// @version 1.0

@Log
@Service
public class RabbitMqProducerService {

    private final AmqpTemplate rabbitTemplate;
    private final FanoutExchange fanoutExchange;

    @Autowired
    public RabbitMqProducerService(AmqpTemplate rabbitTemplate, FanoutExchange fanoutExchange) {
        this.rabbitTemplate = rabbitTemplate;
        this.fanoutExchange = fanoutExchange;
    }

    public void send(String message) {
        rabbitTemplate.convertAndSend(this.fanoutExchange.getName(), "", message);
        log.info("Message sent: " + message);
    }
}