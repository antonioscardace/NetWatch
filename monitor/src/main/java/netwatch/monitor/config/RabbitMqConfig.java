package netwatch.monitor.config;

import org.springframework.amqp.core.FanoutExchange;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

// This class configures a Fanout Exchange for broadcasting messages to all bound queues.
// @author Antonio Scardace
// @version 1.0

@Configuration
public class RabbitMqConfig {
    
    private static final String EXCHANGE_NAME = System.getenv("RABBITMQ_EXCHANGE");

    @Bean
    public FanoutExchange fanoutExchange() {
        return new FanoutExchange(EXCHANGE_NAME);
    }
}