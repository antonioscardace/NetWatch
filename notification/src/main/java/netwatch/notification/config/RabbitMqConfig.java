package netwatch.notification.config;

import org.springframework.amqp.core.AnonymousQueue;
import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.FanoutExchange;
import org.springframework.amqp.core.Queue;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

// This class defines a Fanout Exchange and an Anonymous Queue, binding the queue to the exchange.
// @author Antonio Scardace
// @version 1.0

@Configuration
public class RabbitMqConfig {
    
    private static final String EXCHANGE_NAME = System.getenv("RABBITMQ_EXCHANGE");

    @Bean
    public FanoutExchange fanoutExchange() {
        return new FanoutExchange(EXCHANGE_NAME);
    }

    @Bean
    public Queue queue() {
        return new AnonymousQueue();
    }

    @Bean
    public Binding binding(Queue queue, FanoutExchange fanoutExchange) {
        return BindingBuilder.bind(queue).to(fanoutExchange);
    }
}