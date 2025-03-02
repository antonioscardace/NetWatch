package netwatch.notification.services;

import java.io.IOException;

import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import jakarta.mail.MessagingException;
import lombok.extern.java.Log;
import netwatch.notification.services.sender.ISender;
import netwatch.notification.services.sender.SenderCreator;

// This class is a simple RabbitMQ Consumer.
// Each time a message is consumed from the queue, it is sent to the respective contact.
// @author Antonio Scardace
// @version 1.0

@Log
@Service
public class RabbitMqConsumerService {
    
    private final SenderCreator senderCreator;

    @Autowired
    public RabbitMqConsumerService(SenderCreator senderCreator) {
        this.senderCreator = senderCreator;
    }

    private void sendNotification(String message) throws IOException, MessagingException, JSONException {
        JSONObject json = new JSONObject(message);
        ISender sender = senderCreator.getSender(json.getString("contact_type"));
        sender.send(json.getString("contact"), json.getString("message"));
    }

    @RabbitListener(queues = "#{queue.name}")
    public void receiveMessageAndNotify(String message) throws IOException, MessagingException, JSONException  {
        log.info("Message received: " + message);
        this.sendNotification(message);
    }
}