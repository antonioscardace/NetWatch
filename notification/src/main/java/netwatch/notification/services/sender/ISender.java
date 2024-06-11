package netwatch.notification.services.sender;

import java.io.IOException;

import jakarta.mail.MessagingException;

// Interface representing a base for sending messages to recipients.
// @author Antonio Scardace
// @version 1.0

public interface ISender {
    public void send(String referent, String message) throws IOException, MessagingException;
}