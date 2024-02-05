package netwatch.notification.services.sender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;

// Implementation of {@link ISender} for handling messages directed to Emails.
// HTML is used for formatting messages.
// @author Antonio Scardace
// @version 1.0

@Service
public class EmailSender implements ISender {

    private final JavaMailSender mailSender;

    @Autowired
    public EmailSender(JavaMailSender mailSender) {
        this.mailSender = mailSender;
    }
    
    @Override
    public void send(String emailAddress, String text) throws MessagingException {
        MimeMessage message = mailSender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(message, true, "UTF-8");

        helper.setTo(emailAddress);
        helper.setSubject("NetWatch Notification");
        helper.setText(text, true);
        this.mailSender.send(message);
    }
}