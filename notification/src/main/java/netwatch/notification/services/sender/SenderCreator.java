package netwatch.notification.services.sender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

// Factory class to create {@link ISender} instances based on the contact type.
// Factory-Method Design Pattern used to make the Sender component modular.
// @author Antonio Scardace
// @version 1.0

@Service
public class SenderCreator {

    private final ApplicationContext applicationContext;

    @Autowired
    public SenderCreator(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    public ISender getSender(String contactType) {
        return switch (contactType) {
            case "email" -> this.applicationContext.getBean(EmailSender.class);
            case "slack" -> this.applicationContext.getBean(SlackSender.class);
            case "telegram" -> this.applicationContext.getBean(TelegramSender.class);
            default -> throw new IllegalArgumentException("Unknown Contact type " + contactType);
        };
    }
}