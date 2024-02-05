package netwatch.notification.services.sender;

import java.text.MessageFormat;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

// Implementation of {@link ISender} for handling messages directed to Telegram channels.
// HTML is used for formatting messages.
// @author Antonio Scardace
// @version 1.0

@Service
public class TelegramSender implements ISender {

    private final RestTemplate restTemplate;
    private final String token;

    @Autowired
    public TelegramSender(RestTemplate restTemplate) {
        this.token = System.getenv("TELEGRAM_TOKEN");
        this.restTemplate = restTemplate;
    }

    @Override
    public void send(String telegramChannel, String message) {
        String basicUrl = "https://api.telegram.org/bot{0}/sendMessage?chat_id={1}&text={2}&parse_mode=html";
        String urlApi = MessageFormat.format(basicUrl, this.token, telegramChannel, message);
        restTemplate.getForObject(urlApi, String.class);
    }
}