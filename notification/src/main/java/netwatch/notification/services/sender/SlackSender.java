package netwatch.notification.services.sender;

import java.io.IOException;

import org.springframework.stereotype.Service;

import com.slack.api.Slack;
import com.slack.api.webhook.Payload;

// Implementation of {@link ISender} for handling messages directed to Slack chats.
// Slack doesn't support HTML for formatting messages. Main HTML tags are converted to Markdown.
// @author Antonio Scardace
// @version 1.0

@Service
public class SlackSender implements ISender {
    
    @Override
    public void send(String webhook, String message) throws IOException {
        Slack slack = Slack.getInstance();
        String webhookUrl = "https://hooks.slack.com/services/" + webhook;

        Payload payload = Payload.builder()
            .text(message.replace("<b>", "*")
                .replace("</b>", "*")
                .replace("<i>", "_")
                .replace("</i>", "_")
                .replace("<s>", "~")
                .replace("</s>", "~"))
            .build();
            
        slack.send(webhookUrl, payload);
    }
}