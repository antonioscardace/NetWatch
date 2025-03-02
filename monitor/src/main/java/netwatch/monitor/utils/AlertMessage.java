package netwatch.monitor.utils;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.json.JSONObject;

import netwatch.monitor.entities.Observation;

// This static class provides methods for creating the alert message on two events.
// It can be an error message if the utility goes offline.
// It can be a resolution message if the utility returns online.
// This message is then attached with the utility referent contact info.
// @author Antonio Scardace
// @version 1.0

public class AlertMessage {

    private AlertMessage() {

    }

    private static String getErrorMessage(String address, String name, String timestamp) {
        return String.format("Hi 👋🏻, an error occurred on utility <b>%s</b> (%s) on %s ⚠️", name, address, timestamp);
    }

    private static String getResolutionMessage(String address, String name, String timestamp) {
        return String.format("Hi 👋🏻, the utility <b>%s</b> (%s) returned online on %s ✅", name, address, timestamp);
    }

    private static String getUtcTimestamp() { 
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"); 
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC")); 
        return dateFormat.format(new Date()) + " UTC";
    }

    public static String generate(Observation observation, Boolean backOnline) {
        String address = observation.getUtility().getAddress();
        String name = observation.getUtility().getName();
        String timestamp = AlertMessage.getUtcTimestamp();

        JSONObject message = new JSONObject();
        message.put("contact", observation.getContact().getValue());
        message.put("contact_type", observation.getContact().getType());
        message.put("message",
            Boolean.TRUE.equals(backOnline)
            ? AlertMessage.getResolutionMessage(address, name, timestamp)
            : AlertMessage.getErrorMessage(address, name, timestamp));

        return message.toString();
    }
}