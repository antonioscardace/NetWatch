package netwatch.monitor.requests;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

// Unit tests for the {@link PingRequest} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

class PingRequestTest {

    @Test
    void testRequest_Successful_withLocalhost() {
        PingRequest pingRequest = new PingRequest();
        String ipAddress = "127.0.0.1";
        assertTrue(pingRequest.request(ipAddress));
    }

    @Test
    void testRequest_Successful_withName() {
        PingRequest pingRequest = new PingRequest();
        String ipAddress = "google.com";
        assertTrue(pingRequest.request(ipAddress));
    }

    @Test
    void testRequest_Unsuccessful_withUnvalidIp() {
        PingRequest pingRequest = new PingRequest();
        String invalidIpAddress = "1.1.";
        assertFalse(pingRequest.request(invalidIpAddress));
    }

    @Test
    void testRequest_Unsuccessful_withUnvalidName() {
        PingRequest pingRequest = new PingRequest();
        String invalidIpAddress = "https://google.com";
        assertFalse(pingRequest.request(invalidIpAddress));
    }
}
