package netwatch.monitor.requests;

import java.io.IOException;
import java.net.InetAddress;

import lombok.extern.java.Log;

// Implementation of {@link IRequest} for handling ping-related operations.
// It supports IPv4 and IPv6 addresses. The use of their machine name is discouraged.
// The timeout (the time before the call aborts) is set to 4000ms (4 seconds).
// @author Antonio Scardace
// @version 1.0

@Log
public class PingRequest implements IRequest {
    
    private final Integer timeoutMs;

    public PingRequest() {
        this.timeoutMs = 4000;
    }

    @Override
    public Boolean request(String ip) {
        try {
            InetAddress address = InetAddress.getByName(ip);
            return address.isReachable(this.timeoutMs);
        }
        catch(IOException e) {
            log.warning(e.getMessage());
            return false;
        }
    }
}