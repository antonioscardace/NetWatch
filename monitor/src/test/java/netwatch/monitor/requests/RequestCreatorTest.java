package netwatch.monitor.requests;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

// Unit tests for the {@link RequestCreator} class.
// The tests examine both successful and unsuccessful scenarios.
// @author Antonio Scardace
// @version 1.0

class RequestCreatorTest {

    @Test
    void testGetRequest_Successful() {
        IRequest request = RequestCreator.getRequest("ip");
        assertNotNull(request);
        assertTrue(request instanceof PingRequest);
    }

    @Test
    void testGetRequest_UnknownType_ExceptionThrown() {
        String unknownType = "unknown";
        assertThrows(IllegalArgumentException.class, () -> RequestCreator.getRequest(unknownType));
    }
}