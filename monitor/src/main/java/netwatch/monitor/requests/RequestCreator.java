package netwatch.monitor.requests;

// Factory class to create {@link IRequest} instances based on the type.
// Factory-Method Design Pattern used to make the Request component modular.
// @author Antonio Scardace
// @version 1.0

public class RequestCreator {

    private RequestCreator() {
        
    }

    public static IRequest getRequest(String type) {
        if (type.equals("ip")) return new PingRequest();
        throw new IllegalArgumentException("Unknown Request type " + type);
    }
}