package netwatch.gateway.filters;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import com.google.common.util.concurrent.RateLimiter;
import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;

import lombok.extern.java.Log;

// This class is a Zuul pre-filter responsible for authenticating and authorizing incoming requests.
// It verifies the validity of a token by requesting an Authentication Service.
// It applies the RBAC Pattern by requesting an Authorization Service.
// It includes rate limiting to prevent abuse of the service.
// @author Antonio Scardace
// @version 1.0

@Log
@Component
public class AuthFilter extends ZuulFilter {

    private final RestTemplate restTemplate;
    private final String authenticationUrl = System.getenv("AUTHN_URL");
    private final String authorizationUrl = System.getenv("AUTHZ_URL");
    private static RateLimiter rateLimiter = RateLimiter.create(10);

    @Autowired
    public AuthFilter(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public String filterType() {
        return "pre";
    }

    @Override
    public int filterOrder() {
        return 2;
    }

    @Override
    public boolean shouldFilter() {
        return true;
    }

    private boolean checkAuthorization(String userRole, String method, String object) {
        String authUrl = this.authorizationUrl + "/check" + "?role=" + userRole + "&method=" + method + "&object=" + object;
        return "true".equals(restTemplate.getForObject(authUrl, String.class));
    }

    private boolean isTokenValid(String apiKey) {
        String url = this.authenticationUrl + "/validate/" + apiKey;
        return "true".equals(restTemplate.getForObject(url, String.class));
    }

    private String getUserRole(String apiKey) {
        String url = this.authenticationUrl + "/role/" + apiKey;
        return restTemplate.getForObject(url, String.class);
    }    

    // Zuul pre-filter for authentication and authorization.
    // Checks if the rate limit has been exceeded. If so, responds with a 429 status code.
    // Otherwise, proceeds with the authentication and authorization checks.
    // If the request is a valid sign-in or sign-up operation the filter allows the request to proceed.
    // If the request has a valid API Key and is authorized, the filter allows the request to proceed.
    // Otherwise, sets a Zuul response with a 403 Forbidden status, indicating a "Forbidden Request".
    // The user authorization verification is based on its role, HTTP method, and requested route.

    @Override
    public Object run() {
        RequestContext context = RequestContext.getCurrentContext();

        if (!rateLimiter.tryAcquire(1)) {
            context.setSendZuulResponse(false);
            context.setResponseStatusCode(429);
            context.setResponseBody("Rate limit exceeded.");
            
            log.warning("Rate limit exceeded.");
            return null;
        }
        
        String apiKey = context.getRequest().getHeader("X-API-Key");
        String method = context.getRequest().getMethod();
        String uri = context.getRequest().getRequestURI();
        String route = uri.substring(context.getRequest().getContextPath().length());

        List<String> allowedRoutes = Arrays.asList(
            "/authentication/signin",
            "/authentication/signup"
        );

        if ("POST".equals(method) && allowedRoutes.contains(route))
            return null;

        if (apiKey != null &&
            this.isTokenValid(apiKey) &&
            this.checkAuthorization(this.getUserRole(apiKey), method, route))
            return null;

        context.setSendZuulResponse(false);
        context.setResponseStatusCode(403);
        context.setResponseBody("Forbidden Request.");
        
        log.warning("Forbidden Request: " + method + " " + route);
        return null;
    }
}
