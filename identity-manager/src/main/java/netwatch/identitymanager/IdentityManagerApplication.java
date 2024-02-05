package netwatch.identitymanager;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;

import netwatch.identitymanager.services.CacheService;
import netwatch.identitymanager.token.TokenStore;

@SpringBootApplication
public class IdentityManagerApplication {
	
	public static void main(String[] args) {
		ApplicationContext context = SpringApplication.run(IdentityManagerApplication.class, args);
		CacheService cacheService = context.getBean(CacheService.class);
        TokenStore.setCacheService(cacheService);
	}	
}