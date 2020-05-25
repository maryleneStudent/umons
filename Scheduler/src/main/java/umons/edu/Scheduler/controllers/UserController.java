package umons.edu.Scheduler.controllers;

import java.util.List;

import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import umons.edu.Scheduler.models.ApplicationUser;
import umons.edu.Scheduler.repositories.ApplicationUserRepository;

@RestController
@RequestMapping("/users")
public class UserController {

    private ApplicationUserRepository applicationUserRepository;
    private BCryptPasswordEncoder bCryptPasswordEncoder;

    public UserController(ApplicationUserRepository applicationUserRepository,
                          BCryptPasswordEncoder bCryptPasswordEncoder) {
        this.applicationUserRepository = applicationUserRepository;
        this.bCryptPasswordEncoder = bCryptPasswordEncoder;
    }

    @PostMapping("/sign-up")
    public ApplicationUser signUp(@RequestBody ApplicationUser user) {
        user.setPassword(bCryptPasswordEncoder.encode(user.getPassword()));
        String username = user.getUsername();
        if (applicationUserRepository.findByUsername(username) == null) {
        	return applicationUserRepository.save(user);
        } else {
        	System.err.println("Unable to register user. User already exists");
        	return null;
        }
        	
    }
    
    @GetMapping("/me")
    public ApplicationUser getCurrentUser(@AuthenticationPrincipal ApplicationUser user) {
        return user;
    }
    
    @GetMapping
    public List<ApplicationUser> getUsers() {
    	return applicationUserRepository.findAll();
    }
}