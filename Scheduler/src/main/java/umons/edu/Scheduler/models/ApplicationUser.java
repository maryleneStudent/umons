package umons.edu.Scheduler.models;

import org.springframework.data.annotation.Id;

import lombok.Data;

@Data
public class ApplicationUser {
	@Id
	private String id;
	private Person person;
    private String username;
    private String password;
    

}