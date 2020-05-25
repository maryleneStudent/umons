package umons.edu.Scheduler.models;

import org.springframework.data.mongodb.core.mapping.Document;

import lombok.Data;

@Document("Person")
@Data
public class Person {
	private String lastname;
	private String firstname;
}
