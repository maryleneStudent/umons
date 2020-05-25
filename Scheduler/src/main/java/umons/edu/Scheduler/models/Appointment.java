package umons.edu.Scheduler.models;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import lombok.Data;

@Data
@Document("Appointments")
public class Appointment {
	@Id
	private String id;
	private String plannerId;
	private String doctorId;
	private String userId;
	private Person person;
	private String date;
	private String hour;
	
}
