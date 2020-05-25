package umons.edu.Scheduler.models;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.mongodb.core.mapping.Document;

import lombok.Data;

@Document("Journey")
@Data
public class Journey {
	private List<String> entries = new ArrayList<>();
	
}
