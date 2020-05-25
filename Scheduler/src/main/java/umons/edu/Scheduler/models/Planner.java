package umons.edu.Scheduler.models;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.joda.time.LocalDate;
import org.springframework.data.annotation.Id;

import lombok.Data;

@Data
public class Planner {
	@Id
	private String id;
	private String doctorId;
	private Person person;
	private Map<String, Journey> availableDays = new LinkedHashMap<>();
	private Map<String, Journey> appointments = new LinkedHashMap<>();
	private Map<String, Journey> remainingDays = new LinkedHashMap<>();
	private List<Integer> nonWorkingDays = new ArrayList<>();
	
	public Map<String, Journey> getAvailableDays() {
		return availableDays;
	}
	
	public Map<String, Journey> getAppointments() {
		return appointments;
	}
	
	public Journey getJourneyForDate(String date) {
		Journey journey = availableDays.get(date);
		if (journey == null) {
			journey = new Journey();
			availableDays.put(date, journey);
		}
		return journey;
	}
	
	public Journey getRemainingDaysForDate(String date) {
		Journey journey = remainingDays.get(date);
		if (journey == null) {
			journey = new Journey();
			remainingDays.put(date, journey);
		}
		return journey;
	}
	
	public List<Integer> getNonWorkingDays() {
		return nonWorkingDays;
	}
	
	public void addNonWorkingDay(int day) {
		if (nonWorkingDays == null) {
			nonWorkingDays = new ArrayList<>();
		}
		nonWorkingDays.add(day);
	}
	
	public boolean isWorkingDay(LocalDate local) {
		int dow = local.getDayOfWeek();
		return !nonWorkingDays.contains(dow);
	}
}
