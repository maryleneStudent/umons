package umons.edu.Scheduler.controllers;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import umons.edu.Scheduler.models.Appointment;
import umons.edu.Scheduler.models.Planner;
import umons.edu.Scheduler.services.AppointmentService;
import umons.edu.Scheduler.services.PlannerService;

@RestController
public class AppointmentController {
	@Autowired
	private PlannerService plannerService;
	
	@Autowired
	private AppointmentService appointmentService;
	
	@PostMapping(value = "appointment/create", consumes = MediaType.APPLICATION_JSON_VALUE)
	public Appointment insert(@RequestBody Appointment appointment) {
		Optional<Planner> optPlanner = plannerService.findById(appointment.getPlannerId());
		if (optPlanner.isPresent()) {
			Planner planner = optPlanner.get();
			planner.getRemainingDays().get(appointment.getDate()).getEntries().removeIf(h -> (h.equals(appointment.getHour())));
			plannerService.update(planner);
		}
		return appointmentService.insert(appointment);
	}
	
	@GetMapping(value = "doctor/{id}/appointments", produces = MediaType.APPLICATION_JSON_VALUE)
	public Map<String, List<Appointment>> findAppointmentsForDoctor(@PathVariable("id") String doctorId) {
		List<Appointment> appointmentList = appointmentService.findByDoctorId(doctorId);
		return createMapFromList(appointmentList);
	}

	private Map<String, List<Appointment>> createMapFromList(List<Appointment> appointmentList) {
		Map<String, List<Appointment>> appointmentMap = new LinkedHashMap<>();
		for (Appointment app : appointmentList) {
			if (appointmentMap.get(app.getDate()) == null) {
				appointmentMap.put(app.getDate(), new ArrayList<>());
			}
			appointmentMap.get(app.getDate()).add(app);
		}
		return appointmentMap;
	}
	
	@GetMapping(value = "patient/{id}/appointments", produces = MediaType.APPLICATION_JSON_VALUE)
	public Map<String, List<Appointment>> findAppointmentForLoggedInUser(@PathVariable("id") String userId) {
		List<Appointment> appointmentList = appointmentService.findByUserId(userId);
		return createMapFromList(appointmentList);
	}

}
