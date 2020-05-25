package umons.edu.Scheduler.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import umons.edu.Scheduler.models.Journey;
import umons.edu.Scheduler.models.Planner;
import umons.edu.Scheduler.services.PlannerService;

@RestController
public class PlannerController {

	@Autowired
	private PlannerService plannerService;

	@PostMapping(value = "planners/save", consumes = MediaType.APPLICATION_JSON_VALUE)
	public Planner save(@RequestBody Planner planner) {
		return plannerService.save(planner);
	}

	@PatchMapping(value = "planners/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
	public Planner update(@PathVariable("id") String id, @RequestBody Planner planner) {
		for (String key : planner.getAppointments().keySet()) {
			Journey journey = planner.getAppointments().get(key);
			if (journey != null && !journey.getEntries().isEmpty()) {
				planner.getRemainingDays().get(key).getEntries().removeAll(journey.getEntries());
			}

		}
		return plannerService.update(planner);
	}

	@GetMapping(value = "planners", produces = MediaType.APPLICATION_JSON_VALUE)
	public List<Planner> listPlanner() {
		return plannerService.findAll();
	}

	@GetMapping(value = "planners/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
	public Planner findById(@PathVariable("id") String id) {
		return plannerService.findById(id).get();
	}

	@DeleteMapping(value = "planners/{id}/delete")
	public void delete(@PathVariable("id") String id) {
		plannerService.delete(id);
	}
}
