package umons.edu.Scheduler.services;

import java.util.List;
import java.util.Optional;

import umons.edu.Scheduler.models.Planner;

public interface PlannerService {
	List<Planner> findAll();
	Planner save(Planner planner);
    void delete(String plannerId);
    Planner update(Planner planner);
    Optional<Planner> findById(String id);
}
