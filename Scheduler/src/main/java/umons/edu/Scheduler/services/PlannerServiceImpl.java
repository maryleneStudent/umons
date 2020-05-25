package umons.edu.Scheduler.services;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import umons.edu.Scheduler.models.Planner;
import umons.edu.Scheduler.repositories.PlannerRepository;

@Service
public class PlannerServiceImpl implements PlannerService {

	@Autowired
    private PlannerRepository plannerRepository;
	
	@Override
	public List<Planner> findAll() {
		return plannerRepository.findAll();
	}

	@Override
	public Planner save(Planner planner) {
		return plannerRepository.save(planner);
	}

	@Override
	public void delete(String plannerId) {
		plannerRepository.deleteById(plannerId);
		
	}

	@Override
	public Planner update(Planner planner) {
		return plannerRepository.save(planner);
	}
	
	public Optional<Planner> findById(String id) {
		return plannerRepository.findById(id);
	}

}
