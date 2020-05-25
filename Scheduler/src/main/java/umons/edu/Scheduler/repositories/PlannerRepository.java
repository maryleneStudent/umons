package umons.edu.Scheduler.repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import umons.edu.Scheduler.models.Planner;

@Repository
public interface PlannerRepository extends MongoRepository<Planner,String> {

	    @Override
	    List<Planner> findAll();           // find all planner

	    Optional<Planner> findById(String id);           // find

}
