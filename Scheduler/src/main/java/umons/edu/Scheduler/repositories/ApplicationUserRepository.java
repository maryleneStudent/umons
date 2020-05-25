package umons.edu.Scheduler.repositories;

import org.springframework.data.mongodb.repository.MongoRepository;

import umons.edu.Scheduler.models.ApplicationUser;

public interface ApplicationUserRepository extends MongoRepository<ApplicationUser, String> {
    ApplicationUser findByUsername(String username);
}
