package umons.edu.Scheduler.repositories;

import java.util.List;

import org.springframework.data.mongodb.repository.MongoRepository;

import umons.edu.Scheduler.models.Appointment;

public interface AppointmentRepository extends MongoRepository<Appointment, String> {
	List<Appointment> findByDoctorIdOrderByHourAsc(String doctorId);
	List<Appointment> findByUserIdOrderByHourAsc(String userId);
}
