package umons.edu.Scheduler.services;

import java.util.List;
import java.util.Optional;

import umons.edu.Scheduler.models.Appointment;

public interface AppointmentService {
	List<Appointment> findAll();
	Appointment insert(Appointment appointment);
    void delete(String appointmentId);
    Appointment update(Appointment appointment);
    Optional<Appointment> findById(String id);
    List<Appointment> findByDoctorId(String doctorId);
    List<Appointment> findByUserId(String userId);
}
