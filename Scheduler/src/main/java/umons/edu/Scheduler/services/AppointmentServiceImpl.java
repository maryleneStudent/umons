package umons.edu.Scheduler.services;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import umons.edu.Scheduler.models.Appointment;
import umons.edu.Scheduler.repositories.AppointmentRepository;

@Service
public class AppointmentServiceImpl implements AppointmentService {
	@Autowired
    private AppointmentRepository appointmentRepository;
	
	@Override
	public List<Appointment> findAll() {
		return appointmentRepository.findAll();
	}

	@Override
	public Appointment insert(Appointment appointment) {
		return appointmentRepository.insert(appointment);
	}

	@Override
	public void delete(String appointmentId) {
		appointmentRepository.deleteById(appointmentId);
		
	}

	@Override
	public Appointment update(Appointment appointment) {
		return appointmentRepository.save(appointment);
	}

	@Override
	public Optional<Appointment> findById(String id) {
		return appointmentRepository.findById(id);
	}
	
	@Override
	public List<Appointment> findByDoctorId(String doctorId) {
		return appointmentRepository.findByDoctorIdOrderByHourAsc(doctorId);
	}
	
	@Override
	public List<Appointment> findByUserId(String userId) {
		return appointmentRepository.findByUserIdOrderByHourAsc(userId);
	}

}
