package umons.edu.Scheduler;

import org.joda.time.LocalDate;
import org.joda.time.LocalTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import umons.edu.Scheduler.models.ApplicationUser;
import umons.edu.Scheduler.models.Person;
import umons.edu.Scheduler.models.Planner;
import umons.edu.Scheduler.repositories.ApplicationUserRepository;
import umons.edu.Scheduler.repositories.AppointmentRepository;
import umons.edu.Scheduler.repositories.PlannerRepository;

@Component
public class PlannerRunner implements CommandLineRunner {
	private static final String ADMIN = "admin";

	@Autowired
	private PlannerRepository repository;

	@Autowired
	private ApplicationUserRepository userRepo;
	
	@Autowired
	private AppointmentRepository appRepo;

	// Min appointment hour
	private static final int MIN_HOUR = 8;

	private static final int HALF_HOUR = 30;

	// Period between available time
	private static final int PERIOD = 30;

	// Max appointment hour
	private static final int MAX_HOUR = 19;

	public static Planner planner = new Planner();
	public static final LocalDate today = new LocalDate();

	@Override
	public void run(String... strings) throws Exception {
		repository.deleteAll();
		appRepo.deleteAll();
		if (userRepo.findByUsername(ADMIN) == null) {
			ApplicationUser admin = new ApplicationUser();
			Person person = new Person();
			person.setFirstname("Dr");
			person.setLastname("Maboule");
			admin.setPerson(person);
			admin.setUsername(ADMIN);
			admin.setPassword("nimda");
			userRepo.save(admin);
		}
		if (userRepo.findByUsername("mStudent") == null) {
			ApplicationUser user = new ApplicationUser();
			Person info = new Person();
			info.setLastname("Student");
			info.setFirstname("Marylene");
			user.setPerson(info);
			user.setUsername("mStudent");
			user.setPassword("123");
			userRepo.save(user);
		}
		ApplicationUser admin = userRepo.findByUsername(ADMIN);
		planner.setDoctorId(admin.getId());
		planner.setPerson(admin.getPerson());
		planner.addNonWorkingDay(3); // Add wednesday to non working days
		planner.addNonWorkingDay(6); // Add saturday to non working days
		planner.addNonWorkingDay(7); // Add sunday to non working days
		LocalDate currentDate = new LocalDate();

		addToAvailableDays(planner, today, true);
		int currentMonth = today.getMonthOfYear();
		// Compute 2 monthes from current month
		while (currentDate.getMonthOfYear() <= currentMonth + 2) {
			LocalDate newCurrentDate = currentDate.plusDays(1);
			currentDate = newCurrentDate;
			addToAvailableDays(planner, newCurrentDate, false);
		}
		repository.save(planner);
//		repository.findAll().forEach(System.out::println);

	}

	private static void addToAvailableDays(Planner planner, LocalDate newCurrentDate, boolean isToday) {
		if (planner.isWorkingDay(newCurrentDate)) {
			LocalTime currentTime = setDefaultTime();
			// Compute free schedule
			while (currentTime.getHourOfDay() < MAX_HOUR) {
				LocalTime newCurrentTime = currentTime.plusMinutes(PERIOD);
				currentTime = newCurrentTime;
				if (newCurrentTime.getHourOfDay() != MAX_HOUR) {
					planner.getJourneyForDate(
							newCurrentDate.toString("YYYY/MM/dd") + "-" + newCurrentDate.toString("EEEE dd MMMM YYYY"))
							.getEntries().add(newCurrentTime.toString("HH:mm"));
					planner.getRemainingDaysForDate(
							newCurrentDate.toString("YYYY/MM/dd") + "-" + newCurrentDate.toString("EEEE dd MMMM YYYY"))
							.getEntries().add(newCurrentTime.toString("HH:mm"));
				}
			}

		}
	}

	private static LocalTime setDefaultTime() {
		return new LocalTime(MIN_HOUR, HALF_HOUR, 0, 0);
	}
}
