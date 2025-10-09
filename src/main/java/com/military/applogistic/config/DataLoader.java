package com.military.applogistic.config;

import com.military.applogistic.entity.*;
import com.military.applogistic.repository.*;
import com.military.applogistic.service.TransportSetCalculator;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Component
@RequiredArgsConstructor
@Slf4j
public class DataLoader implements CommandLineRunner {

    private final UserRepository userRepository;
    private final VehicleRepository vehicleRepository;
    private final TransportSetRepository transportSetRepository;
    private final RouteRepository routeRepository;
    private final PasswordEncoder passwordEncoder;
    private final TransportSetCalculator transportSetCalculator;

    @Value("${data.loader.enabled:true}")
    private boolean enabled;

    @Override
    public void run(String... args) throws Exception {
        if (!enabled) {
            log.info("Data loader disabled for this environment");
            return;
        }
        loadInitialData();
    }

    private void loadInitialData() {
        if (userRepository.count() == 0) {
            createUsers();
        }

        if (vehicleRepository.count() == 0) {
            createVehicles();
        }

        if (transportSetRepository.count() == 0) {
            createTransportSets();
        }

        if (routeRepository.count() == 0 && transportSetRepository.count() > 0) {
            createSampleRoutes();
        }

        log.info("Initial data loaded successfully");
    }

    private void createUsers() {
        User admin = new User();
        admin.setUsername("admin");
        admin.setPassword(passwordEncoder.encode("admin123"));
        admin.setEmail("admin@military.gov.pl");
        admin.setRole(User.Role.ADMIN);
        admin.setFirstName("Administrator");
        admin.setLastName("System");
        userRepository.save(admin);

        User operator = new User();
        operator.setUsername("operator");
        operator.setPassword(passwordEncoder.encode("password123"));
        operator.setEmail("operator@military.gov.pl");
        operator.setRole(User.Role.OPERATOR);
        operator.setFirstName("Jan");
        operator.setLastName("Kowalski");
        userRepository.save(operator);

        User driver = new User();
        driver.setUsername("driver");
        driver.setPassword(passwordEncoder.encode("password123"));
        driver.setEmail("driver@military.gov.pl");
        driver.setRole(User.Role.DRIVER);
        driver.setFirstName("Piotr");
        driver.setLastName("Nowak");
        userRepository.save(driver);

        log.info("Created {} users", userRepository.count());
    }

    private void createVehicles() {
        // CIĄGNIKI (DMC = maksymalna masa z ładunkiem, nie masa pustego ciągnika!)
        Vehicle truck1 = new Vehicle();
        truck1.setReferenceNumber("TRK-001");
        truck1.setModel("MAN TGX 26.480");
        truck1.setType(Vehicle.VehicleType.TRANSPORTER);
        truck1.setTotalWeightKg(26000); // DMC - 40% to ~10.4t pustego ciągnika
        truck1.setHeightCm(350);
        truck1.setMaxAxleLoadKg(11500);
        truck1.setActive(true);
        truck1.setCanDriveAlone(false);
        truck1.setVehicleCategory("TRUCK");
        vehicleRepository.save(truck1);

        Vehicle truck2 = new Vehicle();
        truck2.setReferenceNumber("TRK-002");
        truck2.setModel("Mercedes Actros 2545");
        truck2.setType(Vehicle.VehicleType.TRANSPORTER);
        truck2.setTotalWeightKg(25000); // DMC
        truck2.setHeightCm(340);
        truck2.setMaxAxleLoadKg(11000);
        truck2.setActive(true);
        truck2.setCanDriveAlone(false);
        truck2.setVehicleCategory("TRUCK");
        vehicleRepository.save(truck2);

        // ŁADUNKI NA NACZEPĘ (ciężkie)
        Vehicle tank1 = new Vehicle();
        tank1.setReferenceNumber("CRG-001");
        tank1.setModel("Leopard 2A5");
        tank1.setType(Vehicle.VehicleType.CARGO);
        tank1.setTotalWeightKg(62000);
        tank1.setHeightCm(300);
        tank1.setMaxAxleLoadKg(15500);
        tank1.setActive(true);
        tank1.setCanDriveAlone(false);
        tank1.setVehicleCategory("MILITARY_VEHICLE");
        vehicleRepository.save(tank1);

        Vehicle howitzer1 = new Vehicle();
        howitzer1.setReferenceNumber("CRG-002");
        howitzer1.setModel("AHS Krab");
        howitzer1.setType(Vehicle.VehicleType.CARGO);
        howitzer1.setTotalWeightKg(48000);
        howitzer1.setHeightCm(320);
        howitzer1.setMaxAxleLoadKg(12000);
        howitzer1.setActive(true);
        howitzer1.setCanDriveAlone(false);
        howitzer1.setVehicleCategory("MILITARY_VEHICLE");
        vehicleRepository.save(howitzer1);

        // POJAZDY STANDALONE
        Vehicle rosomak = new Vehicle();
        rosomak.setReferenceNumber("CRG-003");
        rosomak.setModel("KTO Rosomak");
        rosomak.setType(Vehicle.VehicleType.CARGO);
        rosomak.setTotalWeightKg(22000);
        rosomak.setHeightCm(280);
        rosomak.setMaxAxleLoadKg(8000);
        rosomak.setActive(true);
        rosomak.setCanDriveAlone(true);
        rosomak.setVehicleCategory("STANDALONE");
        vehicleRepository.save(rosomak);

        Vehicle sprinter = new Vehicle();
        sprinter.setReferenceNumber("CRG-004");
        sprinter.setModel("Mercedes Sprinter 516 CDI");
        sprinter.setType(Vehicle.VehicleType.CARGO);
        sprinter.setTotalWeightKg(3500);
        sprinter.setHeightCm(270);
        sprinter.setMaxAxleLoadKg(1750);
        sprinter.setActive(true);
        sprinter.setCanDriveAlone(true);
        sprinter.setVehicleCategory("STANDALONE");
        vehicleRepository.save(sprinter);

        Vehicle humvee = new Vehicle();
        humvee.setReferenceNumber("CRG-005");
        humvee.setModel("HMMWV Humvee");
        humvee.setType(Vehicle.VehicleType.CARGO);
        humvee.setTotalWeightKg(4500);
        humvee.setHeightCm(185);
        humvee.setMaxAxleLoadKg(2250);
        humvee.setActive(true);
        humvee.setCanDriveAlone(true);
        humvee.setVehicleCategory("STANDALONE");
        vehicleRepository.save(humvee);

        log.info("Created {} vehicles", vehicleRepository.count());
    }

    private void createTransportSets() {
        var transporters = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.TRANSPORTER, true);
        var cargo = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.CARGO, true);

        if (transporters.isEmpty() || cargo.isEmpty()) {
            return;
        }

        Vehicle manTruck = transporters.stream()
                .filter(v -> v.getModel().contains("MAN"))
                .findFirst().orElse(transporters.get(0));

        Vehicle leopard = cargo.stream()
                .filter(v -> v.getModel().contains("Leopard"))
                .findFirst().orElse(null);

        Vehicle rosomak = cargo.stream()
                .filter(v -> v.getModel().contains("Rosomak"))
                .findFirst().orElse(null);

        Vehicle sprinter = cargo.stream()
                .filter(v -> v.getModel().contains("Sprinter"))
                .findFirst().orElse(null);

        // ZESTAW 1: Czołg na naczepie
        if (leopard != null) {
            TransportSet set1 = new TransportSet();
            set1.setTransporter(manTruck);
            set1.setCargo(leopard);
            set1.setDescription("Transport czołgu Leopard 2A5");
            transportSetCalculator.calculateTransportParameters(set1);
            transportSetRepository.save(set1);

            log.info("Utworzono: {}", set1.getDescription());
            log.info("  Masa: {}kg = ciągnik {}kg + naczepa ~15t + ładunek {}kg",
                    set1.getTotalWeightKg(),
                    (int)(manTruck.getTotalWeightKg() * 0.4),
                    leopard.getTotalWeightKg());
        }

        // ZESTAW 2: Rosomak standalone
        if (rosomak != null) {
            TransportSet set2 = new TransportSet();
            set2.setTransporter(manTruck);
            set2.setCargo(rosomak);
            set2.setDescription("KTO Rosomak - samojezdny");
            transportSetCalculator.calculateTransportParameters(set2);
            transportSetRepository.save(set2);

            log.info("Utworzono: {}", set2.getDescription());
            log.info("  Masa: {}kg (pojazd jedzie sam)", set2.getTotalWeightKg());
        }

        // ZESTAW 3: Lekki pojazd
        if (sprinter != null) {
            TransportSet set3 = new TransportSet();
            set3.setTransporter(manTruck);
            set3.setCargo(sprinter);
            set3.setDescription("Sprinter - lekki (auto-akceptacja)");
            transportSetCalculator.calculateTransportParameters(set3);
            transportSetRepository.save(set3);

            log.info("Utworzono: {}", set3.getDescription());
            log.info("  Masa: {}kg (≤5t - bez walidacji)", set3.getTotalWeightKg());
        }

        log.info("Created {} transport sets", transportSetRepository.count());
    }

    private void createSampleRoutes() {
        var transportSets = transportSetRepository.findAll();
        if (transportSets.isEmpty()) {
            return;
        }

        Route route1 = new Route();
        route1.setStartAddress("Warszawa, Polska");
        route1.setEndAddress("Poznań, Polska");
        route1.setStartLatitude(52.2297);
        route1.setStartLongitude(21.0122);
        route1.setEndLatitude(52.4064);
        route1.setEndLongitude(16.9252);
        route1.setTransportSet(transportSets.get(0));
        route1.setCreatedByUsername("operator");
        route1.setRouteDataJson("{}");
        routeRepository.save(route1);

        log.info("Created {} sample routes", routeRepository.count());
    }
}