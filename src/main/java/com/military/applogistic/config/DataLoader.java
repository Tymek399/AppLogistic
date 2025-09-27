package com.military.applogistic.config;

import com.military.applogistic.entity.*;
import com.military.applogistic.repository.*;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
@Slf4j
public class DataLoader implements CommandLineRunner {

    private final UserRepository userRepository;
    private final VehicleRepository vehicleRepository;
    private final TransportSetRepository transportSetRepository;
    private final RouteRepository routeRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    public void run(String... args) throws Exception {
        loadInitialData();
    }

    private void loadInitialData() {
        // Create users if not exist
        if (userRepository.count() == 0) {
            createUsers();
        }

        // Create vehicles if not exist
        if (vehicleRepository.count() == 0) {
            createVehicles();
        }

        // Create transport sets if not exist
        if (transportSetRepository.count() == 0) {
            createTransportSets();
        }

        // Create sample routes if not exist (after transport sets)
        if (routeRepository.count() == 0 && transportSetRepository.count() > 0) {
            createSampleRoutes();
        }

        log.info("Initial data loaded successfully");
    }

    private void createUsers() {
        // Create admin user
        User admin = new User();
        admin.setUsername("admin");
        admin.setPassword(passwordEncoder.encode("admin123"));
        admin.setEmail("admin@military.gov.pl");
        admin.setRole(User.Role.ADMIN);
        admin.setFirstName("Administrator");
        admin.setLastName("System");
        userRepository.save(admin);

        // Create operator user
        User operator = new User();
        operator.setUsername("operator");
        operator.setPassword(passwordEncoder.encode("password123"));
        operator.setEmail("operator@military.gov.pl");
        operator.setRole(User.Role.OPERATOR);
        operator.setFirstName("Jan");
        operator.setLastName("Kowalski");
        userRepository.save(operator);

        // Create driver user
        User driver = new User();
        driver.setUsername("driver");
        driver.setPassword(passwordEncoder.encode("password123"));
        driver.setEmail("driver@military.gov.pl");
        driver.setRole(User.Role.DRIVER);
        driver.setFirstName("Piotr");
        driver.setLastName("Nowak");
        userRepository.save(driver);

        // Create additional driver
        User driver2 = new User();
        driver2.setUsername("driver2");
        driver2.setPassword(passwordEncoder.encode("password123"));
        driver2.setEmail("driver2@military.gov.pl");
        driver2.setRole(User.Role.DRIVER);
        driver2.setFirstName("Marek");
        driver2.setLastName("Wiśniewski");
        userRepository.save(driver2);

        log.info("Created {} users", userRepository.count());
    }

    private void createVehicles() {
        // Transporters (Ciężarówki)
        Vehicle truck1 = new Vehicle();
        truck1.setModel("MAN TGX 26.480");
        truck1.setType(Vehicle.VehicleType.TRANSPORTER);
        truck1.setTotalWeightKg(26000);
        truck1.setHeightCm(350);
        truck1.setMaxAxleLoadKg(11500);
        vehicleRepository.save(truck1);

        Vehicle truck2 = new Vehicle();
        truck2.setModel("Mercedes Actros 2545");
        truck2.setType(Vehicle.VehicleType.TRANSPORTER);
        truck2.setTotalWeightKg(25000);
        truck2.setHeightCm(340);
        truck2.setMaxAxleLoadKg(11000);
        vehicleRepository.save(truck2);

        Vehicle truck3 = new Vehicle();
        truck3.setModel("Volvo FH16 750");
        truck3.setType(Vehicle.VehicleType.TRANSPORTER);
        truck3.setTotalWeightKg(28000);
        truck3.setHeightCm(360);
        truck3.setMaxAxleLoadKg(12000);
        vehicleRepository.save(truck3);

        // Cargo (Sprzęt wojskowy)
        Vehicle tank1 = new Vehicle();
        tank1.setModel("Leopard 2A5");
        tank1.setType(Vehicle.VehicleType.CARGO);
        tank1.setTotalWeightKg(62000);
        tank1.setHeightCm(300);
        tank1.setMaxAxleLoadKg(15500);
        vehicleRepository.save(tank1);

        Vehicle apc1 = new Vehicle();
        apc1.setModel("KTO Rosomak");
        apc1.setType(Vehicle.VehicleType.CARGO);
        apc1.setTotalWeightKg(22000);
        apc1.setHeightCm(280);
        apc1.setMaxAxleLoadKg(8000);
        vehicleRepository.save(apc1);

        Vehicle howitzer1 = new Vehicle();
        howitzer1.setModel("AHS Krab");
        howitzer1.setType(Vehicle.VehicleType.CARGO);
        howitzer1.setTotalWeightKg(48000);
        howitzer1.setHeightCm(320);
        howitzer1.setMaxAxleLoadKg(12000);
        vehicleRepository.save(howitzer1);

        Vehicle radar1 = new Vehicle();
        radar1.setModel("PILICA Radar");
        radar1.setType(Vehicle.VehicleType.CARGO);
        radar1.setTotalWeightKg(15000);
        radar1.setHeightCm(450);
        radar1.setMaxAxleLoadKg(7500);
        vehicleRepository.save(radar1);

        log.info("Created {} vehicles", vehicleRepository.count());
    }

    private void createTransportSets() {
        var transporters = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.TRANSPORTER, true);
        var cargo = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.CARGO, true);

        if (transporters.size() >= 3 && cargo.size() >= 4) {
            // Set 1: MAN + Leopard
            TransportSet set1 = new TransportSet();
            set1.setTransporter(transporters.get(0));
            set1.setCargo(cargo.get(0));
            set1.setDescription("Transport czołgu Leopard 2A5");
            set1.setTotalHeightCm(Math.max(transporters.get(0).getHeightCm(), cargo.get(0).getHeightCm()));
            set1.setTotalWeightKg(transporters.get(0).getTotalWeightKg() + cargo.get(0).getTotalWeightKg());
            set1.setMaxAxleLoadKg(Math.max(transporters.get(0).getMaxAxleLoadKg(), cargo.get(0).getMaxAxleLoadKg()));
            transportSetRepository.save(set1);

            // Set 2: Mercedes + Rosomak
            TransportSet set2 = new TransportSet();
            set2.setTransporter(transporters.get(1));
            set2.setCargo(cargo.get(1));
            set2.setDescription("Transport transportera Rosomak");
            set2.setTotalHeightCm(Math.max(transporters.get(1).getHeightCm(), cargo.get(1).getHeightCm()));
            set2.setTotalWeightKg(transporters.get(1).getTotalWeightKg() + cargo.get(1).getTotalWeightKg());
            set2.setMaxAxleLoadKg(Math.max(transporters.get(1).getMaxAxleLoadKg(), cargo.get(1).getMaxAxleLoadKg()));
            transportSetRepository.save(set2);

            // Set 3: Volvo + Krab
            TransportSet set3 = new TransportSet();
            set3.setTransporter(transporters.get(2));
            set3.setCargo(cargo.get(2));
            set3.setDescription("Transport haubicy Krab");
            set3.setTotalHeightCm(Math.max(transporters.get(2).getHeightCm(), cargo.get(2).getHeightCm()));
            set3.setTotalWeightKg(transporters.get(2).getTotalWeightKg() + cargo.get(2).getTotalWeightKg());
            set3.setMaxAxleLoadKg(Math.max(transporters.get(2).getMaxAxleLoadKg(), cargo.get(2).getMaxAxleLoadKg()));
            transportSetRepository.save(set3);

            // Set 4: MAN + Radar (high vehicle)
            TransportSet set4 = new TransportSet();
            set4.setTransporter(transporters.get(0));
            set4.setCargo(cargo.get(3));
            set4.setDescription("Transport radaru PILICA");
            set4.setTotalHeightCm(Math.max(transporters.get(0).getHeightCm(), cargo.get(3).getHeightCm()));
            set4.setTotalWeightKg(transporters.get(0).getTotalWeightKg() + cargo.get(3).getTotalWeightKg());
            set4.setMaxAxleLoadKg(Math.max(transporters.get(0).getMaxAxleLoadKg(), cargo.get(3).getMaxAxleLoadKg()));
            transportSetRepository.save(set4);

            log.info("Created {} transport sets", transportSetRepository.count());
        }
    }

    private void createSampleRoutes() {
        var transportSets = transportSetRepository.findAll();
        if (!transportSets.isEmpty()) {

            // Sample route 1: Warsaw to Poznan
            Route route1 = new Route();
            route1.setStartAddress("Warszawa, Polska");
            route1.setEndAddress("Poznań, Polska");
            route1.setStartLatitude(52.2297);
            route1.setStartLongitude(21.0122);
            route1.setEndLatitude(52.4064);
            route1.setEndLongitude(16.9252);
            route1.setTransportSet(transportSets.get(0));
            route1.setCreatedByUsername("operator");
            route1.setTotalDistanceKm(310.0);
            route1.setEstimatedTimeMinutes(240);
            route1.setRouteDataJson("{}");
            routeRepository.save(route1);

            // Sample route 2: Krakow to Gdansk (assigned to driver)
            Route route2 = new Route();
            route2.setStartAddress("Kraków, Polska");
            route2.setEndAddress("Gdańsk, Polska");
            route2.setStartLatitude(50.0647);
            route2.setStartLongitude(19.9450);
            route2.setEndLatitude(54.3520);
            route2.setEndLongitude(18.6466);
            route2.setTransportSet(transportSets.size() > 1 ? transportSets.get(1) : transportSets.get(0));
            route2.setCreatedByUsername("operator");
            route2.setAssignedDriverUsername("driver");
            route2.setStatus(Route.RouteStatus.ASSIGNED);
            route2.setTotalDistanceKm(540.0);
            route2.setEstimatedTimeMinutes(420);
            route2.setRouteDataJson("{}");
            route2.setAssignedAt(LocalDateTime.now().minusHours(2));
            routeRepository.save(route2);

            // Sample route 3: Wroclaw to Lublin (active)
            Route route3 = new Route();
            route3.setStartAddress("Wrocław, Polska");
            route3.setEndAddress("Lublin, Polska");
            route3.setStartLatitude(51.1079);
            route3.setStartLongitude(17.0385);
            route3.setEndLatitude(51.2465);
            route3.setEndLongitude(22.5684);
            route3.setTransportSet(transportSets.size() > 2 ? transportSets.get(2) : transportSets.get(0));
            route3.setCreatedByUsername("operator");
            route3.setAssignedDriverUsername("driver2");
            route3.setStatus(Route.RouteStatus.ACTIVE);
            route3.setTotalDistanceKm(380.0);
            route3.setEstimatedTimeMinutes(320);
            route3.setRouteDataJson("{}");
            route3.setAssignedAt(LocalDateTime.now().minusHours(1));
            route3.setStartedAt(LocalDateTime.now().minusMinutes(30));
            routeRepository.save(route3);

            log.info("Created {} sample routes", routeRepository.count());
        }
    }
}