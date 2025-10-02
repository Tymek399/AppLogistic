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
        // TRANSPORTERY (Ciężarówki)
        Vehicle truck1 = new Vehicle();
        truck1.setModel("MAN TGX 26.480");
        truck1.setType(Vehicle.VehicleType.TRANSPORTER);
        truck1.setTotalWeightKg(26000);
        truck1.setHeightCm(350);
        truck1.setMaxAxleLoadKg(11500);
        truck1.setCanDriveAlone(false);
        truck1.setVehicleCategory("TRUCK");
        vehicleRepository.save(truck1);

        Vehicle truck2 = new Vehicle();
        truck2.setModel("Mercedes Actros 2545");
        truck2.setType(Vehicle.VehicleType.TRANSPORTER);
        truck2.setTotalWeightKg(25000);
        truck2.setHeightCm(340);
        truck2.setMaxAxleLoadKg(11000);
        truck2.setCanDriveAlone(false);
        truck2.setVehicleCategory("TRUCK");
        vehicleRepository.save(truck2);

        // CARGO NA NACZEPĘ (Ciężkie)
        Vehicle tank1 = new Vehicle();
        tank1.setModel("Leopard 2A5");
        tank1.setType(Vehicle.VehicleType.CARGO);
        tank1.setTotalWeightKg(62000);
        tank1.setHeightCm(300);
        tank1.setMaxAxleLoadKg(15500);
        tank1.setCanDriveAlone(false); // Wymaga naczepy
        tank1.setVehicleCategory("MILITARY_VEHICLE");
        vehicleRepository.save(tank1);

        Vehicle howitzer1 = new Vehicle();
        howitzer1.setModel("AHS Krab");
        howitzer1.setType(Vehicle.VehicleType.CARGO);
        howitzer1.setTotalWeightKg(48000);
        howitzer1.setHeightCm(320);
        howitzer1.setMaxAxleLoadKg(12000);
        howitzer1.setCanDriveAlone(false);
        howitzer1.setVehicleCategory("MILITARY_VEHICLE");
        vehicleRepository.save(howitzer1);

        // ✅ POJAZDY STANDALONE (Mogą jechać same)
        Vehicle rosomak = new Vehicle();
        rosomak.setModel("KTO Rosomak");
        rosomak.setType(Vehicle.VehicleType.CARGO);
        rosomak.setTotalWeightKg(22000);
        rosomak.setHeightCm(280);
        rosomak.setMaxAxleLoadKg(8000);
        rosomak.setCanDriveAlone(true); // ✅ Jedzie sam
        rosomak.setVehicleCategory("STANDALONE");
        vehicleRepository.save(rosomak);

        Vehicle radar1 = new Vehicle();
        radar1.setModel("PILICA Radar");
        radar1.setType(Vehicle.VehicleType.CARGO);
        radar1.setTotalWeightKg(15000);
        radar1.setHeightCm(450);
        radar1.setMaxAxleLoadKg(7500);
        radar1.setCanDriveAlone(true); // ✅ Jedzie sam
        radar1.setVehicleCategory("STANDALONE");
        vehicleRepository.save(radar1);

        // ✅ LEKKIE POJAZDY (do 5t)
        Vehicle lightTruck = new Vehicle();
        lightTruck.setModel("Mercedes Sprinter 516 CDI");
        lightTruck.setType(Vehicle.VehicleType.CARGO);
        lightTruck.setTotalWeightKg(3500); // 3.5t
        lightTruck.setHeightCm(270);
        lightTruck.setMaxAxleLoadKg(1750);
        lightTruck.setCanDriveAlone(true);
        lightTruck.setVehicleCategory("STANDALONE");
        vehicleRepository.save(lightTruck);

        Vehicle humvee = new Vehicle();
        humvee.setModel("HMMWV Humvee");
        humvee.setType(Vehicle.VehicleType.CARGO);
        humvee.setTotalWeightKg(4500); // 4.5t
        humvee.setHeightCm(185);
        humvee.setMaxAxleLoadKg(2250);
        humvee.setCanDriveAlone(true);
        humvee.setVehicleCategory("STANDALONE");
        vehicleRepository.save(humvee);

        log.info("Created {} vehicles (including standalone)", vehicleRepository.count());
    }

    private void createTransportSets() {
        var transporters = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.TRANSPORTER, true);
        var cargo = vehicleRepository.findByTypeAndActive(Vehicle.VehicleType.CARGO, true);

        if (transporters.isEmpty() || cargo.isEmpty()) {
            log.warn("No transporters or cargo found");
            return;
        }

        // Znajdź pojazdy
        Vehicle manTruck = transporters.stream()
                .filter(v -> v.getModel().contains("MAN"))
                .findFirst().orElse(transporters.get(0));

        Vehicle mercedesTruck = transporters.stream()
                .filter(v -> v.getModel().contains("Mercedes"))
                .findFirst().orElse(transporters.get(0));

        Vehicle leopard = cargo.stream()
                .filter(v -> v.getModel().contains("Leopard"))
                .findFirst().orElse(null);

        Vehicle krab = cargo.stream()
                .filter(v -> v.getModel().contains("Krab"))
                .findFirst().orElse(null);

        Vehicle rosomak = cargo.stream()
                .filter(v -> v.getModel().contains("Rosomak"))
                .findFirst().orElse(null);

        Vehicle sprinter = cargo.stream()
                .filter(v -> v.getModel().contains("Sprinter"))
                .findFirst().orElse(null);

        Vehicle humvee = cargo.stream()
                .filter(v -> v.getModel().contains("Humvee"))
                .findFirst().orElse(null);

        // ZESTAW 1: Czołg na naczepie (ciężki)
        if (leopard != null) {
            TransportSet set1 = new TransportSet();
            set1.setTransporter(manTruck);
            set1.setCargo(leopard);
            set1.setDescription("Transport czołgu Leopard 2A5 na naczepie niskopodwoziowej");
            set1.calculateTransportParameters();
            transportSetRepository.save(set1);
            log.info("Utworzono zestaw: {}", set1.getDescription());
        }

        // ZESTAW 2: Haubica na naczepie (ciężki)
        if (krab != null) {
            TransportSet set2 = new TransportSet();
            set2.setTransporter(mercedesTruck);
            set2.setCargo(krab);
            set2.setDescription("Transport haubicy Krab na naczepie wzmocnionej");
            set2.calculateTransportParameters();
            transportSetRepository.save(set2);
            log.info("Utworzono zestaw: {}", set2.getDescription());
        }

        // ✅ ZESTAW 3: Rosomak jedzie sam (STANDALONE)
        if (rosomak != null) {
            TransportSet set3 = new TransportSet();
            set3.setTransporter(manTruck); // Formalnie potrzebny, ale nie używany
            set3.setCargo(rosomak);
            set3.setDescription("KTO Rosomak - pojazd samojezdny");
            set3.calculateTransportParameters();
            transportSetRepository.save(set3);
            log.info("Utworzono zestaw standalone: {}", set3.getDescription());
            log.info("  - Wysokość: {}cm (pojazd jedzie sam)", set3.getTotalHeightCm());
            log.info("  - Waga: {}kg", set3.getTotalWeightKg());
        }

        // ✅ ZESTAW 4: Lekki pojazd Sprinter (auto-akceptacja)
        if (sprinter != null) {
            TransportSet set4 = new TransportSet();
            set4.setTransporter(manTruck);
            set4.setCargo(sprinter);
            set4.setDescription("Mercedes Sprinter - pojazd lekki (auto-akceptacja tras)");
            set4.calculateTransportParameters();
            transportSetRepository.save(set4);
            log.info("Utworzono zestaw lekki: {}", set4.getDescription());
            log.info("  - Waga: {}kg (≤5t - bez walidacji mostów)", set4.getTotalWeightKg());
        }

        // ✅ ZESTAW 5: Humvee (lekki wojskowy)
        if (humvee != null) {
            TransportSet set5 = new TransportSet();
            set5.setTransporter(manTruck);
            set5.setCargo(humvee);
            set5.setDescription("HMMWV Humvee - pojazd lekki wojskowy");
            set5.calculateTransportParameters();
            transportSetRepository.save(set5);
            log.info("Utworzono zestaw lekki wojskowy: {}", set5.getDescription());
        }

        log.info("Created {} transport sets", transportSetRepository.count());
    }

    private void createSampleRoutes() {
        var transportSets = transportSetRepository.findAll();
        if (transportSets.isEmpty()) {
            return;
        }

        // Trasa 1: Warszawa - Poznań (przykład dla ciężkiego zestawu)
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

        // Trasa 2: Kraków - Gdańsk (przypisana kierowcy)
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

        log.info("Created {} sample routes", routeRepository.count());
    }
}