package com.military.applogistic.service.navigation;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.military.applogistic.entity.Route;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.*;

/**
 * Serwis odpowiedzialny za generowanie plików nawigacyjnych (GPX, KML, Google Maps URL)
 * z zachowaniem zwalidowanej trasy
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class NavigationFileService {

    private final ObjectMapper objectMapper;
    private static final int MAX_GOOGLE_WAYPOINTS = 8; // Google Maps limit ~10, zostawiamy bufor

    /**
     * Generuje plik nawigacyjny w wybranym formacie
     */
    public byte[] generate(Route route, String format) {
        if (route.getRouteDataJson() == null || route.getRouteDataJson().equals("{}")) {
            throw new RuntimeException("Route has no navigation data");
        }

        try {
            Map<String, Object> routeData = objectMapper.readValue(
                    route.getRouteDataJson(), Map.class);

            return switch (format.toLowerCase()) {
                case "gpx" -> generateGpx(routeData, route).getBytes(StandardCharsets.UTF_8);
                case "kml" -> generateKml(routeData, route).getBytes(StandardCharsets.UTF_8);
                case "googlemaps" -> generateGoogleMapsUrl(routeData, route).getBytes(StandardCharsets.UTF_8);
                default -> throw new RuntimeException("Unsupported format: " + format);
            };

        } catch (Exception e) {
            throw new RuntimeException("Error generating navigation file: " + e.getMessage(), e);
        }
    }

    /**
     * Generuje URL do Google Maps z waypointami z zwalidowanej trasy
     */
    private String generateGoogleMapsUrl(Map<String, Object> routeData, Route route) {
        List<double[]> coordinates = extractCoordinates(routeData);

        if (coordinates.isEmpty()) {
            log.warn("No coordinates found in route data, using start/end only");
            return String.format(
                    "https://www.google.com/maps/dir/%f,%f/%f,%f",
                    route.getStartLatitude(), route.getStartLongitude(),
                    route.getEndLatitude(), route.getEndLongitude()
            );
        }

        // Wybierz kluczowe punkty trasy (waypoints)
        List<double[]> waypoints = selectKeyWaypoints(coordinates, MAX_GOOGLE_WAYPOINTS);

        StringBuilder url = new StringBuilder("https://www.google.com/maps/dir/");

        // Start
        url.append(route.getStartLatitude()).append(",").append(route.getStartLongitude());

        // Waypoints
        for (double[] wp : waypoints) {
            url.append("/").append(String.format(Locale.US, "%.6f,%.6f", wp[0], wp[1]));
        }

        // End
        url.append("/").append(route.getEndLatitude()).append(",").append(route.getEndLongitude());

        log.info("Generated Google Maps URL with {} waypoints", waypoints.size());
        return url.toString();
    }

    /**
     * Wybiera kluczowe punkty z trasy (waypoints) dla Google Maps
     * Strategia: równomierne rozmieszczenie punktów + kluczowe zakręty
     */
    private List<double[]> selectKeyWaypoints(List<double[]> allCoordinates, int maxPoints) {
        if (allCoordinates.size() <= maxPoints) {
            return allCoordinates.subList(1, allCoordinates.size() - 1); // bez start/end
        }

        List<double[]> waypoints = new ArrayList<>();

        // Strategia 1: Równomierne rozmieszczenie
        int step = allCoordinates.size() / (maxPoints + 1);
        for (int i = step; i < allCoordinates.size() - step; i += step) {
            if (waypoints.size() < maxPoints) {
                waypoints.add(allCoordinates.get(i));
            }
        }

        // Strategia 2: Dodaj punkty ze znaczącymi zmianami kierunku
        List<double[]> turningPoints = findTurningPoints(allCoordinates);
        for (double[] tp : turningPoints) {
            if (waypoints.size() >= maxPoints) break;
            if (!containsPoint(waypoints, tp)) {
                waypoints.add(tp);
            }
        }

        log.debug("Selected {} waypoints from {} total points", waypoints.size(), allCoordinates.size());
        return waypoints.subList(0, Math.min(waypoints.size(), maxPoints));
    }

    /**
     * Znajduje punkty ze znaczącymi zmianami kierunku (zakręty)
     */
    private List<double[]> findTurningPoints(List<double[]> coordinates) {
        List<double[]> turningPoints = new ArrayList<>();

        for (int i = 1; i < coordinates.size() - 1; i++) {
            double[] prev = coordinates.get(i - 1);
            double[] curr = coordinates.get(i);
            double[] next = coordinates.get(i + 1);

            // Oblicz kąt
            double angle = calculateAngle(prev, curr, next);

            // Jeśli kąt > 30 stopni, to znaczący zakręt
            if (Math.abs(angle) > 30) {
                turningPoints.add(curr);
            }
        }

        return turningPoints;
    }

    /**
     * Oblicza kąt między trzema punktami (w stopniach)
     */
    private double calculateAngle(double[] p1, double[] p2, double[] p3) {
        double bearing1 = calculateBearing(p1[0], p1[1], p2[0], p2[1]);
        double bearing2 = calculateBearing(p2[0], p2[1], p3[0], p3[1]);

        double angle = bearing2 - bearing1;

        // Normalizuj do [-180, 180]
        while (angle > 180) angle -= 360;
        while (angle < -180) angle += 360;

        return angle;
    }

    /**
     * Oblicza bearing (azymut) między dwoma punktami
     */
    private double calculateBearing(double lat1, double lon1, double lat2, double lon2) {
        double dLon = Math.toRadians(lon2 - lon1);
        double y = Math.sin(dLon) * Math.cos(Math.toRadians(lat2));
        double x = Math.cos(Math.toRadians(lat1)) * Math.sin(Math.toRadians(lat2)) -
                Math.sin(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.cos(dLon);

        return Math.toDegrees(Math.atan2(y, x));
    }

    /**
     * Sprawdza czy lista zawiera dany punkt
     */
    private boolean containsPoint(List<double[]> points, double[] point) {
        for (double[] p : points) {
            if (Math.abs(p[0] - point[0]) < 0.0001 && Math.abs(p[1] - point[1]) < 0.0001) {
                return true;
            }
        }
        return false;
    }

    /**
     * Generuje plik GPX z pełną trasą
     */
    private String generateGpx(Map<String, Object> routeData, Route route) {
        StringBuilder gpx = new StringBuilder();

        // Header
        gpx.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        gpx.append("<gpx version=\"1.1\" creator=\"MilitaryLogisticOps\" ");
        gpx.append("xmlns=\"http://www.topografix.com/GPX/1/1\" ");
        gpx.append("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ");
        gpx.append("xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 ");
        gpx.append("http://www.topografix.com/GPX/1/1/gpx.xsd\">\n");

        // Metadata
        gpx.append("  <metadata>\n");
        gpx.append("    <name>Trasa #").append(route.getId()).append(" - Zwalidowana</name>\n");
        gpx.append("    <desc>").append(route.getStartAddress()).append(" → ")
                .append(route.getEndAddress()).append("</desc>\n");
        gpx.append("    <time>").append(Instant.now().toString()).append("</time>\n");

        // Dodaj informacje o walidacji
        if (routeData.containsKey("hasRestrictions") || routeData.containsKey("warnings")) {
            gpx.append("    <keywords>military,validated");
            if (Boolean.TRUE.equals(routeData.get("hasRestrictions"))) {
                gpx.append(",restrictions");
            }
            gpx.append("</keywords>\n");
        }

        gpx.append("  </metadata>\n");

        // Track
        gpx.append("  <trk>\n");
        gpx.append("    <name>Trasa militarna #").append(route.getId()).append("</name>\n");
        gpx.append("    <type>validated-military-route</type>\n");
        gpx.append("    <trkseg>\n");

        List<double[]> coordinates = extractCoordinates(routeData);
        for (double[] coord : coordinates) {
            gpx.append(String.format(Locale.US,
                    "      <trkpt lat=\"%.6f\" lon=\"%.6f\"></trkpt>\n",
                    coord[0], coord[1]));
        }

        gpx.append("    </trkseg>\n");
        gpx.append("  </trk>\n");

        // Dodaj waypoints dla kluczowych punktów
        List<double[]> waypoints = selectKeyWaypoints(coordinates, MAX_GOOGLE_WAYPOINTS);
        for (int i = 0; i < waypoints.size(); i++) {
            double[] wp = waypoints.get(i);
            gpx.append(String.format(Locale.US,
                    "  <wpt lat=\"%.6f\" lon=\"%.6f\">\n", wp[0], wp[1]));
            gpx.append("    <name>WP").append(i + 1).append("</name>\n");
            gpx.append("    <desc>Kluczowy punkt trasy</desc>\n");
            gpx.append("  </wpt>\n");
        }

        gpx.append("</gpx>");

        return gpx.toString();
    }

    /**
     * Generuje plik KML
     */
    private String generateKml(Map<String, Object> routeData, Route route) {
        StringBuilder kml = new StringBuilder();

        // Header
        kml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        kml.append("<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n");
        kml.append("  <Document>\n");
        kml.append("    <name>Trasa #").append(route.getId()).append("</name>\n");
        kml.append("    <description>Zwalidowana trasa militarna</description>\n");

        // Style
        kml.append("    <Style id=\"routeStyle\">\n");
        kml.append("      <LineStyle>\n");
        kml.append("        <color>ff0000ff</color>\n");
        kml.append("        <width>4</width>\n");
        kml.append("      </LineStyle>\n");
        kml.append("    </Style>\n");

        // Placemark
        kml.append("    <Placemark>\n");
        kml.append("      <name>Trasa militarna</name>\n");
        kml.append("      <styleUrl>#routeStyle</styleUrl>\n");
        kml.append("      <LineString>\n");
        kml.append("        <tessellate>1</tessellate>\n");
        kml.append("        <coordinates>\n");

        List<double[]> coordinates = extractCoordinates(routeData);
        for (double[] coord : coordinates) {
            kml.append(String.format(Locale.US,
                    "          %.6f,%.6f,0\n", coord[1], coord[0]));
        }

        kml.append("        </coordinates>\n");
        kml.append("      </LineString>\n");
        kml.append("    </Placemark>\n");
        kml.append("  </Document>\n");
        kml.append("</kml>");

        return kml.toString();
    }

    /**
     * Wyciąga współrzędne z danych trasy
     */
    @SuppressWarnings("unchecked")
    private List<double[]> extractCoordinates(Map<String, Object> routeData) {
        try {
            List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
            if (routes != null && !routes.isEmpty()) {
                Map<String, Object> route = routes.get(0);
                Map<String, Object> overviewPolyline = (Map<String, Object>) route.get("overview_polyline");

                if (overviewPolyline != null) {
                    String encodedPolyline = (String) overviewPolyline.get("points");
                    if (encodedPolyline != null) {
                        return decodePolyline(encodedPolyline);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error extracting coordinates", e);
        }

        return new ArrayList<>();
    }

    /**
     * Dekoduje polyline z Google Maps
     */
    private List<double[]> decodePolyline(String encoded) {
        List<double[]> path = new ArrayList<>();
        int index = 0;
        int len = encoded.length();
        int lat = 0;
        int lng = 0;

        while (index < len) {
            int b;
            int shift = 0;
            int result = 0;
            do {
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20);
            int dlat = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lat += dlat;

            shift = 0;
            result = 0;
            do {
                b = encoded.charAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20);
            int dlng = ((result & 1) != 0 ? ~(result >> 1) : (result >> 1));
            lng += dlng;

            path.add(new double[]{lat / 1E5, lng / 1E5});
        }

        return path;
    }
}