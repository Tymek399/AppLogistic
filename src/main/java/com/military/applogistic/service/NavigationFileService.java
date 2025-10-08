package com.military.applogistic.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.military.applogistic.entity.Route;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.*;

/**
 * Serwis odpowiedzialny za generowanie plików nawigacyjnych (GPX, KML)
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class NavigationFileService {

    private final ObjectMapper objectMapper;

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
                case "gpx" -> generateGpx(routeData).getBytes(StandardCharsets.UTF_8);
                case "kml" -> generateKml(routeData).getBytes(StandardCharsets.UTF_8);
                default -> throw new RuntimeException("Unsupported format: " + format);
            };

        } catch (Exception e) {
            throw new RuntimeException("Error generating navigation file: " + e.getMessage(), e);
        }
    }

    /**
     * Generuje plik GPX
     */
    private String generateGpx(Map<String, Object> routeData) {
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
        gpx.append("    <name>Military Route</name>\n");
        gpx.append("    <time>").append(Instant.now().toString()).append("</time>\n");
        gpx.append("  </metadata>\n");

        // Track
        gpx.append("  <trk>\n");
        gpx.append("    <name>Transport Route</name>\n");
        gpx.append("    <trkseg>\n");

        List<double[]> coordinates = extractCoordinates(routeData);
        for (double[] coord : coordinates) {
            gpx.append(String.format(Locale.US,
                    "      <trkpt lat=\"%.6f\" lon=\"%.6f\"></trkpt>\n",
                    coord[0], coord[1]));
        }

        gpx.append("    </trkseg>\n");
        gpx.append("  </trk>\n");
        gpx.append("</gpx>");

        return gpx.toString();
    }

    /**
     * Generuje plik KML
     */
    private String generateKml(Map<String, Object> routeData) {
        StringBuilder kml = new StringBuilder();

        // Header
        kml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        kml.append("<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n");
        kml.append("  <Document>\n");
        kml.append("    <name>Military Route</name>\n");
        kml.append("    <description>Transport route for military logistics</description>\n");

        // Style
        kml.append("    <Style id=\"routeStyle\">\n");
        kml.append("      <LineStyle>\n");
        kml.append("        <color>ff0000ff</color>\n");
        kml.append("        <width>4</width>\n");
        kml.append("      </LineStyle>\n");
        kml.append("    </Style>\n");

        // Placemark
        kml.append("    <Placemark>\n");
        kml.append("      <name>Transport Route</name>\n");
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