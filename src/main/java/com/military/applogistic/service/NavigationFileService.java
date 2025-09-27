package com.military.applogistic.service;

import com.military.applogistic.entity.Route;
import org.springframework.stereotype.Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.charset.StandardCharsets;
import java.util.*;

@Service
public class NavigationFileService {

    private final ObjectMapper objectMapper = new ObjectMapper();

    public byte[] generateGPXFile(Route route) {
        StringBuilder gpx = new StringBuilder();
        gpx.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        gpx.append("<gpx version=\"1.1\" xmlns=\"http://www.topografix.com/GPX/1/1\">\n");
        gpx.append("  <metadata>\n");
        gpx.append("    <name>").append(escapeXml(route.getStartAddress())).append(" to ").append(escapeXml(route.getEndAddress())).append("</name>\n");
        gpx.append("  </metadata>\n");
        gpx.append("  <rte>\n");
        gpx.append("    <name>Route ").append(route.getId()).append("</name>\n");

        // Add waypoints from route data
        try {
            if (route.getRouteDataJson() != null) {
                Map<String, Object> routeData = objectMapper.readValue(route.getRouteDataJson(), Map.class);
                List<Map<String, Object>> routes = (List<Map<String, Object>>) routeData.get("routes");
                if (routes != null && !routes.isEmpty()) {
                    Map<String, Object> firstRoute = routes.get(0);
                    Object polylineObj = firstRoute.get("overview_polyline");
                    if (polylineObj instanceof Map) {
                        String polyline = (String) ((Map<String, Object>) polylineObj).get("points");
                        if (polyline != null) {
                            List<double[]> points = decodePolyline(polyline);
                            for (double[] point : points) {
                                gpx.append("    <rtept lat=\"").append(point[0]).append("\" lon=\"").append(point[1]).append("\">\n");
                                gpx.append("    </rtept>\n");
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            // Fallback to start and end points only
            addBasicWaypoints(gpx, route);
        }

        gpx.append("  </rte>\n");
        gpx.append("</gpx>");

        return gpx.toString().getBytes(StandardCharsets.UTF_8);
    }

    public byte[] generateKMLFile(Route route) {
        StringBuilder kml = new StringBuilder();
        kml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        kml.append("<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n");
        kml.append("  <Document>\n");
        kml.append("    <name>").append(escapeXml(route.getStartAddress())).append(" to ").append(escapeXml(route.getEndAddress())).append("</name>\n");

        // Add start point
        kml.append("    <Placemark>\n");
        kml.append("      <name>Start: ").append(escapeXml(route.getStartAddress())).append("</name>\n");
        kml.append("      <Point>\n");
        kml.append("        <coordinates>").append(route.getStartLongitude()).append(",").append(route.getStartLatitude()).append(",0</coordinates>\n");
        kml.append("      </Point>\n");
        kml.append("    </Placemark>\n");

        // Add end point
        kml.append("    <Placemark>\n");
        kml.append("      <name>End: ").append(escapeXml(route.getEndAddress())).append("</name>\n");
        kml.append("      <Point>\n");
        kml.append("        <coordinates>").append(route.getEndLongitude()).append(",").append(route.getEndLatitude()).append(",0</coordinates>\n");
        kml.append("      </Point>\n");
        kml.append("    </Placemark>\n");

        // Add route line
        kml.append("    <Placemark>\n");
        kml.append("      <name>Route ").append(route.getId()).append("</name>\n");
        kml.append("      <LineString>\n");
        kml.append("        <coordinates>\n");
        kml.append("          ").append(route.getStartLongitude()).append(",").append(route.getStartLatitude()).append(",0\n");
        kml.append("          ").append(route.getEndLongitude()).append(",").append(route.getEndLatitude()).append(",0\n");
        kml.append("        </coordinates>\n");
        kml.append("      </LineString>\n");
        kml.append("    </Placemark>\n");

        kml.append("  </Document>\n");
        kml.append("</kml>");

        return kml.toString().getBytes(StandardCharsets.UTF_8);
    }

    private void addBasicWaypoints(StringBuilder gpx, Route route) {
        gpx.append("    <rtept lat=\"").append(route.getStartLatitude()).append("\" lon=\"").append(route.getStartLongitude()).append("\">\n");
        gpx.append("      <name>Start</name>\n");
        gpx.append("    </rtept>\n");
        gpx.append("    <rtept lat=\"").append(route.getEndLatitude()).append("\" lon=\"").append(route.getEndLongitude()).append("\">\n");
        gpx.append("      <name>End</name>\n");
        gpx.append("    </rtept>\n");
    }

    private String escapeXml(String text) {
        if (text == null) return "";
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&apos;");
    }

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