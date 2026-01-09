package com.military.applogistic.service;

import com.military.applogistic.entity.TransportSet;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import lombok.Data;
import lombok.AllArgsConstructor;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.*;
import java.util.concurrent.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class OverpassService {

    private final RestTemplate restTemplate;
    private static final String OVERPASS_API_URL = "https://overpass-api.de/api/interpreter";
    private static final double MAX_DISTANCE_FROM_ROUTE_METERS = 500.0;
    private static final int TIMEOUT_SECONDS = 30;
    private static final int MAX_RETRIES = 3;

    private final Map<String, CachedResult> queryCache = new ConcurrentHashMap<>();
    private static final long CACHE_EXPIRY_MS = 3600000;

    public enum InfrastructureType {
        BRIDGE("Most"),
        TUNNEL("Tunel"),
        VIADUCT("Wiadukt"),
        HEIGHT_BARRIER("Ograniczenie wysokości"),
        TOLL_GATE("Bramka opłat");

        private final String polish;
        InfrastructureType(String polish) { this.polish = polish; }
        public String getPolish() { return polish; }
    }

    @Data
    @AllArgsConstructor
    public static class InfrastructurePoint {
        private InfrastructureType type;
        private String name;
        private double latitude;
        private double longitude;
        private Double maxWeightTons;
        private Double maxHeightMeters;
        private String roadName;
        private Map<String, String> tags;
        private String osmId;
        private Integer yearBuilt;

        public boolean canHandle(TransportSet transportSet) {
            if (maxWeightTons != null && transportSet.getTotalWeightKg() / 1000.0 > maxWeightTons) {
                return false;
            }
            if (maxHeightMeters != null && transportSet.getTotalHeightCm() / 100.0 > maxHeightMeters) {
                return false;
            }
            return true;
        }

        public double getDistanceFrom(double lat, double lng) {
            return calculateDistance(this.latitude, this.longitude, lat, lng);
        }

        private double calculateDistance(double lat1, double lon1, double lat2, double lon2) {
            final double R = 6371000;
            double dLat = Math.toRadians(lat2 - lat1);
            double dLon = Math.toRadians(lon2 - lon1);
            double a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
                    Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) *
                            Math.sin(dLon / 2) * Math.sin(dLon / 2);
            double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
            return R * c;
        }
    }

    @Data
    private static class CachedResult {
        private List<InfrastructurePoint> points;
        private long timestamp;

        public boolean isExpired() {
            return System.currentTimeMillis() - timestamp > CACHE_EXPIRY_MS;
        }
    }

    public List<InfrastructurePoint> getInfrastructureAlongRoute(List<double[]> routeCoordinates) {
        if (routeCoordinates == null || routeCoordinates.isEmpty()) {
            log.warn("Empty or null route coordinates provided");
            return new ArrayList<>();
        }

        double minLat = routeCoordinates.stream().mapToDouble(c -> c[0]).min().orElse(0);
        double maxLat = routeCoordinates.stream().mapToDouble(c -> c[0]).max().orElse(0);
        double minLng = routeCoordinates.stream().mapToDouble(c -> c[1]).min().orElse(0);
        double maxLng = routeCoordinates.stream().mapToDouble(c -> c[1]).max().orElse(0);

        String cacheKey = String.format("%.4f_%.4f_%.4f_%.4f", minLat, maxLat, minLng, maxLng);
        CachedResult cached = queryCache.get(cacheKey);

        if (cached != null && !cached.isExpired()) {
            log.info("✅ Używam cache dla infrastruktury (wiek: {}s)",
                    (System.currentTimeMillis() - cached.timestamp) / 1000);
            return filterPointsNearRoute(cached.getPoints(), routeCoordinates);
        }

        String query = buildOverpassQuery(minLat, maxLat, minLng, maxLng);

        // Ta metoda rzuci RuntimeException jeśli API zawiedzie po MAX_RETRIES
        List<InfrastructurePoint> rawPoints = queryOverpassApiWithRetry(query);

        CachedResult result = new CachedResult();
        result.setPoints(rawPoints);
        result.setTimestamp(System.currentTimeMillis());
        queryCache.put(cacheKey, result);

        List<InfrastructurePoint> filteredPoints = filterPointsNearRoute(rawPoints, routeCoordinates);
        log.info("✅ Found {} infrastructure points along the route", filteredPoints.size());
        return filteredPoints;
    }

    private List<InfrastructurePoint> filterPointsNearRoute(
            List<InfrastructurePoint> points,
            List<double[]> routeCoordinates) {

        List<InfrastructurePoint> filtered = new ArrayList<>();
        for (InfrastructurePoint point : points) {
            boolean isNearRoute = false;
            for (double[] routePoint : routeCoordinates) {
                double distance = point.getDistanceFrom(routePoint[0], routePoint[1]);
                if (distance <= MAX_DISTANCE_FROM_ROUTE_METERS) {
                    isNearRoute = true;
                    break;
                }
            }

            if (isNearRoute) {
                filtered.add(point);
            }
        }

        log.info("📊 Filtrowanie: {} -> {} punktów (usunięto {} fałszywych alarmów)",
                points.size(), filtered.size(), points.size() - filtered.size());
        return filtered;
    }

    private String buildOverpassQuery(double minLat, double maxLat, double minLng, double maxLng) {
        return String.format(Locale.US,
                "[out:json][timeout:%d];\n" +
                        "(\n" +
                        "  way[\"bridge\"=\"yes\"](%f,%f,%f,%f);\n" +
                        "  way[\"tunnel\"=\"yes\"](%f,%f,%f,%f);\n" +
                        "  way[\"man_made\"=\"bridge\"](%f,%f,%f,%f);\n" +
                        "  node[\"barrier\"=\"height_restrictor\"](%f,%f,%f,%f);\n" +
                        "  node[\"amenity\"=\"toll_booth\"](%f,%f,%f,%f);\n" +
                        ");\n" +
                        "out body;\n" +
                        ">;\n" +
                        "out skel qt;",
                TIMEOUT_SECONDS,
                minLat, minLng, maxLat, maxLng,
                minLat, minLng, maxLat, maxLng,
                minLat, minLng, maxLat, maxLng,
                minLat, minLng, maxLat, maxLng,
                minLat, minLng, maxLat, maxLng
        );
    }

    private List<InfrastructurePoint> queryOverpassApiWithRetry(String query) {
        int retryDelayMs = 2000;

        for (int attempt = 1; attempt <= MAX_RETRIES; attempt++) {
            try {
                URI uri = UriComponentsBuilder.fromHttpUrl(OVERPASS_API_URL)
                        .queryParam("data", query)
                        .build()
                        .encode()
                        .toUri();

                log.info("📡 Overpass API query (attempt {}/{})", attempt, MAX_RETRIES);

                Map<String, Object> response = restTemplate.getForObject(uri, Map.class);

                if (response != null && response.containsKey("elements")) {
                    List<Map<String, Object>> elements = (List<Map<String, Object>>) response.get("elements");
                    List<InfrastructurePoint> points = parseOverpassResponse(elements);
                    log.info("✅ Overpass API success: {} points found", points.size());
                    return points;
                }

            } catch (Exception e) {
                log.error("❌ Attempt {} failed: {}", attempt, e.getMessage());
                if (attempt < MAX_RETRIES) {
                    try {
                        Thread.sleep(retryDelayMs * attempt);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        throw new RuntimeException("Przerwano oczekiwanie na API Overpass");
                    }
                }
            }
        }

        // FAIL-FAST: Jeśli dotarliśmy tutaj, znaczy że wszystkie próby zawiodły
        log.error("💥 Max retries reached for Overpass API. Verification failed!");
        throw new RuntimeException("KRYTYCZNY BŁĄD: Nie można pobrać danych o infrastrukturze z API Overpass. Walidacja przerwana dla bezpieczeństwa.");
    }

    private List<InfrastructurePoint> parseOverpassResponse(List<Map<String, Object>> elements) {
        List<InfrastructurePoint> points = new ArrayList<>();
        Map<Long, Map<String, Object>> nodes = new HashMap<>();

        for (Map<String, Object> element : elements) {
            if ("node".equals(element.get("type"))) {
                Long id = ((Number) element.get("id")).longValue();
                nodes.put(id, element);
            }
        }

        for (Map<String, Object> element : elements) {
            if ("way".equals(element.get("type"))) {
                InfrastructurePoint point = parseWayElement(element, nodes);
                if (point != null) points.add(point);
            }
        }

        for (Map<String, Object> element : elements) {
            if ("node".equals(element.get("type"))) {
                InfrastructurePoint point = parseNodeElement(element);
                if (point != null) points.add(point);
            }
        }
        return points;
    }

    private InfrastructurePoint parseWayElement(Map<String, Object> element, Map<Long, Map<String, Object>> nodes) {
        Map<String, Object> tags = (Map<String, Object>) element.get("tags");
        if (tags == null) return null;

        InfrastructureType type = null;
        if ("yes".equals(tags.get("bridge")) || "bridge".equals(tags.get("man_made"))) {
            type = InfrastructureType.BRIDGE;
        } else if ("yes".equals(tags.get("tunnel")) || "tunnel".equals(tags.get("man_made"))) {
            type = InfrastructureType.TUNNEL;
        } else if ("viaduct".equals(tags.get("bridge"))) {
            type = InfrastructureType.VIADUCT;
        }

        if (type == null) return null;

        List<?> nodeIdsRaw = (List<?>) element.get("nodes");
        if (nodeIdsRaw == null || nodeIdsRaw.isEmpty()) return null;

        double sumLat = 0, sumLng = 0;
        int count = 0;

        for (Object id : nodeIdsRaw) {
            Map<String, Object> node = nodes.get(((Number) id).longValue());
            if (node != null) {
                sumLat += ((Number) node.get("lat")).doubleValue();
                sumLng += ((Number) node.get("lon")).doubleValue();
                count++;
            }
        }

        if (count == 0) return null;

        String name = (String) tags.getOrDefault("name",
                type.getPolish() + " na " + tags.getOrDefault("ref", "drodze lokalnej"));

        String roadName = (String) tags.getOrDefault("ref", (String) tags.get("highway"));
        Double maxWeight = parseWeight(tags);
        Double maxHeight = parseHeight(tags);

        Map<String, String> stringTags = new HashMap<>();
        tags.forEach((k, v) -> stringTags.put(k, String.valueOf(v)));

        return new InfrastructurePoint(
                type, name, sumLat / count, sumLng / count, maxWeight, maxHeight, roadName, stringTags, "way/" + element.get("id"), parseYear(tags)
        );
    }

    private InfrastructurePoint parseNodeElement(Map<String, Object> element) {
        Map<String, Object> tags = (Map<String, Object>) element.get("tags");
        if (tags == null) return null;

        InfrastructureType type = null;
        if ("height_restrictor".equals(tags.get("barrier"))) type = InfrastructureType.HEIGHT_BARRIER;
        else if ("toll_booth".equals(tags.get("amenity"))) type = InfrastructureType.TOLL_GATE;

        if (type == null) return null;

        Map<String, String> stringTags = new HashMap<>();
        tags.forEach((k, v) -> stringTags.put(k, String.valueOf(v)));

        return new InfrastructurePoint(
                type, type.getPolish(), ((Number) element.get("lat")).doubleValue(), ((Number) element.get("lon")).doubleValue(), null, parseHeight(tags), null, stringTags, "node/" + element.get("id"), null
        );
    }

    private Double parseWeight(Map<String, Object> tags) {
        String[] weightTags = {"maxweight", "maxweight:signed", "weight_limit", "maxweight:physical"};
        for (String tag : weightTags) {
            String value = (String) tags.get(tag);
            if (value != null && !value.trim().isEmpty()) {
                try {
                    value = value.replaceAll("[^0-9.,]", "").replace(",", ".");
                    double weight = Double.parseDouble(value);
                    return weight > 500 ? weight / 1000.0 : weight;
                } catch (Exception e) {
                    log.debug("Could not parse weight: {}", value);
                }
            }
        }
        return null;
    }

    private Double parseHeight(Map<String, Object> tags) {
        String[] heightTags = {"maxheight", "maxheight:physical", "height", "maxheight:signed"};
        for (String tag : heightTags) {
            String value = (String) tags.get(tag);
            if (value != null && !value.trim().isEmpty()) {
                try {
                    value = value.toLowerCase().replaceAll("[^0-9.,ft'\"]", "").replace(",", ".");
                    if (value.contains("ft") || value.contains("'")) return Double.parseDouble(value.replaceAll("[^0-9.]", "")) * 0.3048;
                    return Double.parseDouble(value);
                } catch (Exception e) {
                    log.debug("Could not parse height: {}", value);
                }
            }
        }
        return null;
    }

    private Integer parseYear(Map<String, Object> tags) {
        String[] yearTags = {"start_date", "year_built", "construction_date"};
        for (String tag : yearTags) {
            String value = (String) tags.get(tag);
            if (value != null) {
                try {
                    String yearStr = value.replaceAll("[^0-9]", "");
                    if (yearStr.length() >= 4) return Integer.parseInt(yearStr.substring(0, 4));
                } catch (Exception e) { /* skip */ }
            }
        }
        return null;
    }
}