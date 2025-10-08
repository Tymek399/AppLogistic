package com.military.applogistic.service.api;

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

@Service
@RequiredArgsConstructor
@Slf4j
public class OverpassService {

    private final RestTemplate restTemplate;
    private static final String OVERPASS_API_URL = "https://overpass-api.de/api/interpreter";

    public enum InfrastructureType {
        BRIDGE("Most"),
        TUNNEL("Tunel"),
        VIADUCT("Wiadukt"),
        HEIGHT_BARRIER("Ograniczenie wysokości");

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
        private Map<String, String> tags; // Changed to Map<String, String>

        public boolean canHandle(TransportSet transportSet) {
            if (maxWeightTons != null && transportSet.getTotalWeightKg() / 1000.0 > maxWeightTons) {
                return false;
            }
            if (maxHeightMeters != null && transportSet.getTotalHeightCm() / 100.0 > maxHeightMeters) {
                return false;
            }
            return true;
        }
    }

    public List<InfrastructurePoint> getInfrastructureAlongRoute(List<double[]> routeCoordinates) {
        List<InfrastructurePoint> allInfrastructure = new ArrayList<>();

        if (routeCoordinates == null || routeCoordinates.isEmpty()) {
            log.warn("Empty or null route coordinates provided");
            return allInfrastructure;
        }

        // Calculate bounding box for the route
        double minLat = routeCoordinates.stream().mapToDouble(c -> c[0]).min().orElse(0);
        double maxLat = routeCoordinates.stream().mapToDouble(c -> c[0]).max().orElse(0);
        double minLng = routeCoordinates.stream().mapToDouble(c -> c[1]).min().orElse(0);
        double maxLng = routeCoordinates.stream().mapToDouble(c -> c[1]).max().orElse(0);

        try {
            String query = buildOverpassQuery(minLat, maxLat, minLng, maxLng);
            List<InfrastructurePoint> points = queryOverpassApi(query);
            allInfrastructure.addAll(points);
            log.info("Found {} infrastructure points along the route", allInfrastructure.size());
        } catch (Exception e) {
            log.error("Error querying Overpass API for route: {}", e.getMessage(), e);
        }

        return allInfrastructure;
    }

    private String buildOverpassQuery(double minLat, double maxLat, double minLng, double maxLng) {
        return String.format(Locale.US,
                "[out:json][timeout:25];\n" +
                        "(\n" +
                        "  way[\"bridge\"=\"yes\"](%f,%f,%f,%f);\n" +
                        "  way[\"tunnel\"=\"yes\"](%f,%f,%f,%f);\n" +
                        "  way[\"man_made\"=\"bridge\"](%f,%f,%f,%f);\n" +
                        "  node[\"barrier\"=\"height_restrictor\"](%f,%f,%f,%f);\n" +
                        ");\n" +
                        "out body;\n" +
                        ">;\n" +
                        "out skel qt;",
                minLat, minLng, maxLat, maxLng,
                minLat, minLng, maxLat, maxLng,
                minLat, minLng, maxLat, maxLng,
                minLat, minLng, maxLat, maxLng
        );
    }

    private List<InfrastructurePoint> queryOverpassApi(String query) {
        List<InfrastructurePoint> points = new ArrayList<>();
        int retries = 3;
        int retryDelayMs = 500;

        for (int attempt = 1; attempt <= retries; attempt++) {
            try {
                URI uri = UriComponentsBuilder.fromHttpUrl(OVERPASS_API_URL)
                        .queryParam("data", query)
                        .build()
                        .encode()
                        .toUri();

                log.debug("Querying Overpass API with URI: {}", uri);

                Map<String, Object> response = restTemplate.getForObject(uri, Map.class);
                if (response != null && response.containsKey("elements")) {
                    List<Map<String, Object>> elements = (List<Map<String, Object>>) response.get("elements");
                    points = parseOverpassResponse(elements);
                    break;
                } else {
                    log.warn("No elements in Overpass API response");
                }
            } catch (Exception e) {
                log.error("Attempt {} failed querying Overpass API: {}", attempt, e.getMessage());
                if (attempt == retries) {
                    log.error("Max retries reached, giving up");
                    break;
                }
                try {
                    Thread.sleep(retryDelayMs * attempt); // Exponential backoff
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }

        return points;
    }



    private List<InfrastructurePoint> parseOverpassResponse(List<Map<String, Object>> elements) {
        List<InfrastructurePoint> points = new ArrayList<>();
        Map<Long, Map<String, Object>> nodes = new HashMap<>();

        // Collect nodes
        for (Map<String, Object> element : elements) {
            if ("node".equals(element.get("type"))) {
                Long id = ((Number) element.get("id")).longValue();
                nodes.put(id, element);
            }
        }

        // Process ways
        for (Map<String, Object> element : elements) {
            if ("way".equals(element.get("type"))) {
                Map<String, Object> tags = (Map<String, Object>) element.get("tags");
                if (tags == null) continue;

                InfrastructureType type = null;

                // ✅ MOSTY
                if ("yes".equals(tags.get("bridge")) || "bridge".equals(tags.get("man_made"))) {
                    type = InfrastructureType.BRIDGE;
                }
                // ✅ TUNELE
                else if ("yes".equals(tags.get("tunnel")) || "tunnel".equals(tags.get("man_made"))) {
                    type = InfrastructureType.TUNNEL;
                }
                else if ("viaduct".equals(tags.get("bridge"))) {
                    type = InfrastructureType.VIADUCT;
                }

                if (type != null) {
                    List<?> nodeIdsRaw = (List<?>) element.get("nodes");
                    List<Long> nodeIds = new ArrayList<>();
                    for (Object id : nodeIdsRaw) {
                        nodeIds.add(((Number) id).longValue());
                    }

                    if (!nodeIds.isEmpty()) {
                        double sumLat = 0, sumLng = 0;
                        int count = 0;

                        for (Long nodeId : nodeIds) {
                            Map<String, Object> node = nodes.get(nodeId);
                            if (node != null) {
                                sumLat += ((Number) node.get("lat")).doubleValue();
                                sumLng += ((Number) node.get("lon")).doubleValue();
                                count++;
                            }
                        }

                        if (count > 0) {
                            double lat = sumLat / count;
                            double lng = sumLng / count;

                            String name = (String) tags.getOrDefault("name",
                                    type.getPolish() + " na " + tags.getOrDefault("ref", "drodze lokalnej"));

                            String roadName = (String) tags.get("ref");
                            if (roadName == null) {
                                roadName = (String) tags.get("highway");
                            }

                            Double maxWeight = parseWeight(tags);
                            Double maxHeight = parseHeight(tags);

                            Map<String, String> stringTags = new HashMap<>();
                            tags.forEach((k, v) -> stringTags.put(k, String.valueOf(v)));

                            InfrastructurePoint point = new InfrastructurePoint(
                                    type, name, lat, lng, maxWeight, maxHeight, roadName, stringTags
                            );

                            points.add(point);
                            log.debug("Found {} at {},{}: {} (weight: {}t, height: {}m)",
                                    type, lat, lng, name, maxWeight, maxHeight);
                        }
                    }
                }
            }
        }

        // Process height restrictors (bez zmian)
        for (Map<String, Object> element : elements) {
            if ("node".equals(element.get("type"))) {
                Map<String, Object> tags = (Map<String, Object>) element.get("tags");
                if (tags != null && "height_restrictor".equals(tags.get("barrier"))) {
                    double lat = ((Number) element.get("lat")).doubleValue();
                    double lng = ((Number) element.get("lon")).doubleValue();

                    String name = "Ograniczenie wysokości";
                    Double maxHeight = parseHeight(tags);

                    Map<String, String> stringTags = new HashMap<>();
                    tags.forEach((k, v) -> stringTags.put(k, String.valueOf(v)));

                    InfrastructurePoint point = new InfrastructurePoint(
                            InfrastructureType.HEIGHT_BARRIER,
                            name,
                            lat,
                            lng,
                            null,
                            maxHeight,
                            null,
                            stringTags
                    );
                    points.add(point);
                }
            }
        }

        return points;
    }
    private Double parseWeight(Map<String, Object> tags) {
        String[] weightTags = {"maxweight", "maxweight:signed", "weight_limit"};
        for (String tag : weightTags) {
            String value = (String) tags.get(tag);
            if (value != null && !value.trim().isEmpty()) {
                try {
                    value = value.replaceAll("[^0-9.,]", "").replace(",", ".");
                    return Double.parseDouble(value);
                } catch (Exception e) {
                    log.debug("Could not parse weight: {}", value);
                }
            }
        }
        // Configurable defaults (could be loaded from properties)
        if ("motorway".equals(tags.get("highway")) || "trunk".equals(tags.get("highway"))) {
            return 60.0;
        } else if ("primary".equals(tags.get("highway"))) {
            return 50.0;
        }
        return null;
    }

    private Double parseHeight(Map<String, Object> tags) {
        String[] heightTags = {"maxheight", "maxheight:physical", "height"};
        for (String tag : heightTags) {
            String value = (String) tags.get(tag);
            if (value != null && !value.trim().isEmpty()) {
                try {
                    value = value.toLowerCase().replace("m", "").replace(",", ".").trim();
                    return Double.parseDouble(value);
                } catch (Exception e) {
                    log.debug("Could not parse height: {}", value);
                }
            }
        }
        if ("yes".equals(tags.get("tunnel"))) {
            return 4.5;
        }
        return null;
    }

}