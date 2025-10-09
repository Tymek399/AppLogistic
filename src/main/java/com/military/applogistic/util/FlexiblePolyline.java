package com.military.applogistic.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * ✅ DEKODER HERE FLEXIBLE POLYLINE - FINALNA WERSJA
 * HERE używa STAŁEJ precyzji 5 (10^5) niezależnie od headera!
 */
public class FlexiblePolyline {

    private static final Logger log = LoggerFactory.getLogger(FlexiblePolyline.class);
    private static final String ENCODING_TABLE = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    // ✅ STAŁA PRECYZJA DLA HERE MAPS
    private static final double FIXED_PRECISION = 1000000.0; // 10^5

    public static class LatLng {
        public final double lat;
        public final double lng;

        public LatLng(double lat, double lng) {
            this.lat = lat;
            this.lng = lng;
        }

        @Override
        public String toString() {
            return String.format("(%.6f, %.6f)", lat, lng);
        }
    }

    public static List<LatLng> decode(String encoded) {
        if (encoded == null || encoded.isEmpty()) {
            throw new IllegalArgumentException("Invalid encoded string");
        }

        log.debug("🔍 FlexiblePolyline: Dekodowanie string o długości {}", encoded.length());

        List<LatLng> result = new ArrayList<>();
        Decoder decoder = new Decoder(encoded);

        long header = decoder.decodeUnsignedVarint();
        int headerPrecision = (int) (header & 15);
        int thirdDim = (int) ((header >> 4) & 7);

        log.info("📊 Header: {}, Header precyzja: {} (IGNOROWANA), 3D: {}",
                header, headerPrecision, thirdDim);
        log.info("✅ Używam STAŁEJ precyzji: 10^5 = {}", FIXED_PRECISION);

        long lng = 0;
        long lat = 0;
        long z = 0;

        int pointIndex = 0;

        while (decoder.hasMore()) {
            try {
                lng += decoder.decodeSignedVarint();

                if (!decoder.hasMore()) {
                    log.warn("⚠️ Brak latitude dla punktu {}", pointIndex);
                    break;
                }

                lat += decoder.decodeSignedVarint();

                if (thirdDim > 0 && decoder.hasMore()) {
                    z += decoder.decodeSignedVarint();
                }

                double decodedLng = (double) lng / FIXED_PRECISION;
                double decodedLat = (double) lat / FIXED_PRECISION;

                if (pointIndex < 3) {
                    log.debug("🔍 Punkt {}: raw_lng={}, raw_lat={}, lng={}, lat={}",
                            pointIndex, lng, lat, decodedLng, decodedLat);
                }

                result.add(new LatLng(decodedLat, decodedLng));
                pointIndex++;

            } catch (Exception e) {
                log.error("❌ Błąd dekodowania punktu {}: {}", pointIndex, e.getMessage());
                break;
            }
        }

        log.info("✅ Zdekodowano {} punktów", result.size());

        if (result.size() > 0) {
            log.debug("📍 Start: lat={}, lng={}", result.get(0).lat, result.get(0).lng);
            log.debug("📍 Koniec: lat={}, lng={}",
                    result.get(result.size()-1).lat,
                    result.get(result.size()-1).lng);
        }

        return result;
    }

    private static class Decoder {
        private final String encoded;
        private int index;

        Decoder(String encoded) {
            this.encoded = encoded;
            this.index = 0;
        }

        boolean hasMore() {
            return index < encoded.length();
        }

        long decodeUnsignedVarint() {
            long result = 0;
            int shift = 0;

            while (index < encoded.length()) {
                int value = decodeChar(encoded.charAt(index++));
                result |= (long) (value & 0x1F) << shift;

                if (value < 32) {
                    break;
                }

                shift += 5;

                if (shift > 64) {
                    throw new RuntimeException("Unsigned varint overflow");
                }
            }

            return result;
        }

        long decodeSignedVarint() {
            long result = decodeUnsignedVarint();
            return (result & 1) != 0 ? ~(result >> 1) : (result >> 1);
        }

        int decodeChar(char c) {
            int pos = ENCODING_TABLE.indexOf(c);
            if (pos == -1) {
                throw new IllegalArgumentException("Invalid character: " + c);
            }
            return pos;
        }
    }
}