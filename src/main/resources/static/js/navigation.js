// navigation.js - GPS Navigation Functionality
class NavigationSystem {
    constructor(routeId, routeData) {
        this.routeId = routeId;
        this.routeData = routeData;
        this.map = null;
        this.watchId = null;
        this.isTracking = false;
        this.currentPosition = null;
        this.currentMarker = null;
        this.routePolyline = null;
        this.token = localStorage.getItem('token');
        this.updateInterval = null;
        this.username = localStorage.getItem('username');
        this.stompClient = null;
        this.socket = null;

        this.init();
    }

    init() {
        if (!this.token) {
            window.location.href = '/login.html';
            return;
        }

        if (!this.username) {
            this.showError('Brak nazwy użytkownika. Zaloguj się ponownie.');
            localStorage.clear();
            window.location.href = '/login.html';
            return;
        }

        this.setupAuth();
        this.initMap();
        this.setupEventListeners();
        this.setupAutoLogout();
    }

    setupAuth() {
        const originalFetch = window.fetch;
        window.fetch = (...args) => {
            if (args[1]) {
                args[1].headers = {
                    ...args[1].headers,
                    'Authorization': `Bearer ${this.token}`
                };
            } else {
                args[1] = {
                    headers: {
                        'Authorization': `Bearer ${this.token}`
                    }
                };
            }
            return originalFetch.apply(window, args);
        };
    }

    initMap() {
        if (typeof google === 'undefined' || !google.maps) {
            setTimeout(() => this.initMap(), 100);
            return;
        }

        const centerPoint = this.routeData.startLatitude && this.routeData.startLongitude
            ? { lat: this.routeData.startLatitude, lng: this.routeData.startLongitude }
            : { lat: 52.2297, lng: 21.0122 };

        this.map = new google.maps.Map(document.getElementById('map'), {
            center: centerPoint,
            zoom: 12,
            mapTypeId: 'roadmap'
        });

        this.loadRoutePolyline();
        this.loadWaypoints();
    }

    connectWebSocket() {
        if (typeof SockJS === 'undefined' || typeof Stomp === 'undefined') {
            this.showError('Brak bibliotek SockJS lub Stomp.js. Śledzenie na żywo nie działa.');
            return;
        }

        this.socket = new SockJS('/ws');
        this.stompClient = Stomp.over(this.socket);
        this.stompClient.debug = null;

        const headers = {
            'Authorization': `Bearer ${this.token}`
        };

        this.stompClient.connect(headers, frame => {
            console.log('Połączono z serwerem śledzenia.');
        }, error => {
            this.showError('Błąd połączenia z serwerem śledzenia.');
            console.error('STOMP Error:', error);
        });
    }

    startTracking() {
        if (!this.routeId) {
            this.showError('Brak przypisanej trasy. Nie można rozpocząć śledzenia.');
            return;
        }
        if (this.isTracking) return;

        this.connectWebSocket();

        fetch(`/api/tracking/start-session?routeId=${this.routeId}`, { method: 'POST' })
            .then(response => {
                if (!response.ok) throw new Error('Nie udało się rozpocząć sesji w backendzie.');
                this.showSuccess('Sesja nawigacyjna rozpoczęta.');
            })
            .catch(e => {
                this.showError('Błąd rozpoczęcia sesji: ' + e.message);
                return;
            });

        this.watchId = navigator.geolocation.watchPosition(
            (position) => this.handlePositionUpdate(position),
            (error) => this.handleGeolocationError(error),
            { enableHighAccuracy: true, maximumAge: 0, timeout: 5000 }
        );

        this.isTracking = true;
        document.getElementById('start-btn').disabled = true;
        document.getElementById('stop-btn').disabled = false;
        this.showSuccess('Śledzenie i wysyłanie pozycji rozpoczęte!');
    }

    stopTracking() {
        if (!this.isTracking) return;

        navigator.geolocation.clearWatch(this.watchId);
        this.isTracking = false;

        if (this.stompClient) {
            this.stompClient.disconnect(() => {
                console.log('STOMP disconnected for driver.');
            });
        }

        fetch(`/api/tracking/end-session`, { method: 'POST' })
            .then(response => {
                if (response.ok) {
                    this.showSuccess('Sesja nawigacyjna zakończona.');
                }
            })
            .catch(e => {
                this.showError('Błąd kończenia sesji: ' + e.message);
            });

        document.getElementById('start-btn').disabled = false;
        document.getElementById('stop-btn').disabled = true;
    }

    handlePositionUpdate(position) {
        this.currentPosition = {
            lat: position.coords.latitude,
            lng: position.coords.longitude
        };

        this.updateMapMarker(this.currentPosition);

        if (this.stompClient && this.stompClient.connected) {
            const positionUpdate = {
                username: this.username,
                latitude: this.currentPosition.lat,
                longitude: this.currentPosition.lng,
                routeId: this.routeId
            };

            this.stompClient.send("/app/route-update", {}, JSON.stringify(positionUpdate));
            console.log(`Pozycja wysłana: ${this.currentPosition.lat}, ${this.currentPosition.lng}`);
        } else {
            console.warn('Nie można wysłać pozycji - WebSocket niepołączony.');
        }

        this.checkRouteCompletion(this.currentPosition);
    }

    handleGeolocationError(error) {
        this.showError('Błąd geolokalizacji: ' + error.message);
        console.error('Geolocation Error:', error);
    }

    updateMapMarker(position) {
        if (!this.map) return;

        if (this.currentMarker) {
            this.currentMarker.setPosition(position);
        } else {
            this.currentMarker = new google.maps.Marker({
                position: position,
                map: this.map,
                icon: {
                    url: 'http://maps.google.com/mapfiles/ms/icons/blue-dot.png',
                    scaledSize: new google.maps.Size(32, 32)
                }
            });
        }
        this.map.setCenter(position);
    }

    checkRouteCompletion(position) {
        const endLat = this.routeData.endLatitude;
        const endLng = this.routeData.endLongitude;

        if (!endLat || !endLng) return;

        const distance = this.calculateDistance(position.lat, position.lng, endLat, endLng);
        const distanceKm = distance;

        if (distanceKm < 0.5) {
            document.getElementById('instruction-text').textContent = 'Jesteś blisko celu (mniej niż 500m)!';
        }

        if (distanceKm < 0.1) {
            this.showSuccess('TRASA ZAKOŃCZONA!');
            this.stopTracking();
        }
    }

    calculateDistance(lat1, lon1, lat2, lon2) {
        const R = 6371;
        const dLat = (lat2 - lat1) * Math.PI / 180;
        const dLon = (lon2 - lon1) * Math.PI / 180;
        const a = Math.sin(dLat/2) * Math.sin(dLat/2) +
            Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
            Math.sin(dLon/2) * Math.sin(dLon/2);
        return R * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    }

    loadRoutePolyline() {
        try {
            const routes = this.routeData.routeDataJson ? JSON.parse(this.routeData.routeDataJson).routes : [];
            if (routes.length === 0) return;

            const encodedPolyline = routes[0].overview_polyline.points;
            if (!encodedPolyline) return;

            const path = google.maps.geometry.encoding.decodePath(encodedPolyline);

            this.routePolyline = new google.maps.Polyline({
                path: path,
                geodesic: true,
                strokeColor: '#0000FF',
                strokeOpacity: 0.8,
                strokeWeight: 6
            });

            this.routePolyline.setMap(this.map);

        } catch (e) {
            console.error('Błąd ładowania polilinii trasy:', e);
        }
    }

    loadWaypoints() {
        try {
            const routeJson = this.routeData.routeDataJson ? JSON.parse(this.routeData.routeDataJson) : {};
            const excludedInfrastructure = routeJson.excluded_infrastructure || [];

            excludedInfrastructure.forEach(item => {
                const parts = item.split('|');
                if (parts.length < 3) return;

                const latLng = parts[0].split(',');
                const lat = parseFloat(latLng[0]);
                const lng = parseFloat(latLng[1]);
                const name = parts[1];
                const reason = parts[2];

                new google.maps.Marker({
                    position: { lat: lat, lng: lng },
                    map: this.map,
                    icon: {
                        url: 'http://maps.google.com/mapfiles/ms/icons/red-dot.png',
                        scaledSize: new google.maps.Size(32, 32)
                    },
                    title: `OMINIĘTO: ${name} (Powód: ${reason})`
                });
            });
        } catch (e) {
            console.error('Błąd ładowania wykluczonych punktów:', e);
        }
    }

    centerMapOnPosition() {
        if (this.currentPosition && this.map) {
            this.map.setCenter(this.currentPosition);
        } else {
            this.showError('Brak aktualnej pozycji GPS.');
        }
    }

    setupEventListeners() {
        document.getElementById('start-btn').addEventListener('click', () => this.startTracking());
        document.getElementById('stop-btn').addEventListener('click', () => this.stopTracking());
        document.getElementById('center-btn').addEventListener('click', () => this.centerMapOnPosition());
    }

    setupAutoLogout() {}

    showSuccess(message) {
        const instruction = document.getElementById('instruction-text');
        if (instruction) instruction.textContent = '✅ ' + message;
        console.log('SUCCESS:', message);
    }

    showError(message) {
        const instruction = document.getElementById('instruction-text');
        if (instruction) instruction.textContent = '❌ ' + message;
        console.error('ERROR:', message);
    }

    destroy() {
        if (this.isTracking) {
            this.stopTracking();
        }
        if (this.stompClient) {
            this.stompClient.disconnect();
        }
    }
}