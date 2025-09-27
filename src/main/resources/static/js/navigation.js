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

        this.init();
    }

    init() {
        if (!this.token) {
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

        // Get route coordinates from global variables or defaults
        const startLat = window.startLat || 52.2297;
        const startLng = window.startLng || 21.0122;
        const endLat = window.endLat || 52.4064;
        const endLng = window.endLng || 16.9252;
        const startAddress = window.startAddress || 'Start';
        const endAddress = window.endAddress || 'End';

        // Center map on route
        const centerLat = (startLat + endLat) / 2;
        const centerLng = (startLng + endLng) / 2;

        this.map = new google.maps.Map(document.getElementById('map'), {
            zoom: 10,
            center: { lat: centerLat, lng: centerLng }
        });

        // Start marker
        new google.maps.Marker({
            position: { lat: startLat, lng: startLng },
            map: this.map,
            title: `Start: ${startAddress}`,
            icon: 'http://maps.google.com/mapfiles/ms/icons/green-dot.png'
        });

        // End marker
        new google.maps.Marker({
            position: { lat: endLat, lng: endLng },
            map: this.map,
            title: `Koniec: ${endAddress}`,
            icon: 'http://maps.google.com/mapfiles/ms/icons/red-dot.png'
        });

        // Route line
        this.routePolyline = new google.maps.Polyline({
            path: [
                { lat: startLat, lng: startLng },
                { lat: endLat, lng: endLng }
            ],
            geodesic: true,
            strokeColor: '#FF0000',
            strokeOpacity: 1.0,
            strokeWeight: 3
        });
        this.routePolyline.setMap(this.map);

        // Fit map to route bounds
        const bounds = new google.maps.LatLngBounds();
        bounds.extend({ lat: startLat, lng: startLng });
        bounds.extend({ lat: endLat, lng: endLng });
        this.map.fitBounds(bounds);

        console.log('Navigation map initialized');
    }

    setupEventListeners() {
        const startBtn = document.getElementById('start-tracking');
        const stopBtn = document.getElementById('stop-tracking');
        const centerBtn = document.getElementById('center-map');

        if (startBtn) {
            startBtn.addEventListener('click', () => this.startTracking());
        }

        if (stopBtn) {
            stopBtn.addEventListener('click', () => this.stopTracking());
        }

        if (centerBtn) {
            centerBtn.addEventListener('click', () => this.centerMapOnPosition());
        }
    }

    async startTracking() {
        if (!navigator.geolocation) {
            this.showError('Geolokalizacja nie jest obsługiwana przez tę przeglądarkę.');
            return;
        }

        this.updateStatus('Łączenie...', 'offline');

        try {
            // Login to tracking session
            const response = await fetch(`/api/tracking/login?routeId=${this.routeId}`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });

            if (!response.ok) {
                throw new Error('Błąd logowania do sesji');
            }

            // Start GPS tracking
            this.watchId = navigator.geolocation.watchPosition(
                (position) => this.handlePositionUpdate(position),
                (error) => this.handlePositionError(error),
                {
                    enableHighAccuracy: true,
                    timeout: 10000,
                    maximumAge: 5000
                }
            );

            this.isTracking = true;
            this.updateButtonStates();
            this.updateStatus('GPS Aktywny', 'online');

            // Start periodic updates
            this.startPeriodicUpdates();

        } catch (error) {
            console.error('Error starting tracking:', error);
            this.updateStatus('Błąd logowania', 'offline');
            this.showError('Nie można uruchomić nawigacji. Sprawdź połączenie.');
        }
    }

    async stopTracking() {
        if (this.watchId) {
            navigator.geolocation.clearWatch(this.watchId);
            this.watchId = null;
        }

        if (this.updateInterval) {
            clearInterval(this.updateInterval);
            this.updateInterval = null;
        }

        try {
            await fetch('/api/tracking/logout', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });
        } catch (error) {
            console.error('Error logging out:', error);
        }

        this.isTracking = false;
        this.updateButtonStates();
        this.updateStatus('GPS Nieaktywny', 'offline');
    }

    handlePositionUpdate(position) {
        const positionData = {
            latitude: position.coords.latitude,
            longitude: position.coords.longitude,
            speedKmh: position.coords.speed ? position.coords.speed * 3.6 : 0,
            accuracy: position.coords.accuracy,
            batteryLevel: null,
            timestamp: new Date().toISOString()
        };

        // Try to get battery level
        if ('getBattery' in navigator) {
            navigator.getBattery().then(battery => {
                positionData.batteryLevel = Math.round(battery.level * 100);
                this.updateBatteryDisplay(positionData.batteryLevel);
            }).catch(() => {
                // Battery API not available
            });
        }

        // Send position to backend
        this.sendPositionUpdate(positionData);

        // Update UI and map
        this.updateUI(positionData);
        this.updateMapPosition(positionData);
    }

    async sendPositionUpdate(positionData) {
        try {
            const response = await fetch('/api/tracking/position', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(positionData)
            });

            if (!response.ok) {
                console.warn('Failed to send position update');
            }
        } catch (error) {
            console.error('Error sending position:', error);
        }
    }

    updateUI(position) {
        this.updateElement('speed', `Prędkość: ${Math.round(position.speedKmh)} km/h`);
        this.updateElement('accuracy', `Dokładność: ${Math.round(position.accuracy)}m`);

        // Update connection status
        this.updateStatus('GPS Aktywny', 'online');
    }

    updateMapPosition(position) {
        this.currentPosition = {
            lat: position.latitude,
            lng: position.longitude
        };

        // Remove old marker
        if (this.currentMarker) {
            this.currentMarker.setMap(null);
        }

        // Add current position marker
        this.currentMarker = new google.maps.Marker({
            position: this.currentPosition,
            map: this.map,
            title: 'Twoja pozycja',
            icon: {
                url: 'http://maps.google.com/mapfiles/ms/icons/blue-dot.png',
                scaledSize: new google.maps.Size(32, 32)
            }
        });

        // Check if close to destination
        this.checkDestinationProximity(position);
    }

    checkDestinationProximity(position) {
        const endLat = window.endLat || 52.4064;
        const endLng = window.endLng || 16.9252;

        const distance = this.calculateDistance(
            position.latitude, position.longitude,
            endLat, endLng
        );

        // If within 100 meters of destination
        if (distance < 0.1) {
            this.showSuccess('Dotarłeś do celu!');
            // Optionally complete the route automatically
            // this.completeRoute();
        }
    }

    calculateDistance(lat1, lon1, lat2, lon2) {
        const R = 6371; // Earth radius in km
        const dLat = this.toRadians(lat2 - lat1);
        const dLon = this.toRadians(lon2 - lon1);
        const a = Math.sin(dLat/2) * Math.sin(dLat/2) +
            Math.cos(this.toRadians(lat1)) * Math.cos(this.toRadians(lat2)) *
            Math.sin(dLon/2) * Math.sin(dLon/2);
        const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
        return R * c;
    }

    toRadians(degrees) {
        return degrees * (Math.PI/180);
    }

    handlePositionError(error) {
        let message = 'Błąd GPS: ';
        switch(error.code) {
            case error.PERMISSION_DENIED:
                message += 'Dostęp do geolokalizacji został odrzucony.';
                break;
            case error.POSITION_UNAVAILABLE:
                message += 'Informacje o lokalizacji są niedostępne.';
                break;
            case error.TIMEOUT:
                message += 'Żądanie lokalizacji przekroczyło limit czasu.';
                break;
            default:
                message += 'Wystąpił nieznany błąd.';
                break;
        }
        this.updateStatus(message, 'offline');
        this.showError(message);
    }

    centerMapOnPosition() {
        if (this.currentPosition && this.map) {
            this.map.setCenter(this.currentPosition);
            this.map.setZoom(16);
        } else {
            this.showError('Brak danych o aktualnej pozycji.');
        }
    }

    startPeriodicUpdates() {
        // Update UI every 5 seconds
        this.updateInterval = setInterval(() => {
            if (this.isTracking && this.currentPosition) {
                // Additional periodic checks can go here
                this.updateConnectionStatus();
            }
        }, 5000);
    }

    updateConnectionStatus() {
        // Check if we're still receiving GPS updates
        const now = Date.now();
        if (this.lastUpdateTime && (now - this.lastUpdateTime) > 30000) {
            this.updateStatus('GPS - Brak sygnału', 'offline');
        }
    }

    // UI Helper methods
    updateStatus(message, status) {
        const statusElement = document.getElementById('connection-status');
        if (statusElement) {
            statusElement.textContent = message;
            statusElement.className = `status-${status}`;
        }
    }

    updateElement(id, text) {
        const element = document.getElementById(id);
        if (element) {
            element.textContent = text;
        }
    }

    updateBatteryDisplay(level) {
        this.updateElement('battery', `Bateria: ${level}%`);
    }

    updateButtonStates() {
        const startBtn = document.getElementById('start-tracking');
        const stopBtn = document.getElementById('stop-tracking');

        if (startBtn) startBtn.disabled = this.isTracking;
        if (stopBtn) stopBtn.disabled = !this.isTracking;
    }

    setupAutoLogout() {
        window.addEventListener('beforeunload', () => {
            if (this.isTracking) {
                navigator.sendBeacon('/api/tracking/logout');
            }
        });

        // Visibility API to handle tab switching
        document.addEventListener('visibilitychange', () => {
            if (document.hidden && this.isTracking) {
                // Tab is hidden, continue tracking in background
                console.log('Tab hidden, continuing GPS tracking');
            }
        });
    }

    // Notification methods
    showSuccess(message) {
        this.showNotification(message, 'success');
    }

    showError(message) {
        this.showNotification(message, 'error');
    }

    showNotification(message, type = 'info') {
        // Create notification
        const notification = document.createElement('div');
        notification.className = `alert alert-${type === 'error' ? 'danger' : type} alert-dismissible fade show position-fixed`;
        notification.style.cssText = 'top: 20px; right: 20px; z-index: 9999; min-width: 300px;';
        notification.innerHTML = `
            ${message}
            <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
        `;

        document.body.appendChild(notification);

        // Auto remove
        setTimeout(() => {
            if (notification.parentNode) {
                notification.parentNode.removeChild(notification);
            }
        }, 5000);
    }

    destroy() {
        if (this.isTracking) {
            this.stopTracking();
        }
        if (this.updateInterval) {
            clearInterval(this.updateInterval);
        }
    }
}

// Global functions for backward compatibility
function initNavigation(routeId, routeData) {
    window.navigationSystem = new NavigationSystem(routeId, routeData);
}

function startTracking() {
    if (window.navigationSystem) {
        window.navigationSystem.startTracking();
    }
}

function stopTracking() {
    if (window.navigationSystem) {
        window.navigationSystem.stopTracking();
    }
}

function centerMapOnPosition() {
    if (window.navigationSystem) {
        window.navigationSystem.centerMapOnPosition();
    }
}

// Initialize on window load
window.addEventListener('load', () => {
    // Get route data from global variables set by Thymeleaf
    const routeId = window.routeId;
    if (routeId) {
        initNavigation(routeId, window.routeData || {});
    }
});

// Cleanup on unload
window.addEventListener('beforeunload', () => {
    if (window.navigationSystem) {
        window.navigationSystem.destroy();
    }
});