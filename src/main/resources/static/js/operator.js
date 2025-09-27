// operator.js - Operator Dashboard for Real Google Maps Integration
class OperatorDashboard {
    constructor() {
        this.token = localStorage.getItem('token');
        this.username = localStorage.getItem('username');
        this.map = null;
        this.driverMarkers = {};
        this.transportSets = [];
        this.activeDrivers = [];
        this.refreshInterval = null;
        this.routes = [];
        this.mapInitialized = false;

        this.init();
    }

    init() {
        if (!this.token) {
            window.location.href = '/login.html';
            return;
        }

        this.setupAuth();
        this.loadTransportSets();
        this.setupEventListeners();
        this.startAutoRefresh();
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
        if (!window.google || !window.google.maps) {
            console.error('Google Maps API not available');
            return;
        }

        const mapContainer = document.getElementById('map');
        if (!mapContainer) {
            console.error('Map container not found');
            return;
        }

        try {
            this.map = new window.google.maps.Map(mapContainer, {
                zoom: 6,
                center: { lat: 52.0693, lng: 19.4803 }, // Poland center
                mapTypeControl: true,
                streetViewControl: false,
                fullscreenControl: true,
                zoomControl: true
            });

            this.mapInitialized = true;
            console.log('Google Maps initialized successfully');

            // Setup autocomplete after map initialization
            this.setupAddressAutocomplete();

        } catch (error) {
            console.error('Error initializing Google Maps:', error);
        }
    }

    setupAddressAutocomplete() {
        if (!window.google || !window.google.maps || !window.google.maps.places) {
            console.error('Google Places API not available');
            return;
        }

        const startAddressInput = document.getElementById('start-address');
        const endAddressInput = document.getElementById('end-address');

        try {
            if (startAddressInput) {
                const startAutocomplete = new window.google.maps.places.Autocomplete(startAddressInput, {
                    componentRestrictions: { country: 'pl' },
                    fields: ['address_components', 'geometry', 'formatted_address'],
                    types: ['address']
                });

                startAutocomplete.addListener('place_changed', () => {
                    const place = startAutocomplete.getPlace();
                    if (place.geometry && place.geometry.location) {
                        console.log('Start place selected:', place.formatted_address);
                    }
                });
            }

            if (endAddressInput) {
                const endAutocomplete = new window.google.maps.places.Autocomplete(endAddressInput, {
                    componentRestrictions: { country: 'pl' },
                    fields: ['address_components', 'geometry', 'formatted_address'],
                    types: ['address']
                });

                endAutocomplete.addListener('place_changed', () => {
                    const place = endAutocomplete.getPlace();
                    if (place.geometry && place.geometry.location) {
                        console.log('End place selected:', place.formatted_address);
                    }
                });
            }

            console.log('Address autocomplete initialized successfully');
        } catch (error) {
            console.error('Error setting up autocomplete:', error);
        }
    }

    setupEventListeners() {
        const routeForm = document.getElementById('route-form');
        if (routeForm) {
            routeForm.addEventListener('submit', (e) => this.handleRouteSubmit(e));
        }

        const refreshDriversBtn = document.getElementById('refresh-drivers');
        if (refreshDriversBtn) {
            refreshDriversBtn.addEventListener('click', () => this.updateDriverPositions());
        }

        const refreshRoutesBtn = document.getElementById('refresh-routes');
        if (refreshRoutesBtn) {
            refreshRoutesBtn.addEventListener('click', () => this.loadAllRoutes());
        }

        const centerMapBtn = document.getElementById('center-map');
        if (centerMapBtn) {
            centerMapBtn.addEventListener('click', () => this.centerMapOnPoland());
        }
    }

    async loadTransportSets() {
        try {
            const response = await fetch('/api/vehicles/transport-sets');
            if (!response.ok) {
                throw new Error('Failed to load transport sets');
            }

            this.transportSets = await response.json();
            this.populateTransportSetsSelect();
        } catch (error) {
            console.error('Error loading transport sets:', error);
            this.showError('Błąd podczas ładowania zestawów transportowych');
        }
    }

    populateTransportSetsSelect() {
        const select = document.getElementById('transport-set');
        if (!select) return;

        select.innerHTML = '<option value="">Wybierz zestaw...</option>';

        this.transportSets.forEach(set => {
            const option = document.createElement('option');
            option.value = set.id;
            option.textContent = `${set.transporter.model} + ${set.cargo.model} (${set.totalWeightKg}kg, ${set.totalHeightCm}cm)`;
            select.appendChild(option);
        });
    }

    async loadAllRoutes() {
        const container = document.getElementById('routes-list');
        if (!container) return;

        container.innerHTML = `
            <div class="text-center p-4">
                <div class="spinner-border text-primary" role="status">
                    <span class="visually-hidden">Ładowanie...</span>
                </div>
                <p class="mt-2">Ładowanie tras...</p>
            </div>
        `;

        try {
            const response = await fetch('/api/routes/all');
            if (!response.ok) {
                throw new Error('Failed to load routes');
            }

            this.routes = await response.json();
            this.displayRoutes();
        } catch (error) {
            console.error('Error loading routes:', error);
            container.innerHTML = `
                <div class="alert alert-danger">
                    <h5>Błąd</h5>
                    <p>Nie udało się załadować tras: ${error.message}</p>
                    <button class="btn btn-danger" onclick="operatorDashboard.loadAllRoutes()">
                        Spróbuj ponownie
                    </button>
                </div>
            `;
        }
    }

    displayRoutes() {
        const routesContainer = document.getElementById('routes-list');
        if (!routesContainer) return;

        if (this.routes.length === 0) {
            routesContainer.innerHTML = `
                <div class="text-center p-4">
                    <h4>Brak tras w systemie</h4>
                    <p class="text-muted">Utwórz pierwszą trasę używając formularza obok</p>
                </div>
            `;
            return;
        }

        routesContainer.innerHTML = this.routes.map(route => `
            <div class="route-card mb-3 card">
                <div class="card-header d-flex justify-content-between align-items-center">
                    <h6 class="mb-0">Trasa #${route.id}</h6>
                    <span class="route-status status-${route.status.toLowerCase()}">${this.getStatusText(route.status)}</span>
                </div>
                <div class="card-body">
                    <p><strong>Start:</strong> ${route.startAddress}</p>
                    <p><strong>Koniec:</strong> ${route.endAddress}</p>
                    <div class="row">
                        <div class="col-md-4">
                            <small>Dystans: ${route.totalDistanceKm ? route.totalDistanceKm.toFixed(1) : 'N/A'} km</small>
                        </div>
                        <div class="col-md-4">
                            <small>Czas: ${route.estimatedTimeMinutes || 'N/A'} min</small>
                        </div>
                        <div class="col-md-4">
                            <small>Kierowca: ${route.assignedDriver || 'Brak'}</small>
                        </div>
                    </div>
                    ${this.getRouteWarnings(route)}
                    <div class="mt-2">
                        ${this.getRouteActions(route)}
                    </div>
                </div>
            </div>
        `).join('');
    }

    getRouteWarnings(route) {
        if (route.hasRestrictions || (route.warnings && route.warnings.length > 0)) {
            const warnings = route.warnings || [];
            return `
                <div class="alert alert-warning alert-sm mt-2">
                    <strong>Ostrzeżenia infrastruktury:</strong>
                    <ul class="mb-0 small">
                        ${warnings.map(w => `<li>${w}</li>`).join('')}
                    </ul>
                </div>
            `;
        }
        return '';
    }

    getRouteActions(route) {
        let actions = '';

        if (route.status === 'CREATED') {
            actions += `
                <button class="btn btn-sm btn-primary me-2" onclick="operatorDashboard.showAssignDriverModal(${route.id})">
                    Przypisz kierowcę
                </button>
            `;
        }

        if (route.status !== 'COMPLETED') {
            actions += `
                <button class="btn btn-sm btn-danger" onclick="operatorDashboard.deleteRoute(${route.id})">
                    Usuń
                </button>
            `;
        }

        return actions;
    }

    getStatusText(status) {
        const statusMap = {
            'CREATED': 'Utworzona',
            'ASSIGNED': 'Przypisana',
            'ACTIVE': 'Aktywna',
            'COMPLETED': 'Ukończona'
        };
        return statusMap[status] || status;
    }

    async handleRouteSubmit(event) {
        event.preventDefault();

        const formData = this.getFormData();
        if (!this.validateFormData(formData)) {
            return;
        }

        this.setFormLoading(true);

        try {
            const routeData = await this.geocodeAndCreateRoute(formData);
            this.showRouteCreated(routeData);
            this.resetForm();
            this.loadAllRoutes();
        } catch (error) {
            console.error('Error creating route:', error);
            this.showError('Błąd podczas tworzenia trasy: ' + error.message);
        } finally {
            this.setFormLoading(false);
        }
    }

    getFormData() {
        return {
            transportSetId: document.getElementById('transport-set').value,
            startAddress: document.getElementById('start-address').value.trim(),
            endAddress: document.getElementById('end-address').value.trim(),
            driverUsername: document.getElementById('driver-username').value.trim()
        };
    }

    validateFormData(formData) {
        if (!formData.transportSetId) {
            this.showError('Wybierz zestaw transportowy');
            return false;
        }
        if (!formData.startAddress) {
            this.showError('Podaj adres startowy');
            return false;
        }
        if (!formData.endAddress) {
            this.showError('Podaj adres docelowy');
            return false;
        }
        return true;
    }

    async geocodeAndCreateRoute(formData) {
        if (!window.google || !window.google.maps) {
            throw new Error('Google Maps API nie jest dostępne');
        }

        const geocoder = new window.google.maps.Geocoder();

        try {
            // Geocode start address
            const startLocation = await this.geocodeAddress(geocoder, formData.startAddress);
            formData.startLatitude = startLocation.lat();
            formData.startLongitude = startLocation.lng();

            // Geocode end address
            const endLocation = await this.geocodeAddress(geocoder, formData.endAddress);
            formData.endLatitude = endLocation.lat();
            formData.endLongitude = endLocation.lng();

            // Create route with infrastructure validation
            const routeResponse = await fetch('/api/routes/create', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(formData)
            });

            if (!routeResponse.ok) {
                const errorText = await routeResponse.text();
                throw new Error('Nie udało się utworzyć trasy: ' + errorText);
            }

            const route = await routeResponse.json();

            // Assign driver if specified
            if (formData.driverUsername) {
                await this.assignDriverToRoute(route.id, formData.driverUsername);
            }

            // Show route on map if possible
            this.showRouteOnMap(formData);

            return route;

        } catch (geocodeError) {
            throw new Error('Błąd podczas wyszukiwania adresu: ' + geocodeError.message);
        }
    }

    geocodeAddress(geocoder, address) {
        return new Promise((resolve, reject) => {
            geocoder.geocode({
                address: address + ', Polska',
                componentRestrictions: { country: 'PL' }
            }, (results, status) => {
                if (status === 'OK' && results[0]) {
                    resolve(results[0].geometry.location);
                } else {
                    reject(new Error(`Nie można znaleźć adresu: ${address} (Status: ${status})`));
                }
            });
        });
    }

    showRouteOnMap(routeData) {
        if (!this.map || !this.mapInitialized) {
            console.log('Map not available for route display');
            return;
        }

        try {
            // Clear existing route markers
            // Add start marker
            new window.google.maps.Marker({
                position: { lat: routeData.startLatitude, lng: routeData.startLongitude },
                map: this.map,
                title: 'Start: ' + routeData.startAddress,
                icon: {
                    url: 'http://maps.google.com/mapfiles/ms/icons/green-dot.png',
                    scaledSize: new window.google.maps.Size(32, 32)
                }
            });

            // Add end marker
            new window.google.maps.Marker({
                position: { lat: routeData.endLatitude, lng: routeData.endLongitude },
                map: this.map,
                title: 'Koniec: ' + routeData.endAddress,
                icon: {
                    url: 'http://maps.google.com/mapfiles/ms/icons/red-dot.png',
                    scaledSize: new window.google.maps.Size(32, 32)
                }
            });

            // Draw route line
            const routePath = new window.google.maps.Polyline({
                path: [
                    { lat: routeData.startLatitude, lng: routeData.startLongitude },
                    { lat: routeData.endLatitude, lng: routeData.endLongitude }
                ],
                geodesic: true,
                strokeColor: '#FF0000',
                strokeOpacity: 1.0,
                strokeWeight: 3
            });
            routePath.setMap(this.map);

            // Fit bounds to show entire route
            const bounds = new window.google.maps.LatLngBounds();
            bounds.extend({ lat: routeData.startLatitude, lng: routeData.startLongitude });
            bounds.extend({ lat: routeData.endLatitude, lng: routeData.endLongitude });
            this.map.fitBounds(bounds);

        } catch (error) {
            console.error('Error showing route on map:', error);
        }
    }

    async assignDriverToRoute(routeId, driverUsername) {
        try {
            const response = await fetch(`/api/routes/${routeId}/assign-driver?driverUsername=${encodeURIComponent(driverUsername)}`, {
                method: 'POST'
            });

            if (response.ok) {
                console.log('Driver assigned successfully');
                this.showSuccess(`Kierowca ${driverUsername} został przypisany do trasy ${routeId}`);
            } else {
                console.warn('Failed to assign driver');
                this.showError('Nie udało się przypisać kierowcy');
            }
        } catch (error) {
            console.error('Error assigning driver:', error);
            this.showError('Błąd podczas przypisywania kierowcy');
        }
    }

    async deleteRoute(routeId) {
        if (!confirm('Czy na pewno chcesz usunąć tę trasę?')) {
            return;
        }

        try {
            const response = await fetch(`/api/routes/${routeId}`, {
                method: 'DELETE'
            });

            if (response.ok) {
                this.showSuccess('Trasa została usunięta');
                this.loadAllRoutes();
            } else {
                throw new Error('Nie udało się usunąć trasy');
            }
        } catch (error) {
            console.error('Error deleting route:', error);
            this.showError('Błąd podczas usuwania trasy');
        }
    }

    showAssignDriverModal(routeId) {
        const driverUsername = prompt('Podaj nazwę użytkownika kierowcy:');
        if (driverUsername && driverUsername.trim()) {
            this.assignDriverToRoute(routeId, driverUsername.trim());
        }
    }

    showRouteCreated(route) {
        const resultDiv = document.getElementById('route-result');
        const detailsDiv = document.getElementById('route-details');
        const warningsDiv = document.getElementById('route-warnings');

        if (resultDiv && detailsDiv) {
            detailsDiv.innerHTML = `
                <div>ID Trasy: ${route.id}</div>
                <div>Dystans: ${route.totalDistanceKm ? route.totalDistanceKm.toFixed(1) : 'N/A'} km</div>
                <div>Czas: ${route.estimatedTimeMinutes || 'N/A'} min</div>
                <div>Status walidacji: ${route.hasRestrictions ? 'Znaleziono ograniczenia' : 'Brak ograniczeń'}</div>
            `;

            if (warningsDiv) {
                if (route.warnings && route.warnings.length > 0) {
                    warningsDiv.innerHTML = `
                        <div class="alert alert-warning mt-2">
                            <strong>Ograniczenia infrastruktury:</strong>
                            <ul class="mb-0">
                                ${route.warnings.map(w => `<li>${w}</li>`).join('')}
                            </ul>
                        </div>
                    `;
                } else {
                    warningsDiv.innerHTML = '<div class="alert alert-success mt-2">Trasa bez ograniczeń infrastruktury</div>';
                }
            }

            resultDiv.style.display = 'block';

            setTimeout(() => {
                resultDiv.style.display = 'none';
            }, 8000);
        }

        this.showSuccess('Trasa utworzona z walidacją infrastruktury!');
    }

    async updateDriverPositions() {
        const driversContainer = document.getElementById('active-drivers-list');

        if (driversContainer) {
            driversContainer.innerHTML = `
                <div class="text-center p-3">
                    <div class="spinner-border text-primary spinner-border-sm" role="status">
                        <span class="visually-hidden">Ładowanie...</span>
                    </div>
                    <p class="mt-2 mb-0 small">Ładowanie kierowców...</p>
                </div>
            `;
        }

        try {
            const response = await fetch('/api/tracking/active-drivers');
            if (!response.ok) {
                throw new Error('Failed to fetch drivers');
            }

            this.activeDrivers = await response.json();
            this.updateDriversOnMap();
            this.updateDriversList();
        } catch (error) {
            console.error('Error fetching active drivers:', error);
            if (driversContainer) {
                driversContainer.innerHTML = `
                    <div class="alert alert-warning">
                        <small>Błąd podczas ładowania kierowców</small>
                    </div>
                `;
            }
        }
    }

    updateDriversOnMap() {
        if (!this.mapInitialized || !this.map) {
            console.log('Map not initialized, skipping driver markers update');
            return;
        }

        // Clear existing driver markers
        Object.values(this.driverMarkers).forEach(marker => marker.setMap(null));
        this.driverMarkers = {};

        this.activeDrivers.forEach(driver => {
            if (driver.latitude && driver.longitude) {
                const marker = new window.google.maps.Marker({
                    position: { lat: driver.latitude, lng: driver.longitude },
                    map: this.map,
                    title: `${driver.driverUsername} - ${driver.speedKmh || 0} km/h`,
                    icon: {
                        url: driver.isOnline ?
                            'http://maps.google.com/mapfiles/ms/icons/blue-dot.png' :
                            'http://maps.google.com/mapfiles/ms/icons/orange-dot.png',
                        scaledSize: new window.google.maps.Size(32, 32)
                    }
                });

                const infoWindow = new window.google.maps.InfoWindow({
                    content: `
                        <div>
                            <strong>${driver.driverUsername}</strong><br>
                            Trasa: ${driver.routeDescription || 'Brak'}<br>
                            Prędkość: ${driver.speedKmh || 0} km/h<br>
                            Status: ${driver.isOnline ? 'Online' : 'Offline'}<br>
                            Ostatnia aktualizacja: ${driver.lastUpdate ? new Date(driver.lastUpdate).toLocaleTimeString() : 'N/A'}
                        </div>
                    `
                });

                marker.addListener('click', () => {
                    infoWindow.open(this.map, marker);
                });

                this.driverMarkers[driver.driverUsername] = marker;
            }
        });
    }

    updateDriversList() {
        const listDiv = document.getElementById('active-drivers-list');
        if (!listDiv) return;

        if (this.activeDrivers.length === 0) {
            listDiv.innerHTML = `
                <div class="text-center p-3">
                    <p class="text-muted mb-0">Brak aktywnych kierowców</p>
                    <small class="text-muted">Kierowcy pojawią się po uruchomieniu nawigacji</small>
                </div>
            `;
            return;
        }

        listDiv.innerHTML = this.activeDrivers.map(driver => `
            <div class="driver-card">
                <div class="d-flex justify-content-between align-items-center">
                    <strong>${driver.driverUsername}</strong>
                    <span class="${driver.isOnline ? 'driver-online' : 'driver-offline'}">
                        ${driver.isOnline ? 'Online' : 'Offline'}
                    </span>
                </div>
                <div class="small text-muted">
                    ${driver.routeDescription || 'Brak aktywnej trasy'}<br>
                    Prędkość: ${driver.speedKmh || 0} km/h<br>
                    ${driver.lastUpdate ? new Date(driver.lastUpdate).toLocaleString() : 'Brak danych'}
                </div>
            </div>
        `).join('');
    }

    centerMapOnPoland() {
        if (this.map && this.mapInitialized) {
            this.map.setCenter({ lat: 52.0693, lng: 19.4803 });
            this.map.setZoom(6);
        } else {
            this.showError('Mapa nie jest dostępna');
        }
    }

    setFormLoading(loading) {
        const submitBtn = document.querySelector('#route-form button[type="submit"]');
        const formElements = document.querySelectorAll('#route-form input, #route-form select, #route-form button');

        formElements.forEach(el => {
            el.disabled = loading;
        });

        if (submitBtn) {
            if (loading) {
                submitBtn.innerHTML = '<span class="spinner-border spinner-border-sm me-2"></span>Tworzenie trasy...';
            } else {
                submitBtn.textContent = 'Utwórz Trasę';
            }
        }
    }

    resetForm() {
        const form = document.getElementById('route-form');
        if (form) {
            form.reset();
        }
    }

    startAutoRefresh() {
        if (this.refreshInterval) {
            clearInterval(this.refreshInterval);
        }

        this.refreshInterval = setInterval(() => {
            const driversSection = document.getElementById('drivers-section');
            if (driversSection && driversSection.style.display !== 'none') {
                this.updateDriverPositions();
            }
        }, 30000);
    }

    showSuccess(message) {
        this.showNotification(message, 'success');
    }

    showError(message) {
        this.showNotification(message, 'danger');
    }

    showNotification(message, type = 'info') {
        const notification = document.createElement('div');
        notification.className = `alert alert-${type} alert-dismissible fade show position-fixed`;
        notification.style.cssText = 'top: 20px; right: 20px; z-index: 9999; min-width: 300px; max-width: 400px;';
        notification.innerHTML = `
            ${message}
            <button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
        `;

        document.body.appendChild(notification);

        setTimeout(() => {
            if (notification.parentNode) {
                notification.parentNode.removeChild(notification);
            }
        }, 5000);

        const closeBtn = notification.querySelector('.btn-close');
        if (closeBtn) {
            closeBtn.addEventListener('click', () => {
                if (notification.parentNode) {
                    notification.parentNode.removeChild(notification);
                }
            });
        }
    }

    destroy() {
        if (this.refreshInterval) {
            clearInterval(this.refreshInterval);
        }
    }
}

// Make OperatorDashboard globally available (no ES6 modules)
window.OperatorDashboard = OperatorDashboard;