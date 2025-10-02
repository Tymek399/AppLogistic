// operator.js - Kompletny dashboard operatora
class OperatorDashboard {
    constructor() {
        this.token = localStorage.getItem('token');
        this.username = localStorage.getItem('username');
        this.map = null;
        this.directionsService = null;
        this.directionsRenderer = null;
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
                center: { lat: 52.0693, lng: 19.4803 },
                mapTypeControl: true,
                streetViewControl: false,
                fullscreenControl: true,
                zoomControl: true
            });

            this.directionsService = new window.google.maps.DirectionsService();
            this.directionsRenderer = new window.google.maps.DirectionsRenderer({
                suppressMarkers: false,
                draggable: false,
                polylineOptions: {
                    strokeColor: '#667eea',
                    strokeWeight: 5,
                    strokeOpacity: 0.8
                }
            });
            this.directionsRenderer.setMap(this.map);

            this.mapInitialized = true;
            console.log('Google Maps initialized');

            this.setupAddressAutocomplete();

        } catch (error) {
            console.error('Error initializing Google Maps:', error);
        }
    }

    setupAddressAutocomplete() {
        if (!window.google || !window.google.maps || !window.google.maps.places) {
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
                        this.showRoutePreview();
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
                        this.showRoutePreview();
                    }
                });
            }
        } catch (error) {
            console.error('Error setting up autocomplete:', error);
        }
    }

    setupEventListeners() {
        const routeForm = document.getElementById('route-form');
        if (routeForm) {
            routeForm.addEventListener('submit', (e) => this.handleRouteSubmit(e));
        }

        const refreshDriversBtn = document.getElementById('refresh-drivers-list');
        if (refreshDriversBtn) {
            refreshDriversBtn.addEventListener('click', () => this.loadDriversList());
        }

        const refreshRoutesBtn = document.getElementById('refresh-routes');
        if (refreshRoutesBtn) {
            refreshRoutesBtn.addEventListener('click', () => this.loadAllRoutes());
        }

        const centerMapBtn = document.getElementById('center-map');
        if (centerMapBtn) {
            centerMapBtn.addEventListener('click', () => this.centerMapOnPoland());
        }

        const startAddr = document.getElementById('start-address');
        const endAddr = document.getElementById('end-address');
        if (startAddr) startAddr.addEventListener('blur', () => this.showRoutePreview());
        if (endAddr) endAddr.addEventListener('blur', () => this.showRoutePreview());
    }

    async showRoutePreview() {
        const startAddr = document.getElementById('start-address')?.value;
        const endAddr = document.getElementById('end-address')?.value;

        if (!startAddr || !endAddr || !this.directionsService || !this.directionsRenderer) {
            return;
        }

        try {
            const request = {
                origin: startAddr + ', Polska',
                destination: endAddr + ', Polska',
                travelMode: window.google.maps.TravelMode.DRIVING,
                unitSystem: window.google.maps.UnitSystem.METRIC,
                avoidHighways: false,
                avoidTolls: true,
                provideRouteAlternatives: false
            };

            this.directionsService.route(request, (result, status) => {
                if (status === 'OK') {
                    this.directionsRenderer.setDirections(result);
                }
            });

        } catch (error) {
            console.error('Error showing route preview:', error);
        }
    }

    async loadTransportSets() {
        try {
            const response = await fetch('/api/vehicles/transport-sets');
            if (!response.ok) throw new Error('Failed to load transport sets');

            this.transportSets = await response.json();
            this.populateTransportSetsSelect();
        } catch (error) {
            console.error('Error loading transport sets:', error);
            this.showError('Błąd podczas ładowania zestawów');
        }
    }

    populateTransportSetsSelect() {
        const select = document.getElementById('transport-set');
        if (!select) return;

        select.innerHTML = '<option value="">Wybierz zestaw...</option>';

        this.transportSets.forEach(set => {
            const option = document.createElement('option');
            option.value = set.id;
            const heightTotal = set.totalHeightCm || 0;
            const heightTrailer = set.trailerHeightCm || 0;
            const heightCargo = set.cargo?.heightCm || 0;
            option.textContent = `${set.transporter.model} + ${set.cargo.model} ` +
                `(${set.totalWeightKg}kg, ${heightTotal}cm = naczepa ${heightTrailer}cm + ładunek ${heightCargo}cm)`;
            select.appendChild(option);
        });
    }

    async loadAllRoutes() {
        const container = document.getElementById('routes-list');
        if (!container) return;

        container.innerHTML = '<div class="text-center p-4"><div class="spinner-border text-primary"></div></div>';

        try {
            const response = await fetch('/api/routes/all');
            if (!response.ok) throw new Error('Failed to load routes');

            this.routes = await response.json();
            this.displayRoutes();
        } catch (error) {
            console.error('Error loading routes:', error);
            container.innerHTML = `<div class="alert alert-danger">Błąd: ${error.message}</div>`;
        }
    }

    displayRoutes() {
        const container = document.getElementById('routes-list');
        if (!container) return;

        if (this.routes.length === 0) {
            container.innerHTML = '<div class="text-center p-4"><h4>Brak tras</h4></div>';
            return;
        }

        container.innerHTML = this.routes.map(route => `
            <div class="route-card mb-3 card">
                <div class="card-header d-flex justify-content-between">
                    <h6 class="mb-0">Trasa #${route.id}</h6>
                    <span class="badge bg-primary">${route.status}</span>
                </div>
                <div class="card-body">
                    <p><strong>Start:</strong> ${route.startAddress}</p>
                    <p><strong>Koniec:</strong> ${route.endAddress}</p>
                    ${this.getRouteWarnings(route)}
                    <button class="btn btn-sm btn-info" onclick="operatorDashboard.showValidationDetails(${route.id})">
                        Szczegóły walidacji
                    </button>
                    ${route.status === 'CREATED' ? `
                        <button class="btn btn-sm btn-primary" onclick="operatorDashboard.showAssignDriverModal(${route.id})">
                            Przypisz kierowcę
                        </button>
                    ` : ''}
                </div>
            </div>
        `).join('');
    }

    getRouteWarnings(route) {
        if (route.warnings && route.warnings.length > 0) {
            return `<div class="alert alert-warning"><ul class="mb-0">${route.warnings.map(w => `<li>${w}</li>`).join('')}</ul></div>`;
        }
        return '';
    }

    async showValidationDetails(routeId) {
        try {
            const response = await fetch(`/api/routes/${routeId}/validation-details`);
            if (!response.ok) throw new Error('Failed');

            const data = await response.json();

            let html = '<div class="modal" style="display:block;background:rgba(0,0,0,0.5);position:fixed;top:0;left:0;right:0;bottom:0;z-index:9999;">';
            html += '<div class="modal-dialog modal-lg"><div class="modal-content">';
            html += '<div class="modal-header"><h5>Walidacja trasy #' + routeId + '</h5>';
            html += '<button class="btn-close" onclick="this.closest(\'.modal\').remove()"></button></div>';
            html += '<div class="modal-body">';

            if (data.validationAvailable) {
                if (data.transportSetInfo) {
                    html += '<h6>Parametry zestawu:</h6><ul>';
                    html += `<li>Wysokość: ${data.transportSetInfo.totalHeight_cm}cm `;
                    if (data.transportSetInfo.trailerHeight_cm) {
                        html += `(naczepa ${data.transportSetInfo.trailerHeight_cm}cm + ładunek ${data.transportSetInfo.cargoHeight_cm}cm)`;
                    }
                    html += '</li>';
                    html += `<li>Waga: ${data.transportSetInfo.totalWeight_kg}kg</li></ul>`;
                }

                if (data.violations && data.violations.length > 0) {
                    html += '<h6>Naruszenia:</h6><ul>';
                    data.violations.forEach(v => html += `<li>${v}</li>`);
                    html += '</ul>';
                }

                if (data.restrictions && data.restrictions.length > 0) {
                    html += '<h6>Ograniczenia:</h6><ul>';
                    data.restrictions.forEach(r => html += `<li>${r}</li>`);
                    html += '</ul>';
                }
            } else {
                html += '<p>Brak danych walidacji</p>';
            }

            html += '</div></div></div></div>';

            const modal = document.createElement('div');
            modal.innerHTML = html;
            document.body.appendChild(modal);

        } catch (error) {
            this.showError('Nie udało się załadować walidacji');
        }
    }

    async loadDriversList() {
        const listDiv = document.getElementById('drivers-list');
        if (!listDiv) return;

        listDiv.innerHTML = '<div class="text-center p-4"><div class="spinner-border"></div></div>';

        try {
            const response = await fetch('/api/admin/users');
            if (!response.ok) throw new Error('Failed');

            const allUsers = await response.json();
            const drivers = allUsers.filter(u => u.role === 'DRIVER');

            if (drivers.length === 0) {
                listDiv.innerHTML = `
                    <div class="text-center p-4">
                        <p>Brak kierowców</p>
                        <button class="btn btn-primary" onclick="operatorDashboard.showAddDriverModal()">Dodaj kierowcę</button>
                    </div>
                `;
                return;
            }

            let activeDrivers = [];
            try {
                const activeResponse = await fetch('/api/tracking/active-drivers');
                if (activeResponse.ok) activeDrivers = await activeResponse.json();
            } catch (e) {}

            listDiv.innerHTML = drivers.map(driver => {
                const activeInfo = activeDrivers.find(ad => ad.driverUsername === driver.username);
                const isOnline = activeInfo && activeInfo.isOnline;

                return `
                    <div class="driver-list-item">
                        <div class="d-flex justify-content-between">
                            <div>
                                <h5>${driver.firstName || driver.username} ${driver.lastName || ''}
                                    <span class="${isOnline ? 'driver-online' : 'driver-offline'}">
                                        ${isOnline ? '🟢 Online' : '⚪ Offline'}
                                    </span>
                                </h5>
                                <small>Login: ${driver.username} | Email: ${driver.email}</small>
                                ${activeInfo ? `<div class="mt-2"><small>Trasa: ${activeInfo.routeDescription || 'Brak'}</small></div>` : ''}
                            </div>
                            <button class="btn btn-sm btn-danger" onclick="operatorDashboard.deleteDriver('${driver.username}')">Usuń</button>
                        </div>
                    </div>
                `;
            }).join('');

            this.updateDriversSelect(drivers);

        } catch (error) {
            listDiv.innerHTML = `<div class="alert alert-danger">Błąd: ${error.message}</div>`;
        }
    }

    updateDriversSelect(drivers) {
        const select = document.getElementById('driver-username');
        if (!select) return;

        select.innerHTML = '<option value="">Wybierz kierowcę...</option>' +
            drivers.map(d => `<option value="${d.username}">${d.firstName || d.username} ${d.lastName || ''}</option>`).join('');
    }

    async showAddDriverModal() {
        const username = prompt('Nazwa użytkownika:');
        if (!username) return;

        const password = prompt('Hasło:');
        if (!password) return;

        const email = prompt('Email:');
        if (!email) return;

        try {
            const response = await fetch('/api/admin/users', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    username: username.trim(),
                    password: password.trim(),
                    email: email.trim(),
                    role: 'DRIVER',
                    firstName: username.trim(),
                    lastName: 'Driver'
                })
            });

            if (response.ok) {
                this.showSuccess('Kierowca dodany!');
                this.loadDriversList();
            } else {
                throw new Error('Failed');
            }
        } catch (error) {
            this.showError('Błąd podczas dodawania kierowcy');
        }
    }

    async deleteDriver(username) {
        if (!confirm(`Usunąć kierowcę ${username}?`)) return;

        try {
            const response = await fetch(`/api/admin/users/${username}`, {
                method: 'DELETE'
            });

            if (response.ok) {
                this.showSuccess('Kierowca usunięty');
                this.loadDriversList();
            }
        } catch (error) {
            this.showError('Błąd usuwania');
        }
    }

    async handleRouteSubmit(event) {
        event.preventDefault();

        const formData = {
            transportSetId: document.getElementById('transport-set').value,
            startAddress: document.getElementById('start-address').value.trim(),
            endAddress: document.getElementById('end-address').value.trim(),
            driverUsername: document.getElementById('driver-username').value.trim()
        };

        if (!formData.transportSetId || !formData.startAddress || !formData.endAddress) {
            this.showError('Wypełnij wszystkie pola');
            return;
        }

        this.setFormLoading(true);

        try {
            const geocoder = new window.google.maps.Geocoder();

            const startLocation = await this.geocodeAddress(geocoder, formData.startAddress);
            formData.startLatitude = startLocation.lat();
            formData.startLongitude = startLocation.lng();

            const endLocation = await this.geocodeAddress(geocoder, formData.endAddress);
            formData.endLatitude = endLocation.lat();
            formData.endLongitude = endLocation.lng();

            const response = await fetch('/api/routes/create', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(formData)
            });

            if (!response.ok) throw new Error('Failed to create route');

            const route = await response.json();

            if (formData.driverUsername) {
                await fetch(`/api/routes/${route.id}/assign-driver?driverUsername=${formData.driverUsername}`, {
                    method: 'POST'
                });
            }

            this.showSuccess('Trasa utworzona!');
            document.getElementById('route-form').reset();
            this.loadAllRoutes();

        } catch (error) {
            this.showError('Błąd: ' + error.message);
        } finally {
            this.setFormLoading(false);
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
                    reject(new Error(`Nie znaleziono: ${address}`));
                }
            });
        });
    }

    showAssignDriverModal(routeId) {
        const username = prompt('Nazwa kierowcy:');
        if (!username) return;

        fetch(`/api/routes/${routeId}/assign-driver?driverUsername=${username}`, {
            method: 'POST'
        }).then(r => {
            if (r.ok) {
                this.showSuccess('Kierowca przypisany');
                this.loadAllRoutes();
            }
        });
    }

    centerMapOnPoland() {
        if (this.map) {
            this.map.setCenter({ lat: 52.0693, lng: 19.4803 });
            this.map.setZoom(6);
        }
    }

    setFormLoading(loading) {
        const btn = document.querySelector('#route-form button[type="submit"]');
        document.querySelectorAll('#route-form input, #route-form select, #route-form button').forEach(el => {
            el.disabled = loading;
        });
        if (btn) btn.textContent = loading ? 'Tworzenie...' : 'Utwórz Trasę';
    }

    startAutoRefresh() {
        if (this.refreshInterval) clearInterval(this.refreshInterval);
        this.refreshInterval = setInterval(() => {
            const section = document.getElementById('drivers-section');
            if (section && section.style.display !== 'none') {
                this.loadDriversList();
            }
        }, 30000);
    }

    showSuccess(msg) {
        this.showNotification(msg, 'success');
    }

    showError(msg) {
        this.showNotification(msg, 'danger');
    }

    showNotification(msg, type = 'info') {
        const notif = document.createElement('div');
        notif.className = `alert alert-${type} alert-dismissible fade show position-fixed`;
        notif.style.cssText = 'top:20px;right:20px;z-index:9999;min-width:300px;';
        notif.innerHTML = `${msg}<button class="btn-close" data-bs-dismiss="alert"></button>`;
        document.body.appendChild(notif);
        setTimeout(() => {
            if (notif.parentNode) notif.parentNode.removeChild(notif);
        }, 5000);
    }

    destroy() {
        if (this.refreshInterval) clearInterval(this.refreshInterval);
    }
}

window.OperatorDashboard = OperatorDashboard;