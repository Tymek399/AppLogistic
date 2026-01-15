// operator.js - Wersja z pełną walidacją naczep
class OperatorDashboard {
    constructor() {
        this.token = localStorage.getItem('token');
        this.username = localStorage.getItem('username');
        this.map = null;
        this.directionsService = null;
        this.directionsRenderer = null;
        this.previewRenderer = null;
        this.driverMarkers = {};
        this.transportSets = [];
        this.allTrailers = [];
        this.activeDrivers = [];
        this.refreshInterval = null;
        this.routes = [];
        this.mapInitialized = false;
        this.rejectedPointsMarkers = [];
        this.routesRequiringAcceptance = [];

        this.allCargo = [];
        this.selectedTransportMode = 'trailer';
        this.routePreviewMap = null;
        this.driverLocationMap = null;
        this.driverLocationMarker = null;
        this.driverLocationInterval = null;
        this.currentTrackedDriver = null;

        this.drivers = [];
        this.addDriverModalInstance = null;
        this.assignDriverModalInstance = null;
    }

    init() {
        const username = localStorage.getItem('username');
        if (username) document.getElementById('username').textContent = username;

        this.loadTransportSets();
        this.loadTrailers();
        this.setupEventListeners();
        this.startAutoRefresh();
        this.loadRoutesRequiringAcceptance();

        if (window.bootstrap) {
            const addModalEl = document.getElementById('addDriverModal');
            if (addModalEl) {
                this.addDriverModalInstance = new window.bootstrap.Modal(addModalEl);
            }
            const assignModalEl = document.getElementById('assignDriverModal');
            if (assignModalEl) {
                this.assignDriverModalInstance = new window.bootstrap.Modal(assignModalEl);
            }
        }
    }

    setupAuth() {
        const originalFetch = window.fetch;
        window.fetch = (...args) => {
            const headers = (args[1] && args[1].headers) ? { ...args[1].headers } : {};
            headers['Authorization'] = `Bearer ${this.token}`;

            if (args[1]) {
                args[1].headers = headers;
            } else {
                args[1] = { headers };
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
                polylineOptions: { strokeColor: '#667eea', strokeWeight: 5, strokeOpacity: 0.8 }
            });
            this.directionsRenderer.setMap(this.map);

            this.previewRenderer = new window.google.maps.DirectionsRenderer({
                suppressMarkers: true,
                draggable: false,
                polylineOptions: { strokeColor: '#FFA500', strokeWeight: 4, strokeOpacity: 0.7, zIndex: 1 }
            });
            this.previewRenderer.setMap(this.map);

            const routePreviewEl = document.getElementById('route-preview-map');
            if (routePreviewEl) {
                this.routePreviewMap = new google.maps.Map(routePreviewEl, {
                    zoom: 10,
                    center: { lat: 52.0693, lng: 19.4803 }
                });
            }

            const driverLocEl = document.getElementById('driver-location-map');
            if (driverLocEl) {
                this.driverLocationMap = new google.maps.Map(driverLocEl, {
                    zoom: 16,
                    center: { lat: 52.2297, lng: 21.0122 }
                });
            }

            this.mapInitialized = true;
            console.log('Google Maps initialized');

            this.setupAddressAutocomplete();

        } catch (error) {
            console.error('Error initializing Google Maps:', error);
        }
    }

    setupAddressAutocomplete() {
        if (!window.google || !window.google.maps || !window.google.maps.places) return;

        const startAddressInput = document.getElementById('start-address');
        const endAddressInput = document.getElementById('end-address');

        try {
            const options = {
                componentRestrictions: { country: 'pl' },
                fields: ['address_components', 'geometry', 'formatted_address'],
                types: ['address']
            };

            if (startAddressInput) {
                const startAutocomplete = new window.google.maps.places.Autocomplete(startAddressInput, options);
                startAutocomplete.addListener('place_changed', () => this.showRoutePreview());
            }

            if (endAddressInput) {
                const endAutocomplete = new window.google.maps.places.Autocomplete(endAddressInput, options);
                endAutocomplete.addListener('place_changed', () => this.showRoutePreview());
            }
        } catch (error) {
            console.error('Error setting up autocomplete:', error);
        }
    }

    setupEventListeners() {
        document.getElementById('route-form')?.addEventListener('submit', (e) => this.handleRouteSubmit(e));
        document.getElementById('refresh-drivers-list')?.addEventListener('click', () => this.loadDriversList());
        document.getElementById('refresh-routes')?.addEventListener('click', () => this.loadAllRoutes());
        document.getElementById('center-map')?.addEventListener('click', () => this.centerMapOnPoland());
        document.getElementById('start-address')?.addEventListener('blur', () => this.showRoutePreview());
        document.getElementById('end-address')?.addEventListener('blur', () => this.showRoutePreview());

        document.getElementById('routes-tab')?.addEventListener('click', () => switchTab('routes'));
        document.getElementById('drivers-tab')?.addEventListener('click', () => switchTab('drivers'));
        document.getElementById('vehicles-tab')?.addEventListener('click', () => switchTab('vehicles'));
        document.getElementById('add-equipment-tab')?.addEventListener('click', () => switchTab('add-equipment'));
        document.getElementById('trailers-tab')?.addEventListener('click', () => switchTab('trailers'));

        document.getElementById('vehicle-form')?.addEventListener('submit', (e) => handleVehicleFormSubmit(e));
        document.getElementById('add-transporter-form')?.addEventListener('submit', (e) => handleAddTransporterSubmit(e));
        document.getElementById('add-cargo-form')?.addEventListener('submit', (e) => handleAddCargoSubmit(e));

        document.getElementById('add-trailer-form')?.addEventListener('submit', (e) => handleAddTrailerSubmit(e));
        document.getElementById('add-transport-set-form')?.addEventListener('submit', (e) => handleAddNewTransportSetSubmit(e));

        document.getElementById('cargo-select')?.addEventListener('change', () => checkCargoCapabilities());

        document.getElementById('add-driver-form')?.addEventListener('submit', (e) => this.handleAddDriverSubmit(e));
        document.getElementById('assign-driver-form')?.addEventListener('submit', (e) => this.handleAssignDriverSubmit(e));
    }

    async showRoutePreview() {
        const startAddr = document.getElementById('start-address')?.value;
        const endAddr = document.getElementById('end-address')?.value;
        if (!startAddr || !endAddr || !this.directionsService || !this.previewRenderer) return;

        try {
            const request = {
                origin: startAddr + ', Polska',
                destination: endAddr + ', Polska',
                travelMode: window.google.maps.TravelMode.DRIVING
            };

            this.directionsService.route(request, (result, status) => {
                if (status === 'OK') {
                    this.directionsRenderer.setDirections({ routes: [] });
                    this.previewRenderer.setDirections(result);
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
            this.showError('Błąd podczas ładowania zestawów');
        }
    }

    async loadTrailers() {
        try {
            const response = await fetch('/api/trailers');
            if (!response.ok) {
                throw new Error(`Failed to load trailers: ${response.statusText}`);
            }
            this.allTrailers = await response.json();

            this.populateTrailersSelect();
            this.displayTrailersList();

        } catch (error) {
            this.allTrailers = [];
            console.error('Błąd podczas ładowania naczep:', error);
            this.showError('Błąd podczas ładowania naczep: ' + error.message);
            this.populateTrailersSelect();
            this.displayTrailersList();
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // ZAKTUALIZOWANA METODA - Wyświetla więcej informacji w selectcie naczep
    // ═══════════════════════════════════════════════════════════════════════════
    populateTrailersSelect() {
        const select = document.getElementById('transport-set-trailer-select');
        if (!select) return;

        select.innerHTML = '<option value="">Wybierz naczepę...</option>';

        this.allTrailers.forEach(trailer => {
            const option = document.createElement('option');
            option.value = trailer.id;

            // Wyświetl więcej informacji o naczepie
            const axles = trailer.numberOfAxles ? `${trailer.numberOfAxles} osi` : '';
            const weight = trailer.emptyWeight ? `${trailer.emptyWeight}kg` : '';
            const details = [trailer.type, trailer.maxPayload + 'kg ład.', axles, weight]
                .filter(Boolean)
                .join(', ');

            option.textContent = `${trailer.registrationNumber} (${details})`;
            select.appendChild(option);
        });
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // ZAKTUALIZOWANA METODA - Wyświetla nowe pola naczepy w liście
    // ═══════════════════════════════════════════════════════════════════════════
    displayTrailersList() {
        const container = document.getElementById('trailers-list');
        if (!container) return;

        if (this.allTrailers.length === 0) {
            container.innerHTML = `
                <div class="alert alert-secondary text-center">
                    <p class="mb-2">🚛 Brak zarejestrowanych naczep</p>
                    <small>Dodaj naczepę w zakładce "Dodaj Sprzęt"</small>
                </div>
            `;
            return;
        }

        container.innerHTML = this.allTrailers.map(trailer => `
            <div class="card mb-3 border-info">
                <div class="card-header bg-light py-2">
                    <div class="d-flex justify-content-between align-items-center">
                        <div>
                            <strong class="fs-5 text-primary">${this.escapeHtml(trailer.registrationNumber)}</strong>
                            <span class="badge bg-info text-dark ms-2">${this.escapeHtml(trailer.type || 'Brak typu')}</span>
                        </div>
                        <span class="badge bg-secondary">ID: ${trailer.id}</span>
                    </div>
                </div>
                <div class="card-body py-3">
                    <div class="row">
                        <div class="col-md-4">
                            <h6 class="text-muted mb-2">📋 Identyfikacja</h6>
                            <small class="d-block"><strong>VIN:</strong> ${this.escapeHtml(trailer.vin || '-')}</small>
                        </div>
                        <div class="col-md-4">
                            <h6 class="text-muted mb-2">📐 Wymiary</h6>
                            <small class="d-block"><strong>D×S×W:</strong> ${trailer.length || '?'}m × ${trailer.width || '?'}m × ${trailer.height || '?'}m</small>
                            <small class="d-block"><strong>Max ładowność:</strong> ${trailer.maxPayload ? trailer.maxPayload.toLocaleString() + ' kg' : '?'}</small>
                        </div>
                        <div class="col-md-4">
                            <h6 class="text-muted mb-2">⚙️ Parametry techniczne</h6>
                            <small class="d-block"><strong>Liczba osi:</strong> ${trailer.numberOfAxles || '?'}</small>
                            <small class="d-block"><strong>Wys. bez ładunku:</strong> ${trailer.unloadedHeight ? trailer.unloadedHeight + ' m' : '?'}</small>
                            <small class="d-block"><strong>Masa własna:</strong> ${trailer.emptyWeight ? trailer.emptyWeight.toLocaleString() + ' kg' : '?'}</small>
                        </div>
                    </div>
                </div>
            </div>
        `).join('');

    }
    escapeHtml(unsafe) {
        // Obsługa undefined, null, i pustych wartości
        if (unsafe === undefined || unsafe === null || unsafe === '') {
            return '';
        }

        // Jeśli to tablica lub obiekt, zwróć pusty string
        if (typeof unsafe === 'object') {
            console.warn('escapeHtml received object:', unsafe);
            return '';
        }

        // Konwersja do stringa
        const text = String(unsafe);

        return text
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&#039;");
    }

    populateTransportSetsSelect() {
        const select = document.getElementById('transport-set');
        if (!select) return;
        select.innerHTML = '<option value="">Wybierz zestaw...</option>';
        this.transportSets.forEach(set => {
            const option = document.createElement('option');
            option.value = set.id;
            const heightTotal = set.totalHeightCm || 0;
            const weightTotal = set.totalWeightKg || 0;
            option.textContent = `${set.description} (${weightTotal}kg, ${heightTotal}cm)`;
            select.appendChild(option);
        });
    }

    async loadRoutesRequiringAcceptance() {
        try {
            const response = await fetch('/api/routes/requiring-acceptance');
            if (!response.ok) throw new Error('Failed to load routes requiring acceptance');
            this.routesRequiringAcceptance = await response.json();
            this.displayRoutesRequiringAcceptance();
        } catch (error) {
            console.error('Error loading routes requiring acceptance:', error);
        }
    }

    displayRoutesRequiringAcceptance() {
        const container = document.getElementById('routes-requiring-acceptance');
        if (!container) return;
        if (this.routesRequiringAcceptance.length === 0) {
            container.innerHTML = '';
            return;
        }
        container.innerHTML = `
            <div class="alert alert-warning">
                <h5>⚠️ Trasy wymagające akceptacji operatora (${this.routesRequiringAcceptance.length})</h5>
                <div class="routes-acceptance-list mt-3">
                    ${this.routesRequiringAcceptance.map(route => this.renderRouteRequiringAcceptance(route)).join('')}
                </div>
            </div>
        `;
    }

    renderRouteRequiringAcceptance(route) {
        let rejectedPointsHtml = '';
        const transportInfo = route.routeData?.transportSetInfo || {};
        const transportWeightTons = transportInfo.weightTon;
        const transportHeightMeters = transportInfo.heightM;

        if (route.rejectedPoints && route.rejectedPoints.length > 0) {
            rejectedPointsHtml = `
            <div class="rejected-points mb-3">
                <h6 class="text-danger border-bottom pb-2">Zidentyfikowane punkty problematyczne (${route.rejectedPoints.length}):</h6>
                <div id="decision-list-${route.id}">
                    ${route.rejectedPoints.map((point, idx) => {
                const reasonArray = point.reason || [];
                const mainReason = reasonArray.length > 0 ? reasonArray[0] : 'Brak szczegółowego powodu.';

                // --- POPRAWKA: Tytuł punktu z powodem ---
                const shortReason = mainReason.split(' (')[0] || mainReason;
                const displayName = `<strong>${this.escapeHtml(point.name || 'Obiekt')}</strong> - <span class="text-danger">${this.escapeHtml(shortReason)}</span>`;

                const weightMatch = mainReason.match(/\(Limit nośności: ([\d.]+t)\)/);
                const heightMatch = mainReason.match(/\(Limit wysokości: ([\d.]+m)\)/);

                let limitInfo = '';

                if (weightMatch || heightMatch) {
                    limitInfo += '<ul class="list-unstyled mt-2 mb-0" style="font-size:0.9em; border-left: 2px solid #dee2e6; padding-left: 10px;">';
                    if (weightMatch) {
                        const limitStr = weightMatch[1];
                        const limit = parseFloat(limitStr.replace('t', ''));
                        const actual = transportWeightTons ? transportWeightTons : 0;
                        const isViolation = actual > limit;

                        // Dodano wyliczenie o ile przekroczono
                        const diff = (actual - limit).toFixed(1);

                        limitInfo += `<li class="${isViolation ? 'text-danger fw-bold' : 'text-success'}">
                        <strong>Masa:</strong> ${actual.toFixed(1)}t (Limit: ${limitStr}) 
                        ${isViolation ? `❌ Przekroczono o ${diff}t` : '✅ OK'}
                    </li>`;
                    }
                    if (heightMatch) {
                        const limitStr = heightMatch[1];
                        const limit = parseFloat(limitStr.replace('m', ''));
                        const actual = transportHeightMeters ? transportHeightMeters : 0;
                        const isViolation = actual > limit;

                        limitInfo += `<li class="${isViolation ? 'text-danger fw-bold' : 'text-success'}">
                        <strong>Wysokość:</strong> ${actual.toFixed(2)}m (Limit: ${limitStr}) 
                        ${isViolation ? `❌ Przekroczono o ${(actual - limit).toFixed(2)}m` : '✅ OK'}
                    </li>`;
                    }
                    limitInfo += '</ul>';
                }

                const cityInfo = point.city ? `<div class="text-muted small mt-1"><strong>📍 Miasto:</strong> ${this.escapeHtml(point.city)}</div>` : '';

                return `
                        <div class="rejected-point-item border rounded p-3 mb-2 bg-white" data-point-name="${this.escapeHtml(point.name || 'Unknown Point')}">
                            <div class="point-details mb-2">
                                ${displayName}
                                ${cityInfo}
                            </div>
                            <div class="point-reason mb-3">
                                ${limitInfo}
                                <div class="text-muted small mt-1" style="font-size: 0.8em;">Log: ${this.escapeHtml(mainReason)}</div>
                            </div>
                            <div class="decision-radios d-flex gap-3 align-items-center">
                                <div class="form-check">
                                    <input class="form-check-input" type="radio" name="decision-${route.id}-${idx}" id="accept-${route.id}-${idx}" value="ACCEPTED">
                                    <label class="form-check-label text-success fw-bold" for="accept-${route.id}-${idx}">✅ Akceptuj</label>
                                </div>
                                <div class="form-check">
                                    <input class="form-check-input" type="radio" name="decision-${route.id}-${idx}" id="reject-${route.id}-${idx}" value="REJECTED" checked>
                                    <label class="form-check-label text-danger fw-bold" for="reject-${route.id}-${idx}">❌ Odrzuć</label>
                                </div>
                                <div class="flex-grow-1">
                                    <textarea id="comment-${route.id}-${idx}" class="form-control form-control-sm" rows="1"
                                              placeholder="Komentarz..."></textarea>
                                </div>
                            </div>
                        </div>
                    `;
            }).join('')}
            </div>
        </div>
    `;
        }

        let operatorMessagesHtml = '';
        if (route.operatorMessages && route.operatorMessages.length > 0) {
            operatorMessagesHtml = `
        <div class="alert alert-danger py-2">
            <strong>Problemy z walidacją:</strong>
            <ul class="mb-0 small">${route.operatorMessages.map(msg => `<li>${this.escapeHtml(msg)}</li>`).join('')}</ul>
        </div>`;
        }

        return `
    <div class="card mb-3 border-warning shadow-sm">
        <div class="card-body" id="route-card-acceptance-${route.id}">
            <h6 class="card-title">Trasa #${route.id}: ${this.escapeHtml(route.startAddress)} → ${this.escapeHtml(route.endAddress)}</h6>
            ${operatorMessagesHtml}
            ${rejectedPointsHtml}
            
            <div class="btn-group mt-3 w-100">
                <button class="btn btn-warning fw-bold" onclick="operatorDashboard.submitPointDecisions(${route.id})">
                    Prześlij Decyzje
                </button>
                <button class="btn btn-info" onclick="showRouteOnMap(${route.id})">🗺️ Mapa</button>
                <button class="btn btn-secondary" onclick="operatorDashboard.showValidationDetails(${route.id})">📋 Logi</button>
                <button class="btn btn-danger" onclick="operatorDashboard.deleteRoute(${route.id})">❌ Usuń</button>
            </div>
        </div>
    </div>
`;
    }

    async submitPointDecisions(routeId) {
        const card = document.getElementById(`route-card-acceptance-${routeId}`);
        if (!card) return;

        const decisions = [];
        const pointItems = card.querySelectorAll('.rejected-point-item');

        for (let i = 0; i < pointItems.length; i++) {
            const item = pointItems[i];
            const pointName = item.dataset.pointName;
            const decisionInput = item.querySelector(`input[name="decision-${routeId}-${i}"]:checked`);
            const comment = item.querySelector(`#comment-${routeId}-${i}`).value;

            if (!decisionInput) {
                this.showError("Musisz podjąć decyzję (Akceptuj/Odrzuć) dla każdego punktu.");
                return;
            }

            decisions.push({
                pointName: pointName,
                decision: decisionInput.value,
                comment: comment || ''
            });
        }

        const hasRejections = decisions.some(d => d.decision === 'REJECTED');
        const confirmationMessage = hasRejections ?
            `Odrzucono ${decisions.filter(d => d.decision === 'REJECTED').length} pkt. System spróbuje znaleźć objazd. Kontynuować?` :
            `Zaakceptowano wszystkie punkty. Trasa zostanie zatwierdzona. Kontynuować?`;

        if (!confirm(confirmationMessage)) return;

        try {
            const button = card.querySelector('.btn-warning');
            button.disabled = true;
            button.innerHTML = '<span class="spinner-border spinner-border-sm"></span> Przetwarzanie...';

            const response = await fetch(`/api/routes/${routeId}/review-points`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ decisions })
            });

            const result = await response.json();
            if (!response.ok || result.success === false) {
                throw new Error(result.error || 'Błąd przetwarzania decyzji');
            }

            this.loadRoutesRequiringAcceptance();
            this.loadAllRoutes();

            if (result.action === 'ROUTE_ACCEPTED') {
                this.showSuccess(`✅ ${result.message || 'Trasa zaakceptowana pomyślnie'}`);
            } else if (result.action === 'REQUIRES_REVIEW_AGAIN') {
                this.showMessage(`⚠️ ${result.message || 'Rewalidacja znalazła nowe problemy'}`, 'warning', 15000);
            } else {
                this.showSuccess('Decyzje przetworzone.');
            }

        } catch (error) {
            this.showError('Błąd podczas przesyłania decyzji: ' + error.message);
        } finally {
            const button = card.querySelector('.btn-warning');
            button.disabled = false;
            button.innerHTML = 'Prześlij Decyzje';
        }
    }

    async loadAllRoutes() {
        const container = document.getElementById('routes-list');
        if (!container) return;
        container.innerHTML = '<div class="text-center p-4"><div class="spinner-border text-primary"></div></div>';
        try {
            const response = await fetch('/api/routes/all');
            if (!response.ok) throw new Error('Failed to load routes');
            this.routes = await response.json();
            if (typeof displayRoutesWithAllButtons === 'function') {
                displayRoutesWithAllButtons();
            } else {
                this.displayRoutesSimple();
            }
        } catch (error) {
            console.error('Error loading routes:', error);
            container.innerHTML = `<div class="alert alert-danger">Błąd ładowania tras: ${error.message}</div>`;
        }
    }

    async loadDriversList() {
        const listDiv = document.getElementById('drivers-list');
        if (!listDiv) return;
        listDiv.innerHTML = '<div class="text-center p-4"><div class="spinner-border"></div></div>';

        try {
            const usersResponse = await fetch('/api/admin/users');
            if (!usersResponse.ok) throw new Error('Failed to load users');
            const allUsers = await usersResponse.json();
            const drivers = allUsers.filter(u => u.role === 'DRIVER');

            this.drivers = drivers;

            if (drivers.length === 0) {
                listDiv.innerHTML = `
                    <div class="text-center p-4">
                        <p>Brak kierowców</p>
                        <button class="btn btn-primary" onclick="operatorDashboard.showAddDriverModal()">Dodaj kierowcę</button>
                    </div>
                `;
                this.updateDriversSelect(drivers);
                return;
            }

            let activeDrivers = [];
            try {
                const activeResponse = await fetch('/api/tracking/active-drivers');
                if (activeResponse.ok) activeDrivers = await activeResponse.json();
            } catch (e) {
                console.warn('Nie udało się załadować aktywnych kierowców', e);
            }

            listDiv.innerHTML = drivers.map(driver => {
                const activeInfo = activeDrivers.find(ad => ad.driverUsername === driver.username);
                const isOnline = activeInfo && activeInfo.isOnline;

                const mapButton = isOnline ? `
                    <button class="btn btn-sm btn-info btn-map-driver" onclick="operatorDashboard.showDriverLocation('${driver.username}')">
                        📍 Lokalizacja
                    </button>
                ` : '';

                return `
                    <div class="driver-list-item">
                        <div class="d-flex justify-content-between align-items-center">
                            <div>
                                <strong>${driver.firstName || ''} ${driver.lastName || ''}</strong>
                                <span class="text-muted">(${driver.username})</span>
                                ${isOnline ? '<span class="driver-online ms-2">● Online</span>' : '<span class="driver-offline ms-2">○ Offline</span>'}
                            </div>
                            <div>
                                ${mapButton}
                                <button class="btn btn-sm btn-outline-danger" onclick="operatorDashboard.deleteDriver('${driver.username}')">
                                    Usuń
                                </button>
                            </div>
                        </div>
                        <small class="text-muted">${driver.email || ''}</small>
                    </div>
                `;
            }).join('');

            this.updateDriversSelect(drivers);

        } catch (error) {
            console.error('Error loading drivers:', error);
            listDiv.innerHTML = `<div class="alert alert-danger">Błąd: ${error.message}</div>`;
        }
    }

    updateDriversSelect(drivers) {
        const driversList = drivers.map(d => `<option value="${d.username}">${d.firstName || d.username} ${d.lastName || ''}</option>`).join('');

        const selectMain = document.getElementById('driver-username');
        if (selectMain) {
            selectMain.innerHTML = '<option value="">Wybierz kierowcę (opcjonalnie)...</option>' + driversList;
        }

        const selectModal = document.getElementById('assign-driver-select');
        if (selectModal) {
            selectModal.innerHTML = '<option value="">Wybierz kierowcę...</option>' + driversList;
        }
    }

    async showAddDriverModal() {
        if (!this.addDriverModalInstance) {
            alert('Błąd: Modal nie jest zainicjalizowany');
            return;
        }
        document.getElementById('add-driver-form').reset();
        this.addDriverModalInstance.show();
    }

    async handleAddDriverSubmit(event) {
        event.preventDefault();
        const button = event.submitter;
        button.disabled = true;
        button.innerHTML = '<span class="spinner-border spinner-border-sm"></span> Dodawanie...';

        try {
            const response = await fetch('/api/admin/users', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    username: document.getElementById('driver-form-username').value.trim(),
                    password: document.getElementById('driver-form-password').value.trim(),
                    email: document.getElementById('driver-form-email').value.trim(),
                    role: 'DRIVER',
                    firstName: document.getElementById('driver-form-firstName').value,
                    lastName: document.getElementById('driver-form-lastName').value
                })
            });
            if (response.ok) {
                this.showSuccess('Kierowca dodany!');
                this.addDriverModalInstance.hide();
                this.loadDriversList();
            } else {
                const err = await response.json();
                throw new Error(err.message || 'Failed to add driver');
            }
        } catch (error) {
            this.showError('Błąd podczas dodawania kierowcy: ' + error.message);
        } finally {
            button.disabled = false;
            button.innerHTML = 'Dodaj Kierowcę';
        }
    }

    async deleteDriver(username) {
        if (!confirm(`Czy na pewno usunąć kierowcę ${username}?`)) return;
        try {
            const response = await fetch(`/api/admin/users/${username}`, { method: 'DELETE' });
            if (response.ok) {
                this.showSuccess('Kierowca usunięty');
                this.loadDriversList();
            } else {
                throw new Error('Failed to delete driver');
            }
        } catch (error) {
            this.showError('Błąd usuwania: ' + error.message);
        }
    }

    async handleRouteSubmit(event) {
        event.preventDefault();
        this.setFormLoading(true);

        const formData = {
            transportSetId: document.getElementById('transport-set').value,
            startAddress: document.getElementById('start-address').value.trim(),
            endAddress: document.getElementById('end-address').value.trim(),
            driverUsername: document.getElementById('driver-username').value.trim()
        };

        if (!formData.transportSetId || !formData.startAddress || !formData.endAddress) {
            this.showError('Wypełnij wszystkie pola');
            this.setFormLoading(false);
            return;
        }

        try {
            const geocoder = new window.google.maps.Geocoder();
            const startLocation = await this.geocodeAddress(geocoder, formData.startAddress);
            const endLocation = await this.geocodeAddress(geocoder, formData.endAddress);

            const createRequest = {
                transportSetId: parseInt(formData.transportSetId),
                startAddress: formData.startAddress,
                endAddress: formData.endAddress,
                startLatitude: startLocation.lat(),
                startLongitude: startLocation.lng(),
                endLatitude: endLocation.lat(),
                endLongitude: endLocation.lng()
            };

            const response = await fetch('/api/routes/create', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(createRequest)
            });

            const result = await response.json();

            if (!response.ok || result.success === false) {
                this.showError(result.error || 'Nie udało się utworzyć trasy');
                if (result.detailedReport) this.showDetailedReport(result.detailedReport);
                throw new Error(result.error || 'Create route failed');
            }

            if (result.isDraft && result.hasValidationProblems) {
                this.showMessage('⚠️ Trasa utworzona jako DRAFT - wymaga akceptacji.', 'warning');
                if (result.operatorMessages) this.showOperatorMessages(result.operatorMessages);
                if (result.rejectedPoints) this.showRejectedPointsOnMap(result.rejectedPoints);
                this.loadRoutesRequiringAcceptance();
            } else {
                this.showSuccess(`✅ Trasa utworzona pomyślnie (ID: ${result.routeId})`);
                if (formData.driverUsername && result.routeId) {
                    await fetch(`/api/routes/${result.routeId}/assign-driver?driverUsername=${formData.driverUsername}`, {
                        method: 'POST'
                    });
                    this.showMessage('Kierowca przypisany!', 'info');
                }
            }

            this.previewRenderer.setDirections({ routes: [] });
            if (result.initialGoogleRoute) {
                this.displayRouteOnMapObject(result.initialGoogleRoute, this.previewRenderer);
            }
            if (result.googleMapsRoute) {
                this.displayRouteOnMapObject({ routes: result.googleMapsRoute }, this.directionsRenderer);
            }

            this.loadAllRoutes();
            document.getElementById('route-form').reset();

        } catch (error) {
            this.showError(error.message);
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
                    reject(new Error(`Nie znaleziono adresu: ${address}`));
                }
            });
        });
    }

    async showAssignDriverModal(routeId) {
        if (!this.assignDriverModalInstance) {
            alert('Błąd: Modal nie jest zainicjalizowany');
            return;
        }

        const route = this.routes.find(r => r.id === routeId);
        const routeName = route ? `${route.startAddress} → ${route.endAddress}` : `Trasa #${routeId}`;

        document.getElementById('assign-driver-route-id').value = routeId;
        document.getElementById('assign-driver-route-name').textContent = routeName;
        document.getElementById('assign-driver-select').value = "";

        this.assignDriverModalInstance.show();
    }

    async handleAssignDriverSubmit(event) {
        event.preventDefault();
        const routeId = document.getElementById('assign-driver-route-id').value;
        const username = document.getElementById('assign-driver-select').value;

        if (!username) {
            this.showError('Musisz wybrać kierowcę');
            return;
        }

        const button = event.submitter;
        button.disabled = true;
        button.innerHTML = '<span class="spinner-border spinner-border-sm"></span> Przypisywanie...';

        try {
            const response = await fetch(`/api/routes/${routeId}/assign-driver?driverUsername=${username.trim()}`, {
                method: 'POST'
            });
            if (response.ok) {
                this.showSuccess('Kierowca przypisany');
                this.assignDriverModalInstance.hide();
                this.loadAllRoutes();
            } else {
                const errData = await response.json();
                throw new Error(errData.message || 'Nie udało się przypisać kierowcy');
            }
        } catch (error) {
            this.showError(error.message);
        } finally {
            button.disabled = false;
            button.innerHTML = 'Przypisz';
        }
    }

    displayRouteOnMapObject(directionsResult, renderer, mapInstance = null) {
        const map = mapInstance || this.map;
        if (!renderer || !directionsResult || !map) return;
        renderer.setMap(map);
        try {
            if (directionsResult.routes && Array.isArray(directionsResult.routes) && directionsResult.routes.length > 0) {
                renderer.setDirections(directionsResult);
                if (directionsResult.routes[0].bounds) {
                    map.fitBounds(directionsResult.routes[0].bounds);
                }
            } else {
                console.warn('Otrzymano nieprawidłowe dane trasy do wyświetlenia:', directionsResult);
            }
        } catch (error) {
            console.error('Error displaying route on map:', error);
        }
    }

    showRejectedPointsOnMap(rejectedPoints) {
        this.clearRejectedPointsMarkers();
        const geocoder = new window.google.maps.Geocoder();
        rejectedPoints.forEach(point => {
            const addressToGeocode = (point.name && point.name !== 'Unknown Point' && point.name !== 'Brak') ? point.name + ', Polska' : null;
            if (!addressToGeocode) return;
            geocoder.geocode({ address: addressToGeocode }, (results, status) => {
                if (status === 'OK' && results[0]) {
                    const marker = new window.google.maps.Marker({
                        position: results[0].geometry.location,
                        map: this.map,
                        title: `${point.name}\nPowód: ${point.reason || 'Brak'}`,
                        icon: 'http://maps.google.com/mapfiles/ms/icons/red-dot.png'
                    });
                    const infoWindow = new google.maps.InfoWindow({
                        content: `<h6>${point.name}</h6><p>${point.reason || 'Brak powodu'}</p>`
                    });
                    marker.addListener('click', () => infoWindow.open(this.map, marker));
                    this.rejectedPointsMarkers.push(marker);
                }
            });
        });
    }

    clearRejectedPointsMarkers() {
        this.rejectedPointsMarkers.forEach(marker => marker.setMap(null));
        this.rejectedPointsMarkers = [];
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
            if (!response.ok) throw new Error('Failed to load validation details');
            const details = await response.json();

            // Usuń stary modal jeśli istnieje
            const oldModal = document.getElementById('validation-details-modal');
            if (oldModal) {
                const instance = bootstrap.Modal.getInstance(oldModal);
                if (instance) instance.dispose();
                oldModal.remove();
            }

            const modal = document.createElement('div');
            modal.className = 'modal fade';
            modal.id = 'validation-details-modal';
            modal.innerHTML = `
            <div class="modal-dialog modal-lg">
                <div class="modal-content">
                    <div class="modal-header bg-dark text-white">
                        <h5 class="modal-title">Raport Walidacji Trasy #${routeId}</h5>
                        <button type="button" class="btn-close btn-close-white" data-bs-dismiss="modal" aria-label="Close"></button>
                    </div>
                    <div class="modal-body bg-light">
                        ${this.renderValidationDetails(details)}
                    </div>
                    <div class="modal-footer">
                        <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Zamknij</button>
                    </div>
                </div>
            </div>
        `;

            document.body.appendChild(modal);
            const modalInstance = new bootstrap.Modal(modal);
            modalInstance.show();

            modal.addEventListener('hidden.bs.modal', () => {
                document.body.removeChild(modal);
            });

        } catch (error) {
            console.error('Error loading validation details:', error);
            if (typeof this.showError === 'function') {
                this.showError('Błąd podczas ładowania szczegółów walidacji: ' + error.message);
            } else {
                alert('Błąd: ' + error.message);
            }
        }
    }

    renderValidationDetails(details) {
        let html = '';

        // 1. UZASADNIENIE WYBORU (DLACZEGO TA TRASA?)
        const justification = details.routeJustification || [];
        if (Array.isArray(justification) && justification.length > 0) {
            html += `
            <div class="card mb-3 border-info shadow-sm">
                <div class="card-header bg-info text-white">
                    <h6 class="mb-0"><i class="bi bi-info-circle"></i> Uzasadnienie wyboru trasy</h6>
                </div>
                <div class="card-body">
                    <ul class="mb-0">
                        ${justification.map(line => `<li>${this.escapeHtml(line)}</li>`).join('')}
                    </ul>
                </div>
            </div>
        `;
        }

        // 2. LOGI WALIDACJI (LISTA CO ZOSTAŁO ZWALIDOWANE)
        const attempts = details.attemptReports || [];
        if (attempts.length > 0) {
            html += `
            <div class="card mb-3 shadow-sm">
                <div class="card-header bg-secondary text-white">
                    <h6 class="mb-0">Historia procesu walidacji (Logi)</h6>
                </div>
                <div class="card-body p-0">
                    <table class="table table-sm table-hover mb-0">
                        <thead class="table-light">
                            <tr><th>Próba</th><th>Status</th><th>Szczegóły operacji</th></tr>
                        </thead>
                        <tbody>
                            ${attempts.map(a => `
                                <tr>
                                    <td class="text-center">#${a.attempt}</td>
                                    <td><span class="badge ${a.success ? 'bg-success' : 'bg-warning'}">${a.status || (a.success ? 'OK' : 'Błąd')}</span></td>
                                    <td><small>${this.escapeHtml(a.message)}</small></td>
                                </tr>
                            `).join('')}
                        </tbody>
                    </table>
                </div>
            </div>
        `;
        }

        // 3. PUNKTY ODRZUCONE I NARUSZENIA (POWÓD ODRZUCENIA)
        const hasViolations = details.violations && details.violations.length > 0;
        const rejectedPoints = details.rejectedPoints || [];
        const hasRejected = rejectedPoints.length > 0;

        if (hasViolations || hasRejected) {
            // Sekcja konkretnych punktów odrzuconych
            if (hasRejected) {
                html += `
                <div class="card mb-3 border-danger shadow-sm">
                    <div class="card-header bg-danger text-white">
                        <h6 class="mb-0">Punkty wymagające akceptacji operatora</h6>
                    </div>
                    <div class="card-body">
                        <div class="list-group">
                            ${rejectedPoints.map(p => `
                                <div class="list-group-item list-group-item-action">
                                    <div class="d-flex w-100 justify-content-between">
                                        <h6 class="mb-1 text-danger font-weight-bold">${this.escapeHtml(p.name)}</h6>
                                        <small class="badge bg-outline-danger">${p.foundDuringRevalidation ? 'Rewalidacja' : 'Pierwotny'}</small>
                                    </div>
                                    <p class="mb-1"><strong>Powód:</strong> ${this.escapeHtml(Array.isArray(p.reason) ? p.reason[0] : p.reason)}</p>
                                    <div class="mt-2">
                                        ${p.limitWeight ? `<span class="badge bg-light text-dark border mr-2">Limit wagi: ${p.limitWeight}t</span>` : ''}
                                        ${p.limitHeight ? `<span class="badge bg-light text-dark border">Limit wys.: ${p.limitHeight}m</span>` : ''}
                                    </div>
                                </div>
                            `).join('')}
                        </div>
                    </div>
                </div>
            `;
            }

            // Sekcja pozostałych naruszeń
            if (hasViolations) {
                html += `
                <div class="card mb-3 border-warning shadow-sm">
                    <div class="card-header bg-warning text-dark">
                        <h6 class="mb-0">Inne zidentyfikowane naruszenia</h6>
                    </div>
                    <div class="card-body">
                        <ul class="list-group list-group-flush">
                            ${details.violations.map(v => {
                    const text = typeof v === 'object' ? (v.reason || JSON.stringify(v)) : v;
                    return `<li class="list-group-item text-danger small">${this.escapeHtml(text)}</li>`;
                }).join('')}
                        </ul>
                    </div>
                </div>
            `;
            }
        } else {
            html += `
            <div class="alert alert-success">
                <i class="bi bi-check-circle"></i> Brak krytycznych naruszeń i punktów odrzuconych na tej trasie.
            </div>
        `;
        }

        // 4. INFO O POJEŹDZIE (KONTEKST)
        if (details.transportInfo) {
            html += `
            <div class="card shadow-sm mb-3">
                <div class="card-body py-2 bg-light">
                    <small class="text-muted">
                        Parametry zestawu: <strong>${details.transportInfo.weightTon}t</strong>, 
                        wysokość: <strong>${details.transportInfo.heightM}m</strong>, 
                        typ: ${this.escapeHtml(details.transportInfo.type)}
                    </small>
                </div>
            </div>
        `;
        }

        return html;
    }

    escapeHtml(text) {
        if (!text) return '';
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    showDetailedReport(report) {
        this.showNotification(report, 'danger', 10000);
    }

    showOperatorMessages(messages) {
        this.showNotification(messages.join('<br>'), 'warning', 10000);
    }

    centerMapOnPoland() {
        if (this.map) {
            this.map.setCenter({ lat: 52.0693, lng: 19.4803 });
            this.map.setZoom(6);
        }
    }

    setFormLoading(loading) {
        const form = document.getElementById('route-form');
        if (!form) return;
        const btn = form.querySelector('button[type="submit"]');
        form.querySelectorAll('input, select, button').forEach(el => el.disabled = loading);
        if (btn) btn.innerHTML = loading ?
            '<span class="spinner-border spinner-border-sm"></span> Tworzenie...' :
            'Utwórz Trasę';
    }

    startAutoRefresh() {
        if (this.refreshInterval) clearInterval(this.refreshInterval);

        console.log("Ładowanie danych początkowych...");
        this.loadDriversList();
        this.loadAllRoutes();
        this.loadRoutesRequiringAcceptance();

        this.refreshInterval = setInterval(() => {
            console.log("Auto-refresh...");
            const driversTabActive = document.getElementById('drivers-section').style.display !== 'none';
            const routesTabActive = document.getElementById('routes-section').style.display !== 'none';
            if (driversTabActive) this.loadDriversList();
            if (routesTabActive) {
                this.loadAllRoutes();
                this.loadRoutesRequiringAcceptance();
            }
        }, 30000);
    }

    showSuccess(msg) {
        this.showNotification(msg, 'success');
    }

    showError(msg) {
        this.showNotification(msg, 'danger');
    }

    showMessage(msg, type) {
        this.showNotification(msg, type);
    }

    showNotification(msg, type = 'info', duration = 5000) {
        const container = document.body;
        const notif = document.createElement('div');
        notif.className = `alert alert-${type} alert-dismissible fade show`;
        notif.style.cssText = 'position: fixed; top: 20px; right: 20px; z-index: 9999; min-width: 300px;';
        notif.innerHTML = `${msg}<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>`;
        container.appendChild(notif);
        setTimeout(() => {
            try {
                if (window.bootstrap && window.bootstrap.Alert) {
                    const bsAlert = bootstrap.Alert.getOrCreateInstance(notif);
                    if (bsAlert) bsAlert.close();
                } else if (notif.parentNode) {
                    notif.parentNode.removeChild(notif);
                }
            } catch (e) {
                if (notif.parentNode) notif.parentNode.removeChild(notif);
            }
        }, duration);
    }

    deleteRoute(routeId) {
        if (!confirm(`Czy na pewno chcesz usunąć trasę #${routeId}?`)) return;
        fetch(`/api/routes/${routeId}`, { method: 'DELETE' })
            .then(response => {
                if (!response.ok) throw new Error('Nie udało się usunąć trasy');
                this.showSuccess('Trasa usunięta');
                this.loadAllRoutes();
                this.loadRoutesRequiringAcceptance();
            })
            .catch(error => this.showError(error.message));
    }

    destroy() {
        if (this.refreshInterval) clearInterval(this.refreshInterval);
        if (this.driverLocationInterval) clearInterval(this.driverLocationInterval);
    }

}

// -------------------------------------------------------------------
// START Globalna logika
// -------------------------------------------------------------------

let operatorDashboard;
let googleMapsLoaded = false;

async function initializeSystem() {
    try {
        const token = localStorage.getItem('token');
        if (!token) {
            window.location.href = '/login.html';
            return;
        }

        operatorDashboard = new OperatorDashboard();
        window.operatorDashboard = operatorDashboard;

        operatorDashboard.setupAuth();

        await loadGoogleMaps();
        operatorDashboard.init();

        switchTab('routes');

    } catch (error) {
        console.error('Initialization failed:', error);
        alert('Nie udało się załadować systemu: ' + error.message);
    }
}

async function loadGoogleMaps() {
    if (googleMapsLoaded) return Promise.resolve();

    try {
        const response = await fetch('/api/config/google-maps-key');

        if (!response.ok) throw new Error(`Nie udało się pobrać klucza API (status: ${response.status})`);

        const apiKey = await response.text();
        if (!apiKey || apiKey === 'Google Maps API not configured') {
            throw new Error('Klucz API nie jest skonfigurowany na serwerze');
        }

        return new Promise((resolve, reject) => {
            const script = document.getElementById('google-maps-script');
            script.src = `https://maps.googleapis.com/maps/api/js?key=${apiKey}&libraries=places&callback=onGoogleMapsLoaded`;
            script.onerror = () => reject(new Error('Nie udało się załadować skryptu Google Maps'));

            window.onGoogleMapsLoaded = () => {
                console.log("Google Maps API loaded.");
                googleMapsLoaded = true;
                if(operatorDashboard) {
                    operatorDashboard.initMap();
                }
                resolve();
            };
        });
    } catch (error) {
        console.error('Google Maps loading failed:', error);
        alert('Nie udało się załadować API Google Maps. Powód: ' + error.message);
        throw error;
    }
}

function switchTab(tab) {
    document.querySelectorAll('#routes-section, #drivers-section, #vehicles-section, #add-equipment-section, #trailers-section').forEach(el => el.style.display = 'none');
    document.querySelectorAll('#routes-tab, #drivers-tab, #vehicles-tab, #add-equipment-tab, #trailers-tab').forEach(el => {
        el.classList.remove('btn-primary');
        el.classList.add('btn-outline-primary');
    });

    const tabButton = document.getElementById(tab + '-tab');
    if (tabButton) {
        tabButton.classList.add('btn-primary');
        tabButton.classList.remove('btn-outline-primary');
    }

    const section = document.getElementById(tab + '-section');
    if (section) {
        section.style.display = 'block';
    }

    if (tab === 'routes') {
        if (operatorDashboard?.loadAllRoutes) operatorDashboard.loadAllRoutes();
    } else if (tab === 'drivers') {
        if (operatorDashboard?.loadDriversList) operatorDashboard.loadDriversList();
    } else if (tab === 'vehicles') {
        loadVehiclesTab();
    } else if (tab === 'add-equipment') {
        loadEquipmentLists();
    } else if (tab === 'trailers') {
        if (operatorDashboard?.loadTrailers) operatorDashboard.loadTrailers();
    }
}

function checkCargoCapabilities() {
    const cargoId = document.getElementById('cargo-select').value;
    const container = document.getElementById('transport-mode-container');
    if (!container) return;
    if (!cargoId) {
        container.style.display = 'none';
        return;
    }
    const cargo = operatorDashboard.allCargo.find(c => c.id == cargoId);
    if (!cargo) {
        container.style.display = 'none';
        return;
    }
    if (cargo.canDriveAlone && cargo.totalWeightKg <= 5000) {
        container.style.display = 'block';
        selectTransportMode('trailer');
    } else {
        container.style.display = 'none';
        operatorDashboard.selectedTransportMode = 'trailer';
    }
}

function selectTransportMode(mode) {
    operatorDashboard.selectedTransportMode = mode;
    document.getElementById('transport-mode').value = mode;
    document.querySelectorAll('.transport-mode-option').forEach(el => el.classList.remove('selected'));
    document.querySelector(`[data-mode="${mode}"]`).classList.add('selected');
}
function checkIfCanDriveAlone(cargoId, allCargo) {
    const cargo = allCargo.find(c => c.id == cargoId);
    return !!(cargo && cargo.canDriveAlone);
}
async function loadVehiclesTab() {
    try {
        const [transporters, trailers, cargo, sets] = await Promise.all([
            fetch('/api/vehicles/transporters').then(r => r.json()),
            fetch('/api/trailers').then(r => r.json()),
            fetch('/api/vehicles/cargo').then(r => r.json()),
            fetch('/api/vehicles/transport-sets').then(r => r.json())
        ]);

        operatorDashboard.allCargo = cargo;

        // CIĘŻARÓWKI
        document.getElementById('transporter-select').innerHTML = '<option value="">Wybierz ciężarówkę...</option>' +
            transporters.map(t => `<option value="${t.id}">${t.model} (${t.totalWeightKg}kg, ${t.heightCm}cm)</option>`).join('');

        // NACZEPY
        const trailerSelect = document.getElementById('trailer-select');
        if (trailerSelect) {
            if (trailers.length === 0) {
                trailerSelect.innerHTML = '<option value="">Brak naczep - dodaj w zakładce "Dodaj Sprzęt"</option>';
            } else {
                trailerSelect.innerHTML = '<option value="">Wybierz naczepę...</option>' +
                    trailers.map(t => {
                        const details = [
                            t.type || 'Nieznany typ',
                            `${t.maxPayload}kg`,
                            `${t.numberOfAxles || '?'} osi`
                        ].join(', ');
                        return `<option value="${t.id}">${t.registrationNumber} (${details})</option>`;
                    }).join('');
            }
        }

        // ŁADUNKI
        document.getElementById('cargo-select').innerHTML = '<option value="">Wybierz ładunek...</option>' +
            cargo.map(c => `<option value="${c.id}">${c.model} (${c.totalWeightKg}kg, ${c.heightCm}cm)${c.canDriveAlone ? ' 🚗' : ''}</option>`).join('');

        // ISTNIEJĄCE ZESTAWY
        document.getElementById('existing-sets').innerHTML = sets.length === 0
            ? '<div class="alert alert-info">Brak zestawów. Utwórz pierwszy zestaw.</div>'
            : sets.map(set => `
            <div class="card mb-2">
                <div class="card-body py-2">
                    <h6 class="mb-1">${set.description || 'Zestaw #' + set.id}</h6>
                    <small class="d-block text-muted">🚛 ${set.transporter.model}</small>
                    <small class="d-block text-muted">📦 ${set.cargo.model}</small>
                    ${set.trailer ? `<small class="d-block text-muted">🚜 ${set.trailer.registrationNumber} (${set.trailer.type})</small>` : ''}
                    <small class="d-block"><strong>Masa:</strong> ${set.totalWeightKg}kg | <strong>Wys:</strong> ${set.totalHeightCm}cm</small>
                </div>
            </div>
        `).join('');

        // LISTA NACZEP W ZAKŁADCE ZESTAWY
        const vehiclesTrailersList = document.getElementById('vehicles-trailers-list');
        if (vehiclesTrailersList) {
            if (trailers.length === 0) {
                vehiclesTrailersList.innerHTML = `
                    <div class="alert alert-secondary text-center">
                        <p class="mb-2">🚛 Brak zarejestrowanych naczep</p>
                        <small>Dodaj naczepę w zakładce "Dodaj Sprzęt"</small>
                    </div>
                `;
            } else {
                vehiclesTrailersList.innerHTML = trailers.map(trailer => `
                    <div class="card mb-2 border-info">
                        <div class="card-body py-2">
                            <div class="d-flex justify-content-between align-items-center mb-1">
                                <strong class="text-primary">${trailer.registrationNumber}</strong>
                                <span class="badge bg-info text-dark">${trailer.type || 'N/A'}</span>
                            </div>
                            <small class="d-block text-muted">📏 ${trailer.maxPayload}kg | ${trailer.numberOfAxles || '?'} osi</small>
                            <small class="d-block text-muted">📐 ${trailer.length}m × ${trailer.width}m × ${trailer.height}m</small>
                            <small class="d-block text-muted">⚖️ Masa własna: ${trailer.emptyWeight || '?'}kg</small>
                        </div>
                    </div>
                `).join('');
            }
        }

        console.log('✅ Załadowano dane do sekcji Zestawy Transportowe:', {
            transporters: transporters.length,
            trailers: trailers.length,
            cargo: cargo.length,
            sets: sets.length
        });

    } catch (error) {
        console.error('❌ Błąd ładowania danych zestawów:', error);
        operatorDashboard?.showError('Błąd ładowania danych: ' + error.message);
    }
}

async function handleVehicleFormSubmit(e) {
    e.preventDefault();

    const button = e.submitter;
    button.disabled = true;
    button.innerHTML = '<span class="spinner-border spinner-border-sm"></span> Tworzenie...';

    try {
        const transporterId = document.getElementById('transporter-select').value;
        const trailerId = document.getElementById('trailer-select').value;
        const cargoId = document.getElementById('cargo-select').value;
        const description = document.getElementById('set-description').value.trim();
        const transportMode = document.getElementById('transport-mode')?.value || 'trailer';

        if (!transporterId || !cargoId) {
            throw new Error('Musisz wybrać ciężarówkę i ładunek');
        }

        if (!trailerId && transportMode !== 'self') {
            throw new Error('Musisz wybrać naczepę lub ustawić tryb "Na własnych kołach"');
        }

        const payload = {
            transporterId: parseInt(transporterId),
            cargoId: parseInt(cargoId),
            description: description || undefined,
            transportMode: transportMode
        };

        if (trailerId && transportMode !== 'self') {
            payload.trailerId = parseInt(trailerId);
        }

        console.log('📤 Wysyłanie zestawu:', payload);

        const response = await fetch('/api/vehicles/transport-sets', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });

        if (!response.ok) {
            const err = await response.json().catch(() => ({}));
            throw new Error(err.message || `Błąd ${response.status}`);
        }

        operatorDashboard.showSuccess('✅ Zestaw transportowy utworzony!');
        e.target.reset();
        loadVehiclesTab();
        operatorDashboard.loadTransportSets();

    } catch (error) {
        console.error('❌ Błąd tworzenia zestawu:', error);
        operatorDashboard.showError('Błąd: ' + error.message);
    } finally {
        button.disabled = false;
        button.innerHTML = 'Utwórz';
    }
}

async function handleAddTransporterSubmit(e) {
    e.preventDefault();
    try {
        const regInput = document.getElementById('trans-registration') || document.getElementById('transporter-registration');
        const regValue = regInput ? regInput.value : 'BRAK-' + Date.now();

        const response = await fetch('/api/vehicles/transporters', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                model: document.getElementById('trans-model').value,
                registrationNumber: regValue,
                totalWeightKg: parseInt(document.getElementById('trans-weight').value),
                heightCm: parseInt(document.getElementById('trans-height').value)
            })
        });
        if (!response.ok) throw new Error('Failed to add transporter');
        operatorDashboard.showSuccess('Ciężarówka dodana');
        loadVehiclesTab();
    } catch (error) {
        operatorDashboard.showError('Błąd: ' + error.message);
    }
}

async function handleAddCargoSubmit(e) {
    e.preventDefault();
    try {
        const canDriveAloneElement = document.getElementById('cargo-can-drive-alone');
        const canDriveAlone = canDriveAloneElement ? canDriveAloneElement.checked : false;

        const response = await fetch('/api/vehicles/cargo', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                model: document.getElementById('cargo-model').value,
                totalWeightKg: parseInt(document.getElementById('cargo-weight').value),
                heightCm: parseInt(document.getElementById('cargo-height').value),
                canDriveAlone: canDriveAlone
            })
        });
        if (!response.ok) throw new Error('Failed to add cargo');
        operatorDashboard.showSuccess('Ładunek dodany');
        loadVehiclesTab();
    } catch (error) {
        operatorDashboard.showError('Błąd: ' + error.message);
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// ZAKTUALIZOWANA FUNKCJA - handleAddTrailerSubmit z nowymi polami
// ═══════════════════════════════════════════════════════════════════════════════
async function handleAddTrailerSubmit(event) {
    event.preventDefault();

    // 1. Zbieranie danych z formularza - ISTNIEJĄCE POLA
    const registrationNumber = document.getElementById('trailer-registration-number')?.value?.trim();
    const type = document.getElementById('trailer-type')?.value?.trim();
    const vin = document.getElementById('trailer-vin')?.value?.trim();
    const maxPayload = parseInt(document.getElementById('trailer-max-payload')?.value, 10);
    const length = parseFloat(document.getElementById('trailer-length')?.value);
    const width = parseFloat(document.getElementById('trailer-width')?.value);
    const height = parseFloat(document.getElementById('trailer-height')?.value);

    // NOWE POLA
    const numberOfAxles = parseInt(document.getElementById('trailer-axles')?.value, 10);
    const unloadedHeight = parseFloat(document.getElementById('trailer-unloaded-height')?.value);
    const emptyWeight = parseInt(document.getElementById('trailer-empty-weight')?.value, 10);

    if (typeof operatorDashboard === 'undefined' || operatorDashboard === null) {
        console.error('Błąd: operatorDashboard nie jest zdefiniowany.');
        return;
    }

    // 2. Walidacja podstawowa - ISTNIEJĄCE POLA
    if (!registrationNumber || !type || !vin ||
        isNaN(maxPayload) || maxPayload <= 0 ||
        isNaN(length) || length <= 0 ||
        isNaN(width) || width <= 0 ||
        isNaN(height) || height <= 0) {
        operatorDashboard.showError('Wypełnij wszystkie podstawowe pola poprawnymi danymi (numeryczne wartości > 0).');
        return;
    }

    // WALIDACJA NOWYCH PÓL
    if (isNaN(numberOfAxles) || numberOfAxles < 1 || numberOfAxles > 10) {
        operatorDashboard.showError('Liczba osi musi być liczbą od 1 do 10.');
        return;
    }

    if (isNaN(unloadedHeight) || unloadedHeight <= 0 || unloadedHeight > 5) {
        operatorDashboard.showError('Wysokość bez ładunku musi być liczbą od 0.01 do 5 metrów.');
        return;
    }

    if (isNaN(emptyWeight) || emptyWeight <= 0 || emptyWeight > 30000) {
        operatorDashboard.showError('Masa własna naczepy musi być liczbą od 1 do 30000 kg.');
        return;
    }

    // Walidacja logiczna
    if (unloadedHeight >= height) {
        operatorDashboard.showError('Wysokość bez ładunku powinna być mniejsza niż wysokość całkowita (z ładunkiem).');
        return;
    }

    try {
        const response = await fetch('/api/trailers', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                registrationNumber,
                type,
                vin,
                maxPayload,
                length,
                width,
                height,
                // NOWE POLA
                numberOfAxles,
                unloadedHeight,
                emptyWeight
            })
        });

        if (!response.ok) {
            let errorDetails = `Błąd: ${response.status} ${response.statusText}`;
            try {
                const errorJson = await response.json();
                errorDetails = errorJson.message || errorDetails;
            } catch (e) {}
            throw new Error(errorDetails);
        }

        const savedTrailer = await response.json();
        console.log('✅ Naczepa dodana:', savedTrailer);

        operatorDashboard.showSuccess(`Naczepa ${savedTrailer.registrationNumber} dodana pomyślnie!`);
        document.getElementById('add-trailer-form')?.reset();
        operatorDashboard.loadTrailers();
    } catch (error) {
        console.error('Błąd dodawania naczepy:', error);
        operatorDashboard.showError('Błąd podczas dodawania naczepy: ' + error.message);
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// ZAKTUALIZOWANA FUNKCJA - handleAddNewTransportSetSubmit z obsługą trailerId
// ═══════════════════════════════════════════════════════════════════════════════
async function handleAddNewTransportSetSubmit(e) {
    e.preventDefault();

    const button = e.submitter;
    button.disabled = true;
    button.innerHTML = '<span class="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span> Tworzenie...';

    try {
        const transporterId = document.getElementById('transporter-select').value;
        const cargoId = document.getElementById('cargo-select').value;
        const trailerId = document.getElementById('transport-set-trailer-select').value;
        const description = document.getElementById('set-description').value.trim();

        // Sprawdzenie trybu transportu
        const transportModeElement = document.querySelector('input[name="transport-mode"]:checked');
        const transportMode = transportModeElement ? transportModeElement.value : 'trailer';
        const forceSelfDriving = transportMode === 'self';

        // Walidacja podstawowa
        if (!transporterId || !cargoId) {
            throw new Error('Musisz wybrać ciągnik i ładunek.');
        }

        // Jeśli nie wymuszono self-driving, naczepa jest wymagana
        if (!forceSelfDriving && !trailerId) {
            throw new Error('Musisz wybrać naczepę (lub wymusić jazdę na własnych kołach dla pojazdu samojezdnego).');
        }

        const payload = {
            transporterId: parseInt(transporterId, 10),
            cargoId: parseInt(cargoId, 10),
            description: description,
            transportMode: transportMode
        };

        // Dodaj trailerId tylko gdy wybrana naczepa i nie ma wymuszenia self-driving
        if (trailerId && !forceSelfDriving) {
            payload.trailerId = parseInt(trailerId, 10);
        }

        console.log('📦 Wysyłanie żądania tworzenia zestawu:', payload);

        const response = await fetch('/api/vehicles/transport-sets', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });

        if (!response.ok) {
            const err = await response.json().catch(() => ({}));
            throw new Error(err.message || `Nie udało się utworzyć zestawu. Status: ${response.status}`);
        }

        const savedSet = await response.json();
        console.log('✅ Zestaw transportowy utworzony:', savedSet);

        operatorDashboard.showSuccess('Zestaw transportowy dodany pomyślnie!');
        document.getElementById('add-transport-set-form')?.reset();

        if (operatorDashboard?.loadTransportSets) {
            operatorDashboard.loadTransportSets();
        }
        if (typeof loadEquipmentLists === 'function') {
            loadEquipmentLists();
        }

    } catch (error) {
        console.error('❌ Błąd tworzenia zestawu:', error);
        operatorDashboard.showError('Błąd podczas tworzenia zestawu: ' + error.message);
    } finally {
        button.disabled = false;
        button.innerHTML = 'Utwórz Zestaw';
    }
}

async function loadEquipmentLists() {
    try {
        const [transporters, cargo] = await Promise.all([
            fetch('/api/vehicles/transporters').then(r => r.json()),
            fetch('/api/vehicles/cargo').then(r => r.json())
        ]);

        document.getElementById('transporters-list').innerHTML = transporters.map(t => `
            <div class="card mb-2">
                <div class="card-body p-2">
                    <strong>${t.model}</strong><br>
                    <small>Rejestracja: ${t.registrationNumber}</small><br>
                    <small>Waga: ${t.totalWeightKg}kg, Wys: ${t.heightCm}cm</small>
                </div>
            </div>
        `).join('');

        document.getElementById('cargo-list').innerHTML = cargo.map(c => `
            <div class="card mb-2">
                <div class="card-body p-2">
                    <strong>${c.model}</strong> ${c.canDriveAlone ? '🚗' : ''}<br>
                    <small>Waga: ${c.totalWeightKg}kg, Wys: ${c.heightCm}cm</small>
                </div>
            </div>
        `).join('');
    } catch (error) {
        console.error('Error loading equipment:', error);
    }
}

function assignDriver(routeId) {
    if(operatorDashboard) operatorDashboard.showAssignDriverModal(routeId);
}

function deleteRoute(routeId) {
    if(operatorDashboard) operatorDashboard.deleteRoute(routeId);
}

function refreshRoutes() {
    if (operatorDashboard?.loadAllRoutes) operatorDashboard.loadAllRoutes();
}

function refreshDrivers() {
    if (operatorDashboard?.loadDriversList) operatorDashboard.loadDriversList();
}

function showAddDriverModal() {
    if (operatorDashboard?.showAddDriverModal) operatorDashboard.showAddDriverModal();
}

function logout() {
    if (confirm('Czy na pewno chcesz się wylogować?')) {
        if (operatorDashboard?.destroy) operatorDashboard.destroy();
        localStorage.clear();
        window.location.href = '/login.html';
    }
}

async function displayRoutesWithAllButtons() {
    const container = document.getElementById('routes-list');
    if (!operatorDashboard || !operatorDashboard.routes || operatorDashboard.routes.length === 0) {
        container.innerHTML = `
            <div class="text-center p-5">
                <h4 class="text-muted">Brak tras</h4>
                <p>Utwórz pierwszą trasę używając formularza po lewej stronie</p>
            </div>
        `;
        return;
    }

    let html = '';
    const detailPromises = operatorDashboard.routes.map(route => getRouteValidation(route.id));
    const validations = await Promise.all(detailPromises);

    for (let i = 0; i < operatorDashboard.routes.length; i++) {
        html += createCompleteRouteCard(operatorDashboard.routes[i], validations[i]);
    }
    container.innerHTML = html;
}

async function getRouteValidation(routeId) {
    try {
        const response = await fetch(`/api/routes/${routeId}/validation-details`);
        if (!response.ok) return null;
        return await response.json();
    } catch (error) {
        return null;
    }
}

function createCompleteRouteCard(route, validation) {
    let html = `
        <div class="route-card ${route.isDraft ? 'border-warning' : ''}">
            <div class="route-header">
                <div>
                    <h4 style="color:#0d6efd;margin-bottom:10px;">Trasa #${route.id}</h4>
                    <p class="mb-1"><strong>Start:</strong> ${route.startAddress}</p>
                    <p class="mb-1"><strong>Koniec:</strong> ${route.endAddress}</p>
    `;

    let statusBadge = '';
    if (route.isDraft) {
        statusBadge = '<span class="badge bg-warning text-dark">Wymaga Akceptacji</span>';
    } else if (route.operatorAccepted) {
        statusBadge = `<span class="badge bg-info">Zaakceptowana (Operator)</span>`;
    } else {
        statusBadge = `<span class="badge bg-primary">${route.status}</span>`;
    }
    html += `<p class="mb-2"><strong>Status:</strong> ${statusBadge}</p>`;

    if (validation && validation.validationAvailable) {
        html += '<div class="mt-2">';
        if (validation.lightVehicle) {
            html += '<span class="validation-badge badge-clear">✅ Pojazd lekki (pominięto walidację)</span>';
        } else if (!validation.hasViolations && !validation.hasRestrictions) {
            html += '<span class="validation-badge badge-clear">✅ Trasa sprawdzona i bezpieczna</span>';
        }
        if (validation.hasRestrictions) {
            html += '<span class="validation-badge badge-warning">⚠️ Ograniczenia sprawdzone</span>';
        }
        if (validation.hasViolations) {
            html += '<span class="validation-badge badge-danger">🔄 Użyto trasy alternatywnej</span>';
        }
        if (validation.permits && validation.permits.length > 0) {
            html += `<span class="validation-badge badge-permit">📋 Wymaga ${validation.permits.length} pozwoleń</span>`;
        }
        html += '</div>';
    }

    html += `
                </div>
                <div class="route-actions">
                    <button class="btn btn-info btn-action" onclick="showFullValidation(${route.id})" title="Szczegółowa walidacja">
                        Szczegóły Walidacji
                    </button>
                    <button class="btn btn-success btn-action" onclick="showRouteOnMap(${route.id})" title="Wyświetl trasę na mapie">
                        🗺️ Pokaż na mapie
                    </button>
    `;

    if (route.status === 'CREATED' && !route.isDraft) {
        html += `
                    <button class="btn btn-primary btn-action" onclick="assignDriver(${route.id})" title="Przypisz kierowcę do trasy">
                        Przypisz Kierowcę
                    </button>
        `;
    }

    html += `
                    <button class="btn btn-danger btn-action" onclick="deleteRoute(${route.id})" title="Usuń trasę">
                        Usuń Trasę
                    </button>
                </div>
            </div>
    `;

    if (validation && validation.transportSetInfo) {
        const info = validation.transportSetInfo;
        html += `
            <div class="infrastructure-details">
                <h6><strong>Parametry zestawu:</strong></h6>
                <div class="row mt-2">
                    <div class="col-md-3"><small><strong>Opis:</strong> ${info.description || 'N/A'}</small></div>
                    <div class="col-md-2"><small><strong>Masa:</strong> ${(info.totalWeight_kg/1000).toFixed(1)}t</small></div>
                    <div class="col-md-3">
                        <small><strong>Wysokość:</strong> ${(info.totalHeight_cm/100).toFixed(2)}m
                        ${(info.trailerHeight_cm && info.cargoHeight_cm) ? ` (naczepa ${(info.trailerHeight_cm/100).toFixed(2)}m + ładunek ${(info.cargoHeight_cm/100).toFixed(2)}m)` : ''}
                        </small>
                    </div>
                    <div class="col-md-2"><small><strong>Długość:</strong> ${(info.totalLength_cm/100).toFixed(2)}m</small></div>
                    <div class="col-md-2"><small><strong>Szerokość:</strong> ${(info.totalWidth_cm/100).toFixed(2)}m</small></div>
                </div>
            </div>
        `;
    }
    if (validation && validation.permits && validation.permits.length > 0) {
        html += `
            <div class="permits-section">
                <h6><strong>⚠️ WYMAGANE POZWOLENIA (${validation.permits.length}):</strong></h6>
                ${validation.permits.map((permit, index) => parsePermitString(permit, index + 1)).join('')}
            </div>
        `;
    }
    if (validation && validation.routeJustification && validation.routeJustification.length > 0) {
        html += `
            <div class="mt-3">
                <button class="btn btn-sm btn-outline-primary" onclick="toggleJustification(${route.id})">
                    📋 Dlaczego wybrano tę trasę?
                </button>
                <div id="justification-${route.id}" class="justification-box" style="display:none;">
                    ${validation.routeJustification.map(line => {
            if (line.includes('═══')) return `<div style="font-weight:bold;color:#0d6efd;border-bottom:2px solid #0d6efd;padding:5px 0;margin-top:10px;">${escapeHtml(line)}</div>`;
            let className = '';
            if (line.includes('MOŻNA PRZEJECHAĆ') || line.includes('✔')) className = 'style="color:#28a745;font-weight:bold;background:#d4edda;padding:6px;border-radius:4px;margin:4px 0;"';
            else if (line.includes('OMIJAMY') || line.includes('ZA CIĘŻKI')) className = 'style="color:#dc3545;font-weight:bold;background:#f8d7da;padding:6px;border-radius:4px;margin:4px 0;"';
            else if (line.includes('⚠') || line.includes('UWAGA')) className = 'style="color:#856404;font-weight:bold;background:#fff3cd;padding:6px;border-radius:4px;margin:4px 0;"';
            return `<div ${className}>${escapeHtml(line)}</div>`;
        }).join('')}
                </div>
            </div>
        `;
    }
    html += `</div>`;
    return html;
}

function parsePermitString(permitString, index) {
    let html = `<div class="permit-item"><div style="display:flex; align-items:center; margin-bottom:8px;">`;
    html += `<span style="background:#ff9800;color:white;border-radius:50%;width:28px;height:28px;display:flex;align-items:center;justify-content:center;font-weight:bold;margin-right:12px;">${index}</span>`;
    let location = '', city = '', road = '', description = permitString;

    const cityMatch = permitString.match(/\(([^)]+)\)/);
    if (cityMatch) {
        city = cityMatch[1];
        permitString = permitString.replace(`(${cityMatch[1]})`, '').trim();
    }

    const roadMatch = permitString.match(/- droga: ([^|]+)/);
    if (roadMatch) {
        road = roadMatch[1].trim();
        permitString = permitString.replace(roadMatch[0], '').trim();
    }

    location = permitString.split(' | Przekroczenie')[0].trim();
    description = permitString;

    html += `<div style="flex:1;">`;
    if (location) html += `<div class="permit-location">${escapeHtml(location)}</div>`;
    if (city) html += `<div class="permit-city">📍 ${escapeHtml(city)}</div>`;
    if (road) html += `<div class="permit-road">🛣️ Droga: ${escapeHtml(road)}</div>`;
    html += `</div></div></div>`;
    return html;
}

function toggleJustification(routeId) {
    const element = document.getElementById('justification-' + routeId);
    if (element) {
        element.style.display = element.style.display === 'none' ? 'block' : 'none';
    }
}

function escapeHtml(text) {
    if (!text) return '';
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

async function showFullValidation(routeId) {
    const validation = await getRouteValidation(routeId);
    if (!validation || !validation.validationAvailable) {
        alert('Brak szczegółowych danych walidacji dla tej trasy.');
        return;
    }

    let html = '<div style="max-height: 75vh; overflow-y: auto; padding: 20px;">';
    html += `<h3 style="color:#0d6efd;border-bottom:3px solid #0d6efd;padding-bottom:10px;">Pełna Walidacja Trasy #${routeId}</h3>`;

    if (validation.transportSetInfo) {
        const info = validation.transportSetInfo;
        html += '<div style="background:#e7f3ff;padding:20px;border-radius:8px;margin:20px 0;border:2px solid #0d6efd;">';
        html += '<h5><strong>Parametry zestawu:</strong></h5>';
        html += '<ul style="margin:10px 0;line-height:1.8;">';
        html += `<li><strong>Opis:</strong> ${info.description || 'N/A'}</li>`;
        html += `<li><strong>Wysokość:</strong> ${(info.totalHeight_cm/100).toFixed(2)}m</li>`;
        html += `<li><strong>Masa:</strong> ${(info.totalWeight_kg/1000).toFixed(1)}t</li>`;
        html += `<li><strong>Długość:</strong> ${(info.totalLength_cm/100).toFixed(2)}m</li>`;
        html += `<li><strong>Szerokość:</strong> ${(info.totalWidth_cm/100).toFixed(2)}m</li>`;
        html += '</ul></div>';
    }

    if (validation.permits && validation.permits.length > 0) {
        html += '<div style="background:#fff3cd;border-left:4px solid #ffc107;padding:15px;margin-top:20px;border-radius:5px;">';
        html += `<h6 style="color:#856404;"><strong>⚠️ Wymagane pozwolenia (${validation.permits.length}):</strong></h6><ul>`;
        validation.permits.forEach(p => html += `<li>${escapeHtml(p)}</li>`);
        html += '</ul></div>';
    }

    if (validation.routeJustification && validation.routeJustification.length > 0) {
        html += '<div style="margin-top:20px;"><h5><strong>📋 Uzasadnienie wyboru trasy:</strong></h5>';
        html += '<div style="background:#f8f9fa;padding:15px;border-radius:8px;font-family:monospace;font-size:0.9em;max-height:300px;overflow-y:auto;">';
        validation.routeJustification.forEach(line => {
            let style = '';
            if (line.includes('═══')) style += 'font-weight:bold;color:#0d6efd;border-bottom:2px solid #0d6efd;padding:8px 0;margin-top:15px;';
            else if (line.includes('MOŻNA PRZEJECHAĆ') || line.includes('✔')) style += 'color:#28a745;font-weight:bold;background:#d4edda;padding:8px;border-radius:5px;margin:5px 0;';
            else if (line.includes('OMIJAMY') || line.includes('ZA CIĘŻKI')) style += 'color:#dc3545;font-weight:bold;background:#f8d7da;padding:8px;border-radius:5px;margin:5px 0;';
            else if (line.includes('⚠') || line.includes('UWAGA')) style += 'color:#856404;font-weight:bold;background:#fff3cd;padding:8px;border-radius:5px;margin:5px 0;';
            html += `<div style="${style}">${escapeHtml(line)}</div>`;
        });
        html += '</div></div>';
    }

    if (validation.violations && validation.violations.length > 0) {
        html += '<div style="background:#f8d7da;border-left:4px solid #dc3545;padding:15px;margin-top:20px;border-radius:5px;">';
        html += '<h6 style="color:#721c24;"><strong>Naruszenia (wystąpiły błędy mapowania / brak alternatywy):</strong></h6><ul>';
        validation.violations.forEach(v => html += `<li>${escapeHtml(v)}</li>`);
        html += '</ul></div>';
    }

    if (validation.lightVehicle) {
        html += '<div style="background:#d1ecf1;border-left:4px solid #0c5460;padding:15px;margin-top:20px;border-radius:5px;">';
        html += '<h6 style="color:#0c5460;"><strong>ℹ️ Pojazd lekki</strong></h6><p>Walidacja mostów i nośności została pominięta.</p>';
        html += '</div>';
    }

    html += '</div>';

    const modal = document.createElement('div');
    modal.style.cssText = 'position:fixed;top:0;left:0;right:0;bottom:0;background:rgba(0,0,0,0.9);z-index:9999;display:flex;align-items:center;justify-content:center;padding:20px;';
    modal.innerHTML = `
        <div style="background:white;padding:0;border-radius:12px;max-width:1100px;width:100%;max-height:90vh;overflow:hidden;box-shadow:0 10px 40px rgba(0,0,0,0.5); display: flex; flex-direction: column;">
            ${html}
            <div style="padding:20px;border-top:2px solid #dee2e6;text-align:center;background:#f8f9fa;">
                <button class="btn btn-secondary btn-lg" onclick="this.closest('div').parentElement.parentElement.remove()">Zamknij</button>
            </div>
        </div>
    `;
    document.body.appendChild(modal);
}

async function showRouteOnMap(routeId) {
    try {
        const response = await fetch(`/api/routes/${routeId}/navigation-data`);
        const data = await response.json();
        const modal = document.getElementById('route-map-modal');
        document.getElementById('route-map-title').textContent = `Trasa #${routeId} - ${data.startAddress} → ${data.endAddress}`;
        modal.style.display = 'flex';

        const mapDiv = document.getElementById('route-preview-map');
        mapDiv.innerHTML = '';
        operatorDashboard.routePreviewMap = new google.maps.Map(mapDiv, {
            zoom: 10,
            center: { lat: data.startLat, lng: data.startLng }
        });

        new google.maps.Marker({
            position: { lat: data.startLat, lng: data.startLng },
            map: operatorDashboard.routePreviewMap,
            title: 'Start',
            icon: { path: google.maps.SymbolPath.CIRCLE, scale: 10, fillColor: '#34a853', fillOpacity: 1, strokeColor: 'white', strokeWeight: 3 }
        });
        new google.maps.Marker({
            position: { lat: data.endLat, lng: data.endLng },
            map: operatorDashboard.routePreviewMap,
            title: 'Koniec',
            icon: { path: google.maps.SymbolPath.CIRCLE, scale: 10, fillColor: '#ea4335', fillOpacity: 1, strokeColor: 'white', strokeWeight: 3 }
        });

        let coordinates = [], routeColor = '#4285F4', routeLabel = 'Google Maps';
        if (data.herePolyline) {
            coordinates = decodeHereFlexiblePolyline(data.herePolyline);
            routeColor = '#34a853';
            routeLabel = 'HERE Maps (Zwalidowana)';
        } else if (data.googleMapsRoutes && data.googleMapsRoutes.length > 0) {
            if (data.googleMapsRoutes[0].overview_polyline?.points) {
                coordinates = decodeGooglePolyline(data.googleMapsRoutes[0].overview_polyline.points);
            }
        }

        if (coordinates.length > 0) {
            new google.maps.Polyline({
                path: coordinates,
                geodesic: true,
                strokeColor: routeColor,
                strokeOpacity: 1.0,
                strokeWeight: 5,
                map: operatorDashboard.routePreviewMap
            });
            const bounds = new google.maps.LatLngBounds();
            coordinates.forEach(coord => bounds.extend(coord));
            operatorDashboard.routePreviewMap.fitBounds(bounds);
        }
    } catch (error) {
        console.error('Błąd wyświetlania trasy:', error);
        alert('Nie udało się załadować trasy na mapie');
    }
}

function closeRouteMapModal() {
    document.getElementById('route-map-modal').style.display = 'none';
    const mapDiv = document.getElementById('route-preview-map');
    if (mapDiv) mapDiv.innerHTML = '';
    operatorDashboard.routePreviewMap = null;
}

function decodeHereFlexiblePolyline(encoded) {
    const ENCODING_TABLE = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    let index = 0, lat = 0, lng = 0, coordinates = [];
    const precision = Math.pow(10, ENCODING_TABLE.indexOf(encoded.charAt(0)));
    index++;
    while (index < encoded.length) {
        let shift = 0, result = 0, byte;
        do {
            byte = ENCODING_TABLE.indexOf(encoded.charAt(index++));
            if (byte === -1) throw new Error('Invalid character in polyline');
            result |= (byte & 0x1F) << shift;
            shift += 5;
        } while (byte >= 0x20);
        const latChange = (result & 1) ? ~(result >> 1) : (result >> 1);
        lat += latChange;
        shift = 0; result = 0;
        do {
            byte = ENCODING_TABLE.indexOf(encoded.charAt(index++));
            if (byte === -1) throw new Error('Invalid character in polyline');
            result |= (byte & 0x1F) << shift;
            shift += 5;
        } while (byte >= 0x20);
        const lngChange = (result & 1) ? ~(result >> 1) : (result >> 1);
        lng += lngChange;
        coordinates.push({ lat: lat / precision, lng: lng / precision });
    }
    return coordinates;
}

function decodeGooglePolyline(encoded) {
    const coordinates = [];
    let index = 0, lat = 0, lng = 0;
    while (index < encoded.length) {
        let b, shift = 0, result = 0;
        do {
            b = encoded.charCodeAt(index++) - 63;
            result |= (b & 0x1f) << shift;
            shift += 5;
        } while (b >= 0x20);
        const dlat = ((result & 1) ? ~(result >> 1) : (result >> 1));
        lat += dlat;
        shift = 0; result = 0;
        do {
            b = encoded.charCodeAt(index++) - 63;
            result |= (b & 0x1f) << shift;
            shift += 5;
        } while (b >= 0x20);
        const dlng = ((result & 1) ? ~(result >> 1) : (result >> 1));
        lng += dlng;
        coordinates.push({ lat: lat / 1e5, lng: lng / 1e5 });
    }
    return coordinates;
}

window.addEventListener('beforeunload', () => {
    if (operatorDashboard?.driverLocationInterval) {
        clearInterval(operatorDashboard.driverLocationInterval);
    }
});

document.addEventListener('DOMContentLoaded', initializeSystem);