// operator.js - Kompletna, połączona i naprawiona wersja
class OperatorDashboard {
    constructor() {
        this.token = localStorage.getItem('token');
        this.username = localStorage.getItem('username');
        this.map = null;
        this.directionsService = null;
        this.directionsRenderer = null; // Dla zatwierdzonej trasy
        this.previewRenderer = null; // Dla podglądu pierwotnej trasy
        this.driverMarkers = {};
        this.transportSets = [];
        this.activeDrivers = [];
        this.refreshInterval = null;
        this.routes = [];
        this.mapInitialized = false;
        this.rejectedPointsMarkers = []; // Markery dla odrzuconych punktów
        this.routesRequiringAcceptance = []; // Trasy do akceptacji

        // Zmienne globalne (przeniesione z HTML)
        this.allCargo = [];
        this.selectedTransportMode = 'trailer';
        this.routePreviewMap = null;
        this.driverLocationMap = null;
        this.driverLocationMarker = null;
        this.driverLocationInterval = null;
        this.currentTrackedDriver = null;

        // ✅ ZMIANA: Przechowuje listę kierowców i instancje modali
        this.drivers = [];
        this.addDriverModalInstance = null;
        this.assignDriverModalInstance = null;
    }

    init() {
        // setupAuth() jest teraz wywoływane *przed* init() w initializeSystem()
        // więc nie musimy go tu wywoływać

        const username = localStorage.getItem('username');
        if (username) document.getElementById('username').textContent = username;

        this.loadTransportSets();
        this.setupEventListeners();
        this.startAutoRefresh();
        this.loadRoutesRequiringAcceptance();

        // ✅ ZMIANA: Inicjalizacja instancji modali
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
        // Ta funkcja "łata" globalny 'fetch', aby dodawać token
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
            // Inicjalizacja głównej mapy
            this.map = new window.google.maps.Map(mapContainer, {
                zoom: 6,
                center: { lat: 52.0693, lng: 19.4803 },
                mapTypeControl: true,
                streetViewControl: false,
                fullscreenControl: true,
                zoomControl: true
            });

            this.directionsService = new window.google.maps.DirectionsService();

            // Główny renderer (niebieski)
            this.directionsRenderer = new window.google.maps.DirectionsRenderer({
                suppressMarkers: false,
                draggable: false,
                polylineOptions: { strokeColor: '#667eea', strokeWeight: 5, strokeOpacity: 0.8 }
            });
            this.directionsRenderer.setMap(this.map);

            // Renderer podglądu (pomarańczowy)
            this.previewRenderer = new window.google.maps.DirectionsRenderer({
                suppressMarkers: true,
                draggable: false,
                polylineOptions: { strokeColor: '#FFA500', strokeWeight: 4, strokeOpacity: 0.7, zIndex: 1 }
            });
            this.previewRenderer.setMap(this.map);

            // Inicjalizacja map w modalach (z HTML)
            this.routePreviewMap = new google.maps.Map(document.getElementById('route-preview-map'), {
                zoom: 10,
                center: { lat: 52.0693, lng: 19.4803 }
            });
            this.driverLocationMap = new google.maps.Map(document.getElementById('driver-location-map'), {
                zoom: 16,
                center: { lat: 52.2297, lng: 21.0122 }
            });

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
        // Event listenery dla klasy (formularz, odświeżanie)
        document.getElementById('route-form')?.addEventListener('submit', (e) => this.handleRouteSubmit(e));
        document.getElementById('refresh-drivers-list')?.addEventListener('click', () => this.loadDriversList());
        document.getElementById('refresh-routes')?.addEventListener('click', () => this.loadAllRoutes());
        document.getElementById('center-map')?.addEventListener('click', () => this.centerMapOnPoland());
        document.getElementById('start-address')?.addEventListener('blur', () => this.showRoutePreview());
        document.getElementById('end-address')?.addEventListener('blur', () => this.showRoutePreview());

        // Event listenery dla funkcji globalnych (z HTML)
        document.getElementById('routes-tab')?.addEventListener('click', () => switchTab('routes'));
        document.getElementById('drivers-tab')?.addEventListener('click', () => switchTab('drivers'));
        document.getElementById('vehicles-tab')?.addEventListener('click', () => switchTab('vehicles'));
        document.getElementById('add-equipment-tab')?.addEventListener('click', () => switchTab('add-equipment'));

        document.getElementById('vehicle-form')?.addEventListener('submit', (e) => handleVehicleFormSubmit(e));
        document.getElementById('add-transporter-form')?.addEventListener('submit', (e) => handleAddTransporterSubmit(e));
        document.getElementById('add-cargo-form')?.addEventListener('submit', (e) => handleAddCargoSubmit(e));

        document.getElementById('cargo-select')?.addEventListener('change', () => checkCargoCapabilities());

        // ✅ ZMIANA: Powiązanie handlerów z nowymi formularzami modali
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
                    this.directionsRenderer.setDirections({ routes: [] }); // Wyczyść główną
                    this.previewRenderer.setDirections(result); // Pokaż na podglądzie
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

    /**
     * ✅ ZMODYFIKOWANA FUNKCJA RENDEROWANIA (dla wyświetlania limitów i domyślnego REJECT)
     * Zgodna z poprawioną logiką backendu.
     */
    renderRouteRequiringAcceptance(route) {
        let rejectedPointsHtml = '';
        if (route.rejectedPoints && route.rejectedPoints.length > 0) {
            rejectedPointsHtml = `
                <div class="rejected-points mb-3">
                    <h6>Zidentyfikowane punkty problematyczne (${route.rejectedPoints.length}):</h6>
                    <div id="decision-list-${route.id}">
                        ${route.rejectedPoints.map((point, idx) => {
                // Wydobycie danych z tablicy "reason" (która w backendzie jest listą stringów)
                const reasonArray = point.reason || [];
                const mainReason = reasonArray.length > 0 ? reasonArray[0] : 'Brak szczegółowego powodu.';

                // Dodanie formatowania dla limitów i przekroczeń (jeśli dane są obecne w stringu)
                let detailsHtml = `<span class="text-danger">${escapeHtml(mainReason)}</span>`;

                // Wyszukiwanie limitów w stringu (np. "(Limit nośności: 75.0t)")
                const weightMatch = mainReason.match(/\(Limit nośności: ([\d.]+t)\)/);
                const heightMatch = mainReason.match(/\(Limit wysokości: ([\d.]+m)\)/);

                // Pobranie parametrów transportu (konieczne do porównania)
                const transportWeight = route.routeData?.transportSet?.totalWeightTons;
                const transportHeight = route.routeData?.transportSet?.totalHeightMeters;

                let limitInfo = '';

                if (weightMatch || heightMatch) {
                    limitInfo += '<ul class="list-unstyled mt-2 mb-0" style="font-size:0.9em;">';

                    if (weightMatch) {
                        const limit = parseFloat(weightMatch[1].replace('t', ''));
                        const actual = transportWeight ? transportWeight.toFixed(1) : 'N/A';
                        const isViolation = transportWeight > limit;
                        limitInfo += `<li class="${isViolation ? 'text-danger' : 'text-success'}"><strong>Nośność:</strong> ${actual}t (Limit: ${weightMatch[1]}) ${isViolation ? '❌ Przekroczono' : '✅ OK'}</li>`;
                    }

                    if (heightMatch) {
                        const limit = parseFloat(heightMatch[1].replace('m', ''));
                        const actual = transportHeight ? transportHeight.toFixed(2) : 'N/A';
                        const isViolation = transportHeight > limit;
                        limitInfo += `<li class="${isViolation ? 'text-danger' : 'text-success'}"><strong>Wysokość:</strong> ${actual}m (Limit: ${heightMatch[1]}) ${isViolation ? '❌ Przekroczono' : '✅ OK'}</li>`;
                    }
                    limitInfo += '</ul>';
                }
                // Koniec dodawania formatowania

                return `
                                <div class="rejected-point-item" data-point-name="${escapeHtml(point.name || 'Unknown Point')}">
                                    <div class="point-details">
                                        <strong>${escapeHtml(point.name || 'Punkt bez nazwy')}</strong>
                                    </div>
                                    <div class="point-reason">
                                        ${detailsHtml}
                                        ${limitInfo}
                                    </div>
                                    <div class="decision-radios">
                                        <div class="form-check">
                                            <input class="form-check-input" type="radio" name="decision-${route.id}-${idx}" id="accept-${route.id}-${idx}" value="ACCEPTED">
                                            <label class="form-check-label text-success" for="accept-${route.id}-${idx}">
                                                ✅ Akceptuj
                                            </label>
                                        </div>
                                        <div class="form-check">
                                            <input class="form-check-input" type="radio" name="decision-${route.id}-${idx}" id="reject-${route.id}-${idx}" value="REJECTED" checked>
                                            <label class="form-check-label text-danger" for="reject-${route.id}-${idx}">
                                                ❌ Odrzuć (szukaj objazdu)
                                            </label>
                                        </div>
                                    </div>
                                    <div class="form-group mt-2">
                                        <textarea id="comment-${route.id}-${idx}" class="form-control form-control-sm" rows="1"
                                                  placeholder="Opcjonalny komentarz do punktu..."></textarea>
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
                <p class="text-danger mb-2"><strong>Problemy z walidacją:</strong></p>
                <ul class="mb-3">
                    ${route.operatorMessages.map(msg => `<li>${escapeHtml(msg)}</li>`).join('')}
                </ul>
            `;
        }

        return `
            <div class="card mb-3 border-warning">
                <div class="card-body" id="route-card-acceptance-${route.id}">
                    <h6>Trasa #${route.id}: ${escapeHtml(route.startAddress)} → ${escapeHtml(route.endAddress)}</h6>
                    ${operatorMessagesHtml}
                    ${rejectedPointsHtml}
                    
                    <div class="btn-group mt-3">
                        <button class="btn btn-warning" onclick="operatorDashboard.submitPointDecisions(${route.id})">
                            Prześlij Decyzje
                        </button>
                        <button class="btn btn-info" onclick="showRouteOnMap(${route.id})">
                            🗺️ Pokaż na mapie
                        </button>
                        </div>
                </div>
            </div>
        `;
    }

    /**
     * ✅ KLUCZOWA POPRAWKA LOGIKI: Zbiera indywidualne decyzje i wysyła je do backendu.
     */
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
            // Zablokuj przycisk
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

            // Backend zwróci informację o wyniku (ROUTE_ACCEPTED lub REQUIRES_REVIEW_AGAIN)
            if (result.action === 'ROUTE_ACCEPTED') {
                this.showSuccess(`✅ ${result.message || 'Trasa zaakceptowana pomyślnie'}`);
            } else if (result.action === 'REQUIRES_REVIEW_AGAIN') {
                this.showMessage(`⚠️ ${result.message || 'Rewalidacja znalazła nowe problemy'}`, 'warning', 15000);
            } else {
                this.showSuccess('Decyzje przetworzone.');
            }

            // Przeładuj obie listy
            this.loadRoutesRequiringAcceptance();
            this.loadAllRoutes();

        } catch (error) {
            this.showError('Błąd podczas przesyłania decyzji: ' + error.message);
        } finally {
            const button = card.querySelector('.btn-warning');
            button.disabled = false;
            button.innerHTML = 'Prześlij Decyzje';
        }
    }


    /**
     * Ta funkcja jest teraz wywoływana tylko przez backend, jeśli wszystkie punkty zostały zaakceptowane.
     * Nie jest już bezpośrednio wywoływana z UI dla tras z problemami.
     */
    async acceptRouteWithProblems(routeId) {
        const comment = document.getElementById(`comment-${routeId}`)?.value;
        if (!comment || comment.trim().length < 10) {
            this.showError('Musisz podać uzasadnienie (min. 10 znaków)');
            return;
        }
        const acceptedPoints = [];
        const route = this.routesRequiringAcceptance.find(r => r.id === routeId);
        if (route && route.rejectedPoints) {
            route.rejectedPoints.forEach((point, idx) => {
                const checkbox = document.getElementById(`point-${route.id}-${idx}`);
                if (checkbox && checkbox.checked) {
                    acceptedPoints.push(point.name || 'Unknown Point');
                }
            });
        }
        if (!confirm(`Akceptujesz trasę #${routeId} mimo problemów?\nZaakceptowane punkty: ${acceptedPoints.length}\nKomentarz: ${comment}`)) {
            return;
        }
        try {
            const response = await fetch(`/api/routes/${routeId}/accept-with-problems`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ comment, acceptedPoints })
            });
            if (!response.ok) throw new Error('Failed to accept route');
            this.showSuccess(`✅ Trasa #${routeId} zaakceptowana`);
            this.loadRoutesRequiringAcceptance();
            this.loadAllRoutes();
        } catch (error) {
            this.showError('Błąd podczas akceptacji trasy: ' + error.message);
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
            displayRoutesWithAllButtons(); // Używamy globalnej funkcji z HTML
        } catch (error) {
            console.error('Error loading routes:', error);
            container.innerHTML = `<div class="alert alert-danger">Błąd ładowania tras: ${error.message}</div>`;
        }
    }

    // Ta funkcja nie jest już potrzebna, bo używamy `displayRoutesWithAllButtons`
    // displayRoutes() { ... }

    async loadDriversList() {
        const listDiv = document.getElementById('drivers-list');
        if (!listDiv) return;
        listDiv.innerHTML = '<div class="text-center p-4"><div class="spinner-border"></div></div>';

        try {
            const usersResponse = await fetch('/api/admin/users');
            if (!usersResponse.ok) throw new Error('Nie udało się załadować użytkowników');
            const allUsers = await usersResponse.json();
            const drivers = allUsers.filter(u => u.role === 'DRIVER');

            // ✅ ZMIANA: Zapisz listę kierowców w instancji
            this.drivers = drivers;

            if (drivers.length === 0) {
                listDiv.innerHTML = `
                    <div class="text-center p-4">
                        <p>Brak kierowców</p>
                        <button class="btn btn-primary" onclick="operatorDashboard.showAddDriverModal()">Dodaj kierowcę</button>
                    </div>
                `;
                this.updateDriversSelect(drivers); // Wyczyść listę
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
                    <button class="btn btn-success btn-sm btn-map-driver" onclick="showDriverLocation('${driver.username}')">
                        🗺️ Zobacz na mapie
                    </button>
                ` : '';

                return `
                    <div class="driver-list-item list-group-item">
                        <div class="d-flex justify-content-between">
                            <div>
                                <h5>${driver.firstName || driver.username} ${driver.lastName || ''}
                                    <span class="badge ${isOnline ? 'bg-success' : 'bg-secondary'}">
                                        ${isOnline ? 'Online' : 'Offline'}
                                    </span>
                                </h5>
                                <small>Login: ${driver.username} | Email: ${driver.email}</small>
                                ${activeInfo ? `<div class="mt-2"><small>Trasa: ${activeInfo.routeDescription || 'Brak'}</small></div>` : ''}
                            </div>
                            <div>
                                ${mapButton}
                                <button class="btn btn-sm btn-danger" onclick="operatorDashboard.deleteDriver('${driver.username}')">Usuń</button>
                            </div>
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
        const driversList = drivers.map(d => `<option value="${d.username}">${d.firstName || d.username} ${d.lastName || ''}</option>`).join('');

        // Aktualizuj listę w formularzu tworzenia trasy
        const selectMain = document.getElementById('driver-username');
        if (selectMain) {
            selectMain.innerHTML = '<option value="">Wybierz kierowcę (opcjonalnie)...</option>' + driversList;
        }

        // ✅ ZMIANA: Aktualizuj listę w modalu przypisywania kierowcy
        const selectModal = document.getElementById('assign-driver-select');
        if (selectModal) {
            selectModal.innerHTML = '<option value="">Wybierz kierowcę...</option>' + driversList;
        }
    }

    // ✅ ZMIANA: Ta funkcja teraz tylko pokazuje modal
    async showAddDriverModal() {
        if (this.addDriverModalInstance) {
            document.getElementById('add-driver-form').reset();
            this.addDriverModalInstance.show();
        } else {
            // Fallback, gdyby bootstrap nie zadziałał
            alert('Błąd: Nie można otworzyć formularza. Odśwież stronę.');
        }
    }

    // ✅ ZMIANA: Nowa funkcja do obsługi formularza dodawania kierowcy
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

    // ✅ ZMIANA: Ta funkcja teraz tylko pokazuje modal i ustawia dane
    async showAssignDriverModal(routeId) {
        if (!this.assignDriverModalInstance) {
            alert('Błąd: Modal nie jest zainicjalizowany');
            return;
        }

        // Znajdź trasę, aby wyświetlić jej nazwę
        const route = this.routes.find(r => r.id === routeId);
        const routeName = route ? `${route.startAddress} → ${route.endAddress}` : `Trasa #${routeId}`;

        // Wypełnij formularz w modalu
        document.getElementById('assign-driver-route-id').value = routeId;
        document.getElementById('assign-driver-route-name').textContent = routeName;

        // Zresetuj wybór
        document.getElementById('assign-driver-select').value = "";

        // Pokaż modal
        this.assignDriverModalInstance.show();
    }

    // ✅ ZMIANA: Nowa funkcja do obsługi formularza przypisywania kierowcy
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


    // Ta metoda jest teraz globalna (wywoływana z HTML)
    // async showRouteOnMap(routeId) { ... }

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
                        icon: 'http://googleusercontent.com/maps/google.com/1' // Żółta kropka
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
        if (window.showFullValidation) {
            window.showFullValidation(routeId);
        } else {
            console.error("Brak globalnej funkcji showFullValidation");
        }
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

                // ✅ POPRAWKA: Odświeżenie list po usunięciu
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
// START Globalna logika (przeniesiona z HTML)
// -------------------------------------------------------------------

let operatorDashboard; // Globalna instancja
let googleMapsLoaded = false;

/**
 * Główny punkt startowy aplikacji, wywoływany przez DOMContentLoaded
 */
async function initializeSystem() {
    try {
        const token = localStorage.getItem('token');
        if (!token) {
            window.location.href = '/login.html';
            return;
        }

        // Natychmiast utwórz instancję klasy
        operatorDashboard = new OperatorDashboard();
        window.operatorDashboard = operatorDashboard; // Udostępnij globalnie

        // NAPRAWA BŁĘDU: Najpierw "załataj" fetch, potem pobieraj klucz
        operatorDashboard.setupAuth();

        // Rozpocznij ładowanie mapy (pobierze klucz API)
        await loadGoogleMaps();

        // `onGoogleMapsLoaded` (callback z Google) wywoła `operatorDashboard.initMap()`

        // `init()` klasy zajmie się resztą (ładowaniem danych, itd.)
        // setupAuth() jest już wywołane, więc init() go nie powtórzy
        operatorDashboard.init();

        switchTab('routes');

    } catch (error) {
        console.error('Initialization failed:', error);
        alert('Nie udało się załadować systemu: ' + error.message);
    }
}

/**
 * Ładuje skrypt Google Maps (pobiera klucz API)
 */
async function loadGoogleMaps() {
    if (googleMapsLoaded) return Promise.resolve();

    try {
        // `fetch` jest już "załatan" przez `setupAuth()`
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

            // Definiujemy globalny callback
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
        throw error; // Rzuć błąd dalej, aby zatrzymać `initializeSystem`
    }
}

// -------------------------------------------------------------------
// Funkcje globalne (wywoływane przez onclick z HTML)
// -------------------------------------------------------------------

function switchTab(tab) {
    document.querySelectorAll('#routes-section, #drivers-section, #vehicles-section, #add-equipment-section').forEach(el => el.style.display = 'none');
    document.querySelectorAll('#routes-tab, #drivers-tab, #vehicles-tab, #add-equipment-tab').forEach(el => {
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

async function loadVehiclesTab() {
    try {
        const [transporters, cargo, sets] = await Promise.all([
            fetch('/api/vehicles/transporters').then(r => r.json()),
            fetch('/api/vehicles/cargo').then(r => r.json()),
            fetch('/api/vehicles/transport-sets').then(r => r.json())
        ]);

        operatorDashboard.allCargo = cargo; // Zapisz w instancji

        document.getElementById('transporter-select').innerHTML = '<option value="">Wybierz...</option>' +
            transporters.map(t => `<option value="${t.id}">${t.model} (${t.totalWeightKg}kg)</option>`).join('');

        document.getElementById('cargo-select').innerHTML = '<option value="">Wybierz...</option>' +
            cargo.map(c => `<option value="${c.id}">${c.model} (${c.totalWeightKg}kg, ${c.heightCm}cm)${c.canDriveAlone ? ' 🚗' : ''}</option>`).join('');

        document.getElementById('existing-sets').innerHTML = sets.map(set => `
            <div class="card mb-2">
                <div class="card-body">
                    <h6>${set.description || 'Zestaw #' + set.id}</h6>
                    <small>Ciężarówka: ${set.transporter.model}</small><br>
                    <small>Ładunek: ${set.cargo.model}</small><br>
                    <small>Typ: ${set.trailerType || 'N/A'}</small><br>
                    <small>Waga: ${set.totalWeightKg}kg</small><br>
                    <small>Wysokość: ${set.totalHeightCm}cm</small>
                </div>
            </div>
        `).join('');
    } catch (error) {
        console.error('Error loading vehicles:', error);
    }
}

// Formularze z zakładki "Vehicles"
async function handleVehicleFormSubmit(e) {
    e.preventDefault();
    try {
        const response = await fetch('/api/vehicles/transport-sets', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                transporterId: document.getElementById('transporter-select').value,
                cargoId: document.getElementById('cargo-select').value,
                description: document.getElementById('set-description').value,
                transportMode: operatorDashboard.selectedTransportMode
            })
        });
        if (response.ok) {
            alert('Zestaw utworzony!');
            document.getElementById('vehicle-form').reset();
            document.getElementById('transport-mode-container').style.display = 'none';
            operatorDashboard.selectedTransportMode = 'trailer';
            loadVehiclesTab();
            if (operatorDashboard?.loadTransportSets) operatorDashboard.loadTransportSets();
        }
    } catch (error) {
        console.error('Error:', error);
        alert('Błąd podczas tworzenia zestawu');
    }
}

async function handleAddTransporterSubmit(e) {
    e.preventDefault();
    try {
        const response = await fetch('/api/vehicles/transporters', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                model: document.getElementById('trans-model').value,
                totalWeightKg: parseInt(document.getElementById('trans-weight').value),
                heightCm: parseInt(document.getElementById('trans-height').value),
                maxAxleLoadKg: parseInt(document.getElementById('trans-axle').value)
            })
        });
        if (response.ok) {
            alert('Ciężarówka dodana!');
            document.getElementById('add-transporter-form').reset();
            loadEquipmentLists();
        } else {
            throw new Error('Failed');
        }
    } catch (error) {
        alert('Błąd podczas dodawania ciężarówki');
    }
}

async function handleAddCargoSubmit(e) {
    e.preventDefault();
    try {
        const response = await fetch('/api/vehicles/cargo', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                model: document.getElementById('cargo-model').value,
                totalWeightKg: parseInt(document.getElementById('cargo-weight').value),
                heightCm: parseInt(document.getElementById('cargo-height').value),
                maxAxleLoadKg: parseInt(document.getElementById('cargo-axle').value)
            })
        });
        if (response.ok) {
            alert('Ładunek dodany!');
            document.getElementById('add-cargo-form').reset();
            loadEquipmentLists();
        } else {
            throw new Error('Failed');
        }
    } catch (error) {
        alert('Błąd podczas dodawania ładunku');
    }
}

async function loadEquipmentLists() {
    try {
        const [transporters, cargo] = await Promise.all([
            fetch('/api/vehicles/transporters').then(r => r.json()),
            fetch('/api/vehicles/cargo').then(r => r.json())
        ]);
        document.getElementById('transporters-list').innerHTML = transporters.map(t => `
            <div class="card mb-2"><div class="card-body p-2">
                <strong>${t.model}</strong><br>
                <small>Waga: ${t.totalWeightKg}kg, Wys: ${t.heightCm}cm</small>
            </div></div>
        `).join('');
        document.getElementById('cargo-list').innerHTML = cargo.map(c => `
            <div class="card mb-2"><div class="card-body p-2">
                <strong>${c.model}</strong> ${c.canDriveAlone ? '🚗' : ''}<br>
                <small>Waga: ${c.totalWeightKg}kg, Wys: ${c.heightCm}cm</small>
            </div></div>
        `).join('');
    } catch (error) {
        console.error('Error loading equipment:', error);
    }
}

// Globalne funkcje wywoływane przez onclick, delegujące do instancji
function assignDriver(routeId) {
    // ✅ ZMIANA: Delegowanie do nowej funkcji w instancji
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
    // ✅ ZMIANA: Delegowanie do nowej funkcji w instancji
    if (operatorDashboard?.showAddDriverModal) operatorDashboard.showAddDriverModal();
}

function logout() {
    if (confirm('Czy na pewno chcesz się wylogować?')) {
        if (operatorDashboard?.destroy) operatorDashboard.destroy();
        localStorage.clear();
        window.location.href = '/login.html';
    }
}

// Globalne funkcje z bloku HTML (dla renderowania tras i modali)
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
    for (const route of operatorDashboard.routes) {
        // Używamy "isDraft" i "operatorAccepted" z "nowej" logiki
        const validation = await getRouteValidation(route.id);
        html += createCompleteRouteCard(route, validation);
    }
    container.innerHTML = html;
}

async function getRouteValidation(routeId) {
    try {
        const response = await fetch(`/api/routes/${routeId}/validation-details`);
        if (!response.ok) return null;
        return await response.json();
    } catch (error) {
        console.error('Error loading validation:', error);
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

    // Logika badge'a z "nowej" wersji
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

    // Reszta (uzasadnienie, pozwolenia)
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
        location = permitString.split('(')[0].trim();
        description = permitString.substring(permitString.indexOf(')') + 1).trim();
    }
    const roadMatch = description.match(/- droga: ([^:]+):/);
    if (roadMatch) {
        road = roadMatch[1].trim();
        description = description.substring(description.lastIndexOf(':') + 1).trim();
    } else if (description.includes(':')) {
        const colonIndex = description.indexOf(':');
        if (!location) location = description.substring(0, colonIndex).trim();
        description = description.substring(colonIndex + 1).trim();
    }
    html += `<div style="flex:1;">`;
    if (location) html += `<div class="permit-location">${escapeHtml(location)}</div>`;
    if (city) html += `<div class="permit-city">📍 ${escapeHtml(city)}</div>`;
    if (road) html += `<div class="permit-road">🛣️ Droga: ${escapeHtml(road)}</div>`;
    html += `<div class="permit-description">${escapeHtml(description)}</div>`;
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
    if (!validation) {
        alert('Brak danych walidacji dla tej trasy');
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
        html += `<li><strong>Wysokość:</strong> ${(info.totalHeight_cm/100).toFixed(2)}m`;
        if (info.trailerHeight_cm && info.cargoHeight_cm) {
            html += ` (naczepa ${(info.trailerHeight_cm/100).toFixed(2)}m + ładunek ${(info.cargoHeight_cm/100).toFixed(2)}m)`;
        }
        html += `</li>`;
        html += `<li><strong>Waga:</strong> ${(info.totalWeight_kg/1000).toFixed(1)}t</li>`;
        html += `<li><strong>Długość:</strong> ${(info.totalLength_cm/100).toFixed(2)}m</li>`;
        html += `<li><strong>Szerokość:</strong> ${(info.totalWidth_cm/100).toFixed(2)}m</li>`;
        html += '</ul></div>';
    }
    if (validation.permits && validation.permits.length > 0) {
        html += '<div style="background:#fff3cd;padding:20px;border-radius:8px;margin-top:20px;border:2px solid #ffc107;">';
        html += `<h5 style="color:#856404;"><strong>⚠️ WYMAGANE POZWOLENIA (${validation.permits.length}):</strong></h5>`;
        html += '<ul style="margin:10px 0;line-height:2;">';
        validation.permits.forEach(permit => html += `<li style="margin-bottom:10px;">${escapeHtml(permit)}</li>`);
        html += '</ul></div>';
    }
    if (validation.routeJustification && validation.routeJustification.length > 0) {
        html += '<div style="background:#f8f9fa;padding:20px;border-radius:8px;margin-top:20px;border:2px solid #28a745;">';
        html += '<h5 style="color:#28a745;"><strong>DLACZEGO TA TRASA?</strong></h5>';
        html += '<div style="font-family:monospace;font-size:0.85em;line-height:1.8;margin-top:15px;">';
        validation.routeJustification.forEach(line => {
            let style = 'margin:3px 0;';
            if (line.includes('═══')) style += 'font-weight:bold;color:#0d6efd;font-size:1.15em;border-bottom:2px solid #0d6efd;padding:8px 0;margin-top:15px;';
            else if (line.includes('MOŻNA PRZEJECHAĆ') || line.includes('✔')) style += 'color:#28a745;font-weight:bold;background:#d4edda;padding:8px;border-radius:5px;margin:5px 0;';
            else if (line.includes('OMIJAMY') || line.includes('ZA CIĘŻKI')) style += 'color:#dc3545;font-weight:bold;background:#f8d7da;padding:8px;border-radius:5px;margin:5px 0;';
            else if (line.includes('⚠') || line.includes('UWAGA')) style += 'color:#856404;font-weight:bold;background:#fff3cd;padding:8px;border-radius:5px;margin:5px 0;';
            html += `<div style="${style}">${escapeHtml(line)}</div>`;
        });
        html += '</div></div>';
    }
    if (validation.violations && validation.violations.length > 0) {
        html += '<div style="background:#f8d7da;border-left:4px solid #dc3545;padding:15px;margin-top:20px;border-radius:5px;">';
        html += '<h6 style="color:#721c24;"><strong>Naruszenia (użyto trasy alternatywnej):</strong></h6><ul>';
        validation.violations.forEach(v => html += `<li>${escapeHtml(v)}</li>`);
        html += '</ul></div>';
    }
    if (!validation.hasViolations && !validation.hasRestrictions && !validation.lightVehicle) {
        html += '<div style="background:#d4edda;border-left:4px solid #28a745;padding:15px;margin-top:20px;border-radius:5px;">';
        html += '<h6 style="color:#155724;"><strong>✅ Trasa wolna od ograniczeń!</strong></h6>';
        html += '</div>';
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
        mapDiv.innerHTML = ''; // Wyczyść stary kontener
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

async function showDriverLocation(driverUsername) {
    console.log(`🗺️ Otwieram mapę dla kierowcy: ${driverUsername}`);
    operatorDashboard.currentTrackedDriver = driverUsername;
    const modal = document.getElementById('driver-location-modal');
    document.getElementById('driver-location-title').textContent = `📍 Lokalizacja kierowcy: ${driverUsername}`;
    modal.style.display = 'flex';

    const mapDiv = document.getElementById('driver-location-map');
    mapDiv.innerHTML = '';
    operatorDashboard.driverLocationMap = new google.maps.Map(mapDiv, {
        zoom: 16,
        center: { lat: 52.2297, lng: 21.0122 }
    });

    await updateDriverLocation(driverUsername);
    if (operatorDashboard.driverLocationInterval) clearInterval(operatorDashboard.driverLocationInterval);
    operatorDashboard.driverLocationInterval = setInterval(() => {
        if (operatorDashboard.currentTrackedDriver === driverUsername) {
            updateDriverLocation(driverUsername);
        }
    }, 3000);
}

async function updateDriverLocation(driverUsername) {
    try {
        const response = await fetch(`/api/tracking/driver/${driverUsername}/status`);
        if (!response.ok) throw new Error('Nie udało się pobrać lokalizacji');
        const driverInfo = await response.json();
        if (!driverInfo.isOnline || !driverInfo.latitude || !driverInfo.longitude) {
            document.getElementById('driver-status-live').textContent = '⚪ Offline';
            document.getElementById('driver-status-live').style.color = '#6c757d';
            return;
        }
        const position = { lat: driverInfo.latitude, lng: driverInfo.longitude };
        document.getElementById('driver-speed').textContent = `${Math.round(driverInfo.speedKmh || 0)} km/h`;
        if (driverInfo.lastUpdate) {
            const lastUpdate = new Date(driverInfo.lastUpdate);
            const now = new Date();
            const diffSeconds = Math.floor((now - lastUpdate) / 1000);
            if (diffSeconds < 10) document.getElementById('driver-last-update').textContent = 'teraz';
            else if (diffSeconds < 60) document.getElementById('driver-last-update').textContent = `${diffSeconds}s temu`;
            else document.getElementById('driver-last-update').textContent = `${Math.floor(diffSeconds / 60)}m temu`;
        }
        document.getElementById('driver-status-live').textContent = '🟢 Online';
        document.getElementById('driver-status-live').style.color = '#28a745';

        if (operatorDashboard.driverLocationMarker) {
            operatorDashboard.driverLocationMarker.setPosition(position);
        } else {
            operatorDashboard.driverLocationMarker = new google.maps.Marker({
                position: position,
                map: operatorDashboard.driverLocationMap,
                title: `Kierowca: ${driverUsername}`,
                icon: { path: google.maps.SymbolPath.CIRCLE, scale: 12, fillColor: '#4285F4', fillOpacity: 1, strokeColor: 'white', strokeWeight: 3 }
            });
        }
        operatorDashboard.driverLocationMap.panTo(position);
    } catch (error) {
        console.error('Błąd aktualizacji lokalizacji kierowcy:', error);
        document.getElementById('driver-status-live').textContent = '❌ Błąd';
        document.getElementById('driver-status-live').style.color = '#dc3545';
    }
}

function closeDriverLocationModal() {
    document.getElementById('driver-location-modal').style.display = 'none';
    if (operatorDashboard.driverLocationInterval) clearInterval(operatorDashboard.driverLocationInterval);
    operatorDashboard.driverLocationInterval = null;
    operatorDashboard.currentTrackedDriver = null;
    if (operatorDashboard.driverLocationMarker) {
        operatorDashboard.driverLocationMarker.setMap(null);
        operatorDashboard.driverLocationMarker = null;
    }
    const mapDiv = document.getElementById('driver-location-map');
    if (mapDiv) mapDiv.innerHTML = '';
    operatorDashboard.driverLocationMap = null;
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

function logout() {
    if (confirm('Czy na pewno chcesz się wylogować?')) {
        if (operatorDashboard?.destroy) operatorDashboard.destroy();
        localStorage.clear();
        window.location.href = '/login.html';
    }
}

window.addEventListener('beforeunload', () => {
    if (operatorDashboard?.driverLocationInterval) {
        clearInterval(operatorDashboard.driverLocationInterval);
    }
});

// Start aplikacji
document.addEventListener('DOMContentLoaded', initializeSystem);