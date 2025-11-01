// driver.js - Complete Driver Dashboard Functionality
class DriverDashboard {
    constructor() {
        this.token = localStorage.getItem('token');
        this.username = localStorage.getItem('username');
        this.routes = [];
        this.refreshInterval = null;

        this.init();
    }

    init() {
        if (!this.token) {
            window.location.href = '/login.html';
            return;
        }

        this.setupAuth();
        this.setupEventListeners();
        this.loadRoutes();
        this.startAutoRefresh();
        this.updateUsernameDisplay();
    }

    setupAuth() {
        // Intercept all fetch requests to add authorization header
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

    updateUsernameDisplay() {
        const usernameElement = document.getElementById('username');
        if (usernameElement && this.username) {
            usernameElement.textContent = this.username;
        }
    }

    setupEventListeners() {
        // Refresh button
        const refreshBtn = document.getElementById('refresh-routes');
        if (refreshBtn) {
            refreshBtn.addEventListener('click', () => this.loadRoutes());
        }

        // Manual refresh button (alternative ID)
        const manualRefreshBtn = document.querySelector('[onclick="loadRoutes()"]');
        if (manualRefreshBtn) {
            manualRefreshBtn.addEventListener('click', (e) => {
                e.preventDefault();
                this.loadRoutes();
            });
        }

        // Logout button
        const logoutBtn = document.getElementById('logout-btn');
        if (logoutBtn) {
            logoutBtn.addEventListener('click', () => this.logout());
        }

        // Alternative logout button
        const altLogoutBtn = document.querySelector('[onclick="logout()"]');
        if (altLogoutBtn) {
            altLogoutBtn.addEventListener('click', (e) => {
                e.preventDefault();
                this.logout();
            });
        }
    }

    async loadRoutes(silent = false) {
        if (!silent) {
            this.showLoading();
            this.animateRefreshIcon();
        }

        try {
            const response = await fetch('/api/routes/my-routes');

            if (!response.ok) {
                if (response.status === 401) {
                    this.handleAuthError();
                    return;
                }
                throw new Error(`HTTP ${response.status}: Failed to load routes`);
            }

            this.routes = await response.json();
            this.displayRoutes();
        } catch (error) {
            console.error('Error loading routes:', error);
            if (!silent) {
                this.displayError(error.message || 'Error loading routes');
            }
        } finally {
            this.stopRefreshAnimation();
        }
    }

    animateRefreshIcon() {
        const refreshIcon = document.getElementById('refresh-icon');
        if (refreshIcon) {
            refreshIcon.style.animation = 'spin 1s linear infinite';
        }
    }

    stopRefreshAnimation() {
        const refreshIcon = document.getElementById('refresh-icon');
        if (refreshIcon) {
            refreshIcon.style.animation = '';
        }
    }

    displayRoutes() {
        const container = document.getElementById('routes-container');

        if (!this.routes || this.routes.length === 0) {
            container.innerHTML = this.getNoRoutesHtml();
            return;
        }

        const routesHtml = this.routes.map(route => this.getRouteCardHtml(route)).join('');
        container.innerHTML = routesHtml;

        // Setup event listeners for route actions
        this.setupRouteEventListeners();
    }

    getRouteCardHtml(route) {
        return `
            <div class="route-card" data-route-id="${route.id}">
                <div class="row">
                    <div class="col-md-8">
                        <h5>Trasa #${route.id}</h5>
                        <p class="mb-1"><strong>Start:</strong> ${route.startAddress}</p>
                        <p class="mb-1"><strong>Koniec:</strong> ${route.endAddress}</p>
                        <div class="row mt-2">
                            <div class="col-md-4">
                                <small class="text-muted">Dystans: ${this.formatDistance(route.totalDistanceKm)}</small>
                            </div>
                            <div class="col-md-4">
                                <small class="text-muted">Czas: ${this.formatTime(route.estimatedTimeMinutes)}</small>
                            </div>
                            <div class="col-md-4">
                                <span class="route-status status-${route.status.toLowerCase()}">${this.getStatusText(route.status)}</span>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-4 text-end">
                        <div class="btn-group-vertical" role="group">
                            ${this.getRouteActionsHtml(route)}
                        </div>
                    </div>
                </div>
                ${this.getWarningsSectionHtml(route)}
            </div>
        `;
    }

    getRouteActionsHtml(route) {
        let html = '';

        // Navigation button
        if (route.status === 'ASSIGNED' || route.status === 'ACTIVE') {
            html += `
                <button class="btn btn-success mb-2" onclick="driverDashboard.startNavigation(${route.id})">
                    Nawiguj
                </button>
            `;
        }

        // Start route button
        if (route.status === 'ASSIGNED') {
            html += `
                <button class="btn btn-primary mb-2" onclick="driverDashboard.startRoute(${route.id})">
                    Rozpocznij trasę
                </button>
            `;
        }

        // Complete route button
        if (route.status === 'ACTIVE') {
            html += `
                <button class="btn btn-warning mb-2" onclick="driverDashboard.completeRoute(${route.id})">
                    Zakończ trasę
                </button>
            `;
        }

        // Download buttons
        html += `
            <a href="/api/routes/${route.id}/navigation-file?format=gpx"
               class="btn btn-outline-primary btn-sm mb-1"
               download="route_${route.id}.gpx">
                Pobierz GPX
            </a>
            <a href="/api/routes/${route.id}/navigation-file?format=kml"
               class="btn btn-outline-secondary btn-sm"
               download="route_${route.id}.kml">
                Pobierz KML
            </a>
        `;

        return html;
    }

    getWarningsSectionHtml(route) {
        if (route.hasRestrictions || (route.warnings && route.warnings.length > 0)) {
            const warnings = route.warnings || [];
            return `
                <div class="alert alert-warning mt-3 mb-0">
                    <strong>Ostrzeżenia:</strong>
                    <ul class="mb-0 mt-1">
                        ${warnings.map(warning => `<li>${warning}</li>`).join('')}
                    </ul>
                </div>
            `;
        }
        return '';
    }

    getNoRoutesHtml() {
        return `
            <div class="no-routes">
                <h4>Brak przypisanych tras</h4>
                <p>Skontaktuj się z operatorem w celu przypisania tras.</p>
                <button class="btn btn-outline-primary mt-2" onclick="driverDashboard.loadRoutes()">
                    Sprawdź ponownie
                </button>
            </div>
        `;
    }

    setupRouteEventListeners() {
        // Event delegation for dynamically created elements
        const container = document.getElementById('routes-container');
        if (container) {
            container.addEventListener('click', (e) => {
                if (e.target.matches('[data-action="start-route"]')) {
                    const routeId = e.target.dataset.routeId;
                    this.startRoute(parseInt(routeId));
                }
                if (e.target.matches('[data-action="complete-route"]')) {
                    const routeId = e.target.dataset.routeId;
                    this.completeRoute(parseInt(routeId));
                }
            });
        }
    }

    async startRoute(routeId) {
        if (!confirm('Czy na pewno chcesz rozpocząć tę trasę?')) {
            return;
        }

        try {
            const response = await fetch(`/api/routes/${routeId}/start`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });

            if (response.ok) {
                const updatedRoute = await response.json();
                this.showSuccess('Trasa rozpoczęta pomyślnie!');
                this.loadRoutes();
                // ✅ ZMIANA: Automatyczne przejście do nawigacji po rozpoczęciu trasy
                this.startNavigation(routeId);
            } else {
                throw new Error('Nie udało się rozpocząć trasy');
            }
        } catch (error) {
            console.error('Error starting route:', error);
            this.showError('Błąd podczas rozpoczynania trasy');
        }
    }

    async completeRoute(routeId) {
        if (!confirm('Czy na pewno chcesz zakończyć tę trasę?')) {
            return;
        }

        try {
            const response = await fetch(`/api/routes/${routeId}/complete`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });

            if (response.ok) {
                const updatedRoute = await response.json();
                this.showSuccess('Trasa zakończona pomyślnie!');
                this.loadRoutes();
            } else {
                throw new Error('Nie udało się zakończyć trasy');
            }
        } catch (error) {
            console.error('Error completing route:', error);
            this.showError('Błąd podczas kończenia trasy');
        }
    }

    startNavigation(routeId) {
        // Redirect to navigation page with route ID
        window.location.href = `/navigation/${routeId}`;
    }

    // Utility methods
    formatDistance(distance) {
        return distance ? `${distance.toFixed(1)} km` : 'N/A';
    }

    formatTime(minutes) {
        if (!minutes) return 'N/A';
        const hours = Math.floor(minutes / 60);
        const mins = Math.round(minutes % 60);
        return hours > 0 ? `${hours}h ${mins}min` : `${mins} min`;
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

    showLoading() {
        const container = document.getElementById('routes-container');
        container.innerHTML = `
            <div class="spinner-container">
                <div class="spinner-border text-primary" role="status">
                    <span class="visually-hidden">Ładowanie...</span>
                </div>
                <p class="mt-2">Ładowanie tras...</p>
            </div>
        `;
    }

    displayError(message) {
        const container = document.getElementById('routes-container');
        container.innerHTML = `
            <div class="alert alert-danger">
                <h5>Błąd</h5>
                <p>${message}</p>
                <button class="btn btn-danger" onclick="driverDashboard.loadRoutes()">
                    Spróbuj ponownie
                </button>
            </div>
        `;
    }

    showSuccess(message) {
        this.showNotification(message, 'success');
    }

    showError(message) {
        this.showNotification(message, 'danger');
    }

    showInfo(message) {
        this.showNotification(message, 'info');
    }

    showNotification(message, type = 'info') {
        // Create notification element
        const notification = document.createElement('div');
        notification.className = `alert alert-${type} alert-dismissible fade show position-fixed`;
        notification.style.cssText = 'top: 20px; right: 20px; z-index: 9999; min-width: 300px; max-width: 400px;';
        notification.innerHTML = `
            ${message}
            <button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
        `;

        document.body.appendChild(notification);

        // Auto remove after 5 seconds
        setTimeout(() => {
            if (notification.parentNode) {
                notification.parentNode.removeChild(notification);
            }
        }, 5000);

        // Manual close functionality
        const closeBtn = notification.querySelector('.btn-close');
        if (closeBtn) {
            closeBtn.addEventListener('click', () => {
                if (notification.parentNode) {
                    notification.parentNode.removeChild(notification);
                }
            });
        }
    }

    handleAuthError() {
        this.showError('Sesja wygasła. Zostaniesz przekierowany do strony logowania.');
        setTimeout(() => {
            localStorage.removeItem('token');
            localStorage.removeItem('username');
            window.location.href = '/login.html';
        }, 2000);
    }

    startAutoRefresh() {
        // Clear any existing interval
        if (this.refreshInterval) {
            clearInterval(this.refreshInterval);
        }

        // Start new interval - refresh every minute
        this.refreshInterval = setInterval(() => {
            this.loadRoutes(true); // Silent refresh
        }, 60000);
    }

    logout() {
        if (confirm('Czy na pewno chcesz się wylogować?')) {
            // Clear intervals
            if (this.refreshInterval) {
                clearInterval(this.refreshInterval);
            }

            // Clear storage
            localStorage.removeItem('token');
            localStorage.removeItem('username');

            // Redirect
            window.location.href = '/login.html';
        }
    }

    destroy() {
        if (this.refreshInterval) {
            clearInterval(this.refreshInterval);
        }
    }

    // Methods for backward compatibility with existing HTML onclick handlers
    static loadRoutes() {
        if (window.driverDashboard) {
            window.driverDashboard.loadRoutes();
        }
    }

    static logout() {
        if (window.driverDashboard) {
            window.driverDashboard.logout();
        }
    }
}

// Global functions for backward compatibility
function loadRoutes() {
    if (window.driverDashboard) {
        window.driverDashboard.loadRoutes();
    }
}

function logout() {
    if (window.driverDashboard) {
        window.driverDashboard.logout();
    }
}

// Initialize driver dashboard when DOM is loaded
let driverDashboard;
document.addEventListener('DOMContentLoaded', () => {
    driverDashboard = new DriverDashboard();

    // Make it globally accessible
    window.driverDashboard = driverDashboard;
});

// Cleanup on page unload
window.addEventListener('beforeunload', () => {
    if (driverDashboard) {
        driverDashboard.destroy();
    }
});

// Add CSS animation for refresh icon
const style = document.createElement('style');
style.textContent = `
    @keyframes spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
    }
`;
document.head.appendChild(style);