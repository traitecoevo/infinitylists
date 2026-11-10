// Leaflet map: basemaps, clustered record markers, target polygon and buffer.
// Relies on the vendored Leaflet + markercluster scripts (global `L`).

import { esc } from './table.js'

export class RecordMap {
  constructor(el) {
    this.map = L.map(el, { zoomControl: true, worldCopyJump: true }).setView([-25, 134], 4)

    const esriAttr = 'Tiles &copy; <a href="https://www.esri.com/">Esri</a> &mdash; ' +
      'Source: Esri, Maxar, Earthstar Geographics, and the GIS User Community'
    const imagery = L.tileLayer(
      'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
      { maxZoom: 19, maxNativeZoom: 18, attribution: esriAttr })
    const labels = L.tileLayer(
      'https://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}',
      { maxZoom: 19, maxNativeZoom: 18, pane: 'overlayPane' })
    const osm = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
      maxZoom: 19,
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
    })
    const satellite = L.layerGroup([imagery, labels]).addTo(this.map)
    L.control.layers({ Satellite: satellite, Map: osm }, null, { position: 'topright' }).addTo(this.map)
    L.control.scale({ imperial: false }).addTo(this.map)

    this.overlays = L.layerGroup().addTo(this.map)
  }

  /**
   * @param {object|null} place    GeoJSON feature of the target area
   * @param {object|null} buffered GeoJSON feature of the buffered area (=== place when no buffer)
   * @param {{lat: number, lon: number, species: string, voucher: string}[]} points
   * @param {(p: any) => string} [popupHtml] popup content for a point (must escape
   *   its own content); built lazily when the marker is clicked
   */
  update(place, buffered, points, popupHtml) {
    this.overlays.clearLayers()
    if (!place) return

    const popup = popupHtml || ((p) => `${esc(p.species)} ${esc(p.voucher)}`)
    const cluster = L.markerClusterGroup({ maxClusterRadius: 20, chunkedLoading: true })
    cluster.addLayers(points.map((p) => L.marker([p.lat, p.lon]).bindPopup(() => popup(p))))

    const placeLayer = L.geoJSON(place, { style: { color: 'red', weight: 3, fillOpacity: 0.1 }, interactive: false })
    if (buffered && buffered !== place) {
      this.overlays.addLayer(L.geoJSON(buffered, { style: { color: 'darkorange', weight: 3, fillOpacity: 0.08 }, interactive: false }))
    }
    this.overlays.addLayer(placeLayer)
    this.overlays.addLayer(cluster)

    const bounds = L.geoJSON(buffered || place).getBounds()
    if (bounds.isValid()) this.map.fitBounds(bounds, { padding: [20, 20] })
  }

  invalidate() {
    this.map.invalidateSize()
  }
}
