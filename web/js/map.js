// Leaflet map: basemaps, record markers (clustered pins, or canvas dots for
// large sets), target polygon and buffer.
// Relies on the vendored Leaflet + markercluster scripts (global `L`).

import { esc } from './table.js'
import { pointsLayer } from './points.js'

/** Above this many points, draw canvas dots instead of clustered markers. */
const MAX_MARKERS = 20000

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

    // Small caption in the map corner (used in dot mode).
    const Note = L.Control.extend({
      onAdd: () => {
        const el = L.DomUtil.create('div', 'map-note')
        L.DomEvent.disableClickPropagation(el)
        el.hidden = true
        return el
      },
    })
    this.note = new Note({ position: 'bottomleft' }).addTo(this.map).getContainer()
  }

  setNote(text) {
    this.note.textContent = text
    this.note.hidden = !text
  }

  /**
   * @param {object|null} place    GeoJSON feature of the target area
   * @param {object|null} buffered GeoJSON feature of the buffered area (=== place when no buffer)
   * @param {{lat: number, lon: number, species: string, voucher: string}[]} points
   * @param {(p: any) => string} [popupHtml] popup content for a point (must escape
   *   its own content); built lazily when the marker is clicked
   */
  update(place, buffered, points, popupHtml) {
    this.map.closePopup()
    this.overlays.clearLayers()
    if (!place) { this.setNote(''); return }

    const popup = popupHtml || ((p) => `${esc(p.species)} ${esc(p.voucher)}`)
    let recordsLayer
    if (points.length > MAX_MARKERS) {
      // Too many for individual markers: canvas dots, clickable for the same popup.
      recordsLayer = pointsLayer(points, popup)
      this.setNote(`${points.length.toLocaleString()} records shown as dots ` +
        '(blue: target area, orange: buffer). Zoom in and click a dot for details.')
    } else {
      // Added in one synchronous batch: chunkedLoading kept adding markers
      // after the layer had been replaced, throwing "reading 'getMinZoom'".
      recordsLayer = L.markerClusterGroup({ maxClusterRadius: 20 })
      recordsLayer.addLayers(points.map((p) => L.marker([p.lat, p.lon]).bindPopup(() => popup(p))))
      this.setNote('')
    }

    const placeLayer = L.geoJSON(place, { style: { color: 'red', weight: 3, fillOpacity: 0.1 }, interactive: false })
    if (buffered && buffered !== place) {
      this.overlays.addLayer(L.geoJSON(buffered, { style: { color: 'darkorange', weight: 3, fillOpacity: 0.08 }, interactive: false }))
    }
    this.overlays.addLayer(placeLayer)
    this.overlays.addLayer(recordsLayer)

    const bounds = L.geoJSON(buffered || place).getBounds()
    if (bounds.isValid()) this.map.fitBounds(bounds, { padding: [20, 20] })
  }

  invalidate() {
    this.map.invalidateSize()
  }
}
