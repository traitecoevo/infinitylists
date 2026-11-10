// Fast point layer for large record sets (tens to hundreds of thousands).
//
// Individual Leaflet markers get slow beyond ~20k points, so this draws plain
// dots onto canvas map tiles instead. A uniform grid index keeps each tile's
// drawing and each click lookup proportional to the points nearby rather than
// to the whole set. Relies on the vendored Leaflet (global `L`).

const MAX_RADIUS = 4       // px, used when zoomed in
const CLICK_RADIUS = 8     // px: how close a click must be to a dot
const GRID = 256           // index cells per side

/** Smaller dots when zoomed out, so dense areas show as density, not a blob. */
const dotRadius = (z) => (z <= 9 ? 1.5 : z <= 11 ? 2 : z <= 13 ? 3 : MAX_RADIUS)

const COLOURS = {
  target: 'rgba(39, 128, 227, 0.75)', // blue, like the pins
  buffer: 'rgba(255, 140, 0, 0.75)',  // dark orange, like the buffer outline
}

/** Uniform lon/lat grid over the points' bounding box. */
class GridIndex {
  constructor(points) {
    this.points = points
    let xmin = Infinity, ymin = Infinity, xmax = -Infinity, ymax = -Infinity
    for (const p of points) {
      if (p.lon < xmin) xmin = p.lon
      if (p.lon > xmax) xmax = p.lon
      if (p.lat < ymin) ymin = p.lat
      if (p.lat > ymax) ymax = p.lat
    }
    this.xmin = xmin; this.ymin = ymin
    this.dx = (xmax - xmin) / GRID || 1e-9
    this.dy = (ymax - ymin) / GRID || 1e-9
    this.cells = new Map()
    points.forEach((p, i) => {
      const key = this.cellOf(p.lon, p.lat).join(',')
      const cell = this.cells.get(key)
      if (cell) cell.push(i)
      else this.cells.set(key, [i])
    })
  }

  cellOf(lon, lat) {
    const cx = Math.min(GRID - 1, Math.max(0, Math.floor((lon - this.xmin) / this.dx)))
    const cy = Math.min(GRID - 1, Math.max(0, Math.floor((lat - this.ymin) / this.dy)))
    return [cx, cy]
  }

  /** Indices of points inside [west, south, east, north]. */
  query(west, south, east, north) {
    const [x0, y0] = this.cellOf(west, south)
    const [x1, y1] = this.cellOf(east, north)
    const out = []
    for (let cx = x0; cx <= x1; cx++) {
      for (let cy = y0; cy <= y1; cy++) {
        const cell = this.cells.get(`${cx},${cy}`)
        if (!cell) continue
        for (const i of cell) {
          const p = this.points[i]
          if (p.lon >= west && p.lon <= east && p.lat >= south && p.lat <= north) out.push(i)
        }
      }
    }
    return out
  }
}

/**
 * @param {any[]} points  records with lat, lon and optional area ('in target'/'only in buffer')
 * @param {(p: any) => string} popupHtml
 */
export function pointsLayer(points, popupHtml) {
  const index = new GridIndex(points)

  const Layer = L.GridLayer.extend({
    createTile(coords) {
      const size = this.getTileSize()
      const ratio = window.devicePixelRatio || 1
      const canvas = document.createElement('canvas')
      canvas.width = size.x * ratio
      canvas.height = size.y * ratio
      const ctx = canvas.getContext('2d')
      ctx.scale(ratio, ratio)

      const map = this._map
      const z = coords.z
      const r = dotRadius(z)
      const origin = coords.scaleBy(size)
      // Include dots whose centre is just outside the tile but overlap it.
      const pad = L.point(r + 1, r + 1)
      const nw = map.unproject(origin.subtract(pad), z)
      const se = map.unproject(origin.add(size).add(pad), z)
      const hits = index.query(nw.lng, se.lat, se.lng, nw.lat)

      // Thin dark outline only once dots are big enough to be picked out.
      const outline = r >= 3
      ctx.lineWidth = 0.75
      ctx.strokeStyle = 'rgba(0, 0, 0, 0.45)'
      for (const inTarget of [false, true]) { // draw target dots on top
        ctx.fillStyle = inTarget ? COLOURS.target : COLOURS.buffer
        for (const i of hits) {
          const p = points[i]
          if ((p.area !== 'only in buffer') !== inTarget) continue
          const px = map.project([p.lat, p.lon], z).subtract(origin)
          // One path per dot so overlapping translucent dots build up colour.
          ctx.beginPath()
          ctx.arc(px.x, px.y, r, 0, 2 * Math.PI)
          ctx.fill()
          if (outline) ctx.stroke()
        }
      }
      return canvas
    },
  })

  const layer = new Layer({ pane: 'markerPane', updateWhenZooming: false, keepBuffer: 4 })

  // Clicks: find the nearest dot within CLICK_RADIUS pixels.
  function onClick(e) {
    const map = layer._map
    const z = map.getZoom()
    const at = map.project(e.latlng, z)
    const r = L.point(CLICK_RADIUS, CLICK_RADIUS)
    const nw = map.unproject(at.subtract(r), z)
    const se = map.unproject(at.add(r), z)
    let best = -1, bestD = Infinity
    for (const i of index.query(nw.lng, se.lat, se.lng, nw.lat)) {
      const d = map.project([points[i].lat, points[i].lon], z).distanceTo(at)
      if (d < bestD) { bestD = d; best = i }
    }
    if (best >= 0 && bestD <= CLICK_RADIUS) {
      const p = points[best]
      L.popup().setLatLng([p.lat, p.lon]).setContent(popupHtml(p)).openOn(map)
    }
  }
  // Keep our own handle: Leaflet may clear layer._map before 'remove' fires.
  let attached = null
  layer.on('add', () => { attached = layer._map; attached.on('click', onClick) })
  layer.on('remove', () => { if (attached) attached.off('click', onClick); attached = null })

  return layer
}
