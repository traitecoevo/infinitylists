// Geometry helpers. Pure functions over GeoJSON; relies on the vendored Turf
// UMD bundle, which defines the global `turf`.

/** Circle polygon with a true ground radius in metres. */
export function circlePolygon(lat, lon, radiusM) {
  return turf.circle([lon, lat], radiusM / 1000, { steps: 96, units: 'kilometers' })
}

/**
 * Polygon expanded by `bufferM` metres. A zero buffer returns the input
 * unchanged, matching the Shiny app (which drew an invisible zero buffer).
 */
export function bufferPolygon(feature, bufferM) {
  if (!bufferM) return feature
  const buffered = turf.buffer(feature, bufferM, { units: 'meters' })
  if (!buffered) throw new Error('Could not buffer this polygon')
  return buffered
}

/** [xmin, ymin, xmax, ymax] of a feature. */
export function bbox(feature) {
  return turf.bbox(feature)
}

/** Grow a bbox by `m` metres on every side (approximate; used for tile selection). */
export function expandBbox([xmin, ymin, xmax, ymax], m) {
  const dLat = m / 111320
  const midLat = ((ymin + ymax) / 2) * Math.PI / 180
  const dLon = m / (111320 * Math.max(Math.cos(midLat), 0.01))
  return [xmin - dLon, ymin - dLat, xmax + dLon, ymax + dLat]
}

export function bboxIntersects(a, b) {
  return a[0] <= b[2] && a[2] >= b[0] && a[1] <= b[3] && a[3] >= b[1]
}

/**
 * Build a fast point-in-polygon test for a Polygon/MultiPolygon feature:
 * bbox reject first, then Turf's exact test (boundary counts as inside, like
 * sf::st_intersects in the Shiny app).
 */
export function containsTest(feature) {
  const [xmin, ymin, xmax, ymax] = turf.bbox(feature)
  return (lon, lat) =>
    lon >= xmin && lon <= xmax && lat >= ymin && lat <= ymax &&
    turf.booleanPointInPolygon([lon, lat], feature)
}

/**
 * Merge all Polygon/MultiPolygon geometries in a FeatureCollection into one
 * MultiPolygon feature (drops Z, ignores points/lines).
 */
export function toSinglePolygonFeature(fc, name = '') {
  const polys = []
  const visit = (g) => {
    if (!g) return
    if (g.type === 'Polygon') polys.push(g.coordinates)
    else if (g.type === 'MultiPolygon') polys.push(...g.coordinates)
    else if (g.type === 'GeometryCollection') g.geometries.forEach(visit)
  }
  for (const f of fc.features || []) visit(f.geometry)
  if (polys.length === 0) return null
  const dropZ = (ring) => ring.map(([x, y]) => [x, y])
  const coords = polys.map((p) => p.map(dropZ))
  const geometry = coords.length === 1
    ? { type: 'Polygon', coordinates: coords[0] }
    : { type: 'MultiPolygon', coordinates: coords }
  return { type: 'Feature', properties: { name }, geometry }
}

/** Parse KML text into a single polygon feature (browser only: needs DOMParser). */
export async function parseKml(text, name) {
  const { kml } = await import('../vendor/togeojson/togeojson.js')
  const dom = new DOMParser().parseFromString(text, 'text/xml')
  if (dom.getElementsByTagName('parsererror').length) {
    throw new Error('This file is not valid KML')
  }
  const feature = toSinglePolygonFeature(kml(dom), name)
  if (!feature) throw new Error('No polygons found in this KML file')
  return feature
}
