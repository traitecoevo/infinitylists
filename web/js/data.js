// Loading occurrence data from the static tiles built by data-raw/build_tiles.R.

import { parquetReadObjects } from '../vendor/hyparquet/index.js'
import { bboxIntersects } from './geo.js'

const BASE = 'data/'
const manifests = new Map() // dataset id -> Promise<manifest>
const tiles = new Map()     // "id/file" -> Promise<record[]>

export async function loadDatasets() {
  const res = await fetch(BASE + 'datasets.json')
  if (!res.ok) throw new Error(`Could not load dataset list (${res.status})`)
  return res.json()
}

function loadManifest(id) {
  if (!manifests.has(id)) {
    const p = fetch(`${BASE}${encodeURIComponent(id)}/manifest.json`).then((res) => {
      if (!res.ok) throw new Error(`Could not load ${id} manifest (${res.status})`)
      return res.json()
    })
    p.catch(() => manifests.delete(id))
    manifests.set(id, p)
  }
  return manifests.get(id)
}

/** Convert a parquet row to the compact record the app works with. */
function toRecord(r) {
  const d = r['Collection Date']
  return {
    species: r.Species,
    genus: r.Genus,
    family: r.Family,
    date: d instanceof Date && !isNaN(d) ? d.getTime() : null,
    lat: r.Lat,
    lon: r.Long,
    voucher: r['Voucher Type'],
    // The tiles store "" when Repository equals Link (see build_tiles.R).
    repository: r.Repository === '' ? r.Link : r.Repository,
    recordedBy: r['Recorded by'],
    link: r.Link,
    establishment: r['Establishment Means'],
  }
}

function loadTile(id, file) {
  const key = `${id}/${file}`
  if (!tiles.has(key)) {
    const p = (async () => {
      const res = await fetch(`${BASE}${encodeURIComponent(id)}/${file}`)
      if (!res.ok) throw new Error(`Could not load data tile ${file} (${res.status})`)
      const buf = await res.arrayBuffer()
      const rows = await parquetReadObjects({ file: buf })
      return rows.map(toRecord)
    })()
    p.catch(() => tiles.delete(key))
    tiles.set(key, p)
  }
  return tiles.get(key)
}

/**
 * All records of dataset `id` inside `box` ([xmin, ymin, xmax, ymax]).
 * Only tiles whose bbox intersects `box` are fetched; tiles are cached, so
 * changing taxon or buffer within the same area costs nothing.
 *
 * @param {(info: {tiles: number, bytes: number, cached: number}) => void} [onPlan]
 */
export async function loadRecords(id, box, onPlan) {
  const manifest = await loadManifest(id)
  const needed = manifest.tiles.filter((t) =>
    bboxIntersects([t.xmin, t.ymin, t.xmax, t.ymax], box))
  if (onPlan) {
    const cached = needed.filter((t) => tiles.has(`${id}/${t.file}`)).length
    onPlan({ tiles: needed.length, bytes: needed.reduce((s, t) => s + t.bytes, 0), cached })
  }
  const parts = await Promise.all(needed.map((t) => loadTile(id, t.file)))
  const [xmin, ymin, xmax, ymax] = box
  const out = []
  for (const part of parts) {
    for (const r of part) {
      if (r.lon >= xmin && r.lon <= xmax && r.lat >= ymin && r.lat <= ymax) out.push(r)
    }
  }
  return out
}
