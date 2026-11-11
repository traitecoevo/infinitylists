// Species-list logic, ported from R/server.R (intersect_data, stats_text,
// filtered_data, downloadHandler). Pure functions: no DOM, no fetching.

import { containsTest } from './geo.js'

export const IN_TARGET = 'in target'
export const IN_BUFFER = 'only in buffer'

/**
 * Tag each record as inside the target polygon or only inside the buffer,
 * dropping everything else (R: intersect_data).
 * Returns new objects with an `area` field.
 */
export function classify(records, place, buffered) {
  const inPlace = containsTest(place)
  const inBuffer = buffered === place ? null : containsTest(buffered)
  const out = []
  for (const r of records) {
    if (inPlace(r.lon, r.lat)) out.push({ ...r, area: IN_TARGET })
    else if (inBuffer && inBuffer(r.lon, r.lat)) out.push({ ...r, area: IN_BUFFER })
  }
  return out
}

/** Keep records matching a genus/family choice ('All' keeps everything). */
export function filterTaxon(records, level, value) {
  if (!value || value === 'All') return records
  const key = level === 'family' ? 'family' : 'genus'
  return records.filter((r) => r[key] === value)
}

/** Sorted unique genus or family names present in the records. */
export function taxonChoices(records, level) {
  const key = level === 'family' ? 'family' : 'genus'
  const set = new Set()
  for (const r of records) if (r[key] != null) set.add(r[key])
  return [...set].sort((a, b) => a.localeCompare(b))
}

const uniqueCount = (rows, key) => new Set(rows.map((r) => r[key])).size

/** The summary sentence under the map (R: stats_text). */
export function statsText(records) {
  const totalSpecies = uniqueCount(records, 'species')
  const totalGenera = uniqueCount(records, 'genus')
  const totalFamilies = uniqueCount(records, 'family')
  const native = records.filter((r) => r.establishment === 'native')
  const totalNative = native.length > 0 ? uniqueCount(native, 'species') : 'an unknown number'
  const collections = records.filter((r) => r.voucher === 'Collection')
  const photoAudio = records.filter((r) => r.voucher === 'Photograph' || r.voucher === 'Audio')
  return `There have been ${totalSpecies} species observed (${totalGenera} genera; ` +
    `${totalFamilies} families) with ${collections.length} collections of ` +
    `${uniqueCount(collections, 'species')} species and ${photoAudio.length} ` +
    `photographic or audio records of ${uniqueCount(photoAudio, 'species')} species. ` +
    `Of the ${totalSpecies} species observed, ${totalNative} are considered native`
}

/** In-target first, then newest first, undated last (stable). */
function byAreaThenDate(a, b) {
  const at = a.area === IN_TARGET ? 0 : 1
  const bt = b.area === IN_TARGET ? 0 : 1
  if (at !== bt) return at - bt
  if (a.date === b.date) return 0
  if (a.date == null) return 1
  if (b.date == null) return -1
  return b.date - a.date
}

/** True when the Repository value is an iNaturalist URL (R: grepl("https", Repository)). */
export const isINat = (repository) => typeof repository === 'string' && repository.includes('https')

/**
 * One row per species × establishment means × voucher type, showing the most
 * recent record (preferring in-target records) and the record count; buffer-only
 * rows are dropped for species already recorded in the target (R: filtered_data).
 */
export function speciesList(records) {
  const sorted = [...records].sort(byAreaThenDate)
  const groups = new Map()
  for (const r of sorted) {
    const key = `${r.species}\u0000${r.establishment}\u0000${r.voucher}`
    const g = groups.get(key)
    if (g) g.n += 1
    else groups.set(key, { first: r, n: 1 })
  }
  const rows = [...groups.values()].map(({ first: r, n }) => ({
    species: r.species,
    establishment: r.establishment,
    voucher: r.voucher,
    area: r.area,
    n,
    date: r.date,
    lat: r.lat,
    lon: r.lon,
    repository: r.repository,
    link: r.link,
    recordedBy: r.recordedBy,
  }))
  const inTargetSpecies = new Set(rows.filter((r) => r.area === IN_TARGET).map((r) => r.species))
  return rows.filter((r) => !(r.area === IN_BUFFER && inTargetSpecies.has(r.species)))
}

// CSV export ---------------------------------------------------------------

const pad = (n) => String(n).padStart(2, '0')

export function isoDate(ms) {
  if (ms == null) return ''
  const d = new Date(ms)
  return `${d.getUTCFullYear()}-${pad(d.getUTCMonth() + 1)}-${pad(d.getUTCDate())}`
}

function csvCell(v) {
  if (v == null) return ''
  const s = String(v)
  return /[",\n\r]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s
}

/**
 * All records as CSV, with the same columns and sentence-case headers as the
 * Shiny download.
 */
export function toCsv(records) {
  const cols = [
    ['Species', (r) => r.species],
    ['Genus', (r) => r.genus],
    ['Family', (r) => r.family],
    ['Collection date', (r) => isoDate(r.date)],
    ['Lat', (r) => r.lat],
    ['Long', (r) => r.lon],
    ['Voucher type', (r) => r.voucher],
    ['Repository', (r) => r.repository],
    ['Recorded by', (r) => r.recordedBy],
    ['Link', (r) => r.link],
    ['Establishment means', (r) => r.establishment],
    ['In target area', (r) => r.area],
  ]
  const lines = [cols.map(([h]) => csvCell(h)).join(',')]
  for (const r of records) lines.push(cols.map(([, f]) => csvCell(f(r))).join(','))
  return lines.join('\r\n') + '\r\n'
}

/**
 * CSV filename, as in the Shiny app: <buffer>m-buffer-<release file name>.csv,
 * e.g. 0m-buffer-Living-Atlas-Papilionoidea-AU-2025-03-10.csv
 */
export function csvFilename(bufferM, dataset) {
  return `${bufferM}m-buffer-Living-Atlas-${dataset.id}-${dataset.date}.csv`
}
