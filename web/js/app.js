// Infinity Lists web app: wires the controls to data loading, the species
// list, the table and the map.

import { loadDatasets, loadRecords } from './data.js'
import { bbox, bufferPolygon, circlePolygon, expandBbox, parseKml } from './geo.js'
import {
  classify, csvFilename, filterTaxon, isINat, speciesList, statsText, taxonChoices, toCsv,
} from './summary.js'
import { DataTable, esc } from './table.js'
import { RecordMap } from './map.js'

const $ = (sel) => document.querySelector(sel)

const RADII = [['100m', 100], ['500m', 500], ['1km', 1000], ['2km', 2000],
  ['5km', 5000], ['10km', 10000], ['50km', 50000]]
const DEFAULT_PLACE = 'Heron Island, Australia'

const state = {
  mode: 'preloaded',
  places: new Map(),   // name -> GeoJSON feature
  datasets: [],        // from datasets.json
  uploaded: null,      // {feature, name}
  chosen: null,        // {lat, lon, radius} set when "Go" is pressed (lat/long mode)
  located: null,       // {lat, lon, radius} set when "Go" is pressed (current location)
  current: null,       // {place, buffered, classified, desc} for the active query
  token: 0,            // guards against out-of-order async results
}

const map = new RecordMap($('#map'))
const table = new DataTable($('#table'), { pageSize: 25 })

// Formatting ---------------------------------------------------------------

const dateFmt = new Intl.DateTimeFormat('en-AU', { day: 'numeric', month: 'short', year: 'numeric', timeZone: 'UTC' })
const fmtDate = (ms) => (ms == null ? '' : dateFmt.format(new Date(ms)))

function repositoryLink(r) {
  const label = isINat(r.repository) ? 'iNat' : (r.repository ?? '')
  return /^https?:\/\//i.test(r.link || '')
    ? `<a href="${esc(r.link)}" target="_blank" rel="noopener">${esc(label)}</a>`
    : esc(label)
}

/** Marker popup: species, voucher, date, recorder and a link to the record. */
function popupHtml(r) {
  const lines = [
    `<strong><em>${esc(r.species)}</em></strong>`,
    esc([r.voucher, fmtDate(r.date)].filter(Boolean).join(' · ')),
  ]
  if (r.n > 1) lines.push(`${r.n.toLocaleString()} records; most recent shown`)
  if (r.recordedBy) lines.push(`Recorded by ${esc(r.recordedBy)}`)
  if (/^https?:\/\//i.test(r.link || '')) {
    const where = isINat(r.repository) ? 'iNaturalist'
      : r.link.includes('gbif.org') ? 'GBIF'
      : r.link.includes('ala.org.au') ? 'ALA' : 'source'
    const repo = !isINat(r.repository) && r.repository ? ` (${esc(r.repository)})` : ''
    lines.push(`<a href="${esc(r.link)}" target="_blank" rel="noopener">View record on ${where}</a>${repo}`)
  }
  return `<div class="popup">${lines.join('<br>')}</div>`
}

const LIST_COLUMNS = [
  { key: 'species', label: 'Species' },
  { key: 'establishment', label: 'Establishment means' },
  { key: 'voucher', label: 'Voucher type' },
  { key: 'area', label: 'In target area' },
  { key: 'n', label: 'N', type: 'num' },
  { key: 'date', label: 'Most recent obs.', type: 'date', text: (r) => fmtDate(r.date) },
  { key: 'lat', label: 'Lat', type: 'num' },
  { key: 'lon', label: 'Long', type: 'num' },
  { key: 'repository', label: 'Repository', html: repositoryLink },
  { key: 'recordedBy', label: 'Recorded by' },
]

const RECORD_COLUMNS = [
  { key: 'species', label: 'Species' },
  { key: 'genus', label: 'Genus' },
  { key: 'family', label: 'Family' },
  { key: 'establishment', label: 'Establishment means' },
  { key: 'voucher', label: 'Voucher type' },
  { key: 'area', label: 'In target area' },
  { key: 'date', label: 'Date', type: 'date', text: (r) => fmtDate(r.date) },
  { key: 'lat', label: 'Lat', type: 'num' },
  { key: 'lon', label: 'Long', type: 'num' },
  { key: 'repository', label: 'Repository', html: repositoryLink },
  { key: 'recordedBy', label: 'Recorded by' },
]

// Status -------------------------------------------------------------------

let busyCount = 0
function busy(on) {
  busyCount = Math.max(0, busyCount + (on ? 1 : -1))
  $('#busy').hidden = busyCount === 0
}

function setStatus(msg, isError = false) {
  const el = $('#status')
  el.textContent = msg
  el.classList.toggle('error', isError)
}

// Controls -----------------------------------------------------------------

const val = (sel) => $(sel).value
const radioVal = (name) => document.querySelector(`input[name="${name}"]:checked`).value
const bufferM = () => Number(val('#buffer'))
const level = () => radioVal('taxonOfInterest')
const datasetById = (id) => state.datasets.find((d) => d.id === id)

function fillSelect(sel, options, selected) {
  const el = $(sel)
  el.replaceChildren(...options.map(([label, value]) => {
    const o = document.createElement('option')
    o.textContent = label
    o.value = String(value)
    if (String(value) === String(selected)) o.selected = true
    return o
  }))
}

function fillTaxa(records) {
  const lv = level()
  $('#taxonLabel').textContent = lv === 'family' ? 'Choose a family:' : 'Choose a genus:'
  fillSelect('#taxon', [['All', 'All'], ...taxonChoices(records, lv).map((t) => [t, t])], 'All')
}

/** The active target area, or null with a hint about what the user must do. */
function selectedPlace() {
  switch (state.mode) {
    case 'preloaded': {
      const name = val('#place')
      const feature = state.places.get(name)
      return feature ? { feature, desc: { kind: 'preloaded', name } } : { hint: 'Choose a place' }
    }
    case 'upload':
      return state.uploaded
        ? { feature: state.uploaded.feature, desc: { kind: 'upload', name: state.uploaded.name } }
        : { hint: 'Upload a KML file to build a list for its area.' }
    case 'choose': {
      const c = state.chosen
      return c
        ? { feature: circlePolygon(c.lat, c.lon, c.radius), desc: { kind: 'circle', ...c } }
        : { hint: 'Enter a latitude and longitude, choose a radius and press Go.' }
    }
    case 'current': {
      const c = state.located
      return c
        ? { feature: circlePolygon(c.lat, c.lon, c.radius), desc: { kind: 'current', ...c } }
        : { hint: 'Choose a radius and press Go to use your current location.' }
    }
  }
  return { hint: '' }
}

// Main pipeline ------------------------------------------------------------

/** Empty the map, table and stats, showing `msg` instead. */
function clearOutputs(msg, isError = false) {
  state.token++ // drop any query still in flight
  state.current = null
  map.update(null, null, [])
  table.setData(LIST_COLUMNS, [], { key: 'n', dir: 'desc' })
  $('#stats').textContent = ''
  $('#download').disabled = true
  fillTaxa([])
  setStatus(msg, isError)
}

/** Re-query when the place, buffer or dataset changes. */
async function refreshPlace() {
  const sel = selectedPlace()
  if (!sel.feature) {
    clearOutputs(sel.hint)
    return
  }
  const token = ++state.token

  busy(true)
  try {
    const buffered = bufferPolygon(sel.feature, bufferM())
    const box = expandBbox(bbox(buffered), 50)
    const records = await loadRecords(val('#dataset'), box, ({ tiles, bytes, cached }) => {
      if (token === state.token && cached < tiles) {
        setStatus(`Loading ${tiles} data tile${tiles === 1 ? '' : 's'} (${(bytes / 1e6).toFixed(1)} MB)…`)
      }
    })
    if (token !== state.token) return // a newer query superseded this one
    const classified = classify(records, sel.feature, buffered)
    state.current = { place: sel.feature, buffered, classified, desc: sel.desc }
    fillTaxa(classified)
    setStatus('')
    render()
  } catch (err) {
    if (token !== state.token) return
    console.error(err)
    const hint = location.protocol === 'file:' ? ' Serve the site over http (see web/README.md).' : ''
    setStatus(`Something went wrong: ${err.message}.${hint}`, true)
  } finally {
    busy(false)
  }
}

/** Re-render outputs for the current query (taxon filter / view mode changes). */
function render() {
  const cur = state.current
  if (!cur) return
  const records = filterTaxon(cur.classified, level(), val('#taxon'))
  $('#stats').textContent = statsText(records)
  $('#download').disabled = false

  if ($('#allRecords').checked) {
    table.setData(RECORD_COLUMNS, records, { key: 'date', dir: 'desc' })
    map.update(cur.place, cur.buffered, records, popupHtml)
  } else {
    const list = speciesList(records)
    table.setData(LIST_COLUMNS, list, { key: 'n', dir: 'desc' })
    map.update(cur.place, cur.buffered, list, popupHtml)
  }
}

function download() {
  const cur = state.current
  if (!cur) return
  const records = filterTaxon(cur.classified, level(), val('#taxon'))
  const name = csvFilename(cur.desc, bufferM(), datasetById(val('#dataset')),
    { level: level(), value: val('#taxon') })
  const url = URL.createObjectURL(new Blob([toCsv(records)], { type: 'text/csv;charset=utf-8' }))
  const a = Object.assign(document.createElement('a'), { href: url, download: name })
  document.body.append(a)
  a.click()
  a.remove()
  setTimeout(() => URL.revokeObjectURL(url), 1000)
}

// Event wiring -------------------------------------------------------------

function setMode(mode) {
  state.mode = mode
  document.querySelectorAll('.panel').forEach((p) => { p.hidden = p.dataset.mode !== mode })
  refreshPlace()
}

function useCurrentLocation() {
  if (!navigator.geolocation) {
    setStatus('Your browser does not support location access.', true)
    return
  }
  busy(true)
  setStatus('Finding your location…')
  navigator.geolocation.getCurrentPosition((pos) => {
    busy(false)
    const { latitude, longitude, accuracy } = pos.coords
    $('#geoLat').textContent = latitude
    $('#geoLon').textContent = longitude
    $('#geoAcc').textContent = accuracy
    state.located = { lat: latitude, lon: longitude, radius: Number(val('#radiusCurrent')) }
    refreshPlace()
  }, (err) => {
    busy(false)
    setStatus(`Could not get your location (${err.message}). Allow location access for this site and try again.`, true)
  }, { enableHighAccuracy: true, timeout: 15000, maximumAge: 0 })
}

function useChosenPoint() {
  const lat = Number(val('#latitude'))
  const lon = Number(val('#longitude'))
  if (val('#latitude') === '' || val('#longitude') === '' || !Number.isFinite(lat) || !Number.isFinite(lon) ||
      Math.abs(lat) > 90 || Math.abs(lon) > 180) {
    setStatus('Enter a latitude between -90 and 90 and a longitude between -180 and 180.', true)
    return
  }
  state.chosen = { lat, lon, radius: Number(val('#radiusChoose')) }
  refreshPlace()
}

async function onUpload(e) {
  const file = e.target.files[0]
  if (!file) return
  try {
    const feature = await parseKml(await file.text(), file.name)
    state.uploaded = { feature, name: file.name }
    refreshPlace()
  } catch (err) {
    state.uploaded = null
    clearOutputs(`Could not read ${file.name}: ${err.message}`, true)
  }
}

function selectTab(btn) {
  document.querySelectorAll('.tabs [role="tab"]').forEach((b) => {
    const on = b === btn
    b.setAttribute('aria-selected', String(on))
    $('#' + b.getAttribute('aria-controls')).hidden = !on
  })
  if (btn.id === 't-map') map.invalidate()
}

function wire() {
  document.querySelectorAll('input[name="inputType"]').forEach((r) =>
    r.addEventListener('change', () => setMode(r.value)))
  $('#place').addEventListener('change', refreshPlace)
  $('#uploadKML').addEventListener('change', onUpload)
  $('#goChoose').addEventListener('click', useChosenPoint)
  $('#goCurrent').addEventListener('click', useCurrentLocation)
  for (const id of ['#latitude', '#longitude']) {
    $(id).addEventListener('keydown', (e) => { if (e.key === 'Enter') useChosenPoint() })
  }
  $('#buffer').addEventListener('change', refreshPlace)
  $('#dataset').addEventListener('change', refreshPlace)
  document.querySelectorAll('input[name="taxonOfInterest"]').forEach((r) =>
    r.addEventListener('change', () => { if (state.current) fillTaxa(state.current.classified); render() }))
  $('#taxon').addEventListener('change', render)
  $('#allRecords').addEventListener('change', render)
  $('#download').addEventListener('click', download)
  document.querySelectorAll('.tabs [role="tab"]').forEach((b) =>
    b.addEventListener('click', () => selectTab(b)))
}

// Start-up -----------------------------------------------------------------

async function init() {
  wire()
  for (const sel of ['#radiusCurrent', '#radiusChoose']) fillSelect(sel, RADII, 100)
  busy(true)
  setStatus('Loading…')
  try {
    const [index, places] = await Promise.all([
      loadDatasets(),
      fetch('places.geojson').then((r) => {
        if (!r.ok) throw new Error(`Could not load places (${r.status})`)
        return r.json()
      }),
    ])
    state.datasets = index.datasets
    fillSelect('#dataset', index.datasets.map((d) => [d.label, d.id]), index.default)
    for (const f of places.features) {
      state.places.set(f.properties.name, { type: 'Feature', properties: {}, geometry: f.geometry })
    }
    fillSelect('#place', [...state.places.keys()].map((n) => [n, n]), DEFAULT_PLACE)
  } catch (err) {
    console.error(err)
    const hint = location.protocol === 'file:' ? ' Serve the site over http (see web/README.md).' : ''
    setStatus(`Could not start the app: ${err.message}.${hint}`, true)
    return
  } finally {
    busy(false)
  }
  refreshPlace()
}

init()
