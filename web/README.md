# Infinity Lists web app

A static, browser-only version of the infinitylists Shiny app. All filtering
happens in the browser. Occurrence data comes from small parquet tiles built
by `data-raw/build_tiles.R`. There is no build step: plain HTML, CSS and ES
modules, with pinned libraries in `vendor/` (see `vendor/README.md`).

## Layout of the deployed site

```
app/
  index.html, style.css, js/, vendor/, img/   <- this folder (web/)
  places.geojson                              <- build_tiles.R
  data/datasets.json                          <- build_tiles.R
  data/<dataset>/manifest.json + *.parquet    <- build_tiles.R
```

## Run locally

From the repo root:

```sh
# 1. Get the release data (or reuse a folder you already downloaded)
gh release download --repo traitecoevo/infinitylists --pattern '*.parquet' --dir release-data

# 2. Build tiles + places into site/app (site/ is git-ignored)
Rscript data-raw/build_tiles.R release-data site/app

# 3. Copy the front end next to the data and serve it
cp -R web/. site/app/
python3 -m http.server 8000 --directory site/app
```

Then open <http://localhost:8000>. Opening `index.html` straight from disk
(`file://`) won't work, because browsers block `fetch` for local files. While
editing, re-run the `cp` step (or symlink `site/app/js` → `web/js`) to see
changes.

## Code map

| File | Role |
|---|---|
| `js/app.js` | Wires controls to the pipeline: place → buffer → load tiles → classify → filter → render |
| `js/data.js` | Reads `datasets.json` / manifests and fetches only the tiles that intersect the query bbox (cached) |
| `js/geo.js` | Turf wrappers: circle, buffer, point-in-polygon, KML → polygon |
| `js/summary.js` | Port of `R/server.R`: in-target/buffer classification, stats sentence, species list, CSV |
| `js/table.js` | Small searchable, sortable, paged table |
| `js/map.js` | Leaflet map with clustered markers, target polygon and buffer |
