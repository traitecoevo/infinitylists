# Vendored libraries

Pinned copies, committed so the site has no build step and no runtime CDN
dependency. To upgrade one, replace its folder with the files listed here from
the new npm release and re-test the app.

| Folder | Package | Version | Files taken | License |
|---|---|---|---|---|
| `leaflet/` | leaflet | 1.9.4 | `dist/leaflet.js`, `dist/leaflet.css`, `dist/images/` | BSD-2-Clause |
| `markercluster/` | leaflet.markercluster | 1.5.3 | `dist/leaflet.markercluster.js`, `dist/MarkerCluster*.css` | MIT |
| `turf/` | @turf/turf | 7.2.0 | `turf.min.js` (UMD, exposes global `turf`) | MIT |
| `hyparquet/` | hyparquet | 1.33.0 | `src/*.js` except `node.js` (ES modules, no deps) | MIT |
| `togeojson/` | @tmcw/togeojson | 7.1.2 | `dist/togeojson.es.mjs` renamed to `togeojson.js` | BSD-2-Clause |
