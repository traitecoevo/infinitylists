# Infinity List Server Function

Initializes and sets up the server-side logic for the Infinity List
Shiny application. This function manages the user interactions, data
processing, and rendering of both UI elements and outputs (tables, maps,
stats). It handles file uploads, data filtering, and interaction with
map elements.

## Usage

``` r
infinity_server(...)
```

## Arguments

- ...:

  Additional arguments that might be passed to server functions. These
  could be passed from a Shiny UI function or another server function.

## Value

This function sets up and returns a server function for the Shiny app.
It does not have a direct return value, but rather, it sets up reactive
outputs, observers, and expressions that the Shiny app will utilize.

## Details

The server function handles several main tasks:

- Observes and processes KML file uploads.

- Reactively creates polygons based on user inputs.

- Filters and processes ALA data based on user-selected polygons.

- Computes and displays statistics about observed species.

- Updates select input choices based on the available data.

- Renders and displays a data table with specific details.

- Provides a CSV download handler for the displayed data.

- Renders a Leaflet map with markers, polygons, and buffers.

Key internal functions include:

- `load_place()`: For processing KML files.

- `create_circle_polygon()`: To create polygons from user inputs.

- `points_in_target()`: To determine if points lie within a target
  polygon.

- `points_in_buffer()`: To determine if points lie within a buffer
  around a polygon.

- `filter_by_taxon()`: Filters the ALA data by selected taxa.

- `add_buffer()`: Adds a buffer around a given polygon.

## See also

[`infinitylistApp`](https://traitecoevo.github.io/infinitylists/reference/infinitylistApp.md):
Main function that launches the Infinity List Shiny app.
