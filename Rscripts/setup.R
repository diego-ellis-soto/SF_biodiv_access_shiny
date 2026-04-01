# ============================================================================
# Setup: HuggingFace-optimized data loading
# ============================================================================
# This version uses GDAL virtual file system and temporary downloads
# for efficient loading in cloud/ephemeral environments like HuggingFace Spaces.
# For local development with persistent caching, use setup_local.R instead.

require(shinyjs)
library(shiny)
library(shinydashboard)
library(leaflet)
library(mapboxapi)
library(tidyverse)
library(tidycensus)
library(sf)
library(DT)
library(RColorBrewer)
library(terra)
library(data.table)
library(mapview)
library(sjPlot)
library(sjlabelled)
library(bslib)
library(shinycssloaders)
library(DBI)
library(duckdb)
library(dbplyr)
library(gtfsrouter)
library(tidytransit)
library(fmsb)
library(scales)

# ============================================================================
# API Keys
# ============================================================================
mapbox_token <- Sys.getenv("MAPBOX_TOKEN")

# ============================================================================
# Load Data from HuggingFace
# ============================================================================

# -- Greenspace (read directly from URL via GDAL virtual file system)
osm_greenspace <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/greenspaces_osm_nad83.shp", quiet = TRUE) |>
  st_transform(4326)

if (!"name" %in% names(osm_greenspace)) {
  osm_greenspace$name <- "Unnamed Greenspace"
}

# -- Greenspace Distance Rasters (read directly from URL via GDAL virtual file system)
greenspace_dist_raster <- terra::rast("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/nearest_greenspace_dist.tif")
greenspace_osmid_raster <- terra::rast("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/nearest_greenspace_osmid.tif")

# -- NDVI Raster (read directly from URL via GDAL virtual file system)
ndvi <- terra::rast("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/SF_EastBay_NDVI_Sentinel_10.tif")

# -- GBIF data (loaded via DuckDB parquet in app.R server function)
gbif_parquet <- "https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/gbif_census_ndvi_anno.parquet"

# -- Precomputed CBG data (download to /tmp and load)
download.file(
  'https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/cbg_vect_sf.Rdata',
  '/tmp/cbg_vect_sf.Rdata'
)
load('/tmp/cbg_vect_sf.Rdata')

if (!"unique_species" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$unique_species <- cbg_vect_sf$n_species
}
if (!"n_observations" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$n_observations <- cbg_vect_sf$n
}
if (!"median_inc" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$median_inc <- cbg_vect_sf$medincE
}
if (!"ndvi_mean" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$ndvi_mean <- cbg_vect_sf$ndvi_sentinel
}

# -- Per-CBG Greenspace Coverage Cache
# Precompute greenspace area (m²) per CBG once so that per-isochrone greenspace
# coverage can be resolved via a fast vector join instead of expensive raster
# cellSize() operations (~40–55 s per isochrone).
cbg_gs_cache_path <- "/tmp/cbg_greenspace_coverage.rds"
if (file.exists(cbg_gs_cache_path)) {
  message("Loading per-CBG greenspace coverage from cache...")
  cbg_greenspace_coverage <- readRDS(cbg_gs_cache_path)
} else {
  message("Precomputing per-CBG greenspace coverage (one-time, ~30-60 s)...")
  cbg_greenspace_coverage <- tryCatch({
    cbg_proj  <- st_transform(cbg_vect_sf[, "GEOID"], 3857) |>
      mutate(cbg_area_m2 = as.numeric(st_area(geometry)))
    gs_proj   <- st_transform(osm_greenspace, 3857) |> st_make_valid()
    gs_union  <- st_union(gs_proj)
    cbg_gs_inter <- st_intersection(cbg_proj, gs_union)
    result <- cbg_gs_inter |>
      mutate(greenspace_m2 = as.numeric(st_area(geometry))) |>
      st_drop_geometry() |>
      group_by(GEOID) |>
      summarise(greenspace_m2 = sum(greenspace_m2), .groups = "drop") |>
      right_join(
        cbg_proj |> st_drop_geometry() |> select(GEOID, cbg_area_m2),
        by = "GEOID"
      ) |>
      mutate(greenspace_m2 = tidyr::replace_na(greenspace_m2, 0))
    saveRDS(result, cbg_gs_cache_path)
    message("Per-CBG greenspace coverage saved to cache.")
    result
  }, error = function(e) {
    warning("Per-CBG greenspace precomputation failed: ", e$message)
    NULL
  })
}

# -- Hotspots/Coldspots (read directly from URL via GDAL virtual file system)
biodiv_hotspots <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/hotspots.shp", quiet = TRUE) |>
  st_transform(4326)

biodiv_coldspots <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/coldspots.shp", quiet = TRUE) |>
  st_transform(4326)

# -- RSF Program Projects
rsf_projects <- st_read("data/source/RSF_Program_Projects_polygons.gpkg", quiet = TRUE) |>
  st_transform(4326)

# ============================================================================
# GTFS Data Loading (SF Muni)
# ============================================================================

gtfs_path <- '/Users/diegoellis/Desktop/RSF_next_steps/GPFS_OSM_Transit/sf_muni_gtfs-current/'

# --- 1. Transit stops ---------------------------------------------------------
gtfs_stops_sf <- tryCatch({
  read.csv(file.path(gtfs_path, 'stops.txt')) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
}, error = function(e) {
  warning("GTFS stops failed to load: ", e$message)
  NULL
})

# --- 2. Transit routes as sf LINESTRING (one per shape_id, colored by route) --
gtfs_routes_sf <- tryCatch({
  message("Building GTFS route shapes...")
  gtfs_shapes_raw <- read.csv(file.path(gtfs_path, 'shapes.txt'))
  gtfs_trips_raw  <- read.csv(file.path(gtfs_path, 'trips.txt'))
  gtfs_routes_raw <- read.csv(file.path(gtfs_path, 'routes.txt'))

  shape_route_map <- gtfs_trips_raw |>
    distinct(shape_id, route_id)

  route_meta <- gtfs_routes_raw |>
    select(route_id, route_short_name, route_long_name, route_color) |>
    mutate(route_color_hex = paste0("#", trimws(route_color)))

  shapes_split <- gtfs_shapes_raw |>
    arrange(shape_id, shape_pt_sequence) |>
    group_by(shape_id) |>
    group_split()

  shape_geoms <- lapply(shapes_split, function(s) {
    st_linestring(cbind(s$shape_pt_lon, s$shape_pt_lat))
  })

  st_sf(
    shape_id = sapply(shapes_split, function(s) s$shape_id[1]),
    geometry = st_sfc(shape_geoms, crs = 4326)
  ) |>
    left_join(shape_route_map, by = "shape_id") |>
    left_join(route_meta, by = "route_id")
}, error = function(e) {
  warning("GTFS route shapes failed to load: ", e$message)
  NULL
})

# --- 3. gtfsrouter: load GTFS + preprocess timetable for routing --------------
gtfs_router <- tryCatch({
  gtfs_zip_path <- tempfile(fileext = ".zip")
  old_wd <- getwd()
  setwd(gtfs_path)
  utils::zip(gtfs_zip_path, files = list.files('.', pattern = "\\.txt$"))
  setwd(old_wd)

  gr <- gtfsrouter::extract_gtfs(gtfs_zip_path)
  gtfsrouter::gtfs_timetable(gr, day = "Monday")
}, error = function(e) {
  warning("gtfsrouter failed to initialise: ", e$message)
  NULL
})

# --- 4. Pre-computed transit isochrone cache ----------------------------------
transit_iso_cache <- tryCatch({
  cache_path <- 'data/transit_iso_cache.rds'
  if (file.exists(cache_path)) {
    message("Loading pre-computed transit isochrone cache...")
    readRDS(cache_path)
  } else {
    NULL
  }
}, error = function(e) { warning(e$message); NULL })

# --- 5. Stop headways via tidytransit (AM peak 7-9am) -------------------------
gtfs_stop_headways <- tryCatch({
  message("Computing stop service frequencies (AM peak 7-9am)...")
  gt <- tidytransit::read_gtfs(gtfs_path)
  tidytransit::get_stop_frequency(gt, start_time = 7 * 3600, end_time = 9 * 3600) |>
    group_by(stop_id) |>
    summarise(
      mean_headway_min  = mean(mean_headway, na.rm = TRUE) / 60,
      n_departures_peak = sum(n_departures, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(stop_id = as.character(stop_id))
}, error = function(e) {
  warning("tidytransit headway computation failed: ", e$message)
  NULL
})

if (!is.null(gtfs_stop_headways) && !is.null(gtfs_stops_sf)) {
  gtfs_stops_sf <- gtfs_stops_sf |>
    mutate(stop_id = as.character(stop_id)) |>
    left_join(gtfs_stop_headways, by = "stop_id")
}

# ============================================================================
# CalEnviroScreen 4.0
# ============================================================================
calenviro_path <- '/Users/diegoellis/Downloads/calenviroscreen40gdb_F_2021.gdb'
if (!file.exists(calenviro_path)) {
  calenviro_path <- '/Users/diegoellis/Desktop/Projects/Presentations/Data_Schell_Lab_Tutorial/calenviroscreen40gdb_F_2021.gdb'
}

cenv_sf <- tryCatch({
  message("Loading CalEnviroScreen...")
  sf::st_read(calenviro_path, quiet = TRUE) |>
    dplyr::filter(grepl("san francisco", County, ignore.case = TRUE), !is.na(CIscore)) |>
    dplyr::select(
      Tract, CIscore, CIscoreP,
      PM2_5, PM2_5_Pctl, Traffic, Traffic_Pctl,
      Poverty, Poverty_Pctl, HousBurd, HousBurd_Pctl,
      County
    ) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
}, error = function(e) {
  warning("CalEnviroScreen failed to load: ", e$message)
  NULL
})

# ============================================================================
# SF Environmental Justice Communities
# ============================================================================
sf_ej_path <- '/Users/diegoellis/Downloads/San Francisco Environmental Justice Communities Map_20251217/geo_export_a21b0a0a-7306-46fd-8381-06581cdbe6e9.shp'

sf_ej_sf <- tryCatch({
  message("Loading SF EJ Communities layer...")
  sf::st_read(sf_ej_path, quiet = TRUE) |>
    dplyr::mutate(
      symbol_hex = stringr::str_split(symbol_rgb, ",\\s*") |>
        lapply(function(x) sprintf("#%02X%02X%02X",
                                   as.integer(x[1]), as.integer(x[2]), as.integer(x[3]))) |>
        unlist(),
      ej_label = dplyr::case_when(
        is.na(score) ~ "Not EJ",
        score >= 21  ~ "High EJ burden (21-30)",
        score >= 11  ~ "Moderate EJ burden (11-20)",
        score >= 1   ~ "Low EJ burden (1-10)",
        score == 0   ~ "Score 0",
        TRUE         ~ "Unknown"
      )
    ) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
}, error = function(e) {
  warning("SF EJ layer failed to load: ", e$message)
  NULL
})
