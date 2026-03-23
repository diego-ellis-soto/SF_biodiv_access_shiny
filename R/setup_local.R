# setup
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

# ------------------------------------------------
# 1) API Keys
# ------------------------------------------------
mapbox_token <- Sys.getenv("MAPBOX_TOKEN")
# mb_access_token(mapbox_token, install = FALSE)

# ============================================================================
# Setup: Cache directory and download helper
# ============================================================================
cache_dir <- "data/cached"
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

download_if_missing <- function(url, destfile, name = basename(destfile)) {
  if (!file.exists(destfile)) {
    print(paste("Downloading", name, "from HuggingFace..."))
    download.file(url, destfile, mode = "wb")
  } else {
    print(paste("Loading", name, "from cache"))
  }
}

# ============================================================================
# Load Data
# ============================================================================

# -- Greenspace
greenspace_shp <- file.path(cache_dir, "greenspaces_osm_nad83.shp")
if (!file.exists(greenspace_shp)) {
  print("Downloading greenspaces_osm_nad83.shp from HuggingFace...")
  st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/greenspaces_osm_nad83.shp", quiet = TRUE) |>
    st_write(greenspace_shp, quiet = TRUE)
} else {
  print("Loading greenspaces_osm_nad83.shp from cache")
}
osm_greenspace <- st_read(greenspace_shp, quiet = TRUE) |>
  st_transform(4326)
if (!"name" %in% names(osm_greenspace)) {
  osm_greenspace$name <- "Unnamed Greenspace"
}

# -- Greenspace Distance Rasters
print("Loading greenspace distance rasters from data/output/")
greenspace_dist_raster <- terra::rast("data/output/nearest_greenspace_dist.tif")
greenspace_osmid_raster <- terra::rast("data/output/nearest_greenspace_osmid.tif")

# -- NDVI Raster
ndvi_file <- file.path(cache_dir, "SF_EastBay_NDVI_Sentinel_10.tif")
download_if_missing(
  "https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/SF_EastBay_NDVI_Sentinel_10.tif",
  ndvi_file,
  "NDVI raster"
)
ndvi <- terra::rast(ndvi_file)



# # -- GBIF data
# # Load what is basically inter_gbif !!!!! 
# # load("data/sf_gbif.Rdata")  # => sf_gbif
# gbif_file <- file.path(cache_dir, "gbif_census_ndvi_anno.Rdata")
# download_if_missing(
#   "https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/gbif_census_ndvi_anno.Rdata",
#   gbif_file,
#   "GBIF data"
# )
# load(gbif_file)
# vect_gbif <- vect(sf_gbif)

# -- GBIF data (now loaded via DuckDB in app.R server function)
# Verify parquet file exists
gbif_parquet <- "data/output/gbif_census_ndvi_anno.parquet"
if (!file.exists(gbif_parquet)) {
  stop("GBIF parquet file not found. Please run: source('R/convert_gbif_to_parquet.R')")
}
print("GBIF parquet file verified")

# -- Precomputed CBG data
cbg_file <- file.path(cache_dir, "cbg_vect_sf.Rdata")
download_if_missing(
  "https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/cbg_vect_sf.Rdata",
  cbg_file,
  "CBG data"
)
load(cbg_file)

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
cbg_gs_cache_path <- file.path("data/cache", "cbg_greenspace_coverage.rds")
if (file.exists(cbg_gs_cache_path)) {
  print("Loading per-CBG greenspace coverage from cache...")
  cbg_greenspace_coverage <- readRDS(cbg_gs_cache_path)
} else {
  print("Precomputing per-CBG greenspace coverage (one-time, ~30-60 s)...")
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
    if (!dir.exists("data/cache")) dir.create("data/cache", recursive = TRUE)
    saveRDS(result, cbg_gs_cache_path)
    print("Per-CBG greenspace coverage saved to cache.")
    result
  }, error = function(e) {
    warning("Per-CBG greenspace precomputation failed: ", e$message)
    NULL
  })
}

# -- Hotspots/Coldspots
hotspots_shp <- file.path(cache_dir, "hotspots.shp")
if (!file.exists(hotspots_shp)) {
  print("Downloading hotspots.shp from HuggingFace...")
  st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/hotspots.shp", quiet = TRUE) |>
    st_write(hotspots_shp, quiet = TRUE)
} else {
  print("Loading hotspots.shp from cache")
}
biodiv_hotspots <- st_read(hotspots_shp, quiet = TRUE) |> st_transform(4326)

coldspots_shp <- file.path(cache_dir, "coldspots.shp")
if (!file.exists(coldspots_shp)) {
  print("Downloading coldspots.shp from HuggingFace...")
  st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/coldspots.shp", quiet = TRUE) |>
    st_write(coldspots_shp, quiet = TRUE)
} else {
  print("Loading coldspots.shp from cache")
}
biodiv_coldspots <- st_read(coldspots_shp, quiet = TRUE) |> st_transform(4326)

# -- RSF Program Projects
print("Loading RSF Program Projects from data/source/")
rsf_projects <- st_read("data/source/RSF_Program_Projects_polygons.gpkg", quiet = TRUE) |> 
  st_transform(4326)

# ============================================================================
# GTFS Data Loading (SF Muni)
# ============================================================================

gtfs_path <- '/Users/diegoellis/Desktop/RSF_next_steps/GPFS_OSM_Transit/sf_muni_gtfs-current/'

# --- Transit stops -----------------------------------------------------------
gtfs_stops_sf <- tryCatch({
  read.csv(file.path(gtfs_path, 'stops.txt')) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
}, error = function(e) {
  warning("GTFS stops failed to load: ", e$message); NULL
})

# --- Route shapes ------------------------------------------------------------
gtfs_routes_sf <- tryCatch({
  message("Building GTFS route shapes...")
  gtfs_shapes_raw <- read.csv(file.path(gtfs_path, 'shapes.txt'))
  gtfs_trips_raw  <- read.csv(file.path(gtfs_path, 'trips.txt'))
  gtfs_routes_raw <- read.csv(file.path(gtfs_path, 'routes.txt'))

  shape_route_map <- gtfs_trips_raw |> distinct(shape_id, route_id)
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
  warning("GTFS route shapes failed to load: ", e$message); NULL
})

# --- GTFS zip (persistent path, shared by gtfsrouter + tidytransit) ----------
gtfs_zip_path <- "data/cache/sf_muni_gtfs.zip"
if (!file.exists(gtfs_zip_path) && dir.exists(gtfs_path)) {
  message("Zipping GTFS feed for caching...")
  if (!dir.exists("data/cache")) dir.create("data/cache", recursive = TRUE)
  old_wd <- getwd()
  setwd(gtfs_path)
  utils::zip(gtfs_zip_path, files = list.files(".", pattern = "\\.txt$"))
  setwd(old_wd)
  message("GTFS zip cached at ", gtfs_zip_path)
}

# --- gtfsrouter timetable (use pre-computed cache if available) --------------
gtfs_router <- tryCatch({
  cache_file <- "data/cache/gtfs_timetable_monday.rds"
  if (file.exists(cache_file)) {
    message("Loading pre-computed GTFS timetable from cache...")
    readRDS(cache_file)
  } else if (file.exists(gtfs_zip_path)) {
    message("Pre-computing GTFS timetable...")
    gr <- gtfsrouter::extract_gtfs(gtfs_zip_path)
    gtfsrouter::gtfs_timetable(gr, day = "Monday")
  } else NULL
}, error = function(e) {
  warning("gtfsrouter failed to initialise: ", e$message); NULL
})

# --- Pre-computed transit isochrone cache ------------------------------------
transit_iso_cache <- tryCatch({
  cache_path <- 'data/transit_iso_cache.rds'
  if (file.exists(cache_path)) readRDS(cache_path) else NULL
}, error = function(e) { NULL })

# --- Stop headways via tidytransit (AM peak 7-9am) ---------------------------
headways_cache <- "data/cache/gtfs_stop_headways.rds"
gtfs_stop_headways <- tryCatch({
  if (file.exists(headways_cache)) {
    message("Loading stop headways from cache...")
    readRDS(headways_cache)
  } else if (!is.null(gtfs_zip_path) && file.exists(gtfs_zip_path)) {
    # Reuse the zip already created for gtfsrouter above
    message("Computing stop service frequencies (AM peak 7-9am)...")
    gt <- tidytransit::read_gtfs(gtfs_zip_path)
    hw <- tidytransit::get_stop_frequency(gt, start_time = 7 * 3600, end_time = 9 * 3600) |>
      group_by(stop_id) |>
      summarise(
        mean_headway_min  = mean(mean_headway, na.rm = TRUE) / 60,
        n_departures_peak = sum(n_departures, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(stop_id = as.character(stop_id))
    if (!dir.exists("data/cache")) dir.create("data/cache", recursive = TRUE)
    saveRDS(hw, headways_cache)
    message("Stop headways cached.")
    hw
  } else {
    warning("No GTFS zip available for headway computation.")
    NULL
  }
}, error = function(e) {
  warning("tidytransit headway computation failed: ", e$message); NULL
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
  warning("CalEnviroScreen failed to load: ", e$message); NULL
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
  warning("SF EJ layer failed to load: ", e$message); NULL
})



# 
# # Community Organizations shapefile
# # For now simulate
# 
# # Define San Francisco bounding box coordinates
# sf_bbox <- st_bbox(c(
#   xmin = -122.5247,  # Western longitude
#   ymin = 37.7045,     # Southern latitude
#   xmax = -122.3569,   # Eastern longitude
#   ymax = 37.8334       # Northern latitude
# ), crs = st_crs(4326))  # WGS84 CRS
# 
# # Convert bounding box to polygon
# sf_boundary <- st_as_sfc(sf_bbox) %>% st_make_valid()
# 
# # Transform boundary to projected CRS for accurate buffering (EPSG:3310)
# sf_boundary_proj <- st_transform(sf_boundary, 3310)
# 
# # Set seed for reproducibility
# set.seed(123)
# 
# # Simulate 20 random points within San Francisco boundary
# community_points <- st_sample(sf_boundary_proj, size = 20, type = "random")
# 
# # Convert to sf object with POINT geometry and assign unique names
# community_points_sf <- st_sf(
#   NAME = paste("Community Org", 1:20),
#   geometry = community_points
# )
# # Select first 3 points to buffer
# buffered_points_sf <- community_points_sf[1:3, ] %>%
#   st_buffer(dist = 100)  # Buffer distance in meters
# 
# # Update the NAME column to indicate buffered areas
# buffered_points_sf$NAME <- paste(buffered_points_sf$NAME, "Area")
# community_points_sf <- st_transform(community_points_sf, 4326)
# buffered_points_sf <- st_transform(buffered_points_sf, 4326)
# 
# # Combine points and polygons into one sf object
# community_orgs <- bind_rows(
#   community_points_sf,
#   buffered_points_sf
# )
# 
# # View the combined dataset
# print(community_orgs)
# 
# community_points_only <- community_orgs %>% filter(st_geometry_type(geometry) == "POINT")
# community_polygons_only <- community_orgs %>% filter(st_geometry_type(geometry) == "POLYGON")
# 
