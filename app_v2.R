# Currently downloaded the geomestry of San Francsico every time, pretty unefficient

###############################################################################
# Shiny App: San Francisco Biodiversity Access Decision Support Tool
# Merged / expanded version
# Base app + multimodal transit + EJ + access scores + spider plot
###############################################################################

# =============================================================================
# PACKAGES
# =============================================================================
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

# =============================================================================
# PROJECT SETUP
# =============================================================================
# Set this to your project folder if needed
# setwd('/Users/diegoellis/Desktop/Projects/Postdoc/OLD_SF_BIODIV_ACCESS/SF_biodiv_access/backup-shiny/')

# Use setup_local.R for local development (local files + caching).
# Switch to setup.R for HuggingFace / cloud deployment (remote URLs).
source("R/setup_local.R")

# setup_local.R loads: cbg_vect_sf, ndvi, osm_greenspace, biodiv_hotspots,
# biodiv_coldspots, greenspace_dist_raster, greenspace_osmid_raster,
# gbif_parquet (local parquet path), rsf_projects,
# gtfs_stops_sf, gtfs_routes_sf, gtfs_router,
# cenv_sf, sf_ej_sf

# =============================================================================
# GLOBAL CONFIG
# =============================================================================
mapbox_token <- "pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjbHc3NmI0cDMxYzhyMmt0OXBiYnltMjVtIn0.Thtu6WqIhOfin6AykskM2g"

theme <- bs_theme(
  bootswatch   = "minty",
  base_font    = font_google("Roboto"),
  heading_font = font_google("Roboto Slab"),
  bg           = "#f0fff0",
  fg           = "#2e8b57"
)

# =============================================================================
# OPTIONAL ENVIRONMENT / EQUITY LAYERS
# =============================================================================
calenviro_path <- '/Users/diegoellis/Downloads/calenviroscreen40gdb_F_2021.gdb'
if (!file.exists(calenviro_path)) {
  calenviro_path <- '/Users/diegoellis/Desktop/Projects/Presentations/Data_Schell_Lab_Tutorial/calenviroscreen40gdb_F_2021.gdb'
}

cenv_sf <- tryCatch({
  if (!file.exists(calenviro_path)) stop("CalEnviroScreen file not found")
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
  warning("CalEnviroScreen not loaded: ", e$message)
  NULL
})

sf_ej_path <- '/Users/diegoellis/Downloads/San Francisco Environmental Justice Communities Map_20251217/geo_export_a21b0a0a-7306-46fd-8381-06581cdbe6e9.shp'

sf_ej_sf <- tryCatch({
  if (!file.exists(sf_ej_path)) stop("SF EJ shapefile not found")
  message("Loading SF EJ Communities layer...")
  sf::st_read(sf_ej_path, quiet = TRUE) |>
    dplyr::mutate(
      symbol_hex = stringr::str_split(symbol_rgb, ",\\s*") |>
        lapply(function(x) {
          sprintf("#%02X%02X%02X", as.integer(x[1]), as.integer(x[2]), as.integer(x[3]))
        }) |>
        unlist(),
      ej_label = dplyr::case_when(
        is.na(score) ~ "Not EJ",
        score >= 21  ~ "High EJ burden (21–30)",
        score >= 11  ~ "Moderate EJ burden (11–20)",
        score >= 1   ~ "Low EJ burden (1–10)",
        score == 0   ~ "Score 0",
        TRUE         ~ "Unknown"
      )
    ) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
}, error = function(e) {
  warning("SF EJ layer not loaded: ", e$message)
  NULL
})

# =============================================================================
# GTFS / MUNI
# =============================================================================
gtfs_router <- NULL
gtfs_stops_sf <- NULL
gtfs_routes_sf <- NULL
transit_iso_cache <- NULL
gtfs_stop_headways <- NULL
gtfs_zip_path <- NULL

# Edit this if needed
gtfs_path <- '/Users/diegoellis/Desktop/RSF_next_steps/GPFS_OSM_Transit/sf_muni_gtfs-current/'

if (dir.exists(gtfs_path)) {
  try({
    gtfs_stops_sf <- read.csv(file.path(gtfs_path, 'stops.txt')) |>
      st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
      mutate(stop_id = as.character(stop_id))
    
    message("Building GTFS route shapes — may take a few seconds...")
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
    
    gtfs_routes_sf <- st_sf(
      shape_id  = sapply(shapes_split, function(s) s$shape_id[1]),
      geometry  = st_sfc(shape_geoms, crs = 4326)
    ) |>
      left_join(shape_route_map, by = "shape_id") |>
      left_join(route_meta, by = "route_id")
    
    message("GTFS route shapes ready: ", nrow(gtfs_routes_sf), " shapes / ",
            n_distinct(gtfs_routes_sf$route_id), " routes")
    
    gtfs_zip_path <- tempfile(fileext = ".zip")
    old_wd <- getwd()
    setwd(gtfs_path)
    utils::zip(gtfs_zip_path, files = list.files('.', pattern = "\\.txt$"))
    setwd(old_wd)
    
    gtfs_router <- tryCatch({
      gr <- gtfsrouter::extract_gtfs(gtfs_zip_path)
      gtfsrouter::gtfs_timetable(gr, day = "Monday")
    }, error = function(e) {
      warning("gtfsrouter failed to initialise: ", e$message)
      NULL
    })
    
    transit_cache_path <- "data/transit_iso_cache.rds"
    transit_iso_cache <- tryCatch({
      if (file.exists(transit_cache_path)) readRDS(transit_cache_path) else NULL
    }, error = function(e) NULL)
    
    gtfs_stop_headways <- tryCatch({
      message("Computing stop service frequencies (AM peak 7–9am)...")
      gt <- tidytransit::read_gtfs(gtfs_zip_path)
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
    
    if (!is.null(gtfs_stops_sf) && !is.null(gtfs_stop_headways)) {
      gtfs_stops_sf <- gtfs_stops_sf |>
        left_join(gtfs_stop_headways, by = "stop_id")
    }
  }, silent = TRUE)
}

# =============================================================================
# GBIF UI VALUES FROM PARQUET
# =============================================================================
gbif_classes <- character(0)
gbif_families <- character(0)

if (exists("gbif_parquet")) {
  con_temp <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  try({
    dbExecute(con_temp, "INSTALL spatial; LOAD spatial;")
    dbExecute(con_temp, "INSTALL httpfs; LOAD httpfs;")
    gbif_tab_temp <- tbl(con_temp, paste0("read_parquet('", gbif_parquet, "')"))
    
    gbif_classes <- gbif_tab_temp |>
      distinct(class) |>
      collect() |>
      pull(class) |>
      unique() |>
      sort()
    
    gbif_families <- gbif_tab_temp |>
      distinct(family) |>
      collect() |>
      pull(family) |>
      unique() |>
      sort()
  }, silent = TRUE)
  try(dbDisconnect(con_temp, shutdown = TRUE), silent = TRUE)
}

# =============================================================================
# HELPERS
# =============================================================================
pretty_mode <- function(x) {
  dplyr::case_when(
    x == "driving"         ~ "Driving",
    x == "walking"         ~ "Walking",
    x == "cycling"         ~ "Cycling",
    x == "driving-traffic" ~ "Driving-Traffic",
    x == "transit"         ~ "Transit",
    x == "walk_transit"    ~ "Walk-Transit",
    TRUE                   ~ tools::toTitleCase(x)
  )
}

mode_palette <- c(
  "Driving"         = "#4393C3",
  "Walking"         = "#74C476",
  "Cycling"         = "#FD8D3C",
  "Driving-Traffic" = "#9E9AC8",
  "Transit"         = "#D6604D",
  "Walk-Transit"    = "#E6AB02"
)

scale01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) return(rep(NA_real_, length(x)))
  if ((rng[2] - rng[1]) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

ecdf01 <- function(x, ref) {
  ref <- ref[is.finite(ref)]
  if (length(ref) == 0) return(rep(NA_real_, length(x)))
  f <- ecdf(ref)
  out <- f(x)
  out[is.na(x)] <- NA_real_
  out
}

standardize_iso_sf <- function(x, mode_name, time_min) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  x <- st_as_sf(x)
  st_sf(
    mode = mode_name,
    time = as.numeric(time_min),
    geometry = st_geometry(x),
    crs = st_crs(x)
  )
}

choose_existing_sf_object <- function(candidates) {
  for (nm in candidates) {
    if (exists(nm, inherits = TRUE)) {
      obj <- get(nm, inherits = TRUE)
      if (inherits(obj, "sf")) return(obj)
    }
  }
  NULL
}

safe_vect_gbif_intersection <- function(poly_i) {
  out <- tryCatch({
    if (exists("vect_gbif")) {
      st_as_sf(intersect(vect_gbif, vect(poly_i)))
    } else if (exists("sf_gbif")) {
      st_intersection(sf_gbif, poly_i)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  out
}

safe_biodiv_hotspots <- function() {
  choose_existing_sf_object(c("biodiv_hotspots", "hotspots"))
}

safe_biodiv_coldspots <- function() {
  choose_existing_sf_object(c("biodiv_coldspots", "coldspots"))
}

safe_partner_orgs <- function() {
  choose_existing_sf_object(c(
    "partner_orgs_sf",
    "partner_organizations_sf",
    "community_orgs_sf",
    "community_organizations_sf",
    "partner_orgs",
    "community_orgs",
    "sf_partner_orgs",
    "community_sites_sf"
  ))
}

walk_speed_m_per_min <- 80

build_first_mile_walkshed <- function(location_sf, walk_minutes, mapbox_token) {
  if (is.null(walk_minutes) || walk_minutes <= 0) return(NULL)
  
  tryCatch(
    mb_isochrone(
      location_sf,
      time = walk_minutes,
      profile = "walking",
      access_token = mapbox_token
    ) |>
      st_as_sf() |>
      st_make_valid() |>
      st_transform(4326),
    error = function(e) NULL
  )
}

get_nearest_stops_by_distance <- function(location_sf, gtfs_stops_sf, walk_minutes, max_n = 5) {
  max_dist_m <- walk_minutes * walk_speed_m_per_min
  
  loc_proj   <- st_transform(location_sf, 3857)
  stops_proj <- st_transform(gtfs_stops_sf, 3857)
  
  dists <- as.numeric(st_distance(loc_proj, stops_proj))
  keep  <- which(dists <= max_dist_m)
  
  if (length(keep) == 0) {
    keep <- order(dists)[seq_len(min(max_n, length(dists)))]
  }
  
  gtfs_stops_sf[keep, ] |>
    mutate(
      dist_to_origin_m = round(dists[keep], 1),
      walk_time_to_origin_min = pmax(0, round(dists[keep] / walk_speed_m_per_min, 1))
    ) |>
    arrange(dist_to_origin_m)
}

get_walk_accessible_stops <- function(location_sf, walk_minutes, gtfs_stops_sf, mapbox_token) {
  if (is.null(walk_minutes) || walk_minutes <= 0) return(NULL)
  
  walk_iso <- build_first_mile_walkshed(location_sf, walk_minutes, mapbox_token)
  
  if (!is.null(walk_iso) && nrow(walk_iso) > 0) {
    stops_in_walkshed <- tryCatch(
      st_intersection(gtfs_stops_sf, st_union(walk_iso)),
      error = function(e) NULL
    )
    
    if (!is.null(stops_in_walkshed) && nrow(stops_in_walkshed) > 0) {
      dists <- as.numeric(st_distance(
        st_transform(location_sf, 3857),
        st_transform(stops_in_walkshed, 3857)
      ))
      
      return(
        stops_in_walkshed |>
          mutate(
            access_method = "mapbox_walkshed",
            dist_to_origin_m = round(dists, 1),
            walk_time_to_origin_min = pmax(0, round(dists / walk_speed_m_per_min, 1))
          ) |>
          arrange(walk_time_to_origin_min, dist_to_origin_m)
      )
    }
  }
  
  fallback_stops <- get_nearest_stops_by_distance(
    location_sf   = location_sf,
    gtfs_stops_sf = gtfs_stops_sf,
    walk_minutes  = walk_minutes,
    max_n         = 5
  )
  
  if (!is.null(fallback_stops) && nrow(fallback_stops) > 0) {
    fallback_stops <- fallback_stops |>
      mutate(access_method = "distance_fallback")
  }
  
  fallback_stops
}

extract_transit_minutes <- function(iso_result, dep_secs) {
  if (is.null(iso_result) || nrow(iso_result) == 0) return(numeric(0))
  
  if ("travel_time" %in% names(iso_result)) {
    return(as.numeric(iso_result$travel_time) / 60)
  }
  if ("duration" %in% names(iso_result)) {
    return(as.numeric(iso_result$duration) / 60)
  }
  if ("time" %in% names(iso_result)) {
    return(as.numeric(iso_result$time) / 60)
  }
  if ("arrival_time" %in% names(iso_result)) {
    return((as.numeric(iso_result$arrival_time) - dep_secs) / 60)
  }
  
  rep(NA_real_, nrow(iso_result))
}

build_last_mile_walkshed <- function(
    reachable_sf,
    remaining_walk_col = "remaining_walk_min",
    mapbox_token,
    walk_from_stop_cap_min = 8,
    max_stops = 12
) {
  if (is.null(reachable_sf) || nrow(reachable_sf) == 0) return(NULL)
  if (!(remaining_walk_col %in% names(reachable_sf))) return(NULL)
  
  rs <- reachable_sf |>
    mutate(
      remaining_walk_min = as.numeric(.data[[remaining_walk_col]]),
      remaining_walk_min = pmin(remaining_walk_min, walk_from_stop_cap_min)
    ) |>
    filter(is.finite(remaining_walk_min), remaining_walk_min > 0.5)
  
  if (nrow(rs) == 0) return(NULL)
  
  if ("n_departures_peak" %in% names(rs)) {
    rs <- rs |>
      arrange(desc(n_departures_peak), desc(remaining_walk_min))
  } else if ("mean_headway_min" %in% names(rs)) {
    rs <- rs |>
      arrange(mean_headway_min, desc(remaining_walk_min))
  } else {
    rs <- rs |>
      arrange(desc(remaining_walk_min))
  }
  
  rs <- rs |>
    slice_head(n = max_stops)
  
  walk_polys <- list()
  
  for (i in seq_len(nrow(rs))) {
    stop_i <- rs[i, ]
    walk_t <- floor(as.numeric(stop_i$remaining_walk_min[[1]]))
    if (!is.finite(walk_t) || walk_t <= 0) next
    
    iso_i <- tryCatch(
      mb_isochrone(
        stop_i,
        time = walk_t,
        profile = "walking",
        access_token = mapbox_token
      ),
      error = function(e) NULL
    )
    
    if (!is.null(iso_i) && nrow(iso_i) > 0) {
      walk_polys[[length(walk_polys) + 1]] <- st_as_sf(iso_i)
    }
  }
  
  if (length(walk_polys) == 0) return(NULL)
  
  walk_geom <- dplyr::bind_rows(walk_polys) |>
    st_as_sf() |>
    st_make_valid() |>
    st_union()
  
  st_sf(geometry = walk_geom, crs = 4326)
}

build_walk_transit_isochrone <- function(
    location_sf, total_time_min, dep_secs,
    walk_to_stop_min, walk_from_stop_min,
    gtfs_stops_sf, gtfs_router, mapbox_token,
    departure_window_min = 10,
    departure_step_min = 5,
    max_last_mile_stops = 12,
    include_first_mile_polygon = TRUE
) {
  if (is.null(gtfs_router)) return(NULL)
  if (is.null(total_time_min) || total_time_min <= 0) return(NULL)
  if (is.null(walk_to_stop_min) || walk_to_stop_min <= 0) return(NULL)
  
  first_mile_walkshed <- build_first_mile_walkshed(
    location_sf = location_sf,
    walk_minutes = walk_to_stop_min,
    mapbox_token = mapbox_token
  )
  
  origin_walk_stops <- get_walk_accessible_stops(
    location_sf   = location_sf,
    walk_minutes  = walk_to_stop_min,
    gtfs_stops_sf = gtfs_stops_sf,
    mapbox_token  = mapbox_token
  )
  
  if (is.null(origin_walk_stops) || nrow(origin_walk_stops) == 0) {
    return(NULL)
  }
  
  origin_walk_stops <- origin_walk_stops |>
    mutate(stop_id_chr = as.character(stop_id))
  
  departure_offsets_min <- seq(
    from = 0,
    to   = max(0, departure_window_min),
    by   = max(1, departure_step_min)
  )
  
  reachable_rows <- list()
  
  for (i in seq_len(nrow(origin_walk_stops))) {
    sid <- as.character(origin_walk_stops$stop_id_chr[[i]])
    first_mile_time_i <- as.numeric(origin_walk_stops$walk_time_to_origin_min[[i]])
    first_mile_time_i <- min(first_mile_time_i, walk_to_stop_min, na.rm = TRUE)
    
    if (!is.finite(first_mile_time_i) || first_mile_time_i >= total_time_min) next
    
    for (wait_offset_min in departure_offsets_min) {
      remaining_budget_before_transit <- total_time_min - first_mile_time_i - wait_offset_min
      if (!is.finite(remaining_budget_before_transit) || remaining_budget_before_transit <= 0) next
      
      start_time_i <- dep_secs + wait_offset_min * 60
      end_time_i   <- start_time_i + remaining_budget_before_transit * 60
      
      iso_result <- tryCatch(
        gtfsrouter::gtfs_isochrone(
          gtfs       = gtfs_router,
          from       = sid,
          start_time = start_time_i,
          end_time   = end_time_i,
          from_is_id = TRUE
        ),
        error = function(e) NULL
      )
      
      if (is.null(iso_result) || nrow(iso_result) == 0 || !("stop_id" %in% names(iso_result))) next
      
      transit_min <- extract_transit_minutes(iso_result, start_time_i)
      
      res_i <- iso_result |>
        mutate(
          stop_id_chr = as.character(stop_id),
          origin_stop_id = sid,
          first_mile_walk_min = first_mile_time_i,
          wait_time_min = wait_offset_min,
          transit_time_min = transit_min
        )
      
      if (!("transit_time_min" %in% names(res_i)) || all(is.na(res_i$transit_time_min))) {
        res_i$transit_time_min <- remaining_budget_before_transit
      }
      
      reachable_rows[[length(reachable_rows) + 1]] <- res_i
    }
  }
  
  if (length(reachable_rows) == 0) return(NULL)
  
  reachable_tbl <- dplyr::bind_rows(reachable_rows) |>
    mutate(total_pre_lastmile_min = first_mile_walk_min + wait_time_min + transit_time_min) |>
    group_by(stop_id_chr) |>
    summarise(
      first_mile_walk_min = min(first_mile_walk_min, na.rm = TRUE),
      wait_time_min       = min(wait_time_min, na.rm = TRUE),
      transit_time_min    = min(transit_time_min, na.rm = TRUE),
      best_total_pre_lastmile_min = min(total_pre_lastmile_min, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      remaining_walk_min = total_time_min - best_total_pre_lastmile_min,
      remaining_walk_min = pmin(remaining_walk_min, walk_from_stop_min)
    ) |>
    filter(is.finite(remaining_walk_min), remaining_walk_min > 0.5)
  
  if (nrow(reachable_tbl) == 0) return(NULL)
  
  reachable_sf <- gtfs_stops_sf |>
    mutate(stop_id_chr = as.character(stop_id)) |>
    inner_join(reachable_tbl, by = "stop_id_chr")
  
  if (nrow(reachable_sf) == 0) return(NULL)
  
  last_mile_walkshed <- build_last_mile_walkshed(
    reachable_sf = reachable_sf,
    remaining_walk_col = "remaining_walk_min",
    mapbox_token = mapbox_token,
    walk_from_stop_cap_min = walk_from_stop_min,
    max_stops = max_last_mile_stops
  )
  
  if (is.null(last_mile_walkshed) || nrow(last_mile_walkshed) == 0) return(NULL)
  
  final_geom <- st_union(st_geometry(last_mile_walkshed))
  
  if (include_first_mile_polygon && !is.null(first_mile_walkshed) && nrow(first_mile_walkshed) > 0) {
    final_geom <- st_union(final_geom, st_union(st_geometry(first_mile_walkshed)))
  }
  
  final_sf <- st_sf(geometry = final_geom, crs = 4326) |>
    st_make_valid()
  
  iso_sf <- st_sf(
    mode = "walk_transit",
    time = as.numeric(total_time_min),
    geometry = st_geometry(final_sf),
    crs = 4326
  )
  
  standardize_iso_sf(iso_sf, mode_name = "walk_transit", time_min = total_time_min)
}

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "SF Biodiversity Access Tool"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Isochrone Explorer", tabName = "isochrone", icon = icon("map-marker-alt")),
                menuItem("GBIF Summaries", tabName = "gbif", icon = icon("table")),
                menuItem("Community Science", tabName = "community_science", icon = icon("users")),
                menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    theme = theme,
    useShinyjs(),
    div(id = "loading", style = "display:none; font-size:20px; color:red;", "Calculating..."),
    
    tabItems(
      tabItem(
        tabName = "isochrone",
        fluidRow(
          box(
            title = "Controls", status = "success", solidHeader = TRUE, width = 4,
            
            radioButtons(
              "location_choice",
              "Select Location Method:",
              choices = c("Address (Geocode)" = "address", "Click on Map" = "map_click"),
              selected = "map_click"
            ),
            
            conditionalPanel(
              condition = "input.location_choice == 'address'",
              mapboxGeocoderInput(
                inputId = "geocoder",
                placeholder = "Search for an address",
                access_token = mapbox_token
              )
            ),
            
            checkboxGroupInput(
              "transport_modes",
              "Select Transportation Modes:",
              choices = list(
                "Driving"               = "driving",
                "Walking"               = "walking",
                "Cycling"               = "cycling",
                "Driving with Traffic"  = "driving-traffic",
                "Transit (GTFS)"        = "transit",
                "Walk + Transit (Muni)" = "walk_transit"
              ),
              selected = c("driving", "walking")
            ),
            
            conditionalPanel(
              condition = "input.transport_modes.includes('transit') || input.transport_modes.includes('walk_transit')",
              sliderInput(
                "transit_hour",
                "Transit Departure Hour (24h):",
                min = 5, max = 22, value = 9, step = 1, post = ":00"
              ),
              sliderInput(
                "transit_departure_window_min",
                "Transit departure flexibility window (minutes):",
                min = 0, max = 20, value = 10, step = 5
              ),
              helpText("Several departures after the selected time can be evaluated for walk + transit.")
            ),
            
            conditionalPanel(
              condition = "input.transport_modes.includes('walk_transit')",
              sliderInput(
                "walk_to_stop_min",
                "First-mile walking budget (minutes):",
                min = 1, max = 20, value = 5, step = 1
              ),
              sliderInput(
                "walk_from_stop_min",
                "Maximum last-mile walking budget (minutes):",
                min = 0, max = 20, value = 5, step = 1
              )
            ),
            
            checkboxGroupInput(
              "iso_times",
              "Select Isochrone Times (minutes):",
              choices = list("5" = 5, "10" = 10, "15" = 15),
              selected = c(5, 10)
            ),
            
            actionButton("generate_iso", "Generate Isochrones", icon = icon("play")),
            actionButton("clear_map", "Clear", icon = icon("times"))
          ),
          
          box(
            title = "Map", status = "success", solidHeader = TRUE, width = 8,
            leafletOutput("isoMap", height = 600)
          )
        ),
        
        fluidRow(
          box(title = "Biodiversity Access Score", status = "success", solidHeader = TRUE, width = 3, uiOutput("bioScoreBox")),
          box(title = "Transit Access Score", status = "primary", solidHeader = TRUE, width = 3, uiOutput("transitScoreBox")),
          box(title = "Biodiversity Access Index", status = "warning", solidHeader = TRUE, width = 3, uiOutput("biodiversityAccessIndexBox")),
          box(title = "Closest Greenspace", status = "success", solidHeader = TRUE, width = 3, uiOutput("closestGreenspaceUI"))
        ),
        
        fluidRow(
          box(
            title = "Biodiversity Access Index Profile",
            status = "warning", solidHeader = TRUE, width = 12,
            p("This spider plot summarizes biodiversity access across mobility, biodiversity, observation intensity, environmental quality, and equity context."),
            plotOutput("radarPlot", height = "650px") %>% withSpinner(type = 8, color = "#f0ad4e")
          )
        ),
        
        fluidRow(
          box(
            title = "Summary Data", status = "success", solidHeader = TRUE, width = 12,
            DTOutput("dataTable") %>% withSpinner(type = 8, color = "#28a745")
          )
        ),
        
        fluidRow(
          box(
            title = "Biodiversity & Socioeconomic Summary", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("bioSocPlot", height = "400px") %>% withSpinner(type = 8, color = "#28a745")
          )
        ),
        
        fluidRow(
          box(
            title = "GBIF Records by Institution", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("collectionPlot", height = "400px") %>% withSpinner(type = 8, color = "#28a745")
          )
        ),
        
        fluidRow(
          box(
            title = "Biodiversity & Transit Metrics by Mode",
            status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("transitMetricsPlot", height = "450px") %>% withSpinner(type = 8, color = "#005B95")
          )
        )
      ),
      
      tabItem(
        tabName = "gbif",
        fluidRow(
          box(
            title = "Filters", status = "success", solidHeader = TRUE, width = 4,
            selectInput(
              "class_filter",
              "Select a GBIF Class to Summarize:",
              choices = c("All", gbif_classes),
              selected = "All"
            ),
            selectInput(
              "family_filter",
              "Filter by Family (optional):",
              choices = c("All", gbif_families),
              selected = "All"
            )
          ),
          box(
            title = "Data Summary", status = "success", solidHeader = TRUE, width = 8,
            DTOutput("classTable")
          )
        ),
        fluidRow(
          box(
            title = "Observations vs. Species Richness", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("obsVsSpeciesPlot", height = "300px") %>% withSpinner(type = 8, color = "#28a745"),
            p("This plot displays the relationship between the number of observations and species richness.")
          )
        )
      ),
      
      tabItem(
        tabName = "community_science",
        fluidRow(
          box(
            title = "Partner Community Organizations",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("communityMap", height = 600)
          )
        ),
        fluidRow(
          box(
            title = "Community Organizations Data",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("communityTable") %>% withSpinner(type = 8, color = "#28a745")
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "App Summary", status = "success", solidHeader = TRUE, width = 12,
            tags$b("App Summary:"),
            p("This application allows users to click on a map or geocode an address to generate travel-time isochrones across multiple transportation modes, including walking, cycling, driving, transit, and walk + transit. It retrieves socio-economic data from precomputed Census variables, calculates NDVI, summarizes biodiversity records from GBIF, and integrates environmental justice and transit accessibility information."),
            tags$b("Created by:"),
            p(strong("Diego Ellis Soto, Carl Boettiger, Rebecca Johnson, Christopher J. Schell")),
            p("Contact Information: ", strong("diego.ellissoto@berkeley.edu"))
          )
        ),
        fluidRow(
          box(
            title = "Reimagining San Francisco", status = "success", solidHeader = TRUE, width = 12,
            p("Reimagining San Francisco is an initiative aimed at integrating ecological, social, and technological dimensions to shape a sustainable future for the Bay Area.")
          )
        ),
        fluidRow(
          box(
            title = "Why Biodiversity Access Matters", status = "success", solidHeader = TRUE, width = 12,
            p("Ensuring equitable access to biodiversity is essential for human well-being, ecological resilience, and better urban planning. This tool is intended as a decision-support prototype.")
          )
        ),
        fluidRow(
          box(
            title = "Biodiversity Access Index (BAI)", status = "warning", solidHeader = TRUE, width = 12,
            p("The Biodiversity Access Index combines mobility access, biodiversity potential, observation intensity, environmental quality, and equity context into a composite score."),
            tags$pre("BAI = mean(Mobility_Access, Biodiversity_Potential,\n           Observation_Intensity, Environmental_Quality, Equity_Context)")
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # DuckDB connection
  # ---------------------------------------------------------------------------
  con <- NULL
  gbif_tab <- NULL
  
  if (exists("gbif_parquet")) {
    con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    try({
      dbExecute(con, "INSTALL spatial; LOAD spatial;")
      dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
      gbif_tab <- tbl(con, paste0("read_parquet('", gbif_parquet, "')"))
    }, silent = TRUE)
    
    onStop(function() {
      try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    })
  }
  
  chosen_point <- reactiveVal(NULL)
  
  # ---------------------------------------------------------------------------
  # Benchmarks for BAI
  # ---------------------------------------------------------------------------
  city_benchmarks <- local({
    message("Computing citywide benchmark distributions for BAI...")
    
    cbg_bench <- cbg_vect_sf |>
      st_transform(3857) |>
      mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
      st_transform(4326)
    
    transit_density_bench <- rep(0, nrow(cbg_bench))
    if (!is.null(gtfs_stops_sf)) {
      stop_join <- tryCatch(
        st_join(gtfs_stops_sf, cbg_bench[, c("GEOID")], left = FALSE),
        error = function(e) NULL
      )
      
      if (!is.null(stop_join) && nrow(stop_join) > 0) {
        transit_density_bench <- stop_join |>
          st_drop_geometry() |>
          count(GEOID, name = "n_stops") |>
          right_join(
            cbg_bench |> st_drop_geometry() |> select(GEOID, area_km2),
            by = "GEOID"
          ) |>
          mutate(
            n_stops = replace_na(n_stops, 0),
            transit_density = ifelse(area_km2 > 0, n_stops / area_km2, 0)
          ) |>
          pull(transit_density)
      }
    }
    
    sampling_density_bench <- cbg_bench |>
      st_drop_geometry() |>
      mutate(
        obs = ifelse(is.na(n_observations), 0, n_observations),
        sampling_density = ifelse(area_km2 > 0, obs / area_km2, 0)
      ) |>
      pull(sampling_density)
    
    biodiversity_bench <- cbg_bench |>
      st_drop_geometry() |>
      mutate(unique_species = replace_na(unique_species, 0)) |>
      pull(unique_species)
    
    ndvi_bench <- cbg_bench |>
      st_drop_geometry() |>
      mutate(ndvi_ref = dplyr::coalesce(ndvi_mean, ndvi_sentinel)) |>
      pull(ndvi_ref)
    
    ej_bench <- if (!is.null(sf_ej_sf) && "score" %in% names(sf_ej_sf)) {
      sf_ej_sf |> st_drop_geometry() |> pull(score) |> na.omit()
    } else {
      numeric(0)
    }
    
    list(
      transit_density = transit_density_bench[is.finite(transit_density_bench)],
      biodiversity    = biodiversity_bench[is.finite(biodiversity_bench)],
      sampling        = sampling_density_bench[is.finite(sampling_density_bench)],
      ndvi            = ndvi_bench[is.finite(ndvi_bench)],
      ej              = ej_bench[is.finite(ej_bench)]
    )
  })
  
  # ---------------------------------------------------------------------------
  # Logos
  # ---------------------------------------------------------------------------
  output$combine_logo <- renderImage({
    list(
      src = file.path("www", "Combined_logos.png"),
      width = "50%",
      height = "45%",
      alt = "Combined_logos"
    )
  }, deleteFile = FALSE)
  
  # ---------------------------------------------------------------------------
  # Base map
  # ---------------------------------------------------------------------------
  output$isoMap <- renderLeaflet({
    pal_cbg  <- colorNumeric("YlOrRd", cbg_vect_sf$medincE)
    pal_rich <- colorNumeric("YlOrRd", domain = cbg_vect_sf$unique_species)
    pal_data <- colorNumeric("Blues", domain = cbg_vect_sf$n_observations)
    
    hotspot_sf <- safe_biodiv_hotspots()
    coldspot_sf <- safe_biodiv_coldspots()
    
    m <- leaflet() |>
      addTiles(group = "Street Map (Default)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (ESRI)") |>
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") |>
      addPolygons(
        data = cbg_vect_sf, group = "Income",
        fillColor = ~pal_cbg(medincE), fillOpacity = 0.6,
        color = "white", weight = 1, label = ~GEOID
      ) |>
      addPolygons(
        data = osm_greenspace, group = "Greenspace",
        fillColor = "darkgreen", fillOpacity = 0.3,
        color = "green", weight = 1, label = ~name
      ) |>
      addPolygons(
        data = cbg_vect_sf, group = "Species Richness",
        fillColor = ~pal_rich(unique_species), fillOpacity = 0.6,
        color = "white", weight = 1, label = ~unique_species
      ) |>
      addPolygons(
        data = cbg_vect_sf, group = "Data Availability",
        fillColor = ~pal_data(n_observations), fillOpacity = 0.6,
        color = "white", weight = 1, label = ~n_observations
      )
    
    if (!is.null(hotspot_sf)) {
      m <- m |>
        addPolygons(
          data = hotspot_sf,
          group = "Hotspots (KnowBR)",
          fillColor = "firebrick", fillOpacity = 0.2,
          color = "firebrick", weight = 2
        )
    }
    
    if (!is.null(coldspot_sf)) {
      m <- m |>
        addPolygons(
          data = coldspot_sf,
          group = "Coldspots (KnowBR)",
          fillColor = "navy", fillOpacity = 0.2,
          color = "navy", weight = 2
        )
    }
    
    rsf_proj_sf <- choose_existing_sf_object(c("rsf_projects", "rsf_project_areas", "rsf_polygons", "rsf_programs"))
    if (!is.null(rsf_proj_sf)) {
      m <- m |>
        addPolygons(
          data = rsf_proj_sf,
          group = "RSF Program Projects",
          fillColor = "purple",
          fillOpacity = 0.3,
          color = "purple",
          weight = 1,
          label = ~if (exists("prj_name")) ~prj_name else "RSF Project",
          highlightOptions = highlightOptions(
            weight = 5,
            color = "blue",
            fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          labelOptions = labelOptions(
            style = list("font-weight" = "bold", "color" = "blue"),
            textsize = "12px",
            direction = "auto",
            noHide = FALSE
          )
        )
    }
    
    if (exists("greenspace_dist_raster")) {
      greenspace_vals_clean <- values(greenspace_dist_raster) |>
        as.vector() |>
        (\(x) x[is.finite(x)])()
      
      if (length(greenspace_vals_clean) > 0) {
        upper_limit <- quantile(greenspace_vals_clean, 0.998, na.rm = TRUE)
        
        pal_greenspace_dist <- colorNumeric(
          palette = rev(brewer.pal(9, "YlGnBu")),
          domain = c(0, upper_limit),
          na.color = "transparent"
        )
        
        m <- m |>
          addRasterImage(
            x = greenspace_dist_raster,
            colors = pal_greenspace_dist,
            opacity = 0.65,
            project = TRUE,
            group = "Greenspace Distance"
          ) |>
          addLegend(
            position = "bottomleft",
            pal = pal_greenspace_dist,
            values = c(0, upper_limit),
            title = "Distance to<br>Greenspace (m)",
            group = "Greenspace Distance"
          )
      }
    }
    
    if (!is.null(gtfs_routes_sf)) {
      m <- m |>
        addPolylines(
          data = gtfs_routes_sf,
          group = "Transit Routes",
          color = ~route_color_hex,
          weight = 2,
          opacity = 0.8,
          label = ~paste0(route_short_name, ": ", route_long_name)
        )
    }
    
    if (!is.null(gtfs_stops_sf)) {
      m <- m |>
        addCircleMarkers(
          data = gtfs_stops_sf,
          group = "Transit Stops",
          radius = 4,
          color = "#005B95", fillColor = "#005B95",
          fillOpacity = 0.7, stroke = FALSE,
          label = ~stop_name
        )
    }
    
    if (!is.null(cenv_sf)) {
      pal_cenv <- colorNumeric("YlOrBr", domain = cenv_sf$CIscore, na.color = "transparent")
      m <- m |>
        addPolygons(
          data = cenv_sf,
          group = "CalEnviroScreen (CI Score)",
          fillColor = ~pal_cenv(CIscore),
          fillOpacity = 0.65,
          color = "white",
          weight = 0.5,
          label = ~paste0("CI Score: ", round(CIscore, 1))
        )
    }
    
    if (!is.null(sf_ej_sf)) {
      m <- m |>
        addPolygons(
          data = sf_ej_sf,
          group = "SF EJ Communities",
          fillColor = ~symbol_hex,
          fillOpacity = 0.7,
          color = "white",
          weight = 0.5,
          label = ~paste0(ej_label, ifelse(is.na(score), "", paste0(" (score: ", score, ")")))
        )
    }
    
    m |>
      setView(lng = -122.4194, lat = 37.7749, zoom = 12) |>
      addLayersControl(
        baseGroups = c("Street Map (Default)", "Satellite (ESRI)", "CartoDB.Positron"),
        overlayGroups = c(
          "Income", "Greenspace", "Greenspace Distance", "RSF Program Projects",
          "Hotspots (KnowBR)", "Coldspots (KnowBR)",
          "Species Richness", "Data Availability",
          "CalEnviroScreen (CI Score)", "SF EJ Communities",
          "Transit Routes", "Transit Stops",
          "Isochrones", "Transit Isochrones", "NDVI Raster"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup("Income") |>
      hideGroup("Greenspace") |>
      hideGroup("Greenspace Distance") |>
      hideGroup("RSF Program Projects") |>
      hideGroup("Hotspots (KnowBR)") |>
      hideGroup("Coldspots (KnowBR)") |>
      hideGroup("Species Richness") |>
      hideGroup("Data Availability") |>
      hideGroup("CalEnviroScreen (CI Score)") |>
      hideGroup("SF EJ Communities") |>
      hideGroup("Transit Routes") |>
      hideGroup("Transit Stops")
  })
  
  # ---------------------------------------------------------------------------
  # Location selection
  # ---------------------------------------------------------------------------
  observeEvent(input$isoMap_click, {
    req(input$location_choice == "map_click")
    click <- input$isoMap_click
    if (!is.null(click)) {
      chosen_point(c(lon = click$lng, lat = click$lat))
      leafletProxy("isoMap") |>
        clearGroup("selected_point") |>
        addCircleMarkers(
          lng = click$lng, lat = click$lat,
          radius = 6, color = "firebrick",
          group = "selected_point", label = "Map Click Location"
        )
    }
  })
  
  observeEvent(input$geocoder, {
    req(input$location_choice == "address")
    geocode_result <- input$geocoder
    if (!is.null(geocode_result)) {
      xy <- geocoder_as_xy(geocode_result)
      chosen_point(c(lon = xy[1], lat = xy[2]))
      leafletProxy("isoMap") |>
        clearGroup("selected_point") |>
        addCircleMarkers(
          lng = xy[1], lat = xy[2],
          radius = 6, color = "navy",
          group = "selected_point", label = "Geocoded Address"
        ) |>
        flyTo(lng = xy[1], lat = xy[2], zoom = 13)
    }
  })
  
  observeEvent(input$clear_map, {
    chosen_point(NULL)
    leafletProxy("isoMap") |>
      clearGroup("selected_point") |>
      clearGroup("Isochrones") |>
      clearGroup("Transit Isochrones") |>
      clearGroup("NDVI Raster")
  })
  
  # ---------------------------------------------------------------------------
  # Isochrone generation
  # ---------------------------------------------------------------------------
  isochrones_data <- eventReactive(input$generate_iso, {
    leafletProxy("isoMap") |>
      clearGroup("Isochrones") |>
      clearGroup("Transit Isochrones") |>
      clearGroup("NDVI Raster")
    
    pt <- chosen_point()
    if (is.null(pt)) return(NULL)
    if (length(input$transport_modes) == 0) return(NULL)
    if (length(input$iso_times) == 0) return(NULL)
    
    location_sf <- st_as_sf(
      data.frame(lon = pt["lon"], lat = pt["lat"]),
      coords = c("lon", "lat"),
      crs = 4326
    )
    
    iso_list <- list()
    
    mapbox_modes <- intersect(input$transport_modes, c("driving", "walking", "cycling", "driving-traffic"))
    
    for (mode in mapbox_modes) {
      for (t in as.numeric(input$iso_times)) {
        iso <- tryCatch(
          mb_isochrone(location_sf, time = t, profile = mode, access_token = mapbox_token),
          error = function(e) NULL
        )
        
        if (!is.null(iso)) {
          iso_std <- standardize_iso_sf(iso, mode_name = mode, time_min = t)
          if (!is.null(iso_std)) iso_list <- append(iso_list, list(iso_std))
        }
      }
    }
    
    if ("transit" %in% input$transport_modes && !is.null(gtfs_router) && !is.null(gtfs_stops_sf)) {
      stop_dists <- st_distance(location_sf, gtfs_stops_sf)
      nearest_idx <- which.min(stop_dists)
      nearest_id <- as.character(gtfs_stops_sf$stop_id[nearest_idx])
      dep_secs <- as.numeric(input$transit_hour) * 3600
      
      for (t in as.numeric(input$iso_times)) {
        iso_poly <- NULL
        
        if (!is.null(transit_iso_cache) &&
            !is.null(transit_iso_cache[[nearest_id]]) &&
            !is.null(transit_iso_cache[[nearest_id]][[as.character(t)]])) {
          iso_poly <- transit_iso_cache[[nearest_id]][[as.character(t)]]
        }
        
        if (is.null(iso_poly)) {
          iso_result <- tryCatch(
            gtfsrouter::gtfs_isochrone(
              gtfs       = gtfs_router,
              from       = nearest_id,
              start_time = dep_secs,
              end_time   = dep_secs + t * 60,
              from_is_id = TRUE
            ),
            error = function(e) NULL
          )
          
          if (!is.null(iso_result) && nrow(iso_result) > 2) {
            reachable_sf <- gtfs_stops_sf |>
              filter(stop_id %in% as.character(iso_result$stop_id))
            
            if (nrow(reachable_sf) > 2) {
              iso_poly <- st_convex_hull(st_union(reachable_sf))
            } else if (nrow(reachable_sf) > 0) {
              iso_poly <- st_union(st_buffer(st_transform(reachable_sf, 3857), 100)) |>
                st_transform(4326)
            }
          }
        }
        
        if (!is.null(iso_poly)) {
          iso_sf <- st_sf(
            mode = "transit",
            time = as.numeric(t),
            geometry = st_geometry(st_as_sf(iso_poly)),
            crs = 4326
          )
          iso_sf <- standardize_iso_sf(iso_sf, mode_name = "transit", time_min = t)
          iso_list <- append(iso_list, list(iso_sf))
        }
      }
    }
    
    if ("walk_transit" %in% input$transport_modes && !is.null(gtfs_router) && !is.null(gtfs_stops_sf)) {
      dep_secs <- as.numeric(input$transit_hour) * 3600
      
      valid_times <- as.numeric(input$iso_times)[
        as.numeric(input$iso_times) > input$walk_to_stop_min
      ]
      
      for (t in valid_times) {
        wt_iso <- tryCatch(
          build_walk_transit_isochrone(
            location_sf            = location_sf,
            total_time_min         = t,
            dep_secs               = dep_secs,
            walk_to_stop_min       = input$walk_to_stop_min,
            walk_from_stop_min     = input$walk_from_stop_min,
            gtfs_stops_sf          = gtfs_stops_sf,
            gtfs_router            = gtfs_router,
            mapbox_token           = mapbox_token,
            departure_window_min   = input$transit_departure_window_min,
            departure_step_min     = 5,
            max_last_mile_stops    = 12,
            include_first_mile_polygon = TRUE
          ),
          error = function(e) NULL
        )
        
        if (!is.null(wt_iso) && nrow(wt_iso) > 0) {
          iso_list <- append(iso_list, list(wt_iso))
        }
      }
    }
    
    if (length(iso_list) == 0) return(NULL)
    
    dplyr::bind_rows(iso_list) |>
      st_as_sf() |>
      st_make_valid() |>
      st_transform(4326)
  })
  
  # ---------------------------------------------------------------------------
  # Render isochrones and NDVI
  # ---------------------------------------------------------------------------
  observeEvent(isochrones_data(), {
    iso_data <- isochrones_data()
    req(iso_data)
    
    leafletProxy("isoMap") |>
      clearGroup("Isochrones") |>
      clearGroup("Transit Isochrones") |>
      clearGroup("NDVI Raster")
    
    standard_like_modes <- c("driving", "walking", "cycling", "driving-traffic", "walk_transit")
    standard_iso <- iso_data[iso_data$mode %in% standard_like_modes, ]
    
    if (nrow(standard_iso) > 0) {
      for (i in seq_len(nrow(standard_iso))) {
        poly_i <- standard_iso[i, ]
        mode_i <- as.character(poly_i$mode[[1]])
        time_i <- as.numeric(poly_i$time[[1]])
        mode_label <- pretty_mode(mode_i)
        
        is_walk_transit <- identical(mode_i, "walk_transit")
        line_weight <- if (is_walk_transit) 3 else 2
        dash_style  <- if (is_walk_transit) "5, 5" else NULL
        fill_alpha  <- if (is_walk_transit) 0.28 else 0.35
        
        color_i <- unname(mode_palette[mode_label])
        if (is.na(color_i) || length(color_i) == 0) color_i <- "#666666"
        
        popup_i <- if (is_walk_transit) {
          paste0(
            "<strong>Walk + Transit — ", time_i, " min</strong>",
            "<br><b>First-mile walk:</b> up to ", input$walk_to_stop_min, " min",
            "<br><b>Transit:</b> uses remaining budget after first-mile access",
            "<br><b>Departure flexibility window:</b> ", input$transit_departure_window_min, " min",
            "<br><b>Last-mile walk:</b> up to ", input$walk_from_stop_min, " min",
            "<br><small>Built from reachable transit stops plus walking-network access.</small>"
          )
        } else {
          paste0("<strong>", mode_label, " — ", time_i, " min</strong>")
        }
        
        leafletProxy("isoMap") |>
          addPolygons(
            data = poly_i,
            group = "Isochrones",
            color = color_i,
            weight = line_weight,
            dashArray = dash_style,
            fillOpacity = fill_alpha,
            label = paste0(mode_label, " — ", time_i, " mins"),
            popup = popup_i
          )
      }
    }
    
    transit_iso <- iso_data[iso_data$mode == "transit", ]
    if (nrow(transit_iso) > 0 && !is.null(gtfs_stops_sf)) {
      for (i in seq_len(nrow(transit_iso))) {
        poly_i <- transit_iso[i, ]
        time_i <- as.numeric(poly_i$time[[1]])
        
        n_stops_in <- tryCatch(nrow(st_intersection(gtfs_stops_sf, poly_i)), error = function(e) 0)
        area_km2   <- round(as.numeric(st_area(st_transform(poly_i, 3857))) / 1e6, 2)
        t_score    <- if (area_km2 > 0) round(n_stops_in / area_km2, 2) else NA_real_
        
        gbif_in <- safe_bect <- tryCatch(safe_vect_gbif_intersection(poly_i), error = function(e) NULL)
        n_gbif_rec <- if (!is.null(gbif_in)) nrow(gbif_in) else 0
        n_gbif_sp  <- if (!is.null(gbif_in) && "species" %in% names(gbif_in)) length(unique(gbif_in$species)) else 0
        
        popup_html <- paste0(
          "<strong>Transit Isochrone — ", time_i, " min</strong>",
          "<br><b>Transit Access Score:</b> ", t_score, " stops/km²",
          "<br><b>Muni stops within:</b> ", n_stops_in,
          "<br><b>Area:</b> ", area_km2, " km²",
          "<br><b>GBIF records within:</b> ", n_gbif_rec,
          "<br><b>Unique species:</b> ", n_gbif_sp
        )
        
        leafletProxy("isoMap") |>
          addPolygons(
            data = poly_i,
            group = "Transit Isochrones",
            color = mode_palette[["Transit"]],
            weight = 2,
            dashArray = "6, 4",
            fillOpacity = 0.22,
            label = paste0("Transit ", time_i, " min"),
            popup = popup_html
          )
      }
    }
    
    if (exists("ndvi")) {
      iso_union <- st_union(iso_data)
      iso_union_vect <- vect(iso_union)
      
      ndvi_crop <- terra::crop(ndvi, iso_union_vect)
      ndvi_mask <- terra::mask(ndvi_crop, iso_union_vect)
      ndvi_vals <- values(ndvi_mask)
      ndvi_vals <- ndvi_vals[!is.na(ndvi_vals)]
      
      if (length(ndvi_vals) > 0) {
        ndvi_pal <- colorNumeric("YlGn", domain = range(ndvi_vals, na.rm = TRUE), na.color = "transparent")
        leafletProxy("isoMap") |>
          addRasterImage(x = ndvi_mask, colors = ndvi_pal, opacity = 0.7, project = TRUE, group = "NDVI Raster") |>
          addLegend(position = "bottomright", pal = ndvi_pal, values = ndvi_vals, title = "NDVI")
      }
    }
  })
  
  # ---------------------------------------------------------------------------
  # Per-isochrone summaries
  # ---------------------------------------------------------------------------
  socio_data <- reactive({
    iso_data <- isochrones_data()
    if (is.null(iso_data) || nrow(iso_data) == 0) return(data.frame())
    
    hotspot_union <- safe_biodiv_hotspots()
    coldspot_union <- safe_biodiv_coldspots()
    
    if (!is.null(hotspot_union)) hotspot_union <- st_union(hotspot_union)
    if (!is.null(coldspot_union)) coldspot_union <- st_union(coldspot_union)
    
    acs_wide <- cbg_vect_sf |>
      mutate(population = popE, med_income = medincE)
    
    results <- data.frame()
    
    user_point_sf <- NULL
    pt <- chosen_point()
    if (!is.null(pt)) {
      user_point_sf <- st_as_sf(
        data.frame(lon = pt["lon"], lat = pt["lat"]),
        coords = c("lon", "lat"),
        crs = 4326
      )
    }
    
    min_dist_val_global <- NA_real_
    osm_greenspace_name_global <- NA_character_
    if (!is.null(user_point_sf) && exists("greenspace_dist_raster") && exists("greenspace_osmid_raster")) {
      try({
        min_dist_val_global <- (greenspace_dist_raster |> extract(vect(user_point_sf)) |> pull(1))[1]
        user_point_osm_id <- (greenspace_osmid_raster |> extract(vect(user_point_sf)) |> pull(2))[1]
        osm_greenspace_name_global <- osm_greenspace |>
          mutate(osm_id = as.numeric(osm_id)) |>
          filter(osm_id == user_point_osm_id) |>
          pull(name)
        if (length(osm_greenspace_name_global) == 0 || is.na(osm_greenspace_name_global[1])) {
          osm_greenspace_name_global <- "Unnamed Greenspace"
        } else {
          osm_greenspace_name_global <- osm_greenspace_name_global[1]
        }
      }, silent = TRUE)
    }
    
    for (i in seq_len(nrow(iso_data))) {
      poly_i <- iso_data[i, ]
      vect_poly_i <- vect(poly_i)
      
      dist_hot_km <- if (!is.null(hotspot_union)) {
        round(as.numeric(min(st_distance(poly_i, hotspot_union))) / 1000, 3)
      } else NA_real_
      
      dist_cold_km <- if (!is.null(coldspot_union)) {
        round(as.numeric(min(st_distance(poly_i, coldspot_union))) / 1000, 3)
      } else NA_real_
      
      inter_acs <- tryCatch(intersect(vect(acs_wide), vect_poly_i) |> st_as_sf(), error = function(e) NULL)
      
      pop_total <- 0
      w_income  <- NA_real_
      if (!is.null(inter_acs) && nrow(inter_acs) > 0) {
        inter_acs <- inter_acs |>
          mutate(
            area_num = as.numeric(st_area(st_transform(geometry, 3857))),
            weighted_pop = population * (area_num / sum(area_num, na.rm = TRUE))
          )
        pop_total <- round(sum(inter_acs$weighted_pop, na.rm = TRUE))
        w_income  <- sum(inter_acs$med_income * inter_acs$area_num, na.rm = TRUE) /
          sum(inter_acs$area_num, na.rm = TRUE)
      }
      
      iso_area_m2 <- as.numeric(st_area(st_transform(poly_i, 3857)))
      iso_area_km2 <- round(iso_area_m2 / 1e6, 3)
      
      gs_area_m2 <- 0
      if (exists("greenspace_dist_raster")) {
        dist_crop <- tryCatch(terra::crop(greenspace_dist_raster, vect_poly_i), error = function(e) NULL)
        if (!is.null(dist_crop)) {
          dist_mask <- tryCatch(terra::mask(dist_crop, vect_poly_i), error = function(e) NULL)
          if (!is.null(dist_mask)) {
            is_greenspace <- dist_mask == 0
            cell_areas <- terra::cellSize(is_greenspace, unit = "m")
            gs_area_m2 <- tryCatch(as.numeric(terra::global(cell_areas * is_greenspace, "sum", na.rm = TRUE)[1, 1]), error = function(e) 0)
          }
        }
      }
      gs_percent <- ifelse(iso_area_m2 > 0, 100 * gs_area_m2 / iso_area_m2, 0)
      
      mean_ndvi <- NA_real_
      if (exists("ndvi")) {
        ndvi_vals <- tryCatch(values(terra::mask(terra::crop(ndvi, vect_poly_i), vect_poly_i)), error = function(e) NA)
        ndvi_vals <- ndvi_vals[!is.na(ndvi_vals)]
        mean_ndvi <- ifelse(length(ndvi_vals) > 0, round(mean(ndvi_vals, na.rm = TRUE), 3), NA_real_)
      }
      
      inter_gbif <- safe_vect_gbif_intersection(poly_i)
      n_records <- if (!is.null(inter_gbif)) nrow(inter_gbif) else 0
      n_species <- if (!is.null(inter_gbif) && "species" %in% names(inter_gbif) && nrow(inter_gbif) > 0) length(unique(inter_gbif$species)) else 0
      n_birds   <- if (!is.null(inter_gbif) && nrow(inter_gbif) > 0 && all(c("species", "class") %in% names(inter_gbif))) length(unique(inter_gbif$species[inter_gbif$class == "Aves"])) else 0
      n_mammals <- if (!is.null(inter_gbif) && nrow(inter_gbif) > 0 && all(c("species", "class") %in% names(inter_gbif))) length(unique(inter_gbif$species[inter_gbif$class == "Mammalia"])) else 0
      n_plants  <- if (!is.null(inter_gbif) && nrow(inter_gbif) > 0 && all(c("species", "class") %in% names(inter_gbif))) {
        length(unique(inter_gbif$species[inter_gbif$class %in%
                                           c("Magnoliopsida", "Liliopsida", "Pinopsida", "Polypodiopsida",
                                             "Equisetopsida", "Bryopsida", "Marchantiopsida")]))
      } else 0
      
      n_transit_stops <- NA_real_
      transit_access_score <- NA_real_
      freq_weighted_score <- NA_real_
      mean_headway_iso <- NA_real_
      nearest_stop_m <- NA_real_
      nearest_stop_name <- NA_character_
      
      if (!is.null(gtfs_stops_sf)) {
        inter_transit <- tryCatch(st_intersection(gtfs_stops_sf, poly_i), error = function(e) NULL)
        n_transit_stops <- if (!is.null(inter_transit)) nrow(inter_transit) else 0
        
        dist_transit <- st_distance(poly_i, gtfs_stops_sf)
        nearest_stop_m <- round(as.numeric(min(dist_transit)), 0)
        nearest_stop_name <- gtfs_stops_sf$stop_name[which.min(dist_transit)]
        
        transit_access_score <- ifelse(iso_area_km2 > 0, round(n_transit_stops / iso_area_km2, 2), NA_real_)
        
        freq_weighted_score <- if (!is.null(inter_transit) &&
                                   "mean_headway_min" %in% names(inter_transit) &&
                                   nrow(inter_transit) > 0 &&
                                   iso_area_km2 > 0) {
          hw <- inter_transit$mean_headway_min
          hw <- hw[!is.na(hw) & hw > 0]
          if (length(hw) > 0) round(sum(60 / hw) / iso_area_km2, 2) else NA_real_
        } else NA_real_
        
        mean_headway_iso <- if (!is.null(inter_transit) &&
                                "mean_headway_min" %in% names(inter_transit) &&
                                nrow(inter_transit) > 0) {
          round(mean(inter_transit$mean_headway_min, na.rm = TRUE), 1)
        } else NA_real_
      }
      
      sampling_density_km2 <- ifelse(iso_area_km2 > 0, round(n_records / iso_area_km2, 2), NA_real_)
      
      mean_ciscore <- if (!is.null(cenv_sf)) {
        tryCatch({
          ce_inter <- st_intersection(cenv_sf, poly_i)
          if (nrow(ce_inter) > 0) {
            ce_inter$a <- as.numeric(st_area(st_transform(ce_inter, 3857)))
            round(weighted.mean(ce_inter$CIscore, w = ce_inter$a, na.rm = TRUE), 1)
          } else NA_real_
        }, error = function(e) NA_real_)
      } else NA_real_
      
      mean_traffic_pctl <- if (!is.null(cenv_sf)) {
        tryCatch({
          ce_inter <- st_intersection(cenv_sf, poly_i)
          if (nrow(ce_inter) > 0) {
            ce_inter$a <- as.numeric(st_area(st_transform(ce_inter, 3857)))
            round(weighted.mean(ce_inter$Traffic_Pctl, w = ce_inter$a, na.rm = TRUE), 1)
          } else NA_real_
        }, error = function(e) NA_real_)
      } else NA_real_
      
      mean_ej_score <- if (!is.null(sf_ej_sf)) {
        tryCatch({
          ej_inter <- st_intersection(sf_ej_sf, poly_i)
          if (nrow(ej_inter) > 0) {
            ej_inter$a <- as.numeric(st_area(st_transform(ej_inter, 3857)))
            valid <- ej_inter[!is.na(ej_inter$score), ]
            if (nrow(valid) > 0) {
              round(weighted.mean(valid$score, w = valid$a, na.rm = TRUE), 1)
            } else NA_real_
          } else NA_real_
        }, error = function(e) NA_real_)
      } else NA_real_
      
      row_i <- data.frame(
        Mode                   = pretty_mode(as.character(poly_i$mode[[1]])),
        Time                   = as.numeric(poly_i$time[[1]]),
        IsochroneArea_km2      = iso_area_km2,
        DistToHotspot_km       = dist_hot_km,
        DistToColdspot_km      = dist_cold_km,
        EstimatedPopulation    = pop_total,
        MedianIncome           = round(w_income, 2),
        MeanNDVI               = mean_ndvi,
        GBIF_Records           = n_records,
        GBIF_Species           = n_species,
        Bird_Species           = n_birds,
        Mammal_Species         = n_mammals,
        Plant_Species          = n_plants,
        SamplingDensity_km2    = sampling_density_km2,
        Greenspace_percent     = round(gs_percent, 2),
        Transit_Stops          = n_transit_stops,
        Transit_Access_Score   = transit_access_score,
        Freq_Weighted_Score    = freq_weighted_score,
        Mean_Headway_min       = mean_headway_iso,
        Nearest_Stop_m         = nearest_stop_m,
        Nearest_Stop_Name      = nearest_stop_name,
        CalEnviro_CIscore      = mean_ciscore,
        CalEnviro_Traffic_Pctl = mean_traffic_pctl,
        SF_EJ_Score            = mean_ej_score,
        closest_greenspace     = osm_greenspace_name_global,
        closest_greenspace_dist_m = min_dist_val_global,
        stringsAsFactors       = FALSE
      )
      
      results <- rbind(results, row_i)
    }
    
    iso_union <- st_union(iso_data)
    inter_all_gbif <- safe_vect_gbif_intersection(st_as_sf(iso_union))
    union_n_species <- if (!is.null(inter_all_gbif) && "species" %in% names(inter_all_gbif) && nrow(inter_all_gbif) > 0) {
      length(unique(inter_all_gbif$species))
    } else 0
    
    attr(results, "bio_percentile") <- round(100 * ecdf(cbg_vect_sf$unique_species)(union_n_species), 1)
    
    if (!is.null(gtfs_stops_sf)) {
      sf_city_area_km2 <- 121.4
      attr(results, "city_transit_score") <- round(nrow(gtfs_stops_sf) / sf_city_area_km2, 2)
      attr(results, "mean_transit_score") <- round(mean(results$Transit_Access_Score, na.rm = TRUE), 2)
    } else {
      attr(results, "city_transit_score") <- NA_real_
      attr(results, "mean_transit_score") <- NA_real_
    }
    
    if (nrow(results) > 0) {
      closest_gs <- results |>
        filter(!is.na(closest_greenspace_dist_m)) |>
        slice_min(closest_greenspace_dist_m, n = 1)
      if (nrow(closest_gs) > 0) {
        attr(results, "closest_greenspace") <- closest_gs$closest_greenspace[1]
        attr(results, "closest_greenspace_dist_m") <- closest_gs$closest_greenspace_dist_m[1]
      } else {
        attr(results, "closest_greenspace") <- "None"
        attr(results, "closest_greenspace_dist_m") <- NA_real_
      }
    }
    
    results
  })
  
  # ---------------------------------------------------------------------------
  # Biodiversity Access Index
  # ---------------------------------------------------------------------------
  biodiversity_access_index <- reactive({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    
    ref <- city_benchmarks
    
    eq_ref_inverted <- if (length(ref$ej) > 0) max(ref$ej, na.rm = TRUE) - ref$ej else numeric(0)
    eq_obs_inverted <- ifelse(is.na(df$SF_EJ_Score), NA_real_, max(ref$ej, na.rm = TRUE) - df$SF_EJ_Score)
    
    tmp <- df |>
      mutate(
        Mobility_Access_std        = ecdf01(Transit_Access_Score, ref$transit_density),
        Biodiversity_Potential_std = ecdf01(GBIF_Species, ref$biodiversity),
        Observation_Intensity_std  = ecdf01(SamplingDensity_km2, ref$sampling),
        Environmental_Quality_std  = ecdf01(MeanNDVI, ref$ndvi),
        Equity_Context_std         = ecdf01(eq_obs_inverted, eq_ref_inverted)
      )
    
    tmp$BAI <- rowMeans(
      tmp[, c(
        "Mobility_Access_std",
        "Biodiversity_Potential_std",
        "Observation_Intensity_std",
        "Environmental_Quality_std",
        "Equity_Context_std"
      )],
      na.rm = TRUE
    )
    
    tmp
  })
  
  # ---------------------------------------------------------------------------
  # Summary outputs
  # ---------------------------------------------------------------------------
  output$dataTable <- renderDT({
    df <- socio_data()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No isochrones generated yet.")))
    
    out <- df |>
      mutate(
        MedianIncome = ifelse(is.na(MedianIncome), NA, dollar(MedianIncome)),
        MeanNDVI = round(MeanNDVI, 3)
      )
    
    DT::datatable(
      out,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$bioScoreBox <- renderUI({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    pct <- attr(df, "bio_percentile")
    lbl <- if (is.null(pct)) "N/A" else paste0(pct, "th Percentile")
    wellPanel(HTML(paste0("<h2>", lbl, "</h2><small>Union of all generated isochrones</small>")))
  })
  
  output$transitScoreBox <- renderUI({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    
    mean_score <- attr(df, "mean_transit_score")
    city_score <- attr(df, "city_transit_score")
    
    if (is.null(mean_score) || is.na(mean_score)) {
      return(wellPanel(HTML("<h3>Transit Access Score: N/A</h3>")))
    }
    
    comparison <- if (!is.null(city_score) && !is.na(city_score) && city_score > 0) {
      pct_vs_city <- round(100 * (mean_score - city_score) / city_score, 1)
      sign_label  <- if (pct_vs_city >= 0) "above" else "below"
      paste0("<br><small>", abs(pct_vs_city), "% ", sign_label,
             " SF city average (", city_score, " stops/km²)</small>")
    } else ""
    
    wellPanel(HTML(paste0("<h2>", mean_score, " stops/km²</h2>", comparison)))
  })
  
  output$biodiversityAccessIndexBox <- renderUI({
    df <- biodiversity_access_index()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    mean_index <- round(mean(df$BAI, na.rm = TRUE) * 100, 1)
    
    wellPanel(
      HTML(paste0(
        "<h2>", mean_index, "/100</h2>",
        "<small>Composite biodiversity access index</small>"
      ))
    )
  })
  
  output$closestGreenspaceUI <- renderUI({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    
    gs_name <- attr(df, "closest_greenspace")
    gs_dist <- attr(df, "closest_greenspace_dist_m")
    
    if (is.null(gs_name) || is.na(gs_name)) gs_name <- "None"
    
    tagList(
      strong("Closest Greenspace:"),
      p(gs_name),
      if (!is.null(gs_dist) && !is.na(gs_dist)) p(paste0("Distance: ", round(gs_dist, 1), " m"))
    )
  })
  
  # ---------------------------------------------------------------------------
  # GBIF tab
  # ---------------------------------------------------------------------------
  output$classTable <- renderDT({
    if (is.null(gbif_tab)) {
      return(DT::datatable(data.frame(Message = "GBIF parquet connection not available.")))
    }
    
    q <- gbif_tab
    
    if (input$class_filter != "All") {
      q <- q |> filter(class == input$class_filter)
    }
    if (input$family_filter != "All") {
      q <- q |> filter(family == input$family_filter)
    }
    
    species_counts <- tryCatch({
      q |>
        group_by(species) |>
        summarise(
          n_records = n(),
          .groups = "drop"
        ) |>
        arrange(desc(n_records)) |>
        collect()
    }, error = function(e) NULL)
    
    if (is.null(species_counts) || nrow(species_counts) == 0) {
      return(DT::datatable(data.frame(Message = "No records for that combination.")))
    }
    
    DT::datatable(species_counts, options = list(pageLength = 10), rownames = FALSE)
  })
  
  filtered_data <- reactive({
    data <- cbg_vect_sf
    if ("class" %in% names(data) && input$class_filter != "All") {
      data <- data[data$class == input$class_filter, ]
    }
    if ("family" %in% names(data) && input$family_filter != "All") {
      data <- data[data$family == input$family_filter, ]
    }
    data
  })
  
  output$obsVsSpeciesPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) {
      plot.new()
      title("No data available for selected filters.")
      return(NULL)
    }
    
    ggplot(data, aes(x = log(n_observations + 1), y = log(unique_species + 1))) +
      geom_point(color = "blue", alpha = 0.6) +
      labs(
        x = "Log(Number of Observations + 1)",
        y = "Log(Species Richness + 1)",
        title = "Data Availability vs. Species Richness"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  })
  
  # ---------------------------------------------------------------------------
  # Main plots
  # ---------------------------------------------------------------------------
  output$bioSocPlot <- renderPlot({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    
    df_plot <- df |>
      mutate(IsoLabel = paste0(Mode, "\n", Time, " min"))
    
    p <- ggplot(df_plot, aes(x = IsoLabel, y = GBIF_Species, fill = Mode)) +
      geom_col(alpha = 0.85) +
      scale_fill_manual(values = mode_palette, name = "Mode") +
      labs(
        x = "Isochrone (Mode — Time)",
        y = "Unique Species (bars) | Population × 1,000",
        title = "Biodiversity & Socioeconomic Summary"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        legend.position = "bottom"
      )
    
    if (nrow(df_plot) > 1) {
      p <- p +
        geom_line(aes(y = EstimatedPopulation / 1000, group = 1),
                  color = "black", linewidth = 1, linetype = "dashed") +
        geom_point(aes(y = EstimatedPopulation / 1000),
                   color = "black", size = 3, shape = 21, fill = "white")
    } else {
      p <- p +
        geom_point(aes(y = EstimatedPopulation / 1000),
                   color = "black", size = 3, shape = 21, fill = "white")
    }
    
    p
  })
  
  output$collectionPlot <- renderPlot({
    iso_data <- isochrones_data()
    if (is.null(iso_data) || nrow(iso_data) == 0) {
      plot.new()
      title("No isochrone generated.")
      return(NULL)
    }
    
    if (!is.null(gbif_tab)) {
      iso_union <- st_union(iso_data)
      union_wkt <- st_as_text(st_geometry(iso_union)[[1]])
      
      df_code <- tryCatch({
        gbif_tab |>
          filter(sql(paste0("ST_Intersects(ST_GeomFromText(geom_wkt), ST_GeomFromText('", union_wkt, "'))"))) |>
          group_by(institutionCode) |>
          summarise(count = n(), .groups = "drop") |>
          arrange(desc(count)) |>
          collect() |>
          mutate(truncatedCode = substr(institutionCode, 1, 5))
      }, error = function(e) NULL)
      
      if (!is.null(df_code) && nrow(df_code) > 0) {
        return(
          ggplot(df_code, aes(x = reorder(truncatedCode, -count), y = count)) +
            geom_bar(stat = "identity", fill = "darkorange", alpha = 0.7) +
            labs(
              x = "Institution Code (Truncated)",
              y = "Number of Records",
              title = "GBIF Records by Institution Code (Isochrone Union)"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
            )
        )
      }
    }
    
    inter_gbif <- safe_vect_gbif_intersection(st_as_sf(st_union(iso_data)))
    if (is.null(inter_gbif) || nrow(inter_gbif) == 0 || !("institutionCode" %in% names(inter_gbif))) {
      plot.new()
      title("No GBIF records found in this isochrone.")
      return(NULL)
    }
    
    df_code <- inter_gbif |>
      st_drop_geometry() |>
      group_by(institutionCode) |>
      summarise(count = n(), .groups = "drop") |>
      arrange(desc(count)) |>
      mutate(truncatedCode = substr(institutionCode, 1, 5))
    
    ggplot(df_code, aes(x = reorder(truncatedCode, -count), y = count)) +
      geom_bar(stat = "identity", fill = "darkorange", alpha = 0.7) +
      labs(
        x = "Institution Code (Truncated)",
        y = "Number of Records",
        title = "GBIF Records by Institution Code (Isochrone Union)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      )
  })
  
  output$transitMetricsPlot <- renderPlot({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    
    df_long <- df |>
      mutate(IsoLabel = paste0(Mode, "\n", Time, " min")) |>
      select(IsoLabel, Mode, GBIF_Records, GBIF_Species, Transit_Stops) |>
      pivot_longer(
        cols = c(GBIF_Records, GBIF_Species, Transit_Stops),
        names_to = "Metric",
        values_to = "Value"
      ) |>
      mutate(Metric = recode(
        Metric,
        "GBIF_Records"  = "GBIF Records",
        "GBIF_Species"  = "Unique Species",
        "Transit_Stops" = "Muni Stops Within"
      ))
    
    ggplot(df_long, aes(x = IsoLabel, y = Value, fill = Mode)) +
      geom_col(alpha = 0.85, width = 0.7) +
      geom_text(aes(label = Value), vjust = -0.4, size = 3.2, color = "grey30") +
      scale_fill_manual(values = mode_palette, name = "Mode") +
      facet_wrap(~Metric, scales = "free_y", ncol = 3) +
      labs(
        x = NULL,
        y = "Count",
        title = "Biodiversity & Transit Metrics by Transport Mode"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "bottom"
      )
  })
  
  output$radarPlot <- renderPlot({
    df <- biodiversity_access_index()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    radar_df <- df |>
      mutate(
        ModeTime = paste0(Mode, "_", Time, "m"),
        `Transit Stops Density` = Mobility_Access_std,
        `Unique Species`        = Biodiversity_Potential_std,
        `Sampling Density`      = Observation_Intensity_std,
        `Mean NDVI`             = Environmental_Quality_std,
        `Lower EJ Burden`       = Equity_Context_std
      ) |>
      select(
        ModeTime,
        `Transit Stops Density`,
        `Unique Species`,
        `Sampling Density`,
        `Mean NDVI`,
        `Lower EJ Burden`
      )
    
    radar_mat <- as.data.frame(radar_df[, -1])
    rownames(radar_mat) <- radar_df$ModeTime
    
    radar_mat <- rbind(
      rep(1, ncol(radar_mat)),
      rep(0, ncol(radar_mat)),
      radar_mat
    )
    
    labels <- rownames(radar_mat)[-(1:2)]
    line_cols <- mode_palette[gsub("_(.*)$", "", labels)]
    line_cols[is.na(line_cols)] <- "#666666"
    
    fmsb::radarchart(
      radar_mat,
      axistype = 1,
      pcol = line_cols,
      plwd = 2,
      plty = 1,
      cglcol = "grey80",
      cglty = 1,
      cglwd = 0.8,
      axislabcol = "grey40",
      vlcex = 1,
      title = "Biodiversity Access Index Profile"
    )
    
    legend(
      "topright",
      legend = labels,
      col = line_cols,
      lty = 1,
      lwd = 2,
      cex = 0.75,
      bty = "n"
    )
  })
  
  # ---------------------------------------------------------------------------
  # Community science tab
  # ---------------------------------------------------------------------------
  community_sf <- safe_partner_orgs()
  
  output$communityMap <- renderLeaflet({
    if (is.null(community_sf) || nrow(community_sf) == 0) {
      return(
        leaflet() |>
          addTiles() |>
          setView(lng = -122.4194, lat = 37.7749, zoom = 12)
      )
    }
    
    name_col <- intersect(c("name", "Name", "organization", "organization_name", "org_name"), names(community_sf))
    popup_col <- if (length(name_col) > 0) name_col[1] else names(community_sf)[1]
    
    leaflet(community_sf) |>
      addTiles() |>
      addCircleMarkers(
        radius = 6,
        color = "#2e8b57",
        fillOpacity = 0.9,
        label = ~as.character(.data[[popup_col]]),
        popup = ~as.character(.data[[popup_col]])
      ) |>
      fitBounds(
        lng1 = min(st_coordinates(st_centroid(st_union(community_sf)))[,1], na.rm = TRUE) - 0.05,
        lat1 = min(st_coordinates(st_centroid(st_union(community_sf)))[,2], na.rm = TRUE) - 0.05,
        lng2 = max(st_coordinates(st_centroid(st_union(community_sf)))[,1], na.rm = TRUE) + 0.05,
        lat2 = max(st_coordinates(st_centroid(st_union(community_sf)))[,2], na.rm = TRUE) + 0.05
      )
  })
  
  output$communityTable <- renderDT({
    if (is.null(community_sf) || nrow(community_sf) == 0) {
      return(DT::datatable(data.frame(Message = "No partner/community organization layer found in setup.R.")))
    }
    
    DT::datatable(
      st_drop_geometry(community_sf),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
} # end server

# =============================================================================
# RUN APP
# =============================================================================
shinyApp(ui, server)