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

# ============================================================================
# API Keys
# ============================================================================
mapbox_token <- "pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjbHc3NmI0cDMxYzhyMmt0OXBiYnltMjVtIn0.Thtu6WqIhOfin6AykskM2g" 

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
# DuckDB can read parquet files directly from URLs
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

# -- Hotspots/Coldspots (read directly from URL via GDAL virtual file system)
biodiv_hotspots <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/hotspots.shp", quiet = TRUE) |>
  st_transform(4326)

biodiv_coldspots <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/coldspots.shp", quiet = TRUE) |>
  st_transform(4326)

# -- RSF Program Projects (read directly from URL via GDAL virtual file system)
rsf_projects <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/RSF_Program_Projects_polygons.gpkg", quiet = TRUE) |> 
  st_transform(4326)
