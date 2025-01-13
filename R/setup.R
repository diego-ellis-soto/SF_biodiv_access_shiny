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
mapbox_token <- "pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjbHc3NmI0cDMxYzhyMmt0OXBiYnltMjVtIn0.Thtu6WqIhOfin6AykskM2g" 
# mb_access_token(mapbox_token, install = FALSE)

# ------------------------------------------------
# 2) Load Data
# ------------------------------------------------
# -- Greenspace
getwd()
osm_greenspace <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/greenspaces_osm_nad83.shp", quiet = TRUE) %>%
  st_transform(4326)
if (!"name" %in% names(osm_greenspace)) {
  osm_greenspace$name <- "Unnamed Greenspace"
}

# -- NDVI Raster
ndvi <- terra::rast("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/SF_EastBay_NDVI_Sentinel_10.tif")

# -- GBIF data
# Load what is basically inter_gbif !!!!! 
# load("data/sf_gbif.Rdata")  # => sf_gbif
download.file('https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/gbif_census_ndvi_anno.Rdata', '/tmp/gbif_census_ndvi_anno.Rdata')
load('/tmp/gbif_census_ndvi_anno.Rdata')
vect_gbif <- vect(sf_gbif)
# -- Precomputed CBG data
download.file('https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/cbg_vect_sf.Rdata', '/tmp/cbg_vect_sf.Rdata')
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

# -- Hotspots/Coldspots
biodiv_hotspots  <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/hotspots.shp",  quiet = TRUE) %>% st_transform(4326)
biodiv_coldspots <- st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/coldspots.shp", quiet = TRUE) %>% st_transform(4326)


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
