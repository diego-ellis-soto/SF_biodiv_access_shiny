# Here we make 2 raster layers related to the nearest greenspaces in San Francisco
# at 30m resolution
# - 1) The distance to the nearest greenspace
# - 2) The GEOID of the nearest greenspace (for looking up the polygon)

# Load libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(duckdb)
library(duckdbfs)
library(glue)
library(tictoc)

# Connect to DuckDB
tcon <- dbConnect(duckdb::duckdb())

# Load spatial extension
tcon |> dbExecute("
  INSTALL spatial;
  LOAD spatial;
  SET memory_limit = '200GB';
  SET preserve_insertion_order = false;
  SET threads TO 8;
")

st_crs(st_read("data/source/Greenspaces_osm_nad93/greenspaces_osm_nad83.shp"))$epsg
# Read greenspace data
greenspaces <- st_read("data/source/Greenspaces_osm_nad93/greenspaces_osm_nad83.shp") %>%
  st_transform(3310)

# San Francisco county minus farallon islands
sf <- st_read("data/source/CA_Counties/CA_Counties_TIGER2016.shp") %>%
  filter(NAME == "San Francisco") |>
  st_transform(3310) |>
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) |>
  slice_max(area)


empty.sr <- rast("data/source/slope.tif")


template.sr <- empty.sr %>%
  # Give every cell an ID
  mutate(cell_id = 1:ncell(.)) |>
  # But filter to everything that isn't NA for slope values
  filter(!is.na(prcnt_slope30)) |>
  crop(sf |> st_transform(4326), mask = T) |>
  crop(ext(c(-123, -122, 37.65, 37.85))) |>
  trim()

# Convert template raster to points (centroids) for spatial joins
template_pts <- template.sr |>
  as.points(na.rm = TRUE) |>
  st_as_sf() |>
  st_transform(4326) # Transform back to WGS84 for consistency

# Create directories if they don't exist
dir.create("data/intermediate", showWarnings = FALSE)
dir.create("data/output", showWarnings = FALSE)

# Write template to file for DuckDB
template_pts %>%
  write_sf("data/intermediate/template_pts.gpkg")

# Load data into DuckDB and set up geometries
tcon %>% dbExecute("
CREATE OR REPLACE TABLE greenspace_geo
AS
SELECT * EXCLUDE geom,
  ST_TRANSFORM(geom, 'EPSG:4269', 'EPSG:3310', always_xy := true) AS geom3310,
  ST_Centroid(geom) AS centroid_geom3310,
  ST_SimplifyPreserveTopology(ST_TRANSFORM(geom, 'EPSG:4269', 'EPSG:3310', always_xy := true), 10) AS simple_geom3310
FROM ST_READ('data/source/Greenspaces_osm_nad93/greenspaces_osm_nad83.shp');

CREATE INDEX idx_grn_simple_geom ON greenspace_geo USING RTREE (simple_geom3310);
CREATE INDEX idx_grn_centroid_geom ON greenspace_geo USING RTREE (centroid_geom3310);
")

# Load template grid into DuckDB
tcon %>% dbExecute("
CREATE OR REPLACE TABLE template_grid_geo
AS
SELECT
  cell_id,
  geom,
  ST_TRANSFORM(geom, 'EPSG:4326', 'EPSG:3310', always_xy := true) AS geom3310
FROM ST_Read('data/intermediate/template_pts.gpkg');

CREATE INDEX idx_template_geom ON template_grid_geo USING RTREE (geom3310);
CREATE INDEX idx_template_cell_id ON template_grid_geo (cell_id);
")


# Calculate nearest greenspace distances
nn_query <- glue("
CREATE OR REPLACE TABLE grn_distance_complete AS
WITH distances AS (
  SELECT
    template.cell_id,
    template.geom AS template_geom,
    ST_AsText(template.geom) AS geom_wkt,
    GEOID,
    ST_Distance(template.geom3310, green.simple_geom3310) AS distance_meters
  FROM template_grid_geo AS template, greenspace_geo AS green
)
SELECT
  cell_id,
  template_geom,
  geom_wkt,
  MIN(distance_meters) AS distance_to_greenspace_meters,
  arg_min(GEOID, distance_meters) AS nearest_greenspace_geoid
FROM distances
GROUP BY cell_id, template_geom, geom_wkt;
")

cat("Calculating nearest greenspace distances...\n")
tic()
tcon %>% dbExecute(nn_query)
toc()
# Takes 138 seconds

# Get results from database
sf_dist_complete <- tcon %>%
  tbl("grn_distance_complete") %>%
  select(cell_id, distance_to_greenspace_meters, nearest_greenspace_geoid) %>%
  collect()

# Create empty raster based on template
empty_raster <- empty.sr
values(empty_raster) <- NA

# Join results back to preserve exact cell alignment
aligned_results <- tibble(cells = 1:ncell(empty.sr)) %>%
  left_join(sf_dist_complete %>% rename(cells = cell_id), by = "cells")

# Create distance raster
greenspace_nearest_dist <- empty_raster
values(greenspace_nearest_dist) <- aligned_results$distance_to_greenspace_meters
names(greenspace_nearest_dist) <- "greenspace_nearest_dist"
cropped_greenspace_nearest_dist <- greenspace_nearest_dist |>
  crop(sf |> st_transform(4326), mask = T) |>
  crop(ext(c(-123, -122, 37.65, 37.85))) |>
  trim()

# Create nearest greenspace ID raster
greenspace_nearest_geoid <- empty_raster
values(greenspace_nearest_geoid) <- as.numeric(aligned_results$nearest_greenspace_geoid)
names(greenspace_nearest_geoid) <- "greenspace_nearest_geoid"
cropped_greenspace_nearest_geoid <- greenspace_nearest_geoid |>
  crop(sf |> st_transform(4326), mask = T) |>
  crop(ext(c(-123, -122, 37.65, 37.85))) |>
  trim()

plot(cropped_greenspace_nearest_dist)
plot(cropped_greenspace_nearest_geoid)

# Write rasters
writeRaster(cropped_greenspace_nearest_dist, "data/output/nearest_greenspace_dist.tif", overwrite = TRUE)
writeRaster(cropped_greenspace_nearest_geoid, "data/output/nearest_greenspace_geoid.tif", overwrite = TRUE)
