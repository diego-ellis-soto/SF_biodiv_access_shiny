# ============================================================================
# Prep: sf-gbif.parquet + CBG + NDVI raster → gbif_census_ndvi_anno.parquet
# ============================================================================
# Inputs: data/output/sf-gbif.parquet, data/source/cbg_vect_sf.Rdata,
#         data/source/SF_EastBay_NDVI_Sentinel_10.tif
# Run once after sf-gbif.parquet exists (Rscripts/prep/sf-gbif-parquet.R).

library(sf)
library(terra)
library(arrow)
library(dplyr)

# ----------------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------------

gbif_in <- "data/output/sf-gbif.parquet"
cbg_rdata <- "data/source/cbg_vect_sf.Rdata"
ndvi_tif  <- "data/source/SF_EastBay_NDVI_Sentinel_10.tif"
out_dir   <- "data/output"
out_file  <- file.path(out_dir, "gbif_census_ndvi_anno.parquet")

# ----------------------------------------------------------------------------
# Load
# ----------------------------------------------------------------------------

gbif <- arrow::read_parquet(gbif_in, as_data_frame = TRUE)
load(cbg_rdata)
ndvi <- terra::rast(ndvi_tif)

cbg_acs <- cbg_vect_sf |>
  select(GEOID, medincE, popE, housingE)

# ----------------------------------------------------------------------------
# Points → CBG join → NDVI extract
# ----------------------------------------------------------------------------

pts <- gbif |>
  filter(!is.na(decimallongitude), !is.na(decimallatitude)) |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326, remove = FALSE)

cbg_join <- st_join(pts, cbg_acs, join = st_within, left = TRUE)

# terra accepts sf; layer name from the raster (here: NDVI)
cbg_join$ndvi_sentinel <- terra::extract(ndvi, cbg_join, ID = FALSE)$NDVI

# ----------------------------------------------------------------------------
# App / DuckDB columns
# ----------------------------------------------------------------------------

out <- cbg_join |>
  mutate(
    geom_wkt  = st_as_text(geometry),
    longitude = decimallongitude,
    latitude  = decimallatitude
  ) |>
  st_drop_geometry() |>
  rename(
    institutionCode = institutioncode,
    coordinateUncertaintyInMeters = coordinateuncertaintyinmeters
  )

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(out, out_file)
