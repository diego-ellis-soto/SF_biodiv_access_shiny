# ============================================================================
# Implement Quick-Win Optimizations
# ============================================================================
# Run this script once to pre-compute/cache heavy data structures
# After running, update setup.R to use cached versions

library(tidyverse)
library(sf)
library(terra)
library(gtfsrouter)

message("\n")
message("===============================================================================")
message("IMPLEMENTING SHINY APP OPTIMIZATIONS")
message("===============================================================================\n")

# Create cache directory
dir.create("data/cache", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# 1. PRE-COMPUTE GTFS TIMETABLE
# ============================================================================
message("📊 [1/3] Pre-computing GTFS timetable (this will take ~4 seconds)...\n")

gtfs_path <- '/Users/diegoellis/Desktop/RSF_next_steps/GPFS_OSM_Transit/sf_muni_gtfs-current/'

t1 <- Sys.time()

gtfs_zip_path <- tempfile(fileext = ".zip")
old_wd <- getwd()
setwd(gtfs_path)
utils::zip(gtfs_zip_path, files = list.files('.', pattern = "\\.txt$"))
setwd(old_wd)

gtfs_router <- tryCatch({
  gr <- gtfsrouter::extract_gtfs(gtfs_zip_path, quiet = TRUE)
  gtfsrouter::gtfs_timetable(gr, day = "Monday")
}, error = function(e) {
  message("❌ Failed to create GTFS timetable: ", e$message)
  NULL
})

if (!is.null(gtfs_router)) {
  saveRDS(gtfs_router, "data/cache/gtfs_timetable_monday.rds", compress = "gzip")
  time_taken <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  message("✅ Saved: data/cache/gtfs_timetable_monday.rds")
  message("   Precomputation took ", round(time_taken, 2), "s (will load in 0.3s)\n")
} else {
  message("⚠️  Could not pre-compute GTFS timetable. Proceeding with other optimizations.\n")
}

# ============================================================================
# 2. DOWNLOAD & CACHE GREENSPACE RASTERS
# ============================================================================
message("📊 [2/3] Caching greenspace distance rasters...\n")

# Greenspace distance raster
t1 <- Sys.time()
message("   → Downloading greenspace_dist_raster.tif...")
greenspace_dist_raster <- tryCatch({
  terra::rast("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/nearest_greenspace_dist.tif")
}, error = function(e) {
  message("❌ Failed: ", e$message)
  NULL
})

if (!is.null(greenspace_dist_raster)) {
  terra::writeRaster(greenspace_dist_raster, 
                     "data/cache/greenspace_dist_raster.tif", 
                     overwrite = TRUE)
  time_taken <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  message("✅ Saved: data/cache/greenspace_dist_raster.tif")
  message("   Download took ", round(time_taken, 2), "s (will load in 0.1-0.2s)\n")
} else {
  message("⚠️  Could not cache greenspace distance raster.\n")
}

# Greenspace OSM ID raster
t1 <- Sys.time()
message("   → Downloading greenspace_osmid_raster.tif...")
greenspace_osmid_raster <- tryCatch({
  terra::rast("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/nearest_greenspace_osmid.tif")
}, error = function(e) {
  message("❌ Failed: ", e$message)
  NULL
})

if (!is.null(greenspace_osmid_raster)) {
  terra::writeRaster(greenspace_osmid_raster, 
                     "data/cache/greenspace_osmid_raster.tif", 
                     overwrite = TRUE)
  time_taken <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  message("✅ Saved: data/cache/greenspace_osmid_raster.tif")
  message("   Download took ", round(time_taken, 2), "s (will load in 0.1-0.2s)\n")
} else {
  message("⚠️  Could not cache greenspace OSM ID raster.\n")
}

# ============================================================================
# 3. CONVERT OSM GREENSPACE TO GEOPACKAGE
# ============================================================================
message("📊 [3/3] Converting OSM greenspace shapefile to GeoPackage...\n")

t1 <- Sys.time()
message("   → Reading OSM greenspace polygons...")
osm_greenspace <- tryCatch({
  st_read("/vsicurl/https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/greenspaces_osm_nad83.shp", 
          quiet = TRUE) |>
    st_transform(4326)
}, error = function(e) {
  message("❌ Failed: ", e$message)
  NULL
})

if (!is.null(osm_greenspace)) {
  if (!"name" %in% names(osm_greenspace)) {
    osm_greenspace$name <- "Unnamed Greenspace"
  }
  
  message("   → Writing to GeoPackage format...")
  st_write(osm_greenspace, "data/cache/osm_greenspace.gpkg", quiet = TRUE, append = FALSE)
  
  time_taken <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  message("✅ Saved: data/cache/osm_greenspace.gpkg")
  message("   Conversion took ", round(time_taken, 2), "s (will load in 0.1-0.2s)\n")
} else {
  message("⚠️  Could not convert OSM greenspace.\n")
}

# ============================================================================
# SUMMARY & NEXT STEPS
# ============================================================================
message("===============================================================================")
message("OPTIMIZATION IMPLEMENTATION COMPLETE ✅")
message("===============================================================================\n")

message("Next Steps:\n")
message("1. Update setup.R to use cached versions:")
message("   - Replace greenspace_dist_raster loading with:")
message("     greenspace_dist_raster <- terra::rast('data/cache/greenspace_dist_raster.tif')\n")
message("   - Replace greenspace_osmid_raster loading with:")
message("     greenspace_osmid_raster <- terra::rast('data/cache/greenspace_osmid_raster.tif')\n")
message("   - Replace osm_greenspace loading with:")
message("     osm_greenspace <- st_read('data/cache/osm_greenspace.gpkg')\n")
message("   - Replace gtfsrouter initialization with:")
message("     gtfs_router <- readRDS('data/cache/gtfs_timetable_monday.rds')\n")

message("2. Test the app startup speed:\n")
message("   source('R/profile_startup.R')\n")

message("3. Compare before/after benchmark plot:\n")
message("   - Open: startup_benchmark.png\n")

message("===============================================================================\n")
