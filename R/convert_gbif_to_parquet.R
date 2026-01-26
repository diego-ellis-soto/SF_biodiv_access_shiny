# ============================================================================
# Convert GBIF Data to Parquet Format
# ============================================================================
# This script converts the GBIF .Rdata file to a parquet file with WKT geometry
# for efficient querying with DuckDB spatial functions
#
# Run this script once to create the parquet file:
#   source("R/convert_gbif_to_parquet.R")

library(sf)
library(dplyr)
library(arrow)

# ============================================================================
# Load GBIF Data
# ============================================================================
cache_dir <- "data/cached"
gbif_file <- file.path(cache_dir, "gbif_census_ndvi_anno.Rdata")

if (!file.exists(gbif_file)) {
  print("Downloading GBIF data from HuggingFace...")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  download.file(
    "https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main/gbif_census_ndvi_anno.Rdata",
    gbif_file,
    mode = "wb"
  )
}

print("Loading GBIF .Rdata file...")
load(gbif_file)

# ============================================================================
# Convert to Parquet with WKT Geometry
# ============================================================================
print("Converting to parquet format...")

gbif_df <- sf_gbif |>
  mutate(
    # Add WKT geometry column for DuckDB spatial queries
    geom_wkt = st_as_text(geometry),
    # Extract coordinates for convenience
    longitude = st_coordinates(geometry)[,1],
    latitude = st_coordinates(geometry)[,2]
  ) |>
  # Drop sf geometry to save as regular data.frame
  st_drop_geometry()

# Verify we have the key columns
required_cols <- c("species", "class", "family", "GEOID", "medincE", 
                   "ndvi_sentinel", "institutionCode", "geom_wkt", 
                   "longitude", "latitude")
missing_cols <- setdiff(required_cols, names(gbif_df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# ============================================================================
# Write to Parquet
# ============================================================================
output_dir <- "data/output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, "gbif_census_ndvi_anno.parquet")

print(paste("Writing to", output_file, "..."))
write_parquet(gbif_df, output_file)

# ============================================================================
# Summary
# ============================================================================
print("✓ Conversion complete!")
print(paste("  Rows:", nrow(gbif_df)))
print(paste("  Columns:", ncol(gbif_df)))
print(paste("  File size:", round(file.size(output_file) / 1024^2, 2), "MB"))
print(paste("  Location:", output_file))
