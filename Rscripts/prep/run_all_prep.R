# ============================================================================
# Prep: Run full data build → data/output
# ============================================================================
# Working directory must be the project root (folder containing data/ and Rscripts/).
#
# Order (fast / network steps first; heavy raster build last):
#   1. GBIF .Rdata → parquet
#   2. CBG × greenspace coverage CSV
#   3. Equity layers + RSF staging + GTFS bundle
#   4. Greenspace distance rasters (DuckDB; slow)
#
# Then upload files from data/output/ to the HuggingFace dataset (manually or
# see comments in upload_to_huggingface.R).

root_ok <- file.exists("Rscripts/prep/run_all_prep.R") && file.exists("data")
if (!root_ok) {
  stop("Set working directory to the project root before sourcing this file.")
}

source("Rscripts/prep/convert_gbif_to_parquet.R")
source("Rscripts/prep/build_cbg_greenspace_coverage.R")
source("Rscripts/prep/build_equity_layers.R")
source("Rscripts/prep/stage_static_assets.R")
source("Rscripts/prep/implement_optimizations.R")
source("Rscripts/prep/making-greenspace-raster.R")
