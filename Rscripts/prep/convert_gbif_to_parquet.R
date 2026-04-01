# ============================================================================
# Prep: GBIF .Rdata → data/output/gbif_census_ndvi_anno.parquet
# ============================================================================
# Resolves input in order: data/source/*.Rdata → data/intermediate (HF download)
# Output is always under data/output/ for upload to HuggingFace.

library(sf)
library(dplyr)
library(arrow)
library(glue)

HF_BASE <- "https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main"

src_candidates <- c(
  "data/source/gbif_census_ndvi_anno.Rdata",
  "data/intermediate/gbif_census_ndvi_anno.Rdata"
)
gbif_file <- src_candidates[file.exists(src_candidates)][1]

if (is.na(gbif_file)) {
  dir.create("data/intermediate", recursive = TRUE, showWarnings = FALSE)
  gbif_file <- "data/intermediate/gbif_census_ndvi_anno.Rdata"
  download.file(glue("{HF_BASE}/gbif_census_ndvi_anno.Rdata"), gbif_file, mode = "wb", quiet = TRUE)
}

load(gbif_file)

gbif_df <- sf_gbif |>
  mutate(
    geom_wkt  = st_as_text(geometry),
    longitude = st_coordinates(geometry)[, 1],
    latitude  = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

required_cols <- c(
  "species", "class", "family", "GEOID", "medincE",
  "ndvi_sentinel", "institutionCode", "geom_wkt",
  "longitude", "latitude"
)
missing_cols <- setdiff(required_cols, names(gbif_df))
if (length(missing_cols) > 0) {
  stop(glue("Missing required columns: {toString(missing_cols)}"))
}

output_dir <- "data/output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, "gbif_census_ndvi_anno.parquet")
arrow::write_parquet(gbif_df, output_file)
