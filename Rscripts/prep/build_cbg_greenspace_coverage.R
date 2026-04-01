# ============================================================================
# Prep: Per-CBG greenspace overlap → data/output (CSV)
# ============================================================================
# Loads CBG polygons + OSM greenspace (HuggingFace), intersects in EPSG:3857,
# writes GEOID, greenspace_m2, cbg_area_m2 for the app join.

library(tidyverse)
library(sf)
library(glue)

HF_BASE <- "https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access/resolve/main"
out_dir <- "data/output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cbg_tmp <- tempfile(fileext = ".Rdata")
download.file(glue("{HF_BASE}/cbg_vect_sf.Rdata"), cbg_tmp, mode = "wb", quiet = TRUE)
load(cbg_tmp)
unlink(cbg_tmp)

osm_greenspace <- st_read(glue("/vsicurl/{HF_BASE}/greenspaces_osm_nad83.shp"), quiet = TRUE) |>
  st_transform(4326) |>
  st_make_valid()

cbg_proj <- st_transform(cbg_vect_sf[, "GEOID"], 3857) |>
  mutate(cbg_area_m2 = as.numeric(st_area(geometry)))
gs_proj  <- st_transform(osm_greenspace, 3857) |> st_make_valid()
gs_union <- st_union(gs_proj)

cbg_gs_inter <- st_intersection(cbg_proj, gs_union)
result <- cbg_gs_inter |>
  mutate(greenspace_m2 = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarise(greenspace_m2 = sum(greenspace_m2), .groups = "drop") |>
  right_join(cbg_proj |> st_drop_geometry() |> select(GEOID, cbg_area_m2), by = "GEOID") |>
  mutate(greenspace_m2 = tidyr::replace_na(greenspace_m2, 0)) |>
  mutate(GEOID = as.character(GEOID))

readr::write_csv(result, file.path(out_dir, "cbg_greenspace_coverage.csv"))
