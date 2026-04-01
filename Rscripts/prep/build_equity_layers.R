# ============================================================================
# Prep: CalEnviroScreen + SF EJ → GeoPackages in data/output
# ============================================================================
# Reads proprietary / local inputs from data/source/, writes small derived layers
# for HuggingFace upload.

library(sf)
library(glue)

out_dir <- "data/output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- CalEnviroScreen (SF tracts only) -----------------------------------------
calenviro_path <- "data/source/calenviroscreen40gdb_F_2021.gdb"
calenviro_out  <- file.path(out_dir, "calenviro_sf.gpkg")

if (file.exists(calenviro_path)) {
  sf::st_read(calenviro_path, quiet = TRUE) |>
    dplyr::filter(grepl("san francisco", County, ignore.case = TRUE), !is.na(CIscore)) |>
    dplyr::select(
      Tract, CIscore, CIscoreP,
      PM2_5, PM2_5_Pctl, Traffic, Traffic_Pctl,
      Poverty, Poverty_Pctl, HousBurd, HousBurd_Pctl,
      County
    ) |>
    sf::st_transform(4326) |>
    sf::st_make_valid() |>
    sf::st_write(calenviro_out, delete_dsn = TRUE, quiet = TRUE)
} else {
  warning(glue("Skipping CalEnviroScreen — not found: {calenviro_path}"))
}

# --- SF Environmental Justice Communities -------------------------------------
sf_ej_shp <- "data/source/San Francisco Environmental Justice Communities Map_20260401/geo_export_e303e420-19f7-4166-9736-c1dbeda5b82e.shp"
sf_ej_out <- file.path(out_dir, "sf_ej_communities.gpkg")

if (file.exists(sf_ej_shp)) {
  sf::st_read(sf_ej_shp, quiet = TRUE) |>
    sf::st_transform(4326) |>
    sf::st_make_valid() |>
    sf::st_write(sf_ej_out, delete_dsn = TRUE, quiet = TRUE)
} else {
  warning(glue("Skipping SF EJ layer — not found: {sf_ej_shp}"))
}
