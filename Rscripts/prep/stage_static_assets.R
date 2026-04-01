# ============================================================================
# Prep: Copy static inputs that ship on HuggingFace into data/output
# ============================================================================
# RSF polygons: no transformation — duplicate into output for upload manifest.

library(glue)

out_dir <- "data/output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rsf_src <- "data/source/RSF_Program_Projects_polygons.gpkg"
rsf_dst <- file.path(out_dir, "RSF_Program_Projects_polygons.gpkg")

if (file.exists(rsf_src)) {
  file.copy(rsf_src, rsf_dst, overwrite = TRUE)
} else {
  warning(glue("RSF gpkg not found: {rsf_src}"))
}
