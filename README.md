---
title: Sf Biodiv Access Shiny
emoji: 📚
colorFrom: blue
colorTo: yellow
sdk: docker
pinned: false
---

# SF Biodiversity Access Decision Support Tool

<img src="www/Combined_logos.png" width="60%">

An interactive Shiny app built for the **Reimagining San Francisco (RSF) Initiative** to explore how equitably San Francisco residents can access urban biodiversity across different transportation modes and socioeconomic contexts.

---

## What the App Does

Users select a location anywhere in San Francisco — either by clicking the map or geocoding an address — and choose one or more transportation modes and travel-time thresholds (5, 10, or 15 minutes). The app then:

1. **Generates travel-time isochrones** — polygons representing the area reachable within the chosen time budget — using Mapbox (driving, walking, cycling, traffic-aware driving) and SF Muni GTFS data (transit, walk + transit).
2. **Summarizes biodiversity** within each isochrone: GBIF occurrence records, unique species richness, and breakdowns by taxonomic group (birds, mammals, plants).
3. **Summarizes greenspace access**: OSM greenspace coverage (%), distance to nearest greenspace via raster, and NDVI from Sentinel-2.
4. **Summarizes socioeconomic and environmental justice context**: area-weighted median income and population from ACS Census block groups, CalEnviroScreen Cumulative Impact scores, and SF Environmental Justice community burden scores.
5. **Computes the Biodiversity Access Index (BAI)** — a composite score benchmarked against citywide empirical distributions — and displays it as a spider/radar plot across five dimensions: mobility access, biodiversity potential, observation intensity, environmental quality, and equity context.
6. **Displays partner RSF Program Projects** as a toggleable map layer.

---

## App Tabs

| Tab | Description |
|-----|-------------|
| **Isochrone Explorer** | Interactive map, isochrone generation, BAI spider plot, summary table, and metric plots |
| **GBIF Summaries** | Filter GBIF records by taxonomic class and family within the isochrone; species richness vs. sampling effort plot |
| **Community Science** | Map and table of partner community organizations |
| **About** | Full methodology, data sources, transport mode descriptions, and BAI explanation |

---

## Transportation Modes

| Mode | Engine | Notes |
|------|--------|-------|
| Driving | Mapbox Navigation API | Free-flow road network |
| Walking | Mapbox Navigation API | Pedestrian paths and crossings |
| Cycling | Mapbox Navigation API | Dedicated cycle lanes where available |
| Driving with Traffic | Mapbox Navigation API | Real-time + historical congestion |
| Transit (GTFS) | gtfsrouter | SF Muni timetable-based stop-to-stop routing |
| Walk + Transit (Muni) | Mapbox + gtfsrouter | First-mile walk + Muni ride + last-mile walk, all within total time budget |

---

## Map Layers

All layers are toggleable in the map's layer control panel:

- **Income** — Median household income per census block group (ACS 5-yr)
- **Greenspace** — OSM parks and green areas
- **Greenspace Distance** — Raster showing distance (m) to nearest greenspace
- **RSF Program Projects** — Partner project areas from the RSF Initiative
- **Hotspots / Coldspots (KnowBR)** — Block groups with anomalously high/low species richness relative to sampling effort
- **Species Richness** — Unique GBIF species per census block group
- **Data Availability** — GBIF occurrence records per block group
- **CalEnviroScreen (CI Score)** — Cumulative environmental burden by census tract
- **SF EJ Communities** — SF Environmental Justice community burden scores
- **Transit Routes** — All SF Muni routes from GTFS shapes, colored by official SFMTA route color
- **Transit Stops** — All SF Muni stops with AM peak headway info
- **Isochrones** — Generated travel-time polygons
- **NDVI Raster** — Sentinel-2 NDVI cropped to the isochrone union

---

## Biodiversity Access Index (BAI)

The BAI is a composite indicator benchmarked against **citywide empirical distributions** (ECDFs across all SF census block groups), not just the current session. It comprises five equally-weighted dimensions:

| Dimension | Variable | Direction |
|-----------|----------|-----------|
| Mobility Access | Transit stops / km² 
| Species richness | Unique GBIF species
| Observation Intensity | GBIF records / km²
| Environmental Quality | Mean NDVI
| Equity Context | SF EJ burden score (inverted)

All axes scaled 0–1. BAI = mean of five standardized components.

---

## Repository Structure

```
SF_biodiv_access_shiny/
├── app_v2.R                  # Main app (local development) — sources setup_local.R
├── app.R                     # Cloud/HuggingFace version — sources setup.R
├── R/
│   ├── setup_local.R         # Data loading for local development (with caching)
│   ├── setup.R               # Data loading for cloud/HuggingFace deployment
│   ├── convert_gbif_to_parquet.R   # One-time: converts GBIF .Rdata → .parquet
│   ├── implement_optimizations.R   # One-time: pre-computes GTFS timetable and caches rasters
│   ├── profile_startup.R     # Benchmarks app startup time per loading step
│   └── making-greenspace-raster.R  # Pre-processes greenspace distance rasters
├── data/
│   ├── source/               # Raw source data (not on HuggingFace)
│   │   └── RSF_Program_Projects_polygons.gpkg
│   ├── cached/               # Downloaded HuggingFace data (gitignored)
│   ├── output/               # Processed outputs, e.g. GBIF parquet (gitignored)
│   └── cache/                # Pre-computed objects, e.g. GTFS timetable (gitignored)
└── www/                      # Logos and static assets
```

> **Note:** The `data/` directory is gitignored. See setup instructions below.

---

## Cyberinfrastructure & Data Sources

| Dataset | Source | Format | How Loaded |
|---------|--------|--------|-----------|
| GBIF occurrences (SF) | Global Biodiversity Information Facility | Parquet | DuckDB spatial queries in server |
| Census block groups + ACS | US Census / tidycensus | `.Rdata` | Downloaded from HuggingFace at startup |
| NDVI raster | Sentinel-2 (pre-processed) | GeoTIFF | Cached locally or via `vsicurl` |
| OSM greenspace polygons | OpenStreetMap | Shapefile / GeoPackage | Cached locally or via `vsicurl` |
| Greenspace distance rasters | Derived from OSM (see `making-greenspace-raster.R`) | GeoTIFF | Cached locally or via `vsicurl` |
| Biodiversity hotspots/coldspots | KnowBR analysis on GBIF | Shapefile | Cached locally or via `vsicurl` |
| SF Muni GTFS | SFMTA official GTFS feed | CSV / zip | Loaded from local GTFS directory |
| CalEnviroScreen 4.0 | OEHHA | File geodatabase | Loaded from local path |
| SF EJ Communities | SF Environment | Shapefile | Loaded from local path |
| RSF Program Projects | RSF Initiative | GeoPackage | `data/source/` |

**Remote data** (greenspace, CBG, hotspots, NDVI, GBIF) is hosted on HuggingFace at  
[`boettiger-lab/sf_biodiv_access`](https://huggingface.co/datasets/boettiger-lab/sf_biodiv_access).

**GBIF queries** use [DuckDB](https://duckdb.org/) with the spatial extension, querying a local `.parquet` file directly via SQL `ST_Intersects` — avoiding loading the full ~3M row dataset into memory.

---

## Local Development Setup

### Prerequisites

Install required R packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "mapboxapi", "tidyverse",
  "tidycensus", "sf", "DT", "RColorBrewer", "terra", "data.table",
  "mapview", "sjPlot", "sjlabelled", "bslib", "shinycssloaders",
  "DBI", "duckdb", "dbplyr", "gtfsrouter", "tidytransit", "fmsb", "scales"
))
```

### First-Time Setup

**Step 1: Convert GBIF data to Parquet** (one-time, ~2 min)
```r
source("R/convert_gbif_to_parquet.R")
# Creates: data/output/gbif_census_ndvi_anno.parquet
```

**Step 2: Pre-compute GTFS timetable and cache rasters** (one-time, ~20 sec, optional but recommended)
```r
source("R/implement_optimizations.R")
# Creates: data/cache/gtfs_timetable_monday.rds (~5 MB)
#          data/cache/greenspace_dist_raster.tif
#          data/cache/osm_greenspace.gpkg
```
> If you skip this step the app still works — startup will just be ~4 seconds slower as the GTFS timetable is computed live.

**Step 3: Run the app**
```r
shiny::runApp("app_v2.R")
```

### Startup Performance

Typical startup time: **~6–12 seconds** depending on whether caches exist.

To benchmark where time is spent:
```r
source("R/profile_startup.R")
# Generates startup_benchmark.png and startup_benchmarks.csv
```

| Step | Uncached | Cached |
|------|----------|--------|
| GTFS timetable | ~3.8s | ~0.3s |
| Greenspace rasters | ~1.7s | ~0.2s |
| OSM greenspace | ~1.3s | ~0.1s |
| CBG / hotspots / other | ~2.5s | ~2.5s |

---

## Cloud Deployment (HuggingFace Spaces)

The app is deployed via Docker on HuggingFace Spaces using `app.R` + `R/setup.R`. Data is streamed from the HuggingFace dataset repository via GDAL's `/vsicurl/` virtual filesystem and DuckDB remote parquet reads — no large files need to be bundled in the image.

See `Dockerfile` for the deployment configuration.

---

## Authors

**Diego Ellis Soto**, Avery Hill, Christopher J. Schell, Carl Boettiger, Rebecca Johnson  
University of California Berkeley (ESPM) · California Academy of Sciences  
Contact: [diego.ellissoto@berkeley.edu](mailto:diego.ellissoto@berkeley.edu)

---

## Status & Roadmap

This tool is a **decision-support prototype** co-developed with the RSF Data Working Group. The BAI should be treated as an exploratory indicator; variable weights and reference distributions are subject to revision through stakeholder co-development.

**Planned additions:**
- Impervious surface coverage
- National Walkability Index
- CDC Social Vulnerability Index
- NatureServe biodiversity / rarity maps
- Frequency-weighted multimodal transit accessibility
- Pre-cached transit isochrones for faster queries

---

<img src="www/hexbin_RSF_logo.png" width="80">
