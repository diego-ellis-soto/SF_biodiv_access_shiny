library(tidyverse)
library(duckdb)

# --- Move to shiny -----------------------------------------------------------
# Connect to a new shiny database
scon <- dbConnect(duckdb())
scon |> dbExecute("INSTALL spatial; LOAD spatial;")

# Attach the working database instead of copying it
scon |> dbExecute("ATTACH '~/Data/Occurrences/GBIF/gbif.duckdb' AS gbifmain;")
scon |> tbl("gbifmain.join_lookup")

# Copy GBIF points over
scon |> dbExecute(
  "COPY
(SELECT DISTINCT year,kingdom,phylum,class,\"order\",family,genus,species,decimallatitude,decimallongitude,coordinateuncertaintyinmeters,institutioncode FROM gbifmain.gbif
NATURAL JOIN gbifmain.join_lookup
WHERE stateprovince='California'
AND county_name='San Francisco'
AND institutioncode='iNaturalist'
AND decimallatitude IS NOT NULL
AND decimallongitude IS NOT NULL)
TO
'data/output/sf-gbif.parquet'
(FORMAT parquet);
"
)

# Disconnect from the database
scon |> dbDisconnect(shutdown = TRUE)