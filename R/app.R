# Sharing the app https://shiny.posit.co/r/getstarted/shiny-basics/lesson7/ 
# rsconnect::setAccountInfo(name='diego-ellis-soto', token='A47BE3C9E4B9EBCDFEC889AF31F64154', secret='g2Q2rxeYCiwlH81EkPXcCGsiHMgdyhTznJRmHtea')
# deployApp()
# Add that you can hover over the greespace and get its name
# Improve the titles of the ggplots of the model coefficient estimates and of ggplot using the gbif summary table on data avialability vs species richness. Also log transform these values for better data visualization 
# Also the ggplot of  data avialability vs species richness. should also update if the user decides to subset by class or family. Until then, its okay to retain the general plot using all the data from gbif_sf

# Optimize some calculations? Shorten 





###############################################################################
# Shiny App: San Francisco Biodiversity Access Decision Support Tool
# Author: Diego Ellis Soto, et al.
# University of California Berkeley, ESPM
# California Academy of Sciences
###############################################################################

library(shiny)
library(leaflet)
library(mapboxapi)
library(tidyverse)
library(tidycensus)
library(sf)
library(DT)
library(RColorBrewer)
library(terra)       
library(data.table)  # for fread
library(mapview)     # for mapview objects
library(sjPlot)      # for plotting lm model coefficients
library(sjlabelled)  # optional if needed for sjPlot

# ------------------------------------------------
# 1) API Keys
# ------------------------------------------------
mapbox_token <- "pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjbHc3NmI0cDMxYzhyMmt0OXBiYnltMjVtIn0.Thtu6WqIhOfin6AykskM2g" 
mb_access_token(mapbox_token, install = FALSE)

# ------------------------------------------------
# 2) Load Data
# ------------------------------------------------
# -- Greenspace
osm_greenspace <- st_read("data/greenspaces_osm_nad83.shp", quiet = TRUE) %>%
  st_transform(4326)
if (!"name" %in% names(osm_greenspace)) {
  osm_greenspace$name <- "Unnamed Greenspace"
}

# -- NDVI Raster
ndvi <- rast("data/SF_EastBay_NDVI_Sentinel_10.tif")

# -- GBIF data
load("data/sf_gbif.Rdata")  # => sf_gbif

# -- Precomputed CBG data
load('data/cbg_vect_sf.Rdata')
if (!"unique_species" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$unique_species <- cbg_vect_sf$n_species
}
if (!"n_observations" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$n_observations <- cbg_vect_sf$n
}
if (!"median_inc" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$median_inc <- cbg_vect_sf$medincE
}
if (!"ndvi_mean" %in% names(cbg_vect_sf)) {
  cbg_vect_sf$ndvi_mean <- cbg_vect_sf$ndvi_sentinel
}

# -- Hotspots/Coldspots
biodiv_hotspots  <- st_read("data/hotspots.shp",  quiet = TRUE) %>% st_transform(4326)
biodiv_coldspots <- st_read("data/coldspots.shp", quiet = TRUE) %>% st_transform(4326)

# ------------------------------------------------
# 3) UI
# ------------------------------------------------
ui <- fluidPage(
  titlePanel("San Francisco Biodiversity Access Decision Support Tool"),
  
  fluidRow(
    column(
      width = 12, align = "center",
      tags$img(src = "UC Berkeley_logo.png", 
               height = "120px", style = "margin:10px;"),
      tags$img(src = "California_academy_logo.png", 
               height = "120px", style = "margin:10px;"),
      tags$img(src = "Reimagining_San_Francisco.png", 
               height = "120px", style = "margin:10px;")
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      br(),
      p("This application demonstrates an approach for exploring biodiversity access in San Francisco..."),
      # (Your summary text can go here)
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      br(),
      tags$b("App Summary (Fill out with RSF data working group):"),
      # Increasingly, we ask ourselves about what increasing access to biodiversity really means. 
      #    Importantly, accessibility differs from human mobility in urban planning studies for equitable transportation systems.
      p("
        This application allows users to either click on a map or geocode an address (in progress) 
         to generate travel-time isochrones across multiple transportation modes (e.g., pedestrian, cycling, driving, driving during traffic).
         It retrieves socio-economic data from precomputed Census variables, calculates NDVI, 
         and summarizes biodiversity records from GBIF. We explore what biodiversity access means
         Users can explore information that we often relate to biodiversity in urban environments including greenspace coverage, population estimates, and species diversity within each isochrone."),
      
      tags$b("Reimagining San Francisco (Fill out with CAS):"),
      p("Reimagining San Francisco is an initiative aimed at integrating ecological, social, 
         and technological dimensions to shape a sustainable future for the Bay Area. 
         This collaboration unites diverse stakeholders to explore innovations in urban planning, 
         conservation, and community engagement. The Reimagining San Francisco Data Working Group has been tasked with identifying and integrating multiple sources of socio-ecological biodiversity information in a co-development framework."),
      
      tags$b("Why Biodiversity Access Matters (Polish this):"),
      p("
      # Ensuring equitable access to biodiversity is essential for human well-being, 
      #    ecological resilience, and global policy decisions related to conservation. 
      #    Areas with higher biodiversity can support ecosystem services including pollinators, moderate climate extremes, 
      #    and provide cultural, recreational, and health benefits to local communities. 
         Recognizing that cities are particularly complex socio-ecological systems facing both legacies of sociocultural practices as well as current ongoing dynamic human activities and pressures.
         Incorporating multiple facets of biodiversity metrics alongside variables employed by city planners, human geographers, and decision-makers into urban planning will allow a more integrative lens in creating a sustainable future for cities and their residents."),
      
      tags$b("How We Calculate Biodiversity Access Percentile:"),
      p("Total unique species found within the user-generated isochrone. 
         We then compare that value to the distribution of unique species counts across all census block groups, 
         converting that comparison into a percentile ranking (Polish this, look at the 15 Minute city). 
         A higher percentile indicates greater biodiversity within the chosen area, 
         relative to other parts of the city or region."),
      
      tags$b("Created by:"),
      p(strong("Diego Ellis Soto", "Carl Boettiger, Rebecca Johnson, Christopher J. Schell")),
      
      p("Contact Information", 
        strong("diego.ellissoto@berkeley.edu")),
      
      tags$b("Next Steps:"),
      tags$ul(
        tags$li("Add impervious surface"),
        tags$li("National walkability score"),
        tags$li("Social vulnerability score"),
        tags$li("NatureServe biodiversity maps"),
        tags$li("Calculate cold-hotspots within ggregation of H6 bins instead of by census block group: Ask Carl"),
        tags$li("Species range maps"),
        tags$li("Add common name GBIF"),
        tags$li("Partner orgs"),
        tags$li("Optimize speed -> store variables -> H-ify the world?"),
        tags$li("Brainstorm and co-develop the biodiversity access score"),
        tags$li("For the GBIF summaries, add an annotated GBIF_sf with environmental variables so we can see landcover type association across the biodiversity within the isochrone.")
      )
    )
  ),
  br(),
  
  tabsetPanel(
    
    # 1) Isochrone Explorer
    tabPanel("Isochrone Explorer",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "location_choice", 
                   "Select how to choose your location:",
                   choices = c("Address (Geocode)" = "address", 
                               "Click on Map"      = "map_click"),
                   selected = "map_click"  
                 ),
                 
                 conditionalPanel(
                   condition = "input.location_choice == 'address'",
                   textInput(
                     "user_address", 
                     "Enter Address:", 
                     value = "", 
                     placeholder = "e.g., 1600 Amphitheatre Parkway, Mountain View, CA"
                   )
                 ),
                 
                 checkboxGroupInput(
                   "transport_modes", 
                   "Select Transportation Modes:",
                   choices = list("Driving"             = "driving",
                                  "Walking"             = "walking",
                                  "Cycling"             = "cycling",
                                  "Driving with Traffic"= "driving-traffic"),
                   selected = c("driving", "walking")
                 ),
                 
                 checkboxGroupInput(
                   "iso_times", 
                   "Select Isochrone Times (minutes):",
                   choices = list("5" = 5, "10" = 10, "15" = 15),
                   selected = c(5, 10)
                 ),
                 
                 actionButton("generate_iso", "Generate Isochrones"),
                 actionButton("clear_map", "Clear")
                 
               ),
               
               mainPanel(
                 leafletOutput("isoMap", height = 600),
                 
                 fluidRow(
                   column(12,
                          br(),
                          uiOutput("bioScoreBox"),
                          uiOutput("closestGreenspaceUI")
                   )
                 ),
                 
                 br(),
                 DTOutput("dataTable"),
                 
                 br(),
                 fluidRow(
                   column(12,
                          plotOutput("bioSocPlot", height = "400px")
                   )
                 ),
                 
                 br(),
                 fluidRow(
                   column(12,
                          plotOutput("collectionPlot", height = "300px")
                   )
                 )
               )
             )
    ),
    
    #br.?
    tabPanel(
      "GBIF Summaries",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "class_filter",
            "Select a GBIF Class to Summarize:",
            choices = c("All", sort(unique(sf_gbif$class))), 
            selected = "All"
          ),
          selectInput(
            "family_filter",
            "Filter by Family (optional):",
            choices = c("All", sort(unique(sf_gbif$family))),
            selected = "All"
          )
        ),
        mainPanel(
          DTOutput("classTable"),
          br(),
          h3("Observations vs. Species Richness"),
          plotOutput("obsVsSpeciesPlot", height = "400px"),
          p("This plot displays the relationship between the number of observations and the species richness. Use this visualization to understand data coverage and biodiversity trends.")
        )
      )
    )
    
    
    # )
    
    # Separate section for the plot outside of the "GBIF Summaries" tab
    
       # tabsetPanel(
    
    #   # 1) Isochrone Explorer
    #   tabPanel(
    #     mainPanel(
    #       DTOutput("classTable"),
    #       br(),
    #       fluidRow(
    #         column(
    #           6,
    #           # A simple scatter or line plot for n_observations vs n_species
    #           plotOutput("obsVsSpeciesPlot", height = "300px")
    #         )
    #         # ,
    #         # column(
    #         #   6,
    #         #   # A regression model plot using sjPlot
    #         #   plotOutput("lmCoefficientsPlot", height = "300px")
    #         # )
    #       )
    #     )
    #   )
    # ),
    # 
    # br()
    
  )
  
  
  # fluidRow(
  #   column(
  #     12,
  #     tags$h3("Species Richness vs Data Availability"),
  #     fluidRow(
  #       column(6, uiOutput("mapNUI")),
  #       column(6, uiOutput("mapSpeciesUI"))
  #     )
  #   )
  # )
)

# ------------------------------------------------
# 4) Server
# ------------------------------------------------
server <- function(input, output, session) {
  
  chosen_point <- reactiveVal(NULL)
  
  # ------------------------------------------------
  # Leaflet Base + Hide Overlays
  # ------------------------------------------------
  output$isoMap <- renderLeaflet({
    pal_cbg <- colorNumeric("YlOrRd", cbg_vect_sf$medincE)
    
    pal_rich <- colorNumeric("YlOrRd", domain = cbg_vect_sf$unique_species)
    # 2) Color palette for data availability
    pal_data <- colorNumeric("Blues", domain = cbg_vect_sf$n_observations)
    
    
    leaflet() %>%
      addTiles(group = "Street Map (Default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (ESRI)") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      
      addPolygons(
        data = cbg_vect_sf,
        group = "Income",
        # fillColor = ~pal_cbg(unique_species),
        fillColor = ~pal_cbg(medincE),
        fillOpacity = 0.6,
        color = "white",
        weight = 1,
        label = "Income"
      ) %>%
      
      addPolygons(
        data = osm_greenspace,
        group = "Greenspace",
        fillColor = "darkgreen",
        fillOpacity = 0.3,
        color = "green",
        weight = 1,
        label = ~name,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "blue",
          fillOpacity = 0.5,
          bringToFront = TRUE
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "color" = "blue"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      
      addPolygons(
        data = biodiv_hotspots,
        group = "Hotspots (KnowBR)",
        fillColor = "firebrick",
        fillOpacity = 0.2,
        color = "firebrick",
        weight = 2,
        label = "Biodiversity Hotspot"
      ) %>%
      
      addPolygons(
        data = biodiv_coldspots,
        group = "Coldspots (KnowBR)",
        fillColor = "navyblue",
        fillOpacity = 0.2,
        color = "navyblue",
        weight = 2,
        label = "Biodiversity Coldspot"
      ) %>%
      
      # Add richness and nobs
      # -- Richness layer
      addPolygons(
        data = cbg_vect_sf,
        group = "Species Richness",
        fillColor = ~pal_rich(unique_species),
        fillOpacity = 0.6,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<strong>GEOID: </strong>", GEOID,
          "<br><strong>Species Richness: </strong>", unique_species,
          "<br><strong>Observations: </strong>", n_observations,
          "<br><strong>Median Income: </strong>", median_inc,
          "<br><strong>Mean NDVI: </strong>", ndvi_mean
        )
      ) %>%
      
      # -- Data Availability layer
      addPolygons(
        data = cbg_vect_sf,
        group = "Data Availability",
        fillColor = ~pal_data(n_observations),
        fillOpacity = 0.6,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<strong>GEOID: </strong>", GEOID,
          "<br><strong>Observations: </strong>", n_observations,
          "<br><strong>Species Richness: </strong>", unique_species,
          "<br><strong>Median Income: </strong>", median_inc,
          "<br><strong>Mean NDVI: </strong>", ndvi_mean
        )
      ) %>%
      
      
      setView(lng = -122.4194, lat = 37.7749, zoom = 12) %>%
      addLayersControl(
        baseGroups    = c("Street Map (Default)", "Satellite (ESRI)", "CartoDB.Positron"),
        overlayGroups = c("Income", "Greenspace","Species Richness", "Data Availability", 
                          "Hotspots (KnowBR)", "Coldspots (KnowBR)"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Income") %>%
      hideGroup("Greenspace") %>%
      hideGroup("Hotspots (KnowBR)") %>%
      hideGroup("Coldspots (KnowBR)") %>%
      hideGroup("Species Richness") %>%
      hideGroup("Data Availability")
  })
  
  
  # ------------------------------------------------
  # Observe map clicks (location_choice = 'map_click')
  # ------------------------------------------------
  observeEvent(input$isoMap_click, {
    req(input$location_choice == "map_click")
    click <- input$isoMap_click
    if (!is.null(click)) {
      chosen_point(c(lon = click$lng, lat = click$lat))
      leafletProxy("isoMap") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = click$lng, lat = click$lat,
          radius = 6, color = "firebrick",
          label = "Map Click Location"
        )
    }
  })
  
  # ------------------------------------------------
  # Observe clearinf of map
  # ------------------------------------------------
  observeEvent(input$clear_map, {
    # Reset the chosen point
    chosen_point(NULL)
    
    # Clear all markers and isochrones from the map
    leafletProxy("isoMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearGroup("Isochrones") %>%
      clearGroup("NDVI Raster")
    
    # Optional: Reset any other reactive values if needed
    showNotification("Map cleared. You can select a new location.")
  })
  
  # ------------------------------------------------
  # Generate Isochrones
  # ------------------------------------------------
  isochrones_data <- eventReactive(input$generate_iso, {
    
    leafletProxy("isoMap") %>%
      clearGroup("Isochrones") %>%
      clearGroup("NDVI Raster")
    
    # If user selected address:
    if (input$location_choice == "address") {
      if (nchar(input$user_address) < 5) {
        showNotification("Please enter a more complete address.", type = "error")
        return(NULL)
      }
      
      loc_df <- tryCatch({
        mb_geocode(input$user_address, access_token = mapbox_token)
      }, error = function(e) {
        showNotification(paste("Geocoding failed:", e$message), type = "error")
        NULL
      })
      
      # Check for valid lat/lon
      if (is.null(loc_df) || nrow(loc_df) == 0 || is.na(loc_df$lon[1]) || is.na(loc_df$lat[1])) {
        showNotification("No valid geocoding results found.", type = "warning")
        return(NULL)
      }
      
      chosen_point(c(lon = loc_df$lon[1], lat = loc_df$lat[1]))
      
      leafletProxy("isoMap") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = loc_df$lon[1], lat = loc_df$lat[1],
          radius = 6, color = "navyblue",
          label = "Geocoded Address"
        ) %>%
        setView(lng = loc_df$lon[1], lat = loc_df$lat[1], zoom = 13)
    }
    
    pt <- chosen_point()
    if (is.null(pt)) {
      showNotification("No location selected! Provide an address or click the map.", type = "error")
      return(NULL)
    }
    if (length(input$transport_modes) == 0) {
      showNotification("Select at least one transportation mode.", type = "error")
      return(NULL)
    }
    if (length(input$iso_times) == 0) {
      showNotification("Select at least one isochrone time.", type = "error")
      return(NULL)
    }
    
    location_sf <- st_as_sf(
      data.frame(lon = pt["lon"], lat = pt["lat"]),
      coords = c("lon","lat"), crs = 4326
    )
    
    iso_list <- list()
    for (mode in input$transport_modes) {
      for (t in input$iso_times) {
        iso <- tryCatch({
          mb_isochrone(location_sf, time = as.numeric(t), profile = mode, 
                       access_token = mapbox_token)
        }, error = function(e) {
          showNotification(paste("Isochrone error:", mode, t, e$message), type = "error")
          NULL
        })
        if (!is.null(iso)) {
          iso$mode <- mode
          iso$time <- t
          iso_list <- append(iso_list, list(iso))
        }
      }
    }
    if (length(iso_list) == 0) {
      showNotification("No isochrones generated.", type = "warning")
      return(NULL)
    }
    
    all_iso <- do.call(rbind, iso_list) %>% st_transform(4326)
    all_iso
  })
  
  # ------------------------------------------------
  # Plot Isochrones + NDVI
  # ------------------------------------------------
  observeEvent(isochrones_data(), {
    iso_data <- isochrones_data()
    req(iso_data)
    
    iso_data$iso_group <- paste(iso_data$mode, iso_data$time, sep = "_")
    pal <- colorRampPalette(brewer.pal(8, "Set2"))
    cols <- pal(nrow(iso_data))
    
    for (i in seq_len(nrow(iso_data))) {
      poly_i <- iso_data[i, ]
      leafletProxy("isoMap") %>%
        addPolygons(
          data = poly_i,
          group = "Isochrones",
          color = cols[i],
          weight = 2,
          fillOpacity = 0.4,
          label = paste0(poly_i$mode, " - ", poly_i$time, " mins")
        )
    }
    
    iso_union <- st_union(iso_data)
    iso_union_vect <- vect(iso_union)
    ndvi_crop <- crop(ndvi, iso_union_vect)
    ndvi_mask <- mask(ndvi_crop, iso_union_vect)
    ndvi_vals <- values(ndvi_mask)
    ndvi_vals <- ndvi_vals[!is.na(ndvi_vals)]
    
    if (length(ndvi_vals) > 0) {
      ndvi_pal <- colorNumeric("YlGn", domain = range(ndvi_vals, na.rm = TRUE), na.color = "transparent")
      
      leafletProxy("isoMap") %>%
        addRasterImage(
          x = ndvi_mask,
          colors = ndvi_pal,
          opacity = 0.7,
          project = TRUE,
          group = "NDVI Raster"
        ) %>%
        addLegend(
          position = "bottomright",
          pal = ndvi_pal,
          values = ndvi_vals,
          title = "NDVI"
        )
    }
    
    leafletProxy("isoMap") %>%
      addLayersControl(
        baseGroups = c("Street Map (Default)", "Satellite (ESRI)", "CartoDB.Positron"),
        overlayGroups = c("Income", "Greenspace", 
                          "Hotspots (KnowBR)", "Coldspots (KnowBR)",
                          "Isochrones", "NDVI Raster"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # ------------------------------------------------
  # socio_data Reactive + Summaries
  # ------------------------------------------------
  socio_data <- reactive({
    iso_data <- isochrones_data()
    if (is.null(iso_data) || nrow(iso_data) == 0) {
      return(data.frame())
    }
    
    acs_wide <- cbg_vect_sf %>%
      mutate(
        population = popE,
        med_income = medincE
      )
    
    hotspot_union  <- st_union(biodiv_hotspots)
    coldspot_union <- st_union(biodiv_coldspots)
    
    results <- data.frame()
    
    for (i in seq_len(nrow(iso_data))) {
      poly_i <- iso_data[i, ]
      
      dist_hot  <- st_distance(poly_i, hotspot_union)
      dist_cold <- st_distance(poly_i, coldspot_union)
      dist_hot_km  <- round(as.numeric(min(dist_hot))  / 1000, 3)
      dist_cold_km <- round(as.numeric(min(dist_cold)) / 1000, 3)
      
      inter_acs <- st_intersection(acs_wide, poly_i)
      
      pop_total <- 0
      inc_str   <- "N/A"
      if (nrow(inter_acs) > 0) {
        inter_acs$area <- st_area(inter_acs)
        inter_acs$area_num <- as.numeric(inter_acs$area)
        inter_acs$area_ratio <- inter_acs$area_num / as.numeric(st_area(inter_acs))
        inter_acs$weighted_pop <- inter_acs$population * inter_acs$area_ratio
        
        pop_total <- round(sum(inter_acs$weighted_pop, na.rm = TRUE))
        
        w_income <- sum(inter_acs$med_income * inter_acs$area_num, na.rm = TRUE) /
          sum(inter_acs$area_num, na.rm = TRUE)
        if (!is.na(w_income) && w_income > 0) {
          inc_str <- paste0("$", formatC(round(w_income, 2), format = "f", big.mark = ","))
        }
      }
      
      inter_gs <- st_intersection(osm_greenspace, poly_i)
      gs_area_m2 <- 0
      if (nrow(inter_gs) > 0) {
        gs_area_m2 <- sum(st_area(inter_gs))
      }
      iso_area_m2 <- as.numeric(st_area(poly_i))
      gs_area_m2 <- as.numeric(gs_area_m2)
      gs_percent <- ifelse(iso_area_m2 > 0, 100 * gs_area_m2 / iso_area_m2, 0)
      
      poly_vect <- vect(poly_i)
      ndvi_crop <- crop(ndvi, poly_vect)
      ndvi_mask <- mask(ndvi_crop, poly_vect)
      ndvi_vals <- values(ndvi_mask)
      ndvi_vals <- ndvi_vals[!is.na(ndvi_vals)]
      mean_ndvi <- ifelse(length(ndvi_vals) > 0, round(mean(ndvi_vals, na.rm=TRUE), 3), NA)
      
      inter_gbif <- st_intersection(sf_gbif, poly_i)
      n_records   <- nrow(inter_gbif)
      n_species   <- length(unique(inter_gbif$species))
      
      n_birds   <- length(unique(inter_gbif$species[ inter_gbif$class == "Aves" ]))
      n_mammals <- length(unique(inter_gbif$species[ inter_gbif$class == "Mammalia" ]))
      n_plants  <- length(unique(inter_gbif$species[ inter_gbif$class %in% 
                                                       c("Magnoliopsida","Liliopsida","Pinopsida","Polypodiopsida",
                                                         "Equisetopsida","Bryopsida","Marchantiopsida") ]))
      
      iso_area_km2 <- round(iso_area_m2 / 1e6, 3)
      iso_area_sqm <- round(iso_area_m2, 2)
      
      row_i <- data.frame(
        Mode                = tools::toTitleCase(poly_i$mode),
        Time                = poly_i$time,
        IsochroneArea_m2    = iso_area_sqm,
        IsochroneArea_km2   = iso_area_km2,
        DistToHotspot_km    = dist_hot_km,
        DistToColdspot_km   = dist_cold_km,
        EstimatedPopulation = pop_total,
        MedianIncome        = inc_str,
        MeanNDVI            = ifelse(!is.na(mean_ndvi), mean_ndvi, "N/A"),
        GBIF_Records        = n_records,
        GBIF_Species        = n_species,
        Bird_Species        = n_birds,
        Mammal_Species      = n_mammals,
        Plant_Species       = n_plants,
        Greenspace_m2       = round(gs_area_m2, 2),
        Greenspace_percent  = round(gs_percent, 2),
        stringsAsFactors    = FALSE
      )
      results <- rbind(results, row_i)
    }
    
    iso_union <- st_union(iso_data)
    inter_all_gbif <- st_intersection(sf_gbif, iso_union)
    union_n_species <- length(unique(inter_all_gbif$species))
    rank_percentile <- round(100 * ecdf(cbg_vect_sf$unique_species)(union_n_species), 1)
    attr(results, "bio_percentile") <- rank_percentile
    
    # Closest Greenspace from ANY part of the isochrone
    dist_mat <- st_distance(iso_union, osm_greenspace)  # 1 x N matrix
    if (length(dist_mat) > 0) {
      min_dist <- min(dist_mat)
      min_idx  <- which.min(dist_mat)
      gs_name  <- osm_greenspace$name[min_idx]
      attr(results, "closest_greenspace") <- gs_name
    } else {
      attr(results, "closest_greenspace") <- "None"
    }
    
    results
  })
  
  # ------------------------------------------------
  # Render main summary table
  # ------------------------------------------------
  output$dataTable <- renderDT({
    df <- socio_data()
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame("Message" = "No isochrones generated yet.")))
    }
    DT::datatable(
      df,
      colnames = c(
        "Mode"                 = "Mode",
        "Time (min)"           = "Time",
        "Area (m²)"            = "IsochroneArea_m2",
        "Area (km²)"           = "IsochroneArea_km2",
        "Dist. Hotspot (km)"   = "DistToHotspot_km",
        "Dist. Coldspot (km)"  = "DistToColdspot_km",
        "Population"           = "EstimatedPopulation",
        "Median Income"        = "MedianIncome",
        "Mean NDVI"            = "MeanNDVI",
        "GBIF Records"         = "GBIF_Records",
        "Unique Species"       = "GBIF_Species",
        "Bird Species"         = "Bird_Species",
        "Mammal Species"       = "Mammal_Species",
        "Plant Species"        = "Plant_Species",
        "Greenspace (m²)"      = "Greenspace_m2",
        "Greenspace (%)"       = "Greenspace_percent"
      ),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # ------------------------------------------------
  # Biodiversity Access Score + Closest Greenspace
  # ------------------------------------------------
  output$bioScoreBox <- renderUI({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    
    percentile <- attr(df, "bio_percentile")
    if (is.null(percentile)) percentile <- "N/A"
    else percentile <- paste0(percentile, "th Percentile")
    
    wellPanel(
      HTML(paste0("<h2>Biodiversity Access Score: ", percentile, "</h2>"))
    )
  })
  
  output$closestGreenspaceUI <- renderUI({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    gs_name <- attr(df, "closest_greenspace")
    if (is.null(gs_name)) gs_name <- "None"
    
    tagList(
      strong("Closest Greenspace (from any part of the Isochrone):"),
      p(gs_name)
    )
  })
  
  # ------------------------------------------------
  # Secondary table: user-selected CLASS & FAMILY
  # ------------------------------------------------
  output$classTable <- renderDT({
    iso_data <- isochrones_data()
    if (is.null(iso_data) || nrow(iso_data) == 0) {
      return(DT::datatable(data.frame("Message" = "No isochrones generated yet.")))
    }
    
    iso_union <- st_union(iso_data)
    inter_gbif <- st_intersection(sf_gbif, iso_union)
    
    # Add a quick ACS intersection for mean income & NDVI if needed
    acs_wide <- cbg_vect_sf %>% mutate(
      income = median_inc,
      ndvi   = ndvi_mean
    )
    
    inter_gbif_acs <- st_intersection(inter_gbif, acs_wide)
    
    if (input$class_filter != "All") {
      inter_gbif_acs <- inter_gbif_acs[ inter_gbif_acs$class == input$class_filter, ]
    }
    if (input$family_filter != "All") {
      inter_gbif_acs <- inter_gbif_acs[ inter_gbif_acs$family == input$family_filter, ]
    }
    
    if (nrow(inter_gbif_acs) == 0) {
      return(DT::datatable(data.frame("Message" = "No records for that combination in the isochrone.")))
    }
    
    species_counts <- inter_gbif_acs %>%
      st_drop_geometry() %>%
      group_by(species) %>%
      summarize(
        n_records   = n(),
        mean_income = round(mean(income, na.rm=TRUE), 2),
        mean_ndvi   = round(mean(ndvi, na.rm=TRUE), 3),
        .groups = "drop"
      ) %>%
      arrange(desc(n_records))
    
    DT::datatable(
      species_counts,
      colnames = c("Species", "Number of Records", "Mean Income", "Mean NDVI"),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ------------------------------------------------
  # Ggplot: Biodiversity & Socioeconomic Summary
  # ------------------------------------------------
  output$bioSocPlot <- renderPlot({
    df <- socio_data()
    if (nrow(df) == 0) return(NULL)
    
    df_plot <- df %>%
      mutate(IsoLabel = paste0(Mode, "-", Time, "min"))
    
    ggplot(df_plot, aes(x = IsoLabel)) +
      geom_col(aes(y = GBIF_Species), fill = "steelblue", alpha = 0.7) +
      geom_line(aes(y = EstimatedPopulation / 1000, group = 1), color = "red", size = 1) +
      geom_point(aes(y = EstimatedPopulation / 1000), color = "red", size = 3) +
      labs(
        x = "Isochrone (Mode-Time)",
        y = "Blue bars: Unique Species \n | Red line: Population (thousands)",
        title = "Biodiversity & Socioeconomic Summary"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
  })
  
  # ------------------------------------------------
  # Bar plot: GBIF records by institutionCode
  # ------------------------------------------------
  output$collectionPlot <- renderPlot({
    iso_data <- isochrones_data()
    if (is.null(iso_data) || nrow(iso_data) == 0) {
      plot.new()
      title("No GBIF records found in this isochrone.")
      return(NULL)
    }
    
    iso_union <- st_union(iso_data)
    inter_gbif <- st_intersection(sf_gbif, iso_union)
    if (nrow(inter_gbif) == 0) {
      plot.new()
      title("No GBIF records found in this isochrone.")
      return(NULL)
    }
    
    df_code <- inter_gbif %>%
      st_drop_geometry() %>%
      group_by(institutionCode) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    ggplot(df_code, aes(x = reorder(institutionCode, -count), y = count)) +
      geom_bar(stat = "identity", fill = "darkorange", alpha = 0.7) +
      labs(
        x = "Institution Code",
        y = "Number of Records",
        title = "GBIF Records by Institution Code (Isochrone Union)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
  })
  
  # ------------------------------------------------
  # Additional Section: mapview for species richness vs. data availability
  # ------------------------------------------------
  output$mapNUI <- renderUI({
    map_n <- mapview(cbg_vect_sf, zcol = "n", layer.name="Data Availability (n)")
    map_n@map
  })
  
  output$mapSpeciesUI <- renderUI({
    map_s <- mapview(cbg_vect_sf, zcol = "n_species", layer.name="Species Richness (n_species)")
    map_s@map
  })
  
  # ------------------------------------------------
  # Additional Plot: n_observations vs n_species
  # ------------------------------------------------
  output$obsVsSpeciesPlot <- renderPlot({
    # A simple scatter plot of n_observations vs. n_species from cbg_vect_sf
    ggplot(cbg_vect_sf, aes(x = log(n_observations+1), y = log(unique_species+1)) ) +
      geom_point(color = "blue", alpha = 0.6) +
      labs(
        x = "Number of Observations (n_observations)",
        y = "Number of Species (n_species)",
        title = "Data Availability vs. Species Richness"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ------------------------------------------------
  # Additional Plot: Linear model of n_species ~ n_observations + median_inc + ndvi_mean
  # ------------------------------------------------
  # output$lmCoefficientsPlot <- renderPlot({
  #   # Build a linear model with cbg_vect_sf
  #   # Must ensure there are no NAs
  #   df_lm <- cbg_vect_sf %>% 
  #     filter(!is.na(n_observations), 
  #            !is.na(unique_species),
  #            !is.na(median_inc),
  #            !is.na(ndvi_mean))
  #   
  #   if (nrow(df_lm) < 5) {
  #     # not enough data
  #     plot.new()
  #     title("Not enough data for linear model.")
  #     return(NULL)
  #   }
  #   
  #   # Model
  #   fit <- lm(unique_species ~ n_observations + median_inc + ndvi_mean, data = df_lm)
  #   
  #   # Using sjPlot to visualize coefficients
  #   # We store in an object and then print it
  #   p <- plot_model(fit, show.values = TRUE, value.offset = .3, title = "LM Coefficients: n_species ~ n_observations + median_inc + ndvi_mean")
  #   print(p)
  # })
}

shinyApp(ui, server)



# library(profvis)
# 
# profvis({
#   shinyApp(ui, server)
# })