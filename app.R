###############################################################################
# Shiny App: San Francisco Biodiversity Access Decision Support Tool
# Author: Diego Ellis Soto, et al.
# University of California Berkeley, ESPM
# California Academy of Sciences
###############################################################################
require(shinyjs)
library(shiny)
library(shinydashboard)
library(leaflet)
library(mapboxapi)
library(tidyverse)
library(tidycensus)
library(sf)
library(DT)
library(RColorBrewer)
library(terra)       
library(data.table)
library(mapview)   
library(sjPlot)    
library(sjlabelled)
library(bslib)
library(shinycssloaders)
library(DBI)
library(duckdb)
library(dbplyr)

source('R/setup.R') # Load necessary data (annotated gbif, annotated cbg, ndvi)

# ============================================================================
# Load GBIF dropdown values from parquet (for UI initialization)
# ============================================================================
con_temp <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
dbExecute(con_temp, "INSTALL spatial; LOAD spatial;")
dbExecute(con_temp, "INSTALL httpfs; LOAD httpfs;")
gbif_tab_temp <- tbl(con_temp, paste0("read_parquet('", gbif_parquet, "')"))

gbif_classes <- gbif_tab_temp |> distinct(class) |> collect() |> pull(class) |> sort()
gbif_families <- gbif_tab_temp |> distinct(family) |> collect() |> pull(family) |> sort()

dbDisconnect(con_temp, shutdown = TRUE)
rm(con_temp, gbif_tab_temp)

# Define your Mapbox token securely
mapbox_token <- "pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjbHc3NmI0cDMxYzhyMmt0OXBiYnltMjVtIn0.Thtu6WqIhOfin6AykskM2g" 

# Global theme definition using a green-themed bootswatch
theme <- bs_theme(
  bootswatch = "minty", # 'minty' is a light green-themed bootswatch
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Slab"),
  bg = "#f0fff0",       # Honeydew background
  fg = "#2e8b57"        # SeaGreen foreground
)

# UI
ui <- dashboardPage(
  skin = "green", # shinydashboard skin color
  dashboardHeader(title = "SF Biodiversity Access Tool"
  ),

  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Isochrone Explorer", tabName = "isochrone", icon = icon("map-marker-alt")),
      menuItem("GBIF Summaries", tabName = "gbif", icon = icon("table")),
      menuItem("Community Science", tabName = "community_science", icon = icon("users")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    theme = theme, # Apply the custom theme
    useShinyjs(),  
    # Loading message
    div(id = "loading", style = "display:none; font-size: 20px; color: red;", "Calculating..."),
    
    # fluidRow(
    #   column(
    #     width = 2,
    #     imageOutput("Combined_logos")
    #   )
    # ),
    
    # fluidPage(
    #   box(
    #     tags$img(height = 100, width = 100,src = "Combined_logos.png"),
    #     imageOutput('Combined_logos')
    #   )
    # ),
    
    
    # fluidRow(
    #   column(
    #     width = 2,
    #     imageOutput("uc_berkeley_logo")
    #   ),
    #   column(
    #     width = 4,
    #     imageOutput("california_academy_logo")
    #   ),
    #   column(
    #     width = 6,
    #     imageOutput("reimagining_sf_logo")
    #   )
    # ),
    # fluidPage(
    #   # Application title
    #   # titlePanel("Test app"),
    #   # to render images in the www folder 
    #   box(uiOutput("houz"), width = 3)
    # ),
    
    # 
    # fluidRow(
    #   column(
    #     width = 12, align = "center",
    #     tags$img(src = "UC_Berkeley_logo.png",
    #              height = "200px", style = "margin:10px;", alt = "UC Berkeley Logo"),
    #     tags$img(src = "California_academy_logo.png",
    #              height = "200px", style = "margin:10px;", alt = "California Academy Logo"),
    #     tags$img(src = "Reimagining_San_Francisco.png",
    #              height = "200px", style = "margin:10px;", alt = "Reimagining San Francisco Logo")
    #   )
    # ),
    # fluidPage(
    #   box(
    #     tags$img(height = 100, width = 100,src = "Rlogo.png"),
    #     imageOutput('image_logos')
    #   )
    # ),

    # Tab Items
    tabItems(
      # Isochrone Explorer Tab
      tabItem(tabName = "isochrone",
              fluidRow(
                box(
                  title = "Controls", status = "success", solidHeader = TRUE, width = 4,
                  radioButtons(
                    "location_choice", 
                    "Select Location Method:",
                    choices = c("Address (Geocode)" = "address", 
                                "Click on Map"      = "map_click"),
                    selected = "map_click"  
                  ),
                  
                  conditionalPanel(
                    condition = "input.location_choice == 'address'",
                    mapboxGeocoderInput(
                      inputId = "geocoder",
                      placeholder = "Search for an address",
                      access_token = mapbox_token
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
                  
                  actionButton("generate_iso", "Generate Isochrones", icon = icon("play")),
                  actionButton("clear_map", "Clear", icon = icon("times"))
                ),
                box(
                  title = "Map", status = "success", solidHeader = TRUE, width = 8,
                  leafletOutput("isoMap", height = 600)
                )
              ),
              fluidRow(
                box(
                  title = "Biodiversity Access Score", status = "success", solidHeader = TRUE, width = 6,
                  uiOutput("bioScoreBox")
                ),
                box(
                  title = "Closest Greenspace", status = "success", solidHeader = TRUE, width = 6,
                  uiOutput("closestGreenspaceUI")
                )
              ),
              fluidRow(
                box(
                  title = "Summary Data", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("dataTable") %>% withSpinner(type = 8, color = "#28a745")
                )
              ),
              fluidRow(
                box(
                  title = "Biodiversity & Socioeconomic Summary", status = "success", solidHeader = TRUE, width = 12,
                  plotOutput("bioSocPlot", height = "400px") %>% withSpinner(type = 8, color = "#28a745")
                )
              ),
              fluidRow(
                box(
                  title = "GBIF Records by Institution", status = "success", solidHeader = TRUE, width = 12,
                  plotOutput("collectionPlot", height = "400px") %>% withSpinner(type = 8, color = "#28a745")
                )
              )
      ),
      
      # GBIF Summaries Tab
      tabItem(tabName = "gbif",
              fluidRow(
                box(
                  title = "Filters", status = "success", solidHeader = TRUE, width = 4,
                  selectInput(
                    "class_filter",
                    "Select a GBIF Class to Summarize:",
                    choices = c("All", gbif_classes), 
                    selected = "All"
                  ),
                  selectInput(
                    "family_filter",
                    "Filter by Family (optional):",
                    choices = c("All", gbif_families),
                    selected = "All"
                  )
                ),
                box(
                  title = "Data Summary", status = "success", solidHeader = TRUE, width = 8,
                  DTOutput("classTable")
                )
              ),
              fluidRow(
                box(
                  title = "Observations vs. Species Richness", status = "success", solidHeader = TRUE, width = 12,
                  plotOutput("obsVsSpeciesPlot", height = "300px") %>% withSpinner(type = 8, color = "#28a745"),
                  p("This plot displays the relationship between the number of observations and the species richness. Use this visualization to understand data coverage and biodiversity trends.")
                )
              )
      ),
      # Community Science Tab
      tabItem(tabName = "community_science",
              fluidRow(
                box(
                  title = "Partner Community Organizations", status = "success", solidHeader = TRUE, width = 12,
                  leafletOutput("communityMap", height = 600)
                )
              ),
              fluidRow(
                box(
                  title = "Community Organizations Data", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("communityTable") %>% withSpinner(type = 8, color = "#28a745")
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "App Summary", status = "success", solidHeader = TRUE, width = 12,
                  tags$b("App Summary (Fill out with RSF data working group):"),
                  p("
                    This application allows users to either click on a map or geocode an address 
                    to generate travel-time isochrones across multiple transportation modes 
                    (e.g., pedestrian, cycling, driving, driving during traffic).
                    It retrieves socio-economic data from precomputed Census variables, calculates NDVI, 
                    and summarizes biodiversity records from GBIF. Users can explore information 
                    related to biodiversity in urban environments, including greenspace coverage, 
                    population estimates, and species diversity within each isochrone.
                  "),
                  
                  tags$b("Created by:"),
                  p(strong("Diego Ellis Soto", "Carl Boettiger, Rebecca Johnson, Christopher J. Schell")),
                  
                  p("Contact Information: ", strong("diego.ellissoto@berkeley.edu"))
                )
              ),
              fluidRow(
                box(
                  title = "Reimagining San Francisco", status = "success", solidHeader = TRUE, width = 12,
                  tags$b("Reimagining San Francisco (Fill out with CAS):"),
                  p("Reimagining San Francisco is an initiative aimed at integrating ecological, social, 
                     and technological dimensions to shape a sustainable future for the Bay Area. 
                     This collaboration unites diverse stakeholders to explore innovations in urban planning, 
                     conservation, and community engagement. The Reimagining San Francisco Data Working Group has been tasked with identifying and integrating multiple sources of socio-ecological biodiversity information in a co-development framework.")
                )
              ),
              fluidRow(
                box(
                  title = "Why Biodiversity Access Matters", status = "success", solidHeader = TRUE, width = 12,
                  p("Ensuring equitable access to biodiversity is essential for human well-being, 
                     ecological resilience, and global policy decisions related to conservation. 
                     Areas with higher biodiversity can support ecosystem services including pollinators, moderate climate extremes, 
                     and provide cultural, recreational, and health benefits to local communities. 
                     Recognizing that cities are particularly complex socio-ecological systems facing both legacies of sociocultural practices as well as current ongoing dynamic human activities and pressures.
                     Incorporating multiple facets of biodiversity metrics alongside variables employed by city planners, human geographers, and decision-makers into urban planning will allow a more integrative lens in creating a sustainable future for cities and their residents.")
                )
              ),
              fluidRow(
                box(
                  title = "How We Calculate Biodiversity Access Percentile", status = "success", solidHeader = TRUE, width = 12,
                  p("Total unique species found within the user-generated isochrone. 
                     We then compare that value to the distribution of unique species counts across all census block groups, 
                     converting that comparison into a percentile ranking (Polish this, look at the 15 Minute city). 
                     A higher percentile indicates greater biodiversity within the chosen area, 
                     relative to other parts of the city or region.")
                )
              ),
              fluidRow(
                box(
                  title = "Next Steps", status = "success", solidHeader = TRUE, width = 12,
                  tags$ul(
                    tags$li("Add impervious surface"),
                    tags$li("National walkability score"),
                    tags$li("Social vulnerability score"),
                    tags$li("NatureServe biodiversity maps"),
                    tags$li("Calculate cold-hotspots within aggregation of H6 bins instead of by census block group: Ask Carl"),
                    tags$li("Species range maps"),
                    tags$li("Add common name GBIF"),
                    tags$li("Partner orgs"),
                    tags$li("Optimize speed -> store variables -> H-ify the world?"),
                    tags$li("Brainstorm and co-develop the biodiversity access score"),
                    tags$li("For the GBIF summaries, add an annotated GBIF_sf with environmental variables so we can see landcover type association across the biodiversity within the isochrone.")
                  )
                )
              )
      )
    )
  )
)

# ------------------------------------------------
# Server
# ------------------------------------------------
server <- function(input, output, session) {
  
  # ============================================================================
  # Initialize DuckDB connection (one per session)
  # ============================================================================
  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dbExecute(con, "INSTALL spatial; LOAD spatial;")
  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  
  # Load GBIF parquet as table reference (reuse throughout session)
  gbif_tab <- tbl(con, paste0("read_parquet('", gbif_parquet, "')"))
  
  # Cleanup connection on session end
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
  
  chosen_point <- reactiveVal(NULL)
  
  # ------------------------------------------------
  # Render logos 
  # ------------------------------------------------
  

  output$combine_logo <- renderImage({
    list(
      src = file.path("www", "Combined_logos.png"),
      width = "50%",
      height = "45%",
      alt = "Combined_logos"
    )
  }, deleteFile = FALSE)
  
    # output$uc_berkeley_logo <- renderImage({
  #   list(
  #     src = file.path("www", "UC_Berkeley_logo.png"),
  #     width = "50%",
  #     height = "45%",
  #     alt = "UC Berkeley Logo"
  #   )
  # }, deleteFile = FALSE)
  # 
  # output$california_academy_logo <- renderImage({
  #   list(
  #     src = file.path("www", "California_academy_logo.png"),
  #     width = "50%",
  #     height = "45%",
  #     alt = "California Academy Logo"
  #   )
  # }, deleteFile = FALSE)
  # 
  # output$reimagining_sf_logo <- renderImage({
  #   list(
  #     src = file.path("www", "Reimagining_San_Francisco.png"),
  #     width = "50%",
  #     height = "45%",
  #     alt = "Reimagining San Francisco Logo"
  #   )
  # }, deleteFile = FALSE)


  # ------------------------------------------------
  # Leaflet Base + Hide Overlays
  # ------------------------------------------------
  output$isoMap <- renderLeaflet({
    pal_cbg <- colorNumeric("YlOrRd", cbg_vect_sf$medincE)
    
    pal_rich <- colorNumeric("YlOrRd", domain = cbg_vect_sf$unique_species)
    # Color palette for data availability
    pal_data <- colorNumeric("Blues", domain = cbg_vect_sf$n_observations)
    # browser()
    
    # Color palette for greenspace distance (reversed so closer = cooler colors)

    greenspace_vals_clean <- values(greenspace_dist_raster) |> 
      na.omit() |> 
      as.numeric()

    # Cap at 95th percentile for better color distribution
    upper_limit <- quantile(greenspace_vals_clean, 0.998, na.rm = TRUE)
    # upper_limit <- max(greenspace_vals_clean, na.rm = TRUE)

    pal_greenspace_dist <- colorNumeric(
      "YlOrRd",
      domain = c(0, upper_limit),
      na.color = "transparent",
      reverse = TRUE
    )
    
    leaflet() %>%
      addTiles(group = "Street Map (Default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (ESRI)") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      
      addPolygons(
        data = cbg_vect_sf,
        group = "Income",
        fillColor = ~pal_cbg(medincE),
        fillOpacity = 0.6,
        color = "white",
        weight = 1,
        label=~GEOID,
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
          direction = "auto",
          noHide = FALSE # Labels appear on hover
        )
      ) %>%
      
      addPolygons(
        data = rsf_projects,
        group = "RSF Program Projects",
        fillColor = "purple",
        fillOpacity = 0.3,
        color = "purple",
        weight = 1,
        label = ~prj_name,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "blue",
          fillOpacity = 0.5,
          bringToFront = TRUE
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "color" = "blue"),
          textsize = "12px",
          direction = "auto",
          noHide = FALSE # Labels appear on hover
        )
      ) %>%
      
      addPolygons(
        data = biodiv_hotspots,
        group = "Hotspots (KnowBR)",
        fillColor = "firebrick",
        fillOpacity = 0.2,
        color = "firebrick",
        weight = 2,
        label = ~GEOID,
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
        data = biodiv_coldspots,
        group = "Coldspots (KnowBR)",
        fillColor = "navyblue",
        fillOpacity = 0.2,
        color = "navyblue",
        weight = 2,
        label = ~GEOID,
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
      
      # Add Species Richness Layer
      addPolygons(
        data = cbg_vect_sf,
        group = "Species Richness",
        fillColor = ~pal_rich(unique_species),
        fillOpacity = 0.6,
        color = "white",
        weight = 1,
        label = ~unique_species,
        popup = ~paste0(
          "<strong>GEOID: </strong>", GEOID,
          "<br><strong>Species Richness: </strong>", unique_species,
          "<br><strong>Observations: </strong>", n_observations,
          "<br><strong>Median Income: </strong>", median_inc,
          "<br><strong>Mean NDVI: </strong>", ndvi_mean
        )
      ) %>%
      
      # Add Data Availability Layer
      addPolygons(
        data = cbg_vect_sf,
        group = "Data Availability",
        fillColor = ~pal_data(n_observations),
        fillOpacity = 0.6,
        color = "white",
        weight = 1,
        label = ~n_observations,
        popup = ~paste0(
          "<strong>GEOID: </strong>", GEOID,
          "<br><strong>Observations: </strong>", n_observations,
          "<br><strong>Species Richness: </strong>", unique_species,
          "<br><strong>Median Income: </strong>", median_inc,
          "<br><strong>Mean NDVI: </strong>", ndvi_mean
        )
      ) %>%
      
      # Add Greenspace Distance Raster Layer
      addRasterImage(
        x = greenspace_dist_raster,
        colors = pal_greenspace_dist,
        opacity = 0.6,
        project = TRUE,
        group = "Greenspace Distance"
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal_greenspace_dist,
        values = 0:upper_limit,
        title = "Distance to<br>Greenspace (m)",
        group = "Greenspace Distance"
      ) %>%
      
      setView(lng = -122.4194, lat = 37.7749, zoom = 12) %>%
      addLayersControl(
        baseGroups    = c("Street Map (Default)", "Satellite (ESRI)", "CartoDB.Positron"),
        overlayGroups = c("Income", "Greenspace", "Greenspace Distance", "RSF Program Projects",
                          "Hotspots (KnowBR)", "Coldspots (KnowBR)",
                          "Species Richness", "Data Availability",
                          "Isochrones", "NDVI Raster"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Income") %>%
      hideGroup("Greenspace") %>%
      hideGroup("RSF Program Projects") %>%
      hideGroup("Hotspots (KnowBR)") %>%
      hideGroup("Coldspots (KnowBR)") %>%
      hideGroup("Species Richness") %>%
      hideGroup("Data Availability") %>%
      hideGroup("Greenspace Distance")
  })
  
  
  # ------------------------------------------------
  # Observe map clicks (location_choice = 'map_click')
  # ------------------------------------------------
  observeEvent(input$isoMap_click, {
    req(input$location_choice == "map_click")
    click <- input$isoMap_click
    if (!is.null(click)) {
      chosen_point(c(lon = click$lng, lat = click$lat))
      
      # Provide feedback with coordinates
      showNotification(
        paste0("Map clicked at Longitude: ", round(click$lng, 5), 
               ", Latitude: ", round(click$lat, 5)),
        type = "message"
      )
      
      # Update the map with a marker
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
  # Observe geocoder input
  # ------------------------------------------------
  observeEvent(input$geocoder, {
    req(input$location_choice == "address")
    geocode_result <- input$geocoder
    if (!is.null(geocode_result)) {
      # Extract coordinates
      xy <- geocoder_as_xy(geocode_result)
      
      # Update the chosen_point reactive value
      chosen_point(c(lon = xy[1], lat = xy[2]))
      
      # Provide feedback with the geocoded address and coordinates
      showNotification(
        paste0("Address geocoded to Longitude: ", round(xy[1], 5), 
               ", Latitude: ", round(xy[2], 5)),
        type = "message"
      )
      
      # Update the map with a marker
      leafletProxy("isoMap") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = xy[1], lat = xy[2],
          radius = 6, color = "navyblue",
          label = "Geocoded Address"
        ) %>%
        flyTo(lng = xy[1], lat = xy[2], zoom = 13)
    }
  })
  
  # ------------------------------------------------
  # Observe clearing of map
  # ------------------------------------------------
  observeEvent(input$clear_map, {
    # Reset the chosen point
    chosen_point(NULL)
    
    # Clear all markers and isochrones from the map, but keep other layers
    leafletProxy("isoMap") %>%
      clearMarkers() %>%
      clearGroup("Isochrones") %>%
      clearGroup("NDVI Raster")
    
    # Provide feedback to the user
    showNotification("Map cleared. You can select a new location.", type = "message")
  })
  
  # ------------------------------------------------
  # Generate Isochrones
  # ------------------------------------------------
  isochrones_data <- eventReactive(input$generate_iso, {
    
    leafletProxy("isoMap") %>%
      clearGroup("Isochrones") %>%
      clearGroup("NDVI Raster")
    
    # Validate inputs
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
    # COMMENTED OUT: Something wrong with NDVI file
    # ndvi_crop <- terra::crop(ndvi, iso_union_vect)
    # ndvi_mask <- terra::mask(ndvi_crop, iso_union_vect)
    # ndvi_vals <- values(ndvi_mask)
    # ndvi_vals <- ndvi_vals[!is.na(ndvi_vals)]
    # 
    # if (length(ndvi_vals) > 0) {
    #   ndvi_pal <- colorNumeric("YlGn", domain = range(ndvi_vals, na.rm = TRUE), na.color = "transparent")
    #   
    #   leafletProxy("isoMap") %>%
    #     addRasterImage(
    #       x = ndvi_mask,
    #       colors = ndvi_pal,
    #       opacity = 0.7,
    #       project = TRUE,
    #       group = "NDVI Raster"
    #     ) %>%
    #     removeControl(layerId = "ndvi_legend") %>%
    #     addLegend(
    #       position = "bottomright",
    #       pal = ndvi_pal,
    #       values = ndvi_vals,
    #       title = "NDVI",
    #       layerId = "ndvi_legend"
    #     )
    # }
    
    # Ensure other layers remain
    leafletProxy("isoMap") %>%
      addLayersControl(
        baseGroups = c("Street Map (Default)", "Satellite (ESRI)", "CartoDB.Positron"),
        overlayGroups = c("Income", "Greenspace", "Greenspace Distance", "RSF Program Projects",
                          "Hotspots (KnowBR)", "Coldspots (KnowBR)",
                          "Species Richness", "Data Availability",
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
    
    # Get the clicked/geocoded point coordinates
    user_point <- chosen_point()
    user_point_sf <- NULL
    if (!is.null(user_point)) {
      user_point_sf <- st_as_sf(
        data.frame(lon = user_point["lon"], lat = user_point["lat"]),
        coords = c("lon", "lat"), 
        crs = 4326
      )
    }
    
    acs_wide <- cbg_vect_sf %>%
      mutate(
        population = popE,
        med_income = medincE
      )
    
    hotspot_union  <- st_union(biodiv_hotspots)
    coldspot_union <- st_union(biodiv_coldspots)
    
    results <- data.frame()
    
    # Calculate distance to coldspot and hotspots
    for (i in seq_len(nrow(iso_data))) {
      poly_i <- iso_data[i, ]
      
      dist_hot  <- st_distance(poly_i, hotspot_union)
      dist_cold <- st_distance(poly_i, coldspot_union)
      dist_hot_km  <- round(as.numeric(min(dist_hot))  / 1000, 3)
      dist_cold_km <- round(as.numeric(min(dist_cold)) / 1000, 3)
      
      inter_acs <- st_intersection(acs_wide, poly_i)
      
      vect_acs_wide <- vect(acs_wide)
      vect_poly_i <- vect(poly_i)
      inter_acs <- intersect(vect_acs_wide, vect_poly_i)
      inter_acs = st_as_sf(inter_acs)
      
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
      
      # Intersection with greenspace
      # Closest Greenspace using pre-computed distance raster (fast)
      osm_greenspace_name <- NA
      min_dist_val <- NA
      
      if (!is.null(user_point_sf)) {
        # Extract values from rasters (ensure single value with [1])
        min_dist_val <- (greenspace_dist_raster |> extract(user_point_sf) |> pull(greenspace_nearest_dist))[1]
        user_point_osm_id <- (greenspace_osmid_raster |> extract(user_point_sf) |> pull(2))[1]

        osm_greenspace_name <- osm_greenspace |> mutate(osm_id = as.numeric(osm_id)) |>   
          filter(osm_id == as.numeric(user_point_osm_id)) |> pull(name)
          # browser()
        # Ensure single value for conditional check
        if(length(osm_greenspace_name) == 0 || is.na(osm_greenspace_name[1])) {
          osm_greenspace_name <- "Unnamed Greenspace"
        } else {
          osm_greenspace_name <- osm_greenspace_name[1]
        }
      }
    
      # browser()

      # OLD: Vector intersection approach (slower)
      # vec_osm_greenspace <- vect(osm_greenspace)
      # inter_gs_old <- intersect(vec_osm_greenspace, vect_poly_i)
      # inter_gs_old = st_as_sf(inter_gs_old)
      
      # gs_area_m2_old <- 0
      # if (nrow(inter_gs_old) > 0) {
      #   gs_area_m2_old <- sum(st_area(inter_gs_old))
      # }
      # iso_area_m2_old <- as.numeric(st_area(poly_i))
      # gs_area_m2_old <- as.numeric(gs_area_m2_old)
      # gs_percent_old <- ifelse(iso_area_m2_old > 0, 100 * gs_area_m2_old / iso_area_m2_old, 0)
      
      # NEW: Raster-based approach (should be faster)
      # Crop distance raster to isochrone
      dist_crop <- terra::crop(greenspace_dist_raster, vect_poly_i)
      dist_mask <- terra::mask(dist_crop, vect_poly_i)
      
      # Pixels with distance = 0 are greenspace
      is_greenspace <- dist_mask == 0
      
      # Calculate greenspace area
      # Use cellSize() to get actual area in m² for each cell (handles degree-to-meter conversion)
      cell_areas <- terra::cellSize(is_greenspace, unit = "m")
      # Sum areas where is_greenspace = TRUE
      gs_area_m2 <- as.numeric(terra::global(cell_areas * is_greenspace, "sum", na.rm = TRUE)[1, 1])
      
      # Calculate percentage
      iso_area_m2 <- as.numeric(st_area(poly_i))
      gs_percent <- ifelse(iso_area_m2 > 0, 100 * gs_area_m2 / iso_area_m2, 0)
      
      # NDVI Calculation
      # COMMENTED OUT: Something wrong with NDVI file
      # poly_vect <- vect(poly_i)
      # ndvi_crop <- terra::crop(ndvi, poly_vect)
      # ndvi_mask <- terra::mask(ndvi_crop, poly_vect)
      # ndvi_vals <- values(ndvi_mask)
      # ndvi_vals <- ndvi_vals[!is.na(ndvi_vals)]
      mean_ndvi <- NA  # Temporarily set to NA while NDVI file is unavailable
      # mean_ndvi <- ifelse(length(ndvi_vals) > 0, round(mean(ndvi_vals, na.rm=TRUE), 3), NA)
      
      # Intersection with GBIF data using DuckDB
      iso_wkt <- st_as_text(st_geometry(poly_i)[[1]])
      
      gbif_summary <- gbif_tab |>
        filter(sql(paste0("ST_Intersects(ST_GeomFromText(geom_wkt), ST_GeomFromText('", iso_wkt, "'))"))) |>
        summarise(
          n_records = n(),
          n_species = n_distinct(species),
          n_birds = sql("COUNT(DISTINCT CASE WHEN class = 'Aves' THEN species END)"),
          n_mammals = sql("COUNT(DISTINCT CASE WHEN class = 'Mammalia' THEN species END)"),
          n_plants = sql("COUNT(DISTINCT CASE WHEN class IN ('Magnoliopsida','Liliopsida','Pinopsida','Polypodiopsida','Equisetopsida','Bryopsida','Marchantiopsida') THEN species END)")
        ) |>
        collect()
      
      # Extract values (summarise always returns 1 row, even if 0 matches)
      n_records <- ifelse(nrow(gbif_summary) > 0, gbif_summary$n_records, 0)
      n_species <- ifelse(nrow(gbif_summary) > 0, gbif_summary$n_species, 0)
      n_birds <- ifelse(nrow(gbif_summary) > 0, gbif_summary$n_birds, 0)
      n_mammals <- ifelse(nrow(gbif_summary) > 0, gbif_summary$n_mammals, 0)
      n_plants <- ifelse(nrow(gbif_summary) > 0, gbif_summary$n_plants, 0)

      
      iso_area_km2 <- round(iso_area_m2 / 1e6, 3)
      
      row_i <- data.frame(
        Mode                = tools::toTitleCase(poly_i$mode),
        Time                = poly_i$time,
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
        closest_greenspace = osm_greenspace_name,
        closest_greenspace_dist_m = round(min_dist_val, 1),
        stringsAsFactors    = FALSE
      )
      results <- rbind(results, row_i)
    }
    
    # Calculate biodiversity percentile for all isochrones combined
    iso_union <- st_union(iso_data)
    union_wkt <- st_as_text(st_geometry(iso_union)[[1]])
    
    union_gbif_summary <- gbif_tab |>
      filter(sql(paste0("ST_Intersects(ST_GeomFromText(geom_wkt), ST_GeomFromText('", union_wkt, "'))"))) |>
      summarise(n_species = n_distinct(species)) |>
      collect()
    
    union_n_species <- if (nrow(union_gbif_summary) > 0) union_gbif_summary$n_species[1] else 0
    rank_percentile <- round(100 * ecdf(cbg_vect_sf$unique_species)(union_n_species), 1)
    attr(results, "bio_percentile") <- rank_percentile

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
    
    # Select only the columns to display (excluding Greenspace_m2)
    df_display <- df |>
      select(-Greenspace_m2)
    
    DT::datatable(
      df_display,
      colnames = c(
        "Mode"                 = "Mode",
        "Time (min)"           = "Time",
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
        "Greenspace (%)"       = "Greenspace_percent"
      ),
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
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
    # browser()
    
    closest_gs <- df |> 
    slice_min(closest_greenspace_dist_m) |> 
    slice_head(n = 1) 

    gs_name <- closest_gs$closest_greenspace
    gs_dist <- closest_gs$closest_greenspace_dist_m
    
    tagList(
      strong("Closest Greenspace:"),
      # strong("Closest Greenspace (from any part of the Isochrone):"),
      p(paste0(gs_name, " (", gs_dist, "m away)"))
    )
  })
  
  # ------------------------------------------------
  # Secondary table: user-selected CLASS & FAMILY
  # ------------------------------------------------

  #' Store isochrone union WKT for GBIF tab queries
  #' DuckDB will handle spatial filtering efficiently

  iso_union_wkt <- reactiveVal(NULL) # Store WKT for spatial queries

  # Observe event when tab changes and evaluate when equals 'gbif'
  observeEvent(input$tabs, {
    req(isochrones_data())
    if (input$tabs == "gbif") {
      iso_data <- isochrones_data()
      if (is.null(iso_data) || nrow(iso_data) == 0) {
        iso_union_wkt(NULL)
        return()
      }

      iso_union <- st_union(iso_data)
      union_wkt <- st_as_text(st_geometry(iso_union)[[1]])
      iso_union_wkt(union_wkt)
    }
  })

  output$classTable <- renderDT({
    req(iso_union_wkt())
    
    union_wkt <- iso_union_wkt()
    
    # Start with spatial filter query
    query <- gbif_tab |>
      filter(sql(paste0("ST_Intersects(ST_GeomFromText(geom_wkt), ST_GeomFromText('", union_wkt, "'))")))
    
    # Apply class filter if not "All"
    if (input$class_filter != "All") {
      query <- query |> filter(class == input$class_filter)
    }
    
    # Apply family filter if not "All"
    if (input$family_filter != "All") {
      query <- query |> filter(family == input$family_filter)
    }
    
    # Aggregate by species
    species_counts <- query |>
      group_by(species) |>
      summarize(
        n_records = n(),
        mean_income = round(mean(medincE, na.rm = TRUE), 2),
        mean_ndvi = round(mean(ndvi_sentinel, na.rm = TRUE), 3),
        .groups = "drop"
      ) |>
      arrange(desc(n_records)) |>
      collect()

    if (nrow(species_counts) == 0) {
      return(DT::datatable(data.frame("Message" = "No records for that combination in the isochrone.")))
    }

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
      geom_line(aes(y = EstimatedPopulation / 1000, group = 1), color = "red", linewidth = 1) +
      geom_point(aes(y = EstimatedPopulation / 1000), color = "red", size = 3) +
      labs(
        x = "Isochrone (Mode-Time)",
        y = "Unique Species (Blue) | Population (Red) (Thousands)",
        title = "Biodiversity & Socioeconomic Summary"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title   = element_text(hjust = 0.5, size = 16, face = "bold")
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
    union_wkt <- st_as_text(st_geometry(iso_union)[[1]])
    
    # Query GBIF data with spatial filter and aggregate by institution
    df_code <- gbif_tab |>
      filter(sql(paste0("ST_Intersects(ST_GeomFromText(geom_wkt), ST_GeomFromText('", union_wkt, "'))"))) |>
      group_by(institutionCode) |>
      summarize(count = n(), .groups = "drop") |>
      arrange(desc(count)) |>
      collect() |>
      mutate(truncatedCode = substr(institutionCode, 1, 5))
    
    if (nrow(df_code) == 0) {
      plot.new()
      title("No GBIF records found in this isochrone.")
      return(NULL)
    }
    
    ggplot(df_code, aes(x = reorder(truncatedCode, -count), y = count)) +
      geom_bar(stat = "identity", fill = "darkorange", alpha = 0.7) +
      labs(
        x = "Institution Code (Truncated)",
        y = "Number of Records",
        title = "GBIF Records by Institution Code (Isochrone Union)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title   = element_text(hjust = 0.5, size = 16, face = "bold")
      )
  })
  
  # ------------------------------------------------
  # Additional Plot: n_observations vs n_species
  # ------------------------------------------------
  
  # Make it reactive: obsVsSpeciesPlot updates dynamically based on user-selected class_filter or family_filter. 
  
  filtered_data <- reactive({
    data <- cbg_vect_sf
    if (input$class_filter != "All") {
      data <- data[data$class == input$class_filter, ]
    }
    if (input$family_filter != "All") {
      data <- data[data$family == input$family_filter, ]
    }
    data
  })
  
  output$obsVsSpeciesPlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) {
      plot.new()
      title("No data available for selected filters.")
      return(NULL)
    }
    
    ggplot(data, aes(x = log(n_observations + 1), y = log(unique_species + 1))) +
      geom_point(color = "blue", alpha = 0.6) +
      labs(
        x = "Log(Number of Observations + 1)",
        y = "Log(Species Richness + 1)",
        title = "Data Availability vs. Species Richness"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title   = element_text(hjust = 0.5, size = 16, face = "bold")
      )
  })
  
  
  
  # ------------------------------------------------
  # [Optional: Linear Model Plot (Commented Out)]
  # ------------------------------------------------
  # Uncomment and adjust if needed
  # output$lmCoefficientsPlot <- renderPlot({
  #   df_lm <- cbg_vect_sf %>% 
  #     filter(!is.na(n_observations), 
  #            !is.na(unique_species),
  #            !is.na(median_inc),
  #            !is.na(ndvi_mean))
  #   
  #   if (nrow(df_lm) < 5) {
  #     plot.new()
  #     title("Not enough data for linear model.")
  #     return(NULL)
  #   }
  #   
  #   fit <- lm(unique_species ~ n_observations + median_inc + ndvi_mean, data = df_lm)
  #   
  #   p <- plot_model(fit, show.values = TRUE, value.offset = .3, title = "LM Coefficients: n_species ~ n_observations + median_inc + ndvi_mean")
  #   print(p)
  # })


# 
# # Add Images:
# df_img = data.frame(id = c(1:3), img_path=c('California_academy_logo.png', 'Reimagining_San_Francisco.png', 'UC Berkeley_logo.png'))
# n <- nrow(df_img)
# 
# n <- nrow(df_img)
# 
# observe({
#   for (i in 1:n)
#   {
#     print(i)
#     local({
#       my_i <- i
#       imagename = paste0("img", my_i)
#       print(imagename)
#       output[[imagename]] <-
#         renderImage({
#           list(src = file.path('www', df_img$img_path[my_i]), 
#                width = "100%", height = "55%",
#                alt = "Image failed to render")
#         }, deleteFile = FALSE)
#     })
#   }
# })
# 
# 
# output$houz <- renderUI({
#   
#   image_output_list <- 
#     lapply(1:n,
#            function(i)
#            {
#              imagename = paste0("img", i)
#              imageOutput(imagename)
#            })
#   
#   do.call(tagList, image_output_list)
# })


}



# Run the Shiny app
shinyApp(ui, server)