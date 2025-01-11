# SF_biodiv_access_shiny

App.R runs both the ui and server side of the add and loads necessary objects in R/setup.R. Shiny App working locally, but errors when pushing to ShinyApps.io.

The aim of this Shiny app is to provide decision support for the Reimagining San Francisco Initiative

![Screenshot of the App](www/app_screenshot_1.png)


This Shiny App takes:

Long/Lat on a mac by a users click OR typing of adress using geocoder.

Select a travel time and transportation code to calculate isochromes

The background then allows to identify biodiversity around a calculted isochrome as well as socio-economic and environmental variables

It further calculates a summary table of the GBIF data located within the isochrome


# Next steps: Optimize preanno of sf gbif and cbg

Add Imp Surf, Walking Scores, SVI to cbg_sf

Add community grass root partner orgs locations

Get images to work 

# Public transport ddata

Calculate accessability matrix for SF


# Show difference on the day

