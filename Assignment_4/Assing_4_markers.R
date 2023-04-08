# Leaflet mapping: adding multiple markers to slippy map
# 
# March 23, 2023
# Fadoju, Ayodele

install.packages("leaflet", "leaflet.providers", "tidyverse")

library(leaflet)
library(leaflet.providers)
library(tidyverse)


housingStock80 <- read.csv("Housing_Stock_Southwest_Nigeria_80.csv", header = TRUE)
housingstock1000 <- read.csv("Housing_Stock_Southwest_Nigeria_1000.csv", header = TRUE)
myData <- housingStock80

# Add markers from the CSV to this map
map <- leaflet(myData) %>% 
  addTiles() %>%
  addMarkers(~longitude, ~latitude)
map

names(providers)

map <- leaflet(myData) %>% 
  addTiles(group = "OSM", options = providerTileOptions(minZoom = 4, maxZoom = 10)) %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner", 
    options = providerTileOptions(minZoom = 8, maxZoom = 10)) %>%
  addProviderTiles("Esri.WorldStreetMap", group = "WorldStreetMap") %>%
  addProviderTiles("OpenMapSurfer.Hillshade", group = "Hillshade") %>%
  addProviderTiles("Stamen.TerrainBackground", group = "Stamen") %>%
  addProviderTiles("OneMapSG.Night", group = "Night") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "WorldGrayCanvas") %>%
  addLayersControl(baseGroups = c("OSM", "Toner", "WorldStreetMap", "Hillshade", "Stamen", "Night", "WorldGrayCanvas"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  addCircles(~longitude, ~latitude, popup = paste("<strong>", 
  myData$City, "</strong><br> ", "Housing Units: ", 
  myData$Housing_Units, "<br>", "Monthly Rent Range: ",
  myData$Monthly_Rent_Range), weight = 4, radius=2000, 
  color="red", stroke = TRUE, fillOpacity = 0.8)
map


# Create map with markers and clusters
myData <- housingstock1000

map <- leaflet(myData) %>% 
  addProviderTiles("Stamen.TonerLite", 
                   options = providerTileOptions(minZoom = 4, maxZoom = 10)) %>%
  
  addMarkers(~Longitude, ~Latitude, 
             clusterOptions = markerClusterOptions())
map







