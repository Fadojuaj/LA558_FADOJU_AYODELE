
# March 23, 2023
# Fadoju, Ayodele

# Secondary Hospitals

install.packages("leaflet", "leaflet.providers", "tidyverse", "sf")
library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(readxl)
library(sf)

# Set working directory to the same as this R file.
# Read in the shapefile
Sec_Hospital <- st_read("NGA_Secondary_Hospitals.shp")


# I should have corrected the name of the count field. It is currently 
# last_name_, but I can use dplyr to rename the column!
Sec_Hospital <- Sec_Hospital %>% rename(Hosp_count = ID_1_count)
Sec_Hospital <- Sec_Hospital %>% rename(State = NAME_1)

# convert Sec_Hospital to UTM zone 32N (for example)
st_crs(Sec_Hospital)
Sec_Hospital <- st_transform(Sec_Hospital, crs = 32632)

m <- leaflet() %>%
  setView(8.6753, 9.081999, 6) %>%
  addTiles() %>%
  addPolygons(data = Sec_Hospital, color = "red", fill = NA, weight = 2)
m


# Or maybe only the counties with no secondary hospitals? Look for not > than 0 or NA
HospitalCount_selection2 <- Sec_Hospital %>% 
  filter(is.na(Hosp_count) | !Hosp_count > 0)

m <- leaflet() %>%
  setView(8.6753, 9.081999, 6)  %>%
  addTiles() %>%
  addPolygons(data = HospitalCount_selection2,  # borders of all counties
    color = "#000", fillColor = "blue", weight = 1, 
    opacity = 0.75, fillOpacity = 0.8)
m


# First I better replace the NA in the entire dataframe with a 0.
hospitalCount <- Sec_Hospital %>%
  replace(is.na(.), 0)

# Select the color scheme from Color Brewer
library("RColorBrewer") #I think either Leaflet or tidyverse loads this for you
display.brewer.all()


bins <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
pal <- colorBin("PuRd", domain = hospitalCount$Hosp_count, bins = bins)

m <- leaflet() %>%
  setView(8.6753, 9.081999, 6)  %>%
  addTiles() %>%
  addPolygons(data = hospitalCount,
    fillColor = ~pal(Hosp_count),
    weight = 0.5,
    opacity = 1,
    color = "red",
    dashArray = "1",
    fillOpacity = 0.8)
m


# Add interaction
m <- leaflet() %>%
  setView(8.6753, 9.081999, 6)  %>%
  addTiles() %>%
  addPolygons(data = hospitalCount,
    fillColor = ~pal(Hosp_count),
    weight = 0.5,
    opacity = 1,
    color = "red",
    dashArray = "1",
    fillOpacity = 0.8,  #be careful, you need to switch the ) to a comma
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE)
  )
m


labels <- sprintf(
  "<strong>%s</strong><br/>%g Secondary_Hospital",
  hospitalCount$State, hospitalCount$Hosp_count
) %>% lapply(htmltools::HTML)

m <- leaflet() %>%
  setView(8.6753, 9.081999, 6)  %>%
  addTiles() %>%
  addPolygons(data = hospitalCount,
    fillColor = ~pal(Hosp_count),
    weight = 0.5,
    opacity = 1,
    color = "grey",
    dashArray = "1",
    fillOpacity = 0.8,  #be careful, you need to switch the ) to a comma
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "10px",
      direction = "auto"))
m


#finally we can add a legend! I will append this to m
m %>% addLegend(pal = pal, values = count, opacity = 0.7, title = "Secondary Hospital",
                position = "bottomright")


#now export this as a web page!!!
