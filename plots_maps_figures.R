# plot maps figures
# library ----
library(leaflet)
library(dplyr)
library(gsheet)
library(sf)

# load datasets ----
## tree data ----
trees <- 
  gsheet2tbl("https://docs.google.com/spreadsheets/d/1EzoSJ6p7loAnF8X62e3H68zk_KSnE8Whvy7PnRCPATE/edit?usp=sharing")

## plot locations ----
plots <- 
  gsheet2tbl("https://docs.google.com/spreadsheets/d/1W3ZGlZ7DAAIwWVRsqK9zdD7X7f8kbHP_Lzb8_f0RV2I/edit?usp=sharing")

## transect ----
trans <- 
  read_sf("/Users/declanoberlies/Downloads/mygeodata (4) 2")

## stand ----
stand <- 
  read_sf("/Users/declanoberlies/Downloads/mygeodata (5)")

# mapping ----
# run this code to see the map
leaflet()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Base Map")%>%
  # setView()%>%
  addCircles(data = plots, 
             radius = 11.35,
             color = "red",
             fill = "red",
             weight = 1,
             group = "Sample Plots") %>% # radious is in meters
  addPolylines(data = trans, 
               color = "red",
               weight = 3,
               group = "Transect") %>%
  addPolygons(data = stand, 
              color = "cyan",
              weight = 2,
              group = "Stand") %>%
  addLayersControl(baseGroups = "Base Map", 
                   overlayGroups = c("Stand","Sample Plots","Transect"), 
                   position = "topright") %>%
  addMiniMap(width = 100, height = 100,
             zoomLevelOffset = -8,
             zoomLevelFixed = TRUE)
             # centerFixed = TRUE)
  
