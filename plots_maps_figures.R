# plot maps figures
# library ----
library(leaflet)
library(dplyr)
library(gsheet)
library(sf)
library(knitr)
library(ggplot2)

# load datasets ----
## tree data ----
trees <- 
  read.csv("/Users/declanoberlies/Repos/data_story_5/tree_data.csv", 
           header = FALSE) %>%
  rename(time = V1,
         plot = V2,
         DBH = V3,
         height = V4,
         species = V5) %>%
  mutate(species_acr = species,
         BA = .005454*(DBH^2))

for (i in trees$species_acr){
  if(i == "Quercus coccinea"){
    trees <- trees %>%
      mutate(species_acr = gsub("Quercus coccinea", "QC",trees$species_acr))
  }else if(i == "Liquidambar styraciflua"){
    trees <- trees %>%
      mutate(species_acr = gsub("Liquidambar styraciflua", "LS",trees$species_acr))
  }else if(i == "Oxydendrum arboreum"){
    trees <- trees %>%
      mutate(species_acr = gsub("Oxydendrum arboreum", "OA",trees$species_acr))
  }else if(i == "Acer rubrum"){
    trees <- trees %>%
      mutate(species_acr = gsub("Acer rubrum", "AR",trees$species_acr))
  }else if(i == "Sassafras albidum"){
    trees <- trees %>%
      mutate(species_acr = gsub("Sassafras albidum", "SA",trees$species_acr))
  }else if(i == "Quercus alba"){
    trees <- trees %>%
      mutate(species_acr = gsub("Quercus alba", "OA",trees$species_acr))
  }else if(i == "Nyssa sylvatica"){
    trees <- trees %>%
      mutate(species_acr = gsub("Nyssa sylvatica", "NS",trees$species_acr))
  }
}

## Aroundinaria
Ar_data <- 
  read.csv("/Users/declanoberlies/Repos/data_story_5/Ar_data.csv", header = FALSE) %>%
  rename(time = V1,
         plot = V2,
         area = V3)


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

# summary table ----
plot_sum1 <-
  count(trees %>% group_by(plot)) %>%
  mutate(n = n*10) %>%
  rename(`Trees/Acre` = n)
  
plot_sum2 <-
  trees %>%
  group_by(plot) %>%
  summarise(mean(height), 
            `BA/Acre` = sum(BA)*10
            )
plot_sum <- 
  left_join(plot_sum2, plot_sum1)

Ar_sum1 <-
  count(Ar_data%>% group_by(plot)) %>%
  mutate(n = n*10) %>%
  rename(`Ar Clumps/Acre` = n)

Ar_sum2 <- 
  Ar_data %>%
  group_by(plot) %>%
  summarise(`Ar ft^2/Acre` = sum(area)*10)

Ar_sum <-
  left_join(Ar_sum1,Ar_sum2) %>%
  mutate(`mean clump size` = `Ar ft^2/Acre`/`Ar Clumps/Acre`)

plot_sum <-
  left_join(plot_sum,Ar_sum)
  
kable(plot_sum)

# BA and A graph ----
ggplot(plot_sum,
       aes(x = `BA/Acre`,
           y = ``))

# comparison to original state ----

  
