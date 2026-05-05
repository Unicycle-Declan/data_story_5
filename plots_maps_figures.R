# plot maps figures
# library ----
library(leaflet)
library(dplyr)
library(gsheet)
library(sf)
library(knitr)
library(ggplot2)
library(leafem)
library(leaflet.extras)

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
  }else if(i == "Quercus velutina"){
    trees <- trees %>%
      mutate(species_acr = gsub("Quercus velutina", "QV",trees$species_acr))
  }else if(i == "Quercus pagoda"){
    trees <- trees %>%
      mutate(species_acr = gsub("Quercus pagoda", "QP",trees$species_acr))
  }else if(i == "Quercus montana"){
    trees <- trees %>%
      mutate(species_acr = gsub("Quercus montana", "QM",trees$species_acr))
  }else if(i == "Quercus stellata"){
    trees <- trees %>%
      mutate(species_acr = gsub("Quercus stellata", "QS",trees$species_acr))
  }
} 

trees$species_acr <- 
  gsub("LS ","LS", trees$species_acr)

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

## shortleaf pine ----
shortleaf <-
  read_sf("/Users/declanoberlies/Repos/data_story_5/mygeodata (6)")

# mapping ----
# run this code to see the map
leaflet()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Base Map")%>%
  # setView()%>%
  addCircles(data = plots, 
             radius = 11.35,
             color = "limegreen",
             fill = "limegreen",
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
  addPolygons(data = shortleaf,
              color = "orange",
              weight = 2,
              group = "shortleaf pine") %>%
  addMiniMap(width = 100, height = 100,
             zoomLevelOffset = -8,
             zoomLevelFixed = TRUE) %>%
  addLegend(position = "topright",
            labels = c("Sample Plots", "Transect", "Stand", "shortleaf pine"),
            colors = c("limegreen", "red", "cyan", "orange"),
            title = "Map Layers") %>%
  addLayersControl(baseGroups = "Base Map", 
                   overlayGroups = c("Stand",
                                     "Sample Plots",
                                     "Transect", 
                                     "shortleaf pine"), 
                   position = "topright") %>%
  addResetMapButton()
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
  left_join(plot_sum,Ar_sum) %>%
  rename(`Avg. Height` = `mean(height)`)
  
kable(plot_sum)

## Arundinaria totals ----
Ar_tol <- 
  plot_sum %>%
  summarise(`Ar Clumps/Acre` = mean(`Ar Clumps/Acre`),
            `Ar ft^2/Acre` = mean(`Ar ft^2/Acre`),
            `mean clump size` = mean(`mean clump size`)
                               )
kable(Ar_tol)

# graphs ----
# remove outlyer ----
plot_sum_filt <- plot_sum %>%
  filter(plot != 3)
## with aria ----
## Avg. Height
A <- ggplot(plot_sum_filt,
            aes(x = `Avg. Height`,
                y = `Ar ft^2/Acre`))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "A")

## BA/Acre
B <- ggplot(plot_sum_filt,
            aes(x = `BA/Acre`,
                y = `Ar ft^2/Acre`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "B")

## Trees/Acre
C <- ggplot(plot_sum_filt,
            aes(x = `Trees/Acre`,
                y = `Ar ft^2/Acre`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "C")

A + B + C

# A1
summary(lm(`Ar ft^2/Acre` ~ `Avg. Height`, data = plot_sum_filt))
# B1
summary(lm(`Ar ft^2/Acre` ~ `BA/Acre`, data = plot_sum_filt))
# C1
summary(lm(`Ar ft^2/Acre` ~ `Trees/Acre`, data = plot_sum_filt))

## with clump size ----
## Avg. Height
A <- ggplot(plot_sum_filt,
            aes(x = `Avg. Height`,
                y = `mean clump size`))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "A")

## BA/Acre
B <- ggplot(plot_sum_filt,
            aes(x = `BA/Acre`,
                y = `mean clump size`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "B")

## Trees/Acre
C <- ggplot(plot_sum_filt,
            aes(x = `Trees/Acre`,
                y = `mean clump size`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "C")

A + B + C

# A2
summary(lm(`mean clump size` ~ `Avg. Height`, data = plot_sum_filt))
# B2
summary(lm(`mean clump size` ~ `BA/Acre`, data = plot_sum_filt))
# C2
summary(lm(`mean clump size` ~ `Trees/Acre`, data = plot_sum_filt))


## indicator species ----

ggplot(trees %>%
         left_join(plot_sum %>% 
                     select(plot, `Ar ft^2/Acre`), 
                   by = "plot") %>%
         mutate(plot_label = paste0(plot, ")","           ", `Ar ft^2/Acre`)),
       aes(x = species_acr)) +
  geom_bar() +
  facet_wrap(~plot_label) +
  labs(x = "")

# comparison to original state ---- 




