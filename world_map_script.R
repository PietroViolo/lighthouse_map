# Library
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(raster)
library(terra)
library(plotwidgets)
library(ggshadow)
library(ggspatial)
library(ggnewscale)
library(janitor)
library(rnaturalearth)


# remotes::install_github('ropensci/osmdata')

# POI : https://wiki.openstreetmap.org/wiki/Map_features
  
#Firearms united states
m <- c(-165.2, -74.0, 173.0, 81.0)

#building the query
lighthouses <- m %>% 
  opq (timeout = 300*100) %>%
  add_osm_feature("man_made", "lighthouse")

#query
lighthouses_location <- osmdata_sf(lighthouses)


# Map making

map <- rast("snapshot-2022-06-12T00_00_00Z.tiff")

#map <- crop(map, extent(-125.064, 23.0486, -63.2310, 49.7254))

# Unsaturated version of the map

saturation <- function(rgb, s = .5){
  
  hsl <- rgb2hsl(as.matrix(rgb))
  hsl[2, ] <- s
  
  rgb_new <- as.vector(t(hsl2rgb(hsl)))
  
  return(rgb_new)
  
}

# apply the function to unsaturate with 5%

map_desat <- app(map, saturation, s = .05)

ggplot()  +
  layer_spatial(data = stack(map_desat))  +
  geom_glowpoint(data = lighthouses_location$osm_points,
                 aes(geometry = geometry),
                 alpha = 0.7,
                 color = "#ffff00",
                 shadowcolour = "#ffff00",
                 stat = "sf_coordinates",
                 shadowalpha = 0.005)+
  geom_glowpoint(data = lighthouses_location$osm_points,
                 aes(geometry = geometry),
                 alpha = 0.3,
                 color = "#ffffff",
                 stat = "sf_coordinates",
                 shadowalpha = 0.002) +
  theme_void() +
  theme(plot.title = element_text(size = 50, vjust = -5, colour = "white", hjust = .95))

ggsave("world_map.png", width = 15, height = 15, units = "in", dpi = 350)
