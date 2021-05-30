library(leaflet)
library(sf)
library(tidyverse)
library(glue)
library(mapview)
library(raster)
library(rnaturalearth)


entries <- read.csv("FOREPAST Knowledge Database - FOREPAST_DB.csv", sep=",") %>%
  # rename(latitude = lat,
  #        longitude = lon) %>%
  filter(!is.na(latitude)) # drop non-local entries

#convert to spatial object
entries_sf <- st_as_sf(entries, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

# plot database locations
mapview(entries_sf)

# here using leaflet()
leaflet() %>%
  addTiles(group = "Open Street Maps") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addMarkers(data = entries_sf, clusterOptions = markerClusterOptions(), 
             popup = ~glue("{entity_id} <br> Theme: {theme} <br> Time slice: {time_slice} <br> ")) %>%
  addLayersControl(baseGroups = c("Open Street Maps", "World Imagery"))


# Read Bioregions shapefile
bioregions <- sf::st_read("OE_Bioregions/OE_Bioregions.shp")
mapview(bioregions)

ggplot() +
  geom_sf(data = bioregions, aes(fill = BIOREGIO_1)) +
  theme(legend.position = "none") +
  geom_sf(data=entries_sf) +
  coord_sf(xlim = c(-160, 160), ylim = c(-30, 30)) 

# Overlay bioregions where locations are 
# check 
st_is_valid(bioregions)

# Overlay bioregions where locations are 
bioregions$count <- lengths(st_intersects(bioregions, entries_sf))
st_is_valid(bioregions)

theme_set(theme_bw())

ggplot() +
  geom_sf(data=bioregions, aes(fill=count))+
  scale_fill_viridis_c(option = "magma", "") +
  geom_sf(data=entries_sf, aes(colour="red")) +
  theme(legend.position = "Number of studies") +
  coord_sf(xlim = c(-130, 160), ylim = c(-30, 30)) 


# World data countries
library(maps)
library(rwordlmap)
world <- map_data("world")

world_sf <- st_as_sf(world, coords = c("long", "lat"), 
                       crs = 4326, agr = "constant")

# crop bioregions with countries shapefile
output <- crop(x, y) 


