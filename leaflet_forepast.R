#---------------------------------------------------------------------------------------
# FOREPAST project
# Authors: Koch, A., Wheeler, C., Benito, X.
# contact e-mail: xavier.benito.granell@gmail.com
#---------------------------------------------------------------------------------------

# Load libraries for functions used
library(leaflet)
library(sf)
library(tidyverse)
library(glue)
library(mapview)
library(raster)
library(rnaturalearth)

# World data countries
library(maps)
library(rworldmap)
world <- map_data("world")

world_sf <- st_as_sf(world, coords = c("long", "lat"), 
                     crs = 4326, agr = "constant")

# Read in the database entries
entries <- read.csv("FOREPAST Knowledge Database - FOREPAST_DB_v2.csv", sep=",") %>%
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

# simple plot of Bioregions map
ggplot() +
  geom_sf(data = bioregions, aes(fill = BIOREGIO_1)) +
  theme(legend.position = "none") +
  geom_sf(data=entries_sf) +
  coord_sf(xlim = c(-160, 160), ylim = c(-30, 30)) 

# check 
st_is_valid(bioregions)

# Overlay bioregions where locations are 
entries.clip <- bioregions[entries_sf, ]

# create new variables that recode Bioregions to subrealms
subrealms_clip <- entries.clip %>%
  mutate(subrealm = recode(BIOREGION_, "NT11" = "Andes & Pacific Coast",
                           "NT24" = "Central America",
                           "NT5" ="Andes & Pacific Coast",
                           "NT18" = "Amazonia",
                           "NT17" = "Amazonia",
                           "NT19" = "Amazonia",
                           "NT20" = "Amazonia",
                           "NT12" = "Brazil Cerrado & Atlantic Coast",
                           "AU9" = "Australia",
                           "AU13" = "Australasian Islands & Eastern Indonesia",
                           "AU14" = "Australasian Islands & Eastern Indonesia",
                           "IM2" = "Indian Subcontinent",
                           "IM7" = "Indian Subcontinent",
                           "IM12" = "SouthEast Asian Forests",
                           "IM17" = "Malaysia & Western Indonesia",
                           "IM16" = "Malaysia & Western Indonesia",
                           "IM18" = "Malaysia & Western Indonesia",
                           "AT11" = "Sub-equtorial Afrotropics",
                           "AT13" = "Equatorial Afrotropics",
                           "AT12" = "Equatorial Afrotropics",
                           "AT14" = "Equatorial Afrotropics",
                           "AT7" = "Madagascar & East African Coast",
                           "AT16" = "Equatorial Afrotropics",
                           "AT19" = "Equatorial Afrotropics",
                           "AT21" = "Sub-Saharan Afrotropics",
                           )) 

# Count how many sites there are per subrealm
subrealms_clip$count <- lengths(st_intersects(subrealms_clip, entries_sf))
mapview(subrealms_clip)

#check
st_is_valid(subrealms_clip)

theme_set(theme_bw())

#plot
plt1 <- ggplot() +
  geom_sf(data=subrealms_clip, aes(fill=count))+
  #geom_sf(data = subrealms_clip
  scale_fill_viridis_c(option = "magma", "") +
  geom_sf(data=entries_sf, aes(), colour="grey", shape=17, size=3) +
  geom_sf(data=world_sf, aes(), size=0.3) + #plot world map
  theme(legend.position = "bottom",
         legend.title = element_text("Number of studies")) +
  coord_sf(xlim = c(-130, 160), ylim = c(-30, 30)) 
plt1

#save the plot
ggsave("outputs/counts_subrealms.png",
       plot = last_plot(),
       width = 19,
       height = 8)

