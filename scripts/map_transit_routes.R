library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(janitor)
library(tidycensus)
library(mapview)

transit <- st_read("data/shapefiles/transit_lines/PAAC_Routes_1909.shp") %>% 
  clean_names() %>% 
  st_transform(crs = "+init=epsg:4326")

transit %>% 
  ggplot() +
    geom_sf(aes(color = mode)) +
    theme_minimal()

transit %>% 
  st_set_geometry(NULL) %>% 
  distinct(route, route_name)

transit %>% 
  filter(str_detect(route, "^6|71"))

transit %>% 
  filter(type_serv == "Key Corridor" |
           mode == "Light Rail") %>% 
  ggplot() +
    geom_sf(aes(color = route_name)) +
    geom_sf_label(aes(label = route)) +
    #facet_wrap(~mode) +
    theme_minimal()


####
allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide") %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  mutate(name = case_when(GEOID == "42003020100" ~ "Downtown",
                          GEOID == "42003070300" ~ "Shadyside"))

downtown <- allegheny_tracts %>% 
  filter(GEOID == "42003020100")




allegheny_tracts %>% 
  ggplot() +
    geom_sf()

allegheny_tracts %>% 
  mapview()

mapview(list(allegheny_tracts, transit),
        layer.name = c("Census tracts", "transit"))

mapview(allegheny_tracts, zcol = "name") +
  mapview(transit, zcol = "route")

factpal <- colorFactor(palette = "viridis", domain = transit$route)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = allegheny_tracts,
              color = "#444444",
              stroke = TRUE,
              fillOpacity = 0.5,
              opacity = 1,
              weight = 2) %>% 
  addPolylines(data = transit,
               color = ~factpal(route))
