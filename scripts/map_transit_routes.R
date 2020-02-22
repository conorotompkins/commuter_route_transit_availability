library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(janitor)
library(tidycensus)




transit <- st_read("data/shapefiles/transit/PAAC_Routes_1909.shp") %>% 
  clean_names() %>% 
  st_transform(crs = 4326)

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
  st_transform(crs = 4326) %>%  
  arrange(total_pop) %>% 
  top_n(1, total_pop)

allegheny_tracts


df_joined <- transit %>% 
  st_join(allegheny_tracts, st_crosses, left = TRUE)


df_joined
