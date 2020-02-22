library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(janitor)
library(tidycensus)
library(mapview)

transit <- st_read("data/shapefiles/transit/PAAC_Routes_1909.shp") %>% 
  clean_names() %>%
  distinct(route, route_name, geometry) %>% 
  st_set_crs(4326)

allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide") %>% 
  #st_transform(crs = "+init=epsg:4326") %>% 
  mutate(name = case_when(GEOID == "42003020100" ~ "Downtown",
                          GEOID == "42003070300" ~ "Shadyside")) %>% 
  st_transform(4326)

allegheny_tracts_centroid <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide") %>% 
  #st_transform(crs = "+init=epsg:4326") %>% 
  mutate(name = case_when(GEOID == "42003020100" ~ "Downtown",
                          GEOID == "42003070300" ~ "Shadyside")) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(4326)

allegheny <- allegheny_tracts %>% 
  summarize()

allegheny_tracts %>% 
  ggplot() +
    geom_sf()

commute <- allegheny_tracts %>% 
  filter(!is.na(name))


df_joined_crosses <- transit %>% 
  st_join(commute, st_crosses, left = TRUE)

st_crs(commute)

st_crs(transit)

df_joined_distance <- transit %>% 
  st_join(commute, st_is_within_distance, dist = 2000, left = TRUE)

df_joined_distance %>% 
  filter(!is.na(name))

df_route_distance <- df_joined_distance %>% 
  group_by(route) %>% 
  filter(n() > 1)

df_route_distance %>% 
  #filter(route == "61C") %>% 
  ggplot() +
  #geom_sf(data = allegheny, fill = "#00000000") +
  geom_sf(data = commute, aes(fill = name), color = NA) +
  geom_sf(data = commute, fill = "#00000000", size = 1) +
  geom_sf(aes(color = route)) +
  #facet_wrap(~downtown_flag) +
  theme_void()




df_route_crosses <- df_joined_crosses %>% 
  group_by(route) %>% 
  filter(n() > 1)



df_route_crosses %>% 
  ggplot() +
    geom_sf(data = allegheny, fill = "#00000000") +
    geom_sf(data = commute, aes(fill = name), color = NA) +
    geom_sf(data = commute, fill = "#00000000", size = 1) +
    geom_sf(aes(color = route)) +
    #facet_wrap(~downtown_flag) +
    theme_void()


