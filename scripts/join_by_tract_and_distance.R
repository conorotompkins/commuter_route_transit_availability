library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(tidycensus)

theme_set(theme_minimal())

##load transit data
transit_lines <- st_read("data/shapefiles/transit_lines/PAAC_Routes_1909.shp") %>%
  clean_names() %>%
  distinct(route, route_name, geometry) %>%
  st_transform(4326)

##load tract data
allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide") %>%
  mutate(name = case_when(GEOID == "42003020100" ~ "Downtown",
                          GEOID == "42003070300" ~ "Shadyside")) %>% 
  st_transform(4326)

allegheny_tracts_centroid <- allegheny_tracts %>%
  mutate(name = case_when(GEOID == "42003020100" ~ "Downtown",
                          GEOID == "42003070300" ~ "Shadyside")) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(4326)

allegheny <- allegheny_tracts %>% 
  summarize()

commute_tracts <- allegheny_tracts %>% 
  filter(!is.na(name))

commute_centroids <- allegheny_tracts_centroid %>% 
  filter(!is.na(name))

df_joined_distance <- transit_lines %>% 
  st_join(commute_centroids, st_is_within_distance, dist = 500, left = TRUE) %>% 
  arrange(route)

df_joined_distance <- df_joined_distance%>% 
  filter(!is.na(name))

df_joined_distance <- df_joined_distance %>% 
  group_by(route) %>% 
  filter(n() >= 2)


commute_zoom <- commute_tracts %>% 
  st_bbox()


allegheny_tracts %>% 
  ggplot() +
  geom_sf() +
  #geom_sf(data = allegheny_tracts_centroid) +
  geom_sf(data = commute_centroids)


df_joined_distance %>% 
  ggplot() +
  geom_sf(data = commute_tracts, size = 2,  color = "red") +
  geom_sf(data = commute_centroids, color = "red") +
  geom_sf()






df_joined_distance %>% 
  #filter(route == "61C") %>% 
  ggplot() +
  geom_sf(data = allegheny, fill = "#00000000") +
  geom_sf(data = commute_tracts, aes(fill = name), size = 1, alpha = .5) +
  geom_sf(data = commute_centroids) +
  geom_sf(aes(color = route)) +
  coord_sf(xlim = c(commute_zoom[1], commute_zoom[3]),
           ylim = c(commute_zoom[2], commute_zoom[4])) +
  #facet_wrap(~route) +
  theme_void()
