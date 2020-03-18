library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(tidycensus)
library(leaflet)
library(vroom)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 2)

source("scripts/functions.R")

#load transit data
transit_lines <- st_read("data/shapefiles/transit_lines/PAAC_Routes_1909.shp") %>%
  clean_names() %>%
  mutate_at(vars(-all_of(c("geometry"))), as.character) %>%
  rename(route_id = route,
         service_type = type_serv) %>% 
  distinct(service_type, route_id, route_name, geometry) %>%
  mutate(full_route_name_id = str_c(route_id, route_name, sep = " ")) %>% 
  st_transform(3488)

transit_lines

df_service_type <- transit_lines %>% 
  distinct(service_type, route_id, full_route_name_id) %>% 
  st_drop_geometry()

transit_stops <- st_read("data/shapefiles/transit_stops/PAAC_Stops_1909.shp") %>%
  st_transform(3488) %>% 
  clean_names() %>% 
  mutate_at(vars(-all_of(c("geometry", "routes_cou"))), as.character) %>%
  select(stop_name, routes_served = routes_ser, routes_cou, geometry) %>% 
  distinct(stop_name, routes_served = routes_served, routes_cou, geometry)

transit_stops

####identify maximum number of routes served by a stop
max_routes_served <- transit_stops %>% 
  summarize(max_routes = max(routes_cou)) %>% 
  pull(max_routes)

transit_stops %>% 
  filter(routes_cou == max_routes_served)

####separate routes_served into multiple columns, one per route
transit_stops <- transit_stops %>% 
  separate(routes_served, sep = ", ", into = str_c("route_", 1:max_routes_served), extra = "merge", fill = "right")

transit_stops

transit_stops %>% 
  filter(routes_cou == max_routes_served)

####pivot data longer
transit_stops <- transit_stops %>% 
  pivot_longer(cols = starts_with("route_"), names_to = "route_number", values_to = "route_id") %>% 
  st_as_sf()


transit_stops

transit_stops <- transit_stops %>%
  filter(!is.na(route_id)) %>% 
  left_join(df_service_type)

#load tract data
allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide") %>%
  mutate(name = case_when(GEOID == "42003020100" ~ "Downtown",
                          GEOID == "42003070300" ~ "Shadyside")) %>% 
  st_transform(3488)

####calculate centers of the tracts
allegheny_tracts_centroid <- allegheny_tracts %>%
  mutate(name = case_when(GEOID == "42003020100" ~ "Downtown",
                          GEOID == "42003070300" ~ "Shadyside")) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 3488) %>% 
  st_transform(3488)

####creates table with geometry of the county border
allegheny <- allegheny_tracts %>% 
  summarize()

commute_tracts <- allegheny_tracts# %>% 
  #filter(!is.na(name))

commute_centroids <- allegheny_tracts_centroid #%>% 
  #filter(!is.na(name))


#map
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = st_transform(allegheny, crs = 4326),
              color = "#444444",
              stroke = TRUE,
              fillOpacity = 0,
              opacity = 1,
              weight = 2,
              group = "Census tracts") %>%
  addPolygons(data = st_transform(commute_tracts, crs = 4326),
              #color
              #color = NA,
              #fill
              #fillColor = ~tract_palette(GEOID),
              fillOpacity = .3,
              
              #label
              label = commute_tracts$GEOID,
              
              #highlight
              highlightOptions = highlightOptions(weight = 10, bringToFront = TRUE),
              
              group = "Census tracts")


allegheny_tracts %>% 
  filter(str_detect(GEOID, "42003101100"))

#spatial join
df_stops_joined_distance <- transit_stops %>% 
  st_join(commute_centroids, st_is_within_distance, dist = 700, left = TRUE) %>% 
  arrange(route_id)


#test functions
count_transit_connections(from = "42003101100", to = "42003020100")


df_connections <- vroom("data/summarized_lodes_tracts.csv",
                        col_types = cols("c", "c", "n")) %>% 
  semi_join(allegheny_tracts, by = c("h_tract" = "GEOID")) %>% 
  semi_join(allegheny_tracts, by = c("w_tract" = "GEOID")) %>% 
  filter(!(h_tract == w_tract)) %>% 
  arrange(desc(commuters)) %>% 
  top_n(100)

df_connections

#use count_transit_connections to see how many transit lines serve a commuter route
df_connections %>% 
  rowwise() %>% 
  mutate(connections = count_transit_connections(from = h_tract, to = w_tract))

df_connections %>% 
  rowwise() %>% 
  mutate(connections = count_transit_connections(from = h_tract, to = w_tract)) %>% 
  ggplot(aes(commuters, connections)) +
    geom_point()



#use transit_connection_routes to list which lines serve a commuter route
df_connections %>% 
  rowwise() %>% 
  mutate(transit_connections =  count_transit_connections(from = h_tract, to = w_tract),
         routes_served_by = transit_connection_routes(from = h_tract, to = w_tract))


