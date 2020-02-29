library(tidyverse)
library(sf)
library(janitor)


#summarized tract data
df_tract_centroid_summary <- st_read("data/shapefiles/summarized_census_tract_residents_jobs/summarized_census_tract_residents_jobs.shp") %>%
  st_transform(4326) %>% 
  rename(jobs = cmmtrs_n,
         residents = cmmtrs_t) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(4326)

#st_write(df_tract_centroid_summary, "data/shapefiles/tract_centroid_summary/tract_centroid_summary.shp")



##load transit data
transit_lines <- st_read("data/shapefiles/transit_lines/PAAC_Routes_1909.shp") %>%
  clean_names() %>%
  mutate_at(vars(-all_of(c("geometry"))), as.character) %>%
  mutate(rt_len_mil = as.numeric(rt_len_mil)) %>% 
  rename(route_id = route,
         service_type = type_serv) %>% 
  distinct(service_type, route_id, route_name, rt_len_mil, geometry) %>%
  st_transform(4326)

df_service_type <- transit_lines %>% 
  distinct(service_type, route_id) %>% 
  st_drop_geometry()

str(transit_lines)

transit_stops <- st_read("data/shapefiles/transit_stops/PAAC_Stops_1909.shp") %>%
  st_transform(4326) %>% 
  clean_names() %>% 
  mutate_at(vars(-all_of(c("geometry", "routes_cou"))), as.character) %>%
  select(stop_name, routes_served = routes_ser, routes_cou, geometry) %>% 
  distinct(stop_name, routes_served = routes_served, routes_cou, geometry)

str(transit_stops)

max_routes_served <- transit_stops %>% 
  summarize(max_routes = max(routes_cou)) %>% 
  pull(max_routes)

transit_stops %>% 
  filter(routes_cou == max_routes_served)

transit_stops <- transit_stops %>% 
  separate(routes_served, sep = ", ", into = str_c("route_", 1:max_routes_served), extra = "merge", fill = "right")

str(transit_stops)

transit_stops %>% 
  filter(routes_cou == max_routes_served)

transit_stops <- transit_stops %>% 
  pivot_longer(cols = starts_with("route_"), names_to = "route_number", values_to = "route_id") %>% 
  st_as_sf()

str(transit_stops)

transit_stops <- transit_stops %>%
  filter(!is.na(route_id)) %>% 
  left_join(df_service_type)



#join stops to tract centroids
df_joined <- transit_stops %>% 
  filter(!is.na(route_id), !is.na(service_type)) %>% 
  st_join(df_tract_centroid_summary, st_is_within_distance, 200, left = TRUE)

#st_write(df_joined, "data/shapefiles/joined_stops_tract_centroids/joined_stops_tract_centroids.shp")

#calculate route stats
df_route_stats <- df_joined %>% 
  group_by(route_id, service_type) %>% 
  summarize(jobs = sum(jobs, na.rm = TRUE),
            residents = sum(residents, na.rm = TRUE),
            stop_count = n()) %>% 
  ungroup() %>% 
  left_join(transit_lines %>% st_drop_geometry()) %>% 
  rename(route_length_miles = rt_len_mil) %>% 
  mutate(stops_per_mile = stop_count / route_length_miles)

#st_write(df_route_stats, "data/shapefiles/route_stats/route_stats.shp")