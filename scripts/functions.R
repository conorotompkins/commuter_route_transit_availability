#function to count transit connections between a pair of census tracts
count_transit_connections <- function(from, to){
  from <- enquo(from)
  to <- enquo(to)
  
  df_stops_joined_distance %>% 
    st_drop_geometry() %>% 
    #semi_join
    filter(GEOID %in% c(!!from, !!to)) %>% 
    distinct(route_id, GEOID) %>% 
    count(route_id) %>% 
    filter(n >= 2) %>% 
    nrow()
}

#function to list which `route_id`s serve a pair of tracts
transit_connection_routes <- function(from, to){
  from <- enquo(from)
  to <- enquo(to)
  
  routes <- df_stops_joined_distance %>% 
    st_drop_geometry() %>% 
    #semi_join
    filter(GEOID %in% c(!!from, !!to)) %>%
    distinct(route_id, GEOID) %>% 
    count(route_id) %>% 
    filter(n >= 2) %>% 
    pull(route_id) %>% 
    paste(collapse = ", ")
  
#if routes is empty, return NA, else return routes
}
