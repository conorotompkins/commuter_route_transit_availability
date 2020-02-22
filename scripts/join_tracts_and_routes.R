library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(janitor)
library(tidycensus)
library(mapview)

transit <- st_read("data/shapefiles/transit/PAAC_Routes_1909.shp") %>% 
  clean_names() %>% 
  st_transform(crs = "+init=epsg:4326")

allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide") %>% 
  st_transform(crs = "+init=epsg:4326") %>% 
  mutate(downtown_flag = GEOID == "42003020100")

allegheny <- allegheny_tracts %>% 
  summarize()

downtown <- allegheny_tracts %>% 
  filter(downtown_flag == TRUE)



df_joined <- transit %>% 
  st_join(downtown, st_crosses, left = TRUE)

df_joined %>% 
  ggplot() +
    geom_sf(data = allegheny, fill = "#00000000") +
    geom_sf(data = allegheny_tracts, aes(fill = downtown_flag), color = NA) +
    geom_sf(data = downtown, fill = "#00000000", size = 1) +
    geom_sf(aes(color = downtown_flag)) +
    #facet_wrap(~downtown_flag) +
    scale_fill_manual(values = c("#00000000", "gold")) +
    theme_void()
