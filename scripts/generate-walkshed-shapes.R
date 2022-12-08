library(tidyverse)
library(sf)
library(janitor)
library(tidytransit)
library(leaflet)
library(rgeos)
library(openxlsx)
library(readxl)
library(scales)
library(rstudioapi)
library(tigris)
library(tidycensus)
library(lehdr)
library(osmdata)
library(furrr)
library(sfnetworks)
library(DBI)
library(nntools)
library(broom)
library(progressr)
library(nngeo)
library(lwgeom)
library(httr)
library(jsonlite)
library(valhallr)
library(geosphere)
library(htmlwidgets)

# devtools::install_github("bpb824/valhallr")
# devtools::install_github("PerkinsAndWill/nntools", ref = "master", auth_token = "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")

future::plan(multisession)

# USER INPUTS ---------------------

#Buffer distance
buff_dist <- c(5280/4, 5280/2) # Quarter mile & Half mile

#Coordinate systems
coord_global = 4326
coord_local = 2269 #see here: https://nelsonnygaard.shinyapps.io/coord-system-reference/

#Path to GTFS feed you will use
feed_path = "data/input/gtfs/trimet/2022-09-22.zip"

# NN's Valhalla instance, no need to change
nn_valhalla_hostname <- "128.199.8.29"

# GTFS Table Setup ------------
bus_gtfs = read_gtfs(feed_path)

trips            <- bus_gtfs$trips  
stop_times       <- bus_gtfs$stop_times 
shapes           <- bus_gtfs$shapes 
routes           <- bus_gtfs$routes 
stops            <- bus_gtfs$stops %>%
  filter(!is.na(stop_lon),!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = coord_global) %>%
  st_transform(crs = coord_local)

#Stop Orders
stop_dirs <- stop_times %>% 
  left_join(trips) %>%
  distinct(route_id, direction_id, shape_id, stop_id, stop_sequence) %>% 
  left_join(routes %>% select(route_id,route_short_name)) %>% 
  left_join(bus_gtfs$stops %>% select(stop_id, stop_name,stop_lon,stop_lat)) %>%
  arrange(route_id,direction_id,shape_id,stop_sequence)

#Shape Geometry
shape_geom = shapes %>%
  nest(data = any_of(c('shape_pt_lon','shape_pt_lat','shape_pt_sequence','shape_dist_traveled'))) %>%
  mutate(geometry = map(data, function(df){
    df %>% 
      arrange(shape_pt_sequence) %>%
      select(shape_pt_lon,shape_pt_lat) %>%
      as.matrix() %>%
      st_linestring()
  }) %>%
    st_sfc(crs=coord_global)) %>%
  st_as_sf() %>%
  select(-data) %>%
  st_transform(coord_local) %>%
  mutate(time_period = app_time_period)

#Reference of unique stops served by shape ID
stop_shp_ref <- stop_dirs %>%
  distinct(shape_id,stop_id)

or_counties <- counties(state = "OR", year = 2020)
wash_county_geom <- or_counties %>%
  filter(NAME=="Washington") %>%
  st_transform(coord_local)

wash_co_stops <- stops %>%
  st_transform(coord_local) %>%
  filter(st_intersects(., wash_county_geom, sparse = FALSE))

# Generating Walksheds per Stop ----------

#Buffer distance in kilometers
buff_dist_km <- buff_dist*0.0003048

#Generate walksheds for each stop (based on user input buffer distance)
#This step will take a few minutes depending on the number of stops
with_progress({
  
  p <- progressor(steps = nrow(wash_co_stops))
  
  stop_walksheds_queried <- wash_co_stops %>%
    select(stop_id,stop_name,geometry) %>%
    st_transform(coord_global) %>%
    mutate(walkshed_geom = future_pmap(.l = list(geometry),
                                       .f = function(geom, p){
                                         
                                         p()
                                         
                                         pt_geom <- geom %>% st_coordinates() %>%
                                           as_tibble() %>%
                                           rename(lon = X, lat = Y)
                                         
                                         iso_result <- isochrone(from = pt_geom,
                                                                 costing = "pedestrian",
                                                                 contours = buff_dist_km,
                                                                 metric = "km",
                                                                 hostname = nn_valhalla_hostname)
                                         
                                         return(iso_result %>% select(contour,geometry))
                                         
                                       }, p = p))
})

#Clean up queried walksheds into a single SF dataframe
stop_walksheds <- stop_walksheds_queried %>%
  st_drop_geometry() %>%
  unnest(walkshed_geom) %>%
  mutate(iso_dist_miles = round(contour * 0.621371,2)) %>%
  select(-contour) %>%
  st_as_sf() %>%
  st_transform(coord_global)

write_sf(stop_walksheds,"G:/Current/WASHINGTON_CO_OR_Transit_Study_2020_1005/Analysis/z_Original/valhalla-osm-walksheds/wash-co-walksheds.geojson")

#Diagnostic map
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data = stop_walksheds %>% st_transform(coord_global))