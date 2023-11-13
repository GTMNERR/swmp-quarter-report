# helpful resources
# https://rstudio.github.io/leaflet/basemaps.html 

library(leaflet)
library(htmltools)
library(sf)
library(dplyr)
library(janitor)
library(here)

station_info <- readr::read_csv(here::here('data', 'swmp',
                               'sampling_stations.csv')) %>% janitor::clean_names()
glimpse(station_info)

spatial <- station_info %>% 
  filter(grepl("gtm",nerr_site_id)) %>% 
  filter(!grepl("nut", station_code)) %>% 
  select(station_code, latitude, longitude) %>% 
  mutate(longitude = -1*longitude)

shape_data <- st_read(here::here('data', 
                                 'shapefiles', 
                                 'qa_4269_GTM_RB_2014_simplified_DLE.shp'))



# spatial %>% 
#   leaflet(width = 900) %>% 
#   addTiles() %>% 
#   addMarkers(clusterOptions = markerClusterOptions(), 
#              label = ~htmlEscape(station_code),
#              labelOptions = labelOptions(textsize = "15px")) %>% 
#   addPolygons(data = shape_data, weight = 5, col = 'red')
  # addWMSTiles(
  #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
  #   layers = "nexrad-n0r-900913",
  #   options = WMSTileOptions(format = "image/png", transparent = TRUE),
  #   attribution = "Weather data © 2012 IEM Nexrad"
  # )