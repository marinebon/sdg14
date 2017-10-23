library(tidyverse)
library(icosa)
library(sf)
library(geosphere)

dir_hex = '/mbon-local/boundaries/hex'

tesselations = list(
  # c(4,4),
  # c(4,4,2),
  # c(4,4,4),
  # c(4,4,6),
  c(4,4,8),
  c(4,4,16))
for (v in tesselations){
  
  h = hexagrid(v)
  h_sp = SpPolygons(h, res=50)
  
  h_sf = h_sp %>% 
    st_as_sf()  %>%
    st_transform(leaflet:::epsg4326) %>%
    mutate(
      area_km2 = as.numeric(st_area(geometry)) / (1000*1000),
      edge_km  = 3^(1/4) * sqrt(2 * area_km2 / 9),
      width_km = sqrt(3) * edge_km)
  
  v_s = paste(v, collapse='x')
  shp = file.path(dir_hex, sprintf('hex_width%04.0fkm_icosa%s.shp', max(h_sf$width_km), v_s))
  cat(shp,'\n')
  write_sf(h_sf, shp)
}

# https://rechneronline.de/pi/hexagon.php
# edge of 287 km -> width ~ 500 km (497 km)


