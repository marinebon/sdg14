# overview: read in sst, chl (*.nc), seascapes (*.mat) and write geotiffs used for GeoServer WMS tiling with time
# 1. read nc/mat to brick to indiv raster
# 1. optimize raster output
#   - [Raster Data Optimization — GeoServer Training](http://geoserver.geo-solutions.it/edu/en/enterprise/raster.html)
#   - [Data Considerations — GeoServer 2.13.x User Manual](http://docs.geoserver.org/latest/en/user/production/data.html)

# packages ----
library(tools)
library(tidyverse)
library(stringr)
library(lubridate)
library(rgdal)
library(ncdf4)
library(raster)
# install.packages(c('gdalUtils','leaflet'))
library(gdalUtils)
library(leaflet)

# package options ----
rasterOptions(tmpdir='/mbon-local/tmp-raster') 

# paths & vars ----
if (basename(getwd()) != 'satellite') setwd('satellite')

dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big', # BB's Mac
  'Windows' = 'P:',                             # constance.bren.ucsb.edu
  'Linux'   = '/mbon/data_big')                 # mbon.marine.usf.edu
dir_wms = '/mbon-local/geoserver/satellite' # TODO: generalize for laptop
sat_csv = 'satellite_products.csv'
# TODO: gl_sea_[curr|clim]_09km_mo

# helper functions ----

f2date = function(f, p){
  
  # TODO: clim
  
  yj       = str_sub(basename(f), 2, 8)
  date_mid = date(
    sprintf('%s-01-01', str_sub(yj,1,4))) +   #  1st of year
    days(as.integer(str_sub(yj,5,8))-1 + 14)  # 15th of month in julian days
  
  date_mid
}


mat2raster = function(f){
  # convert to raster
  r = 'mat'
  r
}

nc2raster = function(f, p){
  # convert nc to raster
  # nc_path = f
  
  nc = nc_open(f)
  lon = ncvar_get(nc, 'longitude')[,1]
  lat = ncvar_get(nc, 'latitude')[1,]
  
  # TODO: if (is_clim)
  
  # read raster for single variable in nc
  r = raster(f, varname=p[['f_var']])
  
  # realign spatial
  r = flip(r, direction='y')
  r <- setExtent(r, extent(min(lon), max(lon), min(lat), max(lat)))
  crs(r) <- leaflet:::epsg4326
  
  r
}

f2raster = function(f, p){ # f
  ext = file_ext(f)
  
  ext_handled = c('nc','mat')
  if (!ext %in% ext_handled) 
    stop(sprintf('Filename extension "%s" not handled (only %s).', ext, paste(ext_handled, collapse=',')))
  
  if (ext == 'nc'){
    r = nc2raster(f, p)
  } 
  if (ext == 'mat'){
    r = mat2raster(f, p)
  }
  
  r
}

r2tif4gs = function(r, p, tif){
  # optimize for Geoserver WMS
  # [Data Considerations — GeoServer 2.13.x User Manual](http://docs.geoserver.org/latest/en/user/production/data.html)
  
  # project to Mercator for consuming by leaflet
  proj_method = ifelse(p[['is_clim']], 'ngb'    , 'bilinear')
  tran_method = ifelse(p[['is_clim']], 'nearest', 'bilinear')
  addo_method = ifelse(p[['is_clim']], 'nearest', 'average')
  
  # r0 = r # r = r0
  r = projectRaster(r, res=p[['res_km']]*1000, crs=leaflet:::epsg3857, method=proj_method)
  # TODO: Warning message: 106 projected point(s) not finite
  
  tif_tmp1 = tempfile(fileext = '.tif')  # tif_tmp1 = tif_tmp; rm(tif_tmp)
  tif_tmp2 = tempfile(fileext = '.tif')  # tif_tmp1 = tif_tmp; rm(tif_tmp)
  writeRaster(r, tif_tmp1, options=c('COMPRESS=NONE'), overwrite=T) # file.exists(tif_tmp1); file.size(tif_tmp1)/(1000*1000) # MB
  
  # add inner tiles
  gdal_translate(tif_tmp1, tif_tmp2, of = 'GTiff', co = 'TILED=YES', r=tran_method)
  file.remove(tif_tmp1) # gdalinfo(tif)
  
  # add overviews
  gdaladdo(tif_tmp2, levels=c(2,4,8,16), r=addo_method)
  
  file.copy(tif_tmp2, tif)
  file.remove(tif_tmp2)
}

# iterate over satellite products ----
products = read_csv(sat_csv) %>%
  filter(do==T) # View(products)
for (i in 1:nrow(products)){ # i = 1
  
  p       = products[i,]
  dir_in  = file.path(dir_root, p[['dir']])
  dir_out = file.path(dir_wms, p[['key']])
  
  if (!dir.exists(dir_out))
    dir.create(dir_out)
  
  files = list.files(dir_in, sprintf('.*\\.%s$', p[['f_ext']]) , full.names=T)
  
  cat(sprintf('\n%d of %d: %s\n  paths:\n    %s ->\n    %s\n  files: %d *.%s\n', i, nrow(products), p[['key']], dir_in, dir_out, length(files), p[['f_ext']]))
  
  # iterate over files ----
  t0 = Sys.time(); n_done = 0
  for (j in seq_along(files)){ # j = 1
    
    # f vars
    f = files[j]
    tif = sprintf('%s/r_%s.tif', dir_out, f2date(f, p) %>% format('%Y%m%d'))
    
    # TODO: multiply for clim
    if (!file.exists(tif) | p[['redo']]){
    
      # report
      t1 = Sys.time()
      if (n_done > 0 ){
        t_min_left = as.numeric((difftime(t1, t0, units='min') / n_done) * (length(files) - j))
      } else {
        t_min_left = -9999
      }
      cat(sprintf(
        '  %03d of %d: %s -> %s [%s, %1.1f min to go]\n', 
        j, length(files), basename(f), basename(tif), 
        format(t1, tz='America/Los_Angeles',usetz=TRUE), t_min_left))
      
      
      # file (nc|mat) to raster
      r = f2raster(f, p)
      
      # write raster, optimized for Geoserver WMS
      r2tif4gs(r, p, tif)
      
      n_done = n_done + 1
    } # end: if (!file.exists(tif) | p[['redo']])
    
  } # end: for (f in files)
} # end: for (i in 1:length(products))
  

# TODO: create dir_out parameter files for GeoServer
# * [Using the ImageMosaic plugin for raster time-series data — GeoServer 2.12.x User Manual](http://docs.geoserver.org/stable/en/user/tutorials/imagemosaic_timeseries/imagemosaic_timeseries.html)
# * [Tile Caching with GeoWebCache — GeoServer Training](http://geoserver.geo-solutions.it/edu/en/enterprise/gwc.html)