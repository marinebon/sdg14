# overview: read in sst, chl (*.nc), seascapes (*.mat) and write geotiffs used for GeoServer WMS tiling with time
# 1. read nc/mat to brick to indiv raster
# 1. optimize raster output
#   - [Raster Data Optimization — GeoServer Training](http://geoserver.geo-solutions.it/edu/en/enterprise/raster.html)

# packages ----
library(tools)
library(tidyverse)
library(stringr)
library(lubridate)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(raster)
library(leaflet)

# paths & vars ----
if (basename(getwd()) != 'satellite') setwd('satellite')

dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big', # BB's Mac
  'Windows' = 'P:',                             # constance.bren.ucsb.edu
  'Linux'   = '/mbon/data_big')                 # mbon.marine.usf.edu

dir_wms = '/mbon/geoserver/satellite' # TODO: generalize for laptop

sat_products = list(
  # each element is a uniquely identified data product with parameters for processing
  'gl_sst_curr_09km_mo' = list(
    'area'     = 'global',
    'content'  = 'sst',
    'is_clim'  = F,
    'res_km'   = 9,
    'step'     = 'month',
    'dir'      = 'satellite/sst4/anom_9km',
    'f_ext'    = 'nc',
    'f_var'    = 'mean',
    'redo'     = F),
  'gl_sst_clim_09km_mo' = list(
    'area'     = 'global',
    'content'  = 'sst',
    'is_clim'  = T,
    'res_km'   = 9,
    'step'     = 'month',
    'clim_beg' = '', 
    'clim_end' = '', 
    'dir'      = 'satellite/sst4/clim_9km',
    'f_ext'    = 'nc',
    'redo'     = F),
  'gl_chl_clim_09km_mo' = list(
    'area'     = 'global',
    'content'  = 'chl',
    'is_clim'  = T,
    'res_km'   = 9,
    'step'     = 'month',
    'clim_beg' = '', 
    'clim_end' = '', 
    'dir'      = 'satellite/chlor_a/clim_9km',
    'f_ext'    = 'nc',
    'redo'     = F),
  'gl_chl_curr_09km_mo' = list(
    'area'     = 'global',
    'content'  = 'chl',
    'is_clim'  = T,
    'res_km'   = 9,
    'step'     = 'month',
    'dir'      = 'satellite/chlor_a/anom_9km',
    'f_ext'    = 'nc',
    'f_var'    = 'mean',    
    'redo'     = F))
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

r2tif4gs = function(r, p){
  # optimize for Geoserver WMS
  # [Data Considerations — GeoServer 2.13.x User Manual](http://docs.geoserver.org/latest/en/user/production/data.html)
  
  # project to Mercator for consuming by leaflet
  proj_method = ifelse(p[['is_clim']], 'ngb', 'bilinear')
  # r0 = r # r = r0
  r = projectRaster(r, res=p[['res_km']]*1000, crs=leaflet:::epsg3857, method=proj_method)
  # TODO: Warning message: 106 projected point(s) not finite
  
  tif_tmp1 = tempfile(fileext = '.tif')  # tif_tmp1 = tif_tmp; rm(tif_tmp)
  tif_tmp2 = tempfile(fileext = '.tif')
  writeRaster(r, tif_tmp1)
  # file.exists(tif_tmp1); file.size(tif_tmp1)/(1000*1000) # MB
  
  gdal_translate(tif_tmp1, tif_tmp2, ot, strict, of = "GTiff", b, mask, expand, outsize, tr, r, scale, exponent, unscale, srcwin, projwin, projwin_srs, epo, eco, a_srs, a_ullr, a_nodata, mo, co, gcp, q, sds, stats, norat, oo, sd_index, output_Raster = FALSE, ignore.full_scan = TRUE, verbose = FALSE, ...)
  
  file.create('/mbon/touch-rstudio.txt')
  file.exists('/mnt/mbon-supplement')

  
}

# iterate over satellite products ----

for (i in 1:length(sat_products)){ # i = 1
  
  p       = sat_products[[1]]
  p_key   = names(sat_products)[1]
  dir_in  = file.path(dir_root, p[['dir']])
  dir_out = file.path(dir_wms, p_key)
  
  # cd /mnt/mbon-supplement
  # sudo chgrp -R users geoserver
  # sudo chmod -R 775 geoserver
  if (!dir.exists(dir_out))
    dir.create(dir_out)
  
  files = list.files(dir_in, sprintf('.*\\.%s$', p[['f_ext']]) , full.names=T)
  
  cat(sprintf('\n%d of %d: %s\n  paths:\n    %s ->\n    %s\n  files: %d *.%s\n', i, length(sat_products), p_name, dir_in, dir_out, length(files), p[['f_ext']]))
  
  # iterate over files ----
  for (f in files){ # f = files[[1]]
    
    # f vars
    date_mid = f2date(f, p)
    tif = sprintf('r_.tif')
    tif      = sprintf('%s/r_%s.tif', dir_out, format(date_mid, '%Y%m%d'))
    
    
    
    # file (nc|mat) to raster
    r = f2raster(f, p)
    
    # write rater, optimized for Geoserver WMS
    r = r2tif4gs(f, p)
    
    # TODO: multiply for clim
    
    
    
    
  }
  

  
  

}

