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
library(R.matlab)
library(RCurl)

# paths & vars ----
if (basename(getwd()) != 'satellite') setwd('satellite')

dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big',  # BB's Mac
  'Linux'   = '/mbon/data_big')                  # mbon.marine.usf.edu
dir_gs_pfx = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big/geoserver/satellite',  # BB's Mac
  'Linux'   = '/mbon-local/geoserver/satellite') # mbon.marine.usf.edu
dir_4326_pfx = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big/satellite_tif_epsg4326',  # BB's Mac
  'Linux'   = '/mbon-local/satellite_tif_epsg4326')        # mbon.marine.usf.edu
dir_tmp = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Users/bbest/Data/tmp-raster',  # BB's Mac
  'Linux'   = '/mbon-local/tmp-raster')        # mbon.marine.usf.edu

sat_csv = 'data_small/satellite_products.csv'

rasterOptions(tmpdir=dir_tmp, tmptime=2) # showTmpFiles(); removeTmpFiles()

db = switch(
  Sys.info()[['sysname']],
  'Linux' = list(
    host = read_delim('/etc/hosts', '\t', col_names=c('ip','host')) %>%
      separate(host, c('name','image'), extra='merge', fill='left') %>%
      filter(name=='postgis') %>%
      .$ip,
    port = 5432,
    user = 'docker',
    pass = readLines('/mbon/.pgsql_pass_docker'),
    name = 'docker'),
  'Darwin' = list(
    host = '172.17.0.2',
    port = 5432,
    user = 'docker',
    pass = readLines('/mbon/.pgsql_pass_docker'),
    name = 'docker'))
db$dsn=sprintf(
  "PG:dbname=%s host=%s port=%s user=%s password=%s", 
  db$name, db$host, db$port, db$user, db$pass)

# TODO: gl_[sst|chl|sea]_clim_09km_mo

# helper functions ----

f2date = function(f, p){
  
  yj = switch(
    as.character(p[['content']] == 'seascape'),
    'TRUE'  = str_replace(basename(f), sprintf('.*_([0-9]+)\\.%s$', p[['f_ext']]), '\\1'),
    'FALSE' = str_sub(basename(f), 2, 8))
  date_mid = date(
    sprintf('%s-01-01', str_sub(yj,1,4))) +   #  1st of year
    days(as.integer(str_sub(yj,5,8))-1 + 14)  # 15th of month in julian days
  
  date_mid
}

mat2raster = function(f, p){
  # convert mat to raster
  m = readMat(f)
  r = raster(m$CLASS) %>%
    flip('y') %>% 
    setExtent(extent(-180, 180, -90, 90))
  crs(r) = leaflet:::epsg4326
  # names(r) # plot(r)
  
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

r2tif = function(r, tif_4326){
  writeRaster(r, tif_4326, options=c('COMPRESS=NONE','TILED=YES'), overwrite=T)
}

r2tif4gs = function(tif_4326, tif_gs, p){
  # optimize for Geoserver WMS
  # [Data Considerations — GeoServer 2.13.x User Manual](http://docs.geoserver.org/latest/en/user/production/data.html)
  
  # project to Mercator for consuming by leaflet
  warp_method = ifelse(p[['is_categorical']], 'near'   , 'bilinear')
  addo_method = ifelse(p[['is_categorical']], 'nearest', 'average')
  
  # raster::projectRaster() | gdalUtils::gdalwarp() didn't yield readable EPSG:3857 by GeoServer, so using system2 command
  # plus gdal binaries (using system2) 5-10x faster
  system2('gdalwarp', c(
    '-s_srs EPSG:4326','-t_srs EPSG:3857', 
    paste('-r', warp_method),
    '-te -20037508.34 -20037508.34 20037508.34 20037508.34', # 85.05° N & S: https://tilemill-project.github.io/tilemill/docs/guides/reprojecting-geotiff/
    '-co COMPRESS=NONE', 
    '-co TILED=YES',
    '-overwrite',
    tif_4326, 
    tif_gs), stdout=F)
  
  system2('gdaladdo', c(
    paste('-r', addo_method),
    tif_gs,
    '2 4 8 16 32'), stdout=F)
}

fetch_seascapes = function(){

  # csv
  csv_whoi  = 'data_small/seascapes_whoi_ftp.csv'
  
  # ftp params
  url_pre   = 'ftp://ftp.whoi.edu'
  url_paths = list(
    gl_curr  = 'MBON/GLOBAL/BETA/SDG14/seascape/MODIS_3VAR_MO9k/')
    #fl       = 'MBON/GOM_FK/SMALL_MO/',
    #mb       = 'MBON/ENPAC_MB/SMALL_MO/')
  passwd    = read_lines(file.path(dir_root, 'satellite/.mkavanaugh_passwd_ftp_whoi_mbon'))
  usrpwd    = sprintf('mkavanaugh:%s', passwd)
  
  # dir params
  dir_seascapes = file.path(dir_root, 'satellite/seascapes')
  sapply(
    sprintf('%s/%s', dir_seascapes, names(url_paths)), 
    function(x) dir.create(x, recursive=T, showWarnings=F))
  
  # get ftp connection
  con = getCurlHandle(ftp.use.epsv=F, userpwd=usrpwd)
  
  if (!file.exists(csv_whoi)){
    
    # get list of filenames
    for (i in 1:length(names(url_paths))){ # i = 1
      
      # get file, url, local path for in ftp path
      u_i = url_paths[i]
      p_i = file.path(dir_seascapes, names(url_paths)[i])
      d_i = tibble(
        fname = getURL(
          file.path(url_pre, u_i), curl=con, 
          ftp.use.epsv=F, dirlistonly=T) %>% str_split('\n') %>% .[[1]]) %>%
        filter(fname != '') %>%
        mutate(
          url  = file.path(url_pre, u_i, fname),
          path = file.path(p_i, fname))
      
      if (i == 1){
        d = d_i
      } else {
        d = bind_rows(d, d_i)
      }
      
      # wait 5 seconds before next request, otherwise get 'Access denied: 530'
      Sys.sleep(5)
    }
    
    # write csv
    write_csv(d, csv_whoi)
  }
  
  # con = getCurlHandle(ftp.use.epsv=F, userpwd=usrpwd)
  d = read_csv(csv_whoi) %>%
    mutate(
      path_exists = file.exists(path)) %>%
    filter(
      !path_exists,
      str_detect(fname, '.*\\.mat$'))
  
  for (i in 1:nrow(d)){ # i = 1
    #for (i in c(178:181,496:499)){
    cat(sprintf('%03d (of %d): fetching %s \n', i, nrow(d), d$fname[i]))
    content = getBinaryURL(d$url[i], curl=con)
    writeBin(content, d$path[i])
  }
}
#fetch_seascapes()

write_gs_properties = function(p, dir_gs){
  cat('  writing GeoServer *.properties: datastore, indexer, timeregex\n')
  
  # Could not list layers for this store, an error occurred retrieving them: Failed to create reader from file:satellite/gl_sst_curr_09km_mo and hints null
  
  # datastore.properties
  writeLines(
    text = paste0(
"SPI=org.geotools.data.postgis.PostgisNGDataStoreFactory
host=",db$host,"
port=5432
database=mbon_gis
schema=public
user=docker
passwd=",db$pass,"
Loose\\ bbox=true
Estimated\\ extends=false
validate\\ connections=true
Connection\\ timeout=10
preparedStatements=true
"),
    con = file.path(dir_gs, 'datastore.properties'))
  
  # indexer.properties
  writeLines(
    text = paste0(
"TimeAttribute=ingestion
ElevationAttribute=elevation
Schema=*the_geom:Polygon,location:String,ingestion:java.util.Date,elevation:Integer
PropertyCollectors=TimestampFileNameExtractorSPI[timeregex](ingestion)
"),
    con = file.path(dir_gs, 'indexer.properties'))
  
  # timeregex.properties
  writeLines(
    text = paste0(
"regex=[0-9]{8}
"),
    con = file.path(dir_gs, 'timeregex.properties'))
  
}

# TODO: [Manage ImageMosaic content through REST API — GeoServer Training](http://geoserver.geo-solutions.it/edu/en/multidim/rest/index.html)
# library(geosapi) # devtools::install_github("eblondel/geosapi")
# gsman <- GSManager$new(
#   url = 'http://mbon.marine.usf.edu:8080/geoserver', #baseUrl of the Geoserver
#   user = 'admin', pwd = readLines('/mbon/.pgsql_pass_docker'), #credentials
#   logger = NULL #logger, for info or debugging purpose
# )
# 
# curl -v -u admin:M******#* -XGET "http://mbon.marine.usf.edu:8080/geoserver"
# curl -v -u admin:M******#* -XGET "http://mbon.marine.usf.edu:8080/geoserver/rest/workspaces/satellite/coveragestores/sst4_anom_27km/sst_monthly_mean_27km/index.xml"

# iterate over satellite products ----
products = read_csv(sat_csv) %>%
  filter(do==T) # View(products)
for (i in 1:nrow(products)){ # i = 1

  p        = products[i,]
  dir_in   = file.path(dir_root   , p[['dir']])
  dir_gs   = file.path(dir_gs_pfx , p[['key']])
  dir_4326 = file.path(dir_4326_pfx, p[['key']])
  
  if (!dir.exists(dir_gs))
    dir.create(dir_gs)
  if (!dir.exists(dir_4326))
    dir.create(dir_4326)
  
  files = list.files(dir_in, sprintf('.*\\.%s$', p[['f_ext']]) , full.names=T)
  
  if (p[['key']] == 'gl_sst_curr_09km_mo_test3')
    files=files[1:3]
  
  cat(sprintf('\n%d of %d: %s\n  paths:\n    %s\n    ->%s\n    ->%s\n  files: %d *.%s\n', i, nrow(products), p[['key']], dir_in, dir_4326, dir_gs, length(files), p[['f_ext']]))
  
  # iterate over files ----
  t0 = Sys.time(); n_done = 0
  for (j in seq_along(files)){ # j = 1 # j=2
    
    # f vars
    f = files[j]
    f_tif = sprintf('r_%s.tif', f2date(f, p) %>% format('%Y%m%d'))
    tif_4326 = file.path(dir_4326, f_tif)
    tif_gs = file.path(dir_gs, f_tif)
    
    # TODO: multiply for clim
    if (!file.exists(tif_4326) | p[['redo_tif_4326']]){
    
      # report
      t1 = Sys.time()
      if (n_done > 0 ){
        t_min_left = as.numeric((difftime(t1, t0, units='min') / n_done) * (length(files) - j + 1))
      } else {
        t_min_left = -9999
      }
      cat(sprintf(
        '  %03d of %d: %s -> 4326:%s [%s, %1.1f min to go]\n', 
        j, length(files), basename(f), basename(tif_4326), 
        format(t1, tz='America/Los_Angeles',usetz=TRUE), t_min_left))
      
      # file (nc|mat) to raster
      r = f2raster(f, p)
      
      # raster to tif, unprojected geographic
      r2tif(r, tif_4326)
    
      
    } # end: if (!file.exists(tif_4326) | p[['redo_tif_4326']])
    
    if (!file.exists(tif_gs) | p[['redo_tif_gs']]){
      
      cat(sprintf('                 -> 3857:%s\n', basename(tif_4326)))
      
      # write raster, projected to Mercator & optimized for Geoserver WMS
      r2tif4gs(tif_4326, tif_gs, p)
      
      n_done = n_done + 1  
    } # end: if (!file.exists(tif_gs) | p[['redo_tif_gs']])
    
  } # end: for (f in files)
  
  # write geoserver property files
  write_gs_properties(p, dir_gs)
  
  # TODO: set permissions
  # ben@mbon: cd /mnt/mbon-supplement/geoserver; sudo chgrp -R users satellite; sudo chmod 775 -R satellite
  
} # end: for (i in 1:length(products))

# TODO: create dir_out parameter files for GeoServer
# * [Using the ImageMosaic plugin for raster time-series data — GeoServer 2.12.x User Manual](http://docs.geoserver.org/stable/en/user/tutorials/imagemosaic_timeseries/imagemosaic_timeseries.html)
# * [Tile Caching with GeoWebCache — GeoServer Training](http://geoserver.geo-solutions.it/edu/en/enterprise/gwc.html)