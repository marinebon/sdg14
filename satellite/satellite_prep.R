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
library(sf)
# install.packages(c('gdalUtils','leaflet'))
library(gdalUtils)
library(leaflet)
library(R.matlab)
library(RCurl)
library(fasterize) # devtools::install_github('ecohealthalliance/fasterize')
select    = dplyr::select
summarise = dplyr::summarise
summarize = dplyr::summarize

# paths & vars ----
if (basename(getwd()) != 'satellite') setwd('satellite')

dir_data_big = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big',  # BB's Mac
  'Linux'   = '/mbon/data_big')                  # mbon.marine.usf.edu
dir_local = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big/local',  # BB's Mac
  'Linux'   = '/mbon-local') # mbon.marine.usf.edu
dir_tmp    = file.path(dir_local, 'tmp-raster')
dir_gs_sat = file.path(dir_local, 'geoserver/satellite')
sat_csv    = 'data_small/satellite_products.csv'

dir_extract  = file.path(dir_local, 'raster-extract')
dir_ref      = file.path(dir_local, 'raster-extract/reference')
ref_tif      = file.path(dir_gs_sat, 'gl_chl_curr_09km_mo/r_20020915.tif')
cid_grd      = file.path(dir_ref, 'gl_09km_cell.grd')
eez_shp      = file.path(dir_data_big, 'technical/boundaries/eez/eez.shp')
cid_eez_csv  = file.path(dir_ref, 'gl_09km_cell-eez.csv')

# raster temp dir important!
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

# read in reference cellid raster and eez data
do_eez_extraction = T
if (do_eez_extraction){
  # raster
  d_cid_eez = read_csv(cid_eez_csv)
  s_cid_km2 = stack(cid_grd)
  
  # eez shp df
  d_eez = read_sf(eez_shp) %>%
    filter(
      Pol_type == '200NM') %>%
    select(
      eez_mrgid     = MRGID,
      eez_territory = Territory1) %>%
    st_set_geometry(NULL)
  
  # eez area from raster
  d_eez_km2 = d_eez %>%
    left_join(
      getValues(s_cid_km2) %>%
        as_tibble() %>%
        inner_join(
          d_cid_eez, by='cellid'),
      by='eez_mrgid') %>%
    group_by(eez_territory, eez_mrgid) %>%
    summarise(
      eez_area_km2 = sum(area_km2)) %>%
    ungroup()
}

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
  m = try(readMat(f))
  
  if (class(m) == 'try-error'){
    r = NULL
  } else {
    r = raster(m$CLASS) %>%
      flip('y') %>% 
      setExtent(extent(-180, 180, -90, 90))
    crs(r) = leaflet:::epsg4326
    # names(r) # plot(r)
  }
  
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

r2tif = function(r, tif, p){
  # optimize for Geoserver WMS
  # [Data Considerations — GeoServer 2.13.x User Manual](http://docs.geoserver.org/latest/en/user/production/data.html)
  
  addo_method = ifelse(p[['is_categorical']], 'nearest', 'average')
  
  if (p[['content']] == 'chl')
    r = log(r)
  
  writeRaster(r, tif, options=c('COMPRESS=NONE','TILED=YES'), overwrite=T)
  
  system2('gdaladdo', c(
    paste('-r', addo_method),
    tif,
    '2 4 8 16 32'), stdout=F)
}

create_sld = function(p, dir_tif){
  sld = file.path(dir_tif, sprintf('%s_style.sld', p[['content']]))
  
  cat(sprintf('  writing style: %s\n', sld))
  # tifs = list.files(file.path(dir_gs_sat, 'gl_chl_curr_09km_mo'), '\\.tif$', full.names=T)[1:137]
  # r1 = raster(tifs[1])
  # for (i in seq_along(tifs)){
  #   tif = tifs[i]
  #   cat(sprintf('%03d: %s\n', i, tif))
  #   r2 = raster(tif)
  #   compareRaster(r1, r2)
  #   extent(r1)
  #   extent(r2)
  # }
  # s = stack(tifs)
  # log(max(maxValue(s)))
  # log(min(minValue(s)))
  
  var = p[['content']]
  
  var_colors = list(
    seascape = list(
      type   = 'interval',
      colors = 'Spectral',
      min    = 1,
      max    = 14,
      n      = 14),
    chl = list(
      type   = 'ramp',
      colors = 'Greens',
      min    = -6.91,
      max    = 4.61,
      n      = 7),
    sst = list(
      type   = 'ramp',
      colors = 'Reds',
      min    = -4,
      max    = 36,
      n      = 7))
  #v = var_colors[[]]
  #var = 'seascape'
  v = var_colors[[var]]
  
  pal256 = colorRampPalette(RColorBrewer::brewer.pal(9, v$colors))(256)
  round(seq.int(v$min, v$max, length.out=v$n))
  
  #cols = pal256[c(1,round(256/2),256)] # sst Reds: "#FFF5F0" "#FB6A4A" "#67000D"
  cols = pal256[seq.int(1, 256, length.out=v$n)] # sst Reds: "#FFF5F0" "#FB6A4A" "#67000D"
  vals = round(seq.int(v$min, v$max, length.out=v$n), 1)
  
  xml = paste0(
    '<?xml version="1.0" encoding="ISO-8859-1"?>
    <StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc"
    xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.0.0/StyledLayerDescriptor.xsd">
    <NamedLayer>
    <Name>',p[['content']],'</Name>
    <UserStyle>
    <Name>',p[['content']],'</Name>
    <Title>Simple style for ',p[['content']],'</Title>
    <Abstract>Basic color Map</Abstract>
    <FeatureTypeStyle>
    <Rule>
    <RasterSymbolizer>
    <Opacity>0.8</Opacity>
    <ColorMap type="', v$type, '">
    <ColorMapEntry color="#000000" quantity="-9999" label="nodata" opacity="0.0"/>
    ', paste(sprintf('<ColorMapEntry color="%s" quantity="%g" label="%g"/>',cols, vals, vals), collapse='\n'),'
    </ColorMap>
    </RasterSymbolizer>
    </Rule>
    </FeatureTypeStyle>
    </UserStyle>
    </NamedLayer>
    </StyledLayerDescriptor>
    ')
  
  writeLines(text=xml, con=sld)
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
  passwd    = read_lines(file.path(dir_data_big, 'satellite/.mkavanaugh_passwd_ftp_whoi_mbon'))
  usrpwd    = sprintf('mkavanaugh:%s', passwd)
  
  # dir params
  dir_seascapes = file.path(dir_data_big, 'satellite/seascapes')
  sapply(
    sprintf('%s/%s', dir_seascapes, names(url_paths)), 
    function(x) dir.create(x, recursive=T, showWarnings=F))
  
  # get ftp connection
  con = getCurlHandle(ftp.use.epsv=F, userpwd=usrpwd)
  
  #if (!file.exists(csv_whoi)){
  
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
        url  = sprintf('%s/%s%s', url_pre, u_i, fname),
        path = file.path(p_i, fname))
    
    if (i == 1){
      d = d_i
    } else {
      d = bind_rows(d, d_i)
    }
    
    # wait 5 seconds before next request, otherwise get 'Access denied: 530'
    #Sys.sleep(5)
  }
  
  # write csv
  write_csv(d, csv_whoi)
  #}
  
  con = getCurlHandle(ftp.use.epsv=F, userpwd=usrpwd) # Access denied: 530
  d = read_csv(csv_whoi) %>%
    mutate(
      path_exists = file.exists(path)) %>%
    filter(
      !path_exists,
      str_detect(fname, '.*\\.mat$'))
  
  for (i in 1:nrow(d)){ # i = 1
    #for (i in c(178:181,496:499)){
    cat(sprintf('%03d (of %d): fetching %s \n', i, nrow(d), d$fname[i]))
    f_url = d$url[i]
    f_mat = d$path[i]
    content = getBinaryURL(f_url, curl=con)
    writeBin(content, f_mat)
  }
}
#fetch_seascapes()

write_gs_properties = function(p, dir_tif){
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
    con = file.path(dir_tif, 'datastore.properties'))
  
  # indexer.properties
  writeLines(
    text = paste0(
      "TimeAttribute=ingestion
      ElevationAttribute=elevation
      Schema=*the_geom:Polygon,location:String,ingestion:java.util.Date,elevation:Integer
      PropertyCollectors=TimestampFileNameExtractorSPI[timeregex](ingestion)
      "),
    con = file.path(dir_tif, 'indexer.properties'))
  
  # timeregex.properties
  writeLines(
    text = paste0(
      "regex=[0-9]{8}
      "),
    con = file.path(dir_tif, 'timeregex.properties'))
  
}

create_cid_grd = function(ref_tif, cid_grd){
  r_ref = raster(ref_tif)
  
  # get cellid
  r_cid = setValues(r_ref, values=as.integer(1:ncell(r_ref)))
  names(r_cid) = 'cellid'
  
  # get area
  r_km2 = area(r_cid)
  names(r_km2) = 'area_km2'
  
  # write stack
  stack(r_cid, r_km2) %>%
    writeRaster(cid_grd)
}

if (!file.exists(cid_grd)){
  create_ref_tif(ref_tif, eez_shp, cid_grd, cid_eez_csv)
}

create_cid_eez_csv = function(cid_grd, eez_shp, cid_eez_csv){
  
  # get reference cellid stack
  r_cid = raster(cid_grd, layer='cellid')
  
  # fetch eez
  eez_sf = read_sf(eez_shp) %>%
    mutate(
      MRGID = as.integer(MRGID)) %>%
    filter(
      Pol_type == '200NM')
  
  # fasterize
  eez_r = fasterize(eez_sf, r_cid, field='MRGID', fun='first') # plot(eez_r)
  names(eez_r) = 'eez_mrgid'
  
  # find missing eez too small to be picked up by fasterize
  eez_miss_pts = eez_sf %>% 
    select(MRGID, Territory1) %>%
    filter(
      !MRGID %in% unique(eez_r)) %>%
    mutate(
      geom_centroid = st_centroid(geometry)) %>%
    st_set_geometry('geom_centroid') %>%
    as('Spatial')
  
  eez_miss_d = tibble(
    eez_mrgid = eez_miss_pts$MRGID,
    cellid    = extract(r_cid, eez_miss_pts))
  
  # extract cellids from rasters
  d  = getValues(stack(eez_r, r_cid)) %>%
    as_tibble() %>%
    filter(!is.na(eez_mrgid)) %>%
    bind_rows(
      eez_miss_d)
  
  # write cid_eez_csv
  write_csv(d, cid_eez_csv)
  
}

if (!file.exists(cid_eez_csv)){
  create_cid_eez_csv(cid_grd, eez_shp, cid_eez_csv)
}

r2eez_csv = function(i){ # i=1
  # assumes s, p, etc loaded
  
  lyr = names(s)[i]
  
  # time reporting
  if (i == 1){
    t1 <<- Sys.time()
    cat(sprintf('%03d of %d: %s\n', i, nlayers(s), lyr))
  } else {
    ti = Sys.time()
    min_ea = as.numeric((difftime(ti, t1, units='min')) / (i-1))
    t_min_togo =  min_ea * (nlayers(s) - i + 1)
    cat(sprintf('%03d of %d: %s [%s, %1.1f min to go]\n', 
                i, nlayers(s), lyr,
                format(ti, tz='America/Los_Angeles',usetz=TRUE), t_min_togo))
  }
  
  # stack with cellid, area_km2
  s_r = stack(s_cid_km2, raster(s, lyr))
  
  # calculate mean, sd, area by eez per raster
  d = getValues(s_r) %>%
    as_tibble() %>%
    gather(raster, value, -cellid, -area_km2) %>%
    left_join(
      d_cid_eez, by='cellid') %>%
    filter(!is.na(eez_mrgid))
  
  if (p[['is_categorical']]){
    # include area NA as another class of interest
    d = d %>%
      group_by(raster, eez_mrgid, value) %>%
      summarise(
        area_km2 = sum(area_km2)) %>%
      ungroup()
    
  } else {
    d = d %>%
      filter(!is.na(value)) %>% # exclude NA
      group_by(raster, eez_mrgid) %>%
      summarize(
        mean     = weighted.mean(value, area_km2),
        sd       = sd(value),
        area_km2 = sum(area_km2)) %>%
      ungroup()
  }
  
  d
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
for (i in 1:nrow(products)){ # i = 3
  
  p       = products[i,]
  dir_in  = file.path(dir_data_big, p[['dir']])
  dir_tif = file.path(dir_gs_sat  , p[['key']])
  
  if (!dir.exists(dir_tif))
    dir.create(dir_tif)
  
  files = list.files(dir_in, sprintf('.*\\.%s$', p[['f_ext']]) , full.names=T)
  
  if (p[['key']] %in% c('gl_sst_curr_09km_mo_test3','gl_chl_curr_09km_mo_test3'))
    files=files[1:3]
  
  cat(sprintf('\n%d of %d: %s\n  paths:\n    %s\n    ->%s\n  files: %d *.%s\n', i, nrow(products), p[['key']], dir_in, dir_tif, length(files), p[['f_ext']]))
  
  # iterate over files ----
  t0 = Sys.time(); n_done = 0
  for (j in seq_along(files)){ # j = 1 # j=2
    
    # f vars
    f = files[j]
    tif = sprintf('%s/r_%s.tif', dir_tif, f2date(f, p) %>% format('%Y%m%d'))
    
    # TODO: multiply for clim
    if (!file.exists(tif) | p[['redo_tif']]){
      
      # report
      t1 = Sys.time()
      if (n_done > 0 ){
        t_min_left = as.numeric((difftime(t1, t0, units='min') / n_done) * (length(files) - j + 1))
      } else {
        t_min_left = -9999
      }
      cat(sprintf(
        '  %03d of %d: %s -> 4326:%s [%s, %1.1f min to go]\n', 
        j, length(files), basename(f), basename(tif), 
        format(t1, tz='America/Los_Angeles',usetz=TRUE), t_min_left))
      
      # file (nc|mat) to raster
      r = f2raster(f, p)
      if (is.null(r)){
        cat('    Error reading!\n')
        next()
      }
      
      # raster to tif, unprojected geographic
      r2tif(r, tif, p)
      
      n_done = n_done + 1  
    } # end: if (!file.exists(tif) | p[['redo_tif']])
    
  } # end: for (f in files)
  
  
  # extract eez ----
  eez_csv = sprintf('%s/eez_%s.csv', dir_extract, p[['key']])
  if (!file.exists(eez_csv) | p[['redo_eez']]){
    
    # get full stack of rasters in dir_tif
    s = stack(list.files(dir_tif, '.*\\.tif', full.names=T)) # names(s)
    
    # too big to gather, so map summary into combined df
    d = map_df(seq_along(names(s)), r2eez_csv) # ~20 min for nlayers(s)=181 gl_sst_curr_09km_mo
    
    # join eez names and sort, calculate pct area of total eez
    d = d_eez_km2 %>%
      left_join(
        d, by='eez_mrgid') %>%
      mutate(
        pct_eez = area_km2 / eez_area_km2) %>%
      arrange(eez_territory, raster)
    
    write_csv(d, eez_csv)
  }
  
  # write geoserver property files
  write_gs_properties(p, dir_tif)
  
  # write sld
  create_sld(p, dir_tif)
  # TODO: set permissions
  # ben@mbon: cd /mnt/mbon-supplement/geoserver; sudo chgrp -R users satellite; sudo chmod 775 -R satellite
  
} # end: for (i in 1:length(products))

# TODO: create dir_out parameter files for GeoServer
# * [Using the ImageMosaic plugin for raster time-series data — GeoServer 2.12.x User Manual](http://docs.geoserver.org/stable/en/user/tutorials/imagemosaic_timeseries/imagemosaic_timeseries.html)
# * [Tile Caching with GeoWebCache — GeoServer Training](http://geoserver.geo-solutions.it/edu/en/enterprise/gwc.html)