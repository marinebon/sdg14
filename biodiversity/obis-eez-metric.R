# setup ----
knitr::opts_chunk$set(echo=T, message=F, warning=F)
library(tidyverse)
library(stringr)
library(sf)
library(geosphere)
library(mapview)
library(robis)  # devtools::install_github('iobis/robis')
library(robis2) # devtools::install_github('iobis/robis2')
library(rmapshaper)
library(geojsonio)
library(rgeos)
library(scales)
library(DT)
library(leaflet)
library(htmltools)
library(htmltools)

if (basename(getwd()) != 'technical') setwd('technical')

dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big', # BB's Mac
  'Windows' = 'P:',                             # constance.bren.ucsb.edu
  'Linux'   = '/mbon/data_big')                 # mbon.marine.usf.edu
dir_data = file.path(dir_root, 'biodiversity')
eez_all_rdata    = file.path(dir_data, 'eez_all.rdata')
eez_smp_rdata    = file.path(dir_data, 'eez_smp.rdata')
wdpa_dir = file.path(dir_root, 'wdpa')
wdpa_url  = 'http://wcmc.io/wdpa_current_release'
wdpa_zip  = file.path(wdpa_dir, 'WDPA_Oct2017_Public.zip') # dir_tmp = '~/Data/mbon_data_big/wdpa' %>% normalizePath()
wdpa_gdb  = file.path(wdpa_dir, 'WDPA_Oct2017_Public.gdb')
wdpa_shp  = file.path(wdpa_dir, 'WDPA_poly_Oct2017.shp')
wdpa_gpkg = file.path(wdpa_dir, 'WDPA_Oct2017_Public.gpkg')

obis_cols_drop = c(
  # ID fields:
  # 'id','institutionID','collectionID','datasetID','eventID','identificationID','individualID','locationID','materialSampleID','occurrenceID','scientificNameID','taxonConceptID'
  'acceptedNameUsage','acceptedNameUsageID','accessRights','associatedMedia','associatedReferences','associatedSequences','associatedTaxa',
  'basisOfRecord','behavior','bibliographicCitation',
  'collectionID','continent','coordinatePrecision','coordinateUncertaintyInMeters','countryCode','county',
  'dataGeneralizations','datasetID','dateIdentified','division','dynamicProperties',
  'establishmentMeans','eventID','eventRemarks','eventTime',
  'fieldNotes','fieldNumber','footprintSRS','footprintWKT','forma',
  'geodeticDatum',
  'habitat','higherClassification','higherGeography','higherGeographyID',
  'identificationID','identificationQualifier','identificationReferences','identificationRemarks','identifiedBy','individualCount',
  'individualID','informationWithheld','infraclass','infrakingdom','infraorder','infraphylum','institutionID','island','islandGroup',
  'language','lifeStage','locality','locationAccordingTo','locationID','locationRemarks',
  'materialSampleID','maximumDepthInMeters','minimumDepthInMeters','modified','municipality',
  'nameAccordingTo','nameAccordingToID','namePublishedIn','namePublishedInID',
  'occurrenceRemarks','occurrenceStatus','originalNameUsage','originalNameUsageID','otherCatalogNumbers','ownerInstitutionCode',
  'parvorder',
  'section',
  'recordNumber','recordedBy','references','reproductiveCondition','rights','rightsHolder',
  'scientificNameAuthorship','scientificNameID','sex','source',
  'stateProvince','subclass','subdivision','subfamily','subforma','subgenus','subkingdom','suborder',
  'subphylum','subsection','subspecies','subterclass','subtribe','subvariety',
  'superclass','superfamily','superorder','supertribe',
  'taxonConceptID','taxonRemarks','taxonomicStatus',
  'tribe','type','typeStatus',
  'variety','vernacularName',
  'waterBody',
  'phylum','class','order','family','genus','species') # covered by checklist

# fetch-eezs ----
if (!file.exists(eez_all_rdata)){
  eez_url = 'http://geo.vliz.be/geoserver/MarineRegions/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=MarineRegions:eez'
  eez_all = read_sf(eez_url)           # 2.2 min
  
  # make geometries valid
  if (!is.na(sf_extSoftVersion()["lwgeom"])) {
    suppressWarnings(st_is_valid(eez_all))
    eez_all = st_make_valid(eez_all)
    st_is_valid(eez_all)
    eez_all = eez_all %>% st_cast()
  }  
  
  saveRDS(eez_all, file=eez_all_rdata) # store compressed
}
if (!exists('eez_all')){
  eez_all = readRDS(eez_all_rdata) %>%
    st_set_crs(4326)
}

eez_all %>%
  write_sf('/mbon-local/postgresql/shp/eez_wfs.shp') # /var/lib/postgresql/shp/eez_wfs.shp

# show table without geom
# eez_all %>%
#   st_set_geometry(NULL)

# simplify-eezs ----
if (!file.exists(eez_smp_rdata)){
  
  eez_smp = eez_all %>%
    geojson_json() %>%
    ms_simplify() %>%
    geojson_sp() %>%
    st_as_sf()
  
  saveRDS(eez_smp, file=eez_smp_rdata) # store compressed
} 
if (!exists('eez_smp')){
  eez_smp = readRDS(eez_smp_rdata)  
}

#mapview(eez_smp)
# leaflet(eez_smp) %>%
#   addTiles() %>%
#   addPolygons()

# fetch-wdpa ----
if (!file.exists(wdpa_gpkg)){
  download.file(wdpa_url, wdpa_zip)
  unzip(wdpa_zip, exdir=wdpa_dir)
  #st_layers(wdpa_gdb)
  # Driver: OpenFileGDB 
  # Available layers:
  #            layer_name geometry_type features fields
  # 1  WDPA_point_Oct2017   Multi Point    18442     26
  # 2 WDPA_source_Oct2017            NA      492     17
  # 3   WDPA_poly_Oct2017 Multi Polygon   216026     30
  wdpa_all = read_sf(dsn=wdpa_gdb, 'WDPA_poly_Oct2017')
  
  # check if your layer contains problematic MULTISURFACE geometries
  if ('MULTISURFACE' %in% unique(st_geometry_type(st_geometry(wdpa_all)))){
    # Error in slice_impl(.data, dots) : 
    #   Evaluation error: Evaluation error: ParseException: Unknown WKB type 12.
    wdpa_all = wdpa_all %>%
      st_cast('MULTIPOLYGON') # 72 sec
  }
  write_sf(wdpa_all, wdpa_shp) # 16.8 min
  gdalUtils::ogr2ogr(wdpa_shp, wdpa_gpkg, layer='WDPA_poly_Oct2017', f='GPKG', overwrite=T) # 3.9 min
  # [Tidying feature geometries with sf](http://r-spatial.org/r/2017/03/19/invalid.html#tidying-feature-geometries)
}
if (!exists('wdpa_all')){
  wdpa_all = read_sf(dsn=wdpa_gpkg, 'WDPA_poly_Oct2017') # 8.6 min
  if ('WDPA_PI' %in% names(wdpa_all)){
    wdpa_all = rename(wdpa_all, WDPA_PID = WDPA_PI)
  }
}

# world bbox
world_wkt = 'POLYGON ((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
world_sf = readWKT(world_wkt) %>% st_as_sf() %>% st_set_crs(4326)

# fetch-obis-by-eez ----
territories = eez_smp %>% filter(pol_type == '200NM') %>% .$territory1 %>% as.character() %>% sort()
#for (i in seq_along(territories)){
# TODO: fix Alaska [i=1] post eez-Alaska__obis.csv
#   001 of 230: Alaska - 2017-10-10 23:12:16
#   Error in CPL_geos_op2(op, st_geometry(x), st_geometry(y)) : 
#     Evaluation error: TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point -148.37773894399993 60.75520225300005 at -148.37773894399993 60.75520225300005.

# 036 of 230: Canada - 2017-10-12 04:55:59
# Retrieved 6620000 records of 6713332 (98%)
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#   Operation was aborted by an application callback

# 176 of 230: Russia - 2017-10-13 03:18:13
# Retrieved 120000 records of 18896133 (0%)Retrieved 240000 records of 18896133 (1%)Retrieved 360000 records of 18896133 (1%)Retrieved 490000 records of 18896133 (2%)Retrieved 610000 records of 18896133 (%))Retrieved 730000 records of 18896133 (3%)Retrieved 860000 records of 18896133 (4%)Retrieved 980000 records of 18896133 (5%)Retrieved 1100000 records of 18896133 (5%)Retrieved 1220000 records of 18896133 (6%)Retrieved 1340000 records of 18896133 (7%)Retrieved 1460000 records of 18896133 (7%)Retrieved 1580000 records of 18896133 (8%)Retrieved 1700000 records of 18896133 (8%)Retrieved 1820000 records of 18896154 (9%)Retrieved 1940000 records of 18896133 (10%)Retrieved 2060000 records of 18896133 (10%)Retrieved 2170000 records of 18896133 (11%)Retrieved 2290000 records of 18896133 (12%)Retrieved 2410000 records of 18896133 (12%)Retrieved 2530000 records of 18896133 (13%)Retrieved 2640000 records of 18896133 (13%)Retrieved 2760000 records of 18896133 (14%)Retrieved 2880000 records of 18896133 (15%)Retrieved 3e+0600 records of 18896133 (15%)Retrieved 3110000 records of 18896133 (16%)Retrieved 3230000 records of 18896133 (17%)Retrieved 3350000 records of 18896133 (17%)Retrieved 3470000 records of 18896133 (18%)Retrieved 3580000 records of 18896133 (18%)Retrieved 3700000 records of 18896133 (19%)Retrieved 3820000 records of 18896133 (20%)Retrieved 3940000 records of 18896133 (20%)Retrieved 4050000 records of 18896133 (21%)Retrieved 4170000 records of 18896133 (22%)Retrieved 4290000 records of 18896133 (22%)Retrieved 4410000 records of 18896133 (23%)Retrieved 4520000 records of 18896133 (23%)Retrieved 4640000 records of 18896133 (24%)Retrieved 4760000 records of 18896133 (25%)Retrieved 4880000 records of 18896133 (25%)Retrieved 4990000 records of 18896133 (26%)Retrieved 5110000 records of 18896133 (27%)Retrieved 5230000 records of 18896133 (27%)Retrieved 5350000 records of 18896133 (28%)Retrieved 5460000 records of 18896133 (28%)Retrieved 5580000 records of 18896133 (29%)Retrieved 5700000 records of 18896133 (30%)Retrieved 5820000 records of 18896133 (30%)Retrieved 5930000 records of 18896133 (31%)Retrieved 6050000 records of 18896133 (32%)Retrieved 6170000 records of 18896133 (32%)Retrieved 6290000 records of 18896133 (33%)Retrieved 6400000 records of 18896133 (33%)Retrieved 6520000 records of 18896133 (34%)Retrieved 6640000 records of 18896133 (35%)Retrieved 6760000 records of 18896133 (35%)Retrieved 6870000 records of 18896133 (36%)Retrieved 6990000 records of 18896133 (36%)Retrieved 7110000 records of 18896133 (37%)Retrieved 7230000 records of 18896133 (38%)Retrieved 7340000 records of 18896133 (38%)Retrieved 7460000 records of 18896133 (39%)Retrieved 7580000 records of 18896133 (40%)Retrieved 7700000 records of 18896133 (40%)Retrieved 7810000 records of 18896133 (41%)Retrieved 7930000 records of 18896133 (41%)Retrieved 8050000 records of 18896133 (42%)Retrieved 8170000 records of 18896133 (43%)Retrieved 8280000 records of 18896133 (43%)Retrieved 8400000 records of 18896133 (44%)Retrieved 8520000 records of 18896133 (45%)Retrieved 8640000 records of 18896133 (45%)Retrieved 8750000 records of 18896133 (46%)Retrieved 8870000 records of 18896133 (46%)Retrieved 8990000 records of 18896133 (47%)Retrieved 9110000 records of 18896133 (48%)Retrieved 9220000 records of 18896133 (48%)Retrieved 9340000 records of 18896133 (49%)Retrieved 9460000 records of 18896133 (50%)Retrieved 9580000 records of 18896133 (50%)Retrieved 9690000 records of 18896133 (51%)Retrieved 9810000 records of 18896133 (51%)Retrieved 9930000 records of 18896133 (52%)Retrieved 10050000 records of 18896133 (53%)Retrieved 10160000 records of 18896133 (53%)Retrieved 10270000 records of 18896133 (54%)Retrieved 10390000 records of 18896133 (54%)Retrieved 10500000 records of 18896133 (55%)Retrieved 10620000 records of 18896133 (56%)Retrieved 10730000 records of 18896133 (56%)Retrieved 10850000 records of 18896133 (57%)Retrieved 10960000 records of 18896133 (58%)Retrieved 11080000 records of 18896133 (58%)Retrieved 11190000 records of 18896133 (59%)Retrieved 11310000 records of 18896133 (59%)Retrieved 11420000 records of 18896133 (60%)Retrieved 11540000 records of 18896133 (61%)Retrieved 11650000 records of 18896133 (61%)Retrieved 11770000 records of 18896133 (62%)Retrieved 11880000 records of 18896133 (62%)Retrieved 1.2e+07 records of  18896133 (63%)Retrieved 12110000 records of 18896133 (64%)Retrieved 12230000 records of 18896133 (64%)Retrieved 12340000 records of 18896133 (65%)Retrieved 12460000 records of 18896133 (65%)Retrieved 12570000 records of 18896133 (66%)Retrieved 12690000 records of 18896133 (67%)Retrieved 12800000 records of 18896133 (67%)Retrieved 12920000 records of 18896133 (68%)Retrieved 13030000 records of 18896133 (68%)Retrieved 13150000 records of 18896133 (69%)Retrieved 13260000 records of 18896133 (70%)Retrieved 13380000 records of 18896133 (70%)Retrieved 13490000 records of 18896133 (71%)Retrieved 13610000 records of 18896133 (71%)Retrieved 13720000 records of 18896133 (72%)Retrieved 13840000 records of 18896133 (73%)Retrieved 13950000 records of 18896133 (73%)Retrieved 14070000 records of 18896133 (74%)Retrieved 14180000 records of 18896133 (75%)Retrieved 14300000 records of 18896133 (75%)Retrieved 14410000 records of 18896133 (76%)Retrieved 14530000 records of 18896133 (76%)Retrieved 14640000 records of 18896133 (77%)Retrieved 14760000 records of 18896133 (78%)Retrieved 14870000 records of 18896133 (78%)Retrieved 14990000 records of 18896133 (79%)Retrieved 15100000 records of 18896133 (79%)Retrieved 15220000 records of 18896133 (80%)Retrieved 15330000 records of 18896133 (81%)Retrieved 15450000 records of 18896133 (81%)Retrieved 15560000 records of 18896133 (82%)Retrieved 15680000 records of 18896133 (82%)Retrieved 15790000 records of 18896133 (83%)Retrieved 15910000 records of 18896133 (84%)Retrieved 16020000 records of 18896133 (84%)Retrieved 16140000 records of 18896133 (85%)Retrieved 16250000 records of 18896133 (85%)Retrieved 16370000 records of 18896133 (86%)Retrieved 16480000 records of 18896133 (87%)Retrieved 16600000 records of 18896133 (87%)Retrieved 16710000 records of 18896133 (88%)Retrieved 16830000 records of 18896133 (89%)Retrieved 16940000 records of 18896133 (89%)Retrieved 17060000 records of 18896133 (90%)Retrieved 17170000 records of 18896133 (90%)Retrieved 17290000 records of 18896133 (91%)Retrieved 17400000 records of 18896133 (92%)Retrieved 17520000 records of 18896133 (92%)Retrieved 17630000 records of 18896133 (93%)Retrieved 17750000 records of 18896133 (93%)Retrieved 17860000 records of 18896133 (94%)Retrieved 17980000 records of 18896133 (95%)Retrieved 18090000 records of 18896133 (95%)Retrieved 18210000 records of 18896133 (96%)Retrieved 18320000 records of 18896133 (96%)Retrieved 18440000 records of 18896133 (97%)Retrieved 18550000 records of 18896133 (98%)Retrieved 18670000 records of 18896133 (98%)Retrieved 18780000 records of 18896133 (99%)Retrieved 18896133 records of 18896133 (99%)18896133 (100%)
# Error in system(paste(which, shQuote(names[i])), intern = TRUE, ignore.stderr = TRUE) : 
#   cannot popen '/usr/bin/which 'pdflatex' 2>/dev/null', probable reason 'Cannot allocate memory'

for (i in 177:length(territories)){
#for (i in 28:length(territories)){ # TODO: Alaska on laptop?
#for (ter in c('Galapagos','Colombia','Costa Rica','Ecuador','Panama')){ # ter = 'Colombia'; 
#for (ter in c('Costa Rica','Ecuador','Panama')){ # ter = 'Colombia'; # ter = 'Alaska'
#   i = which(territories==ter)
  
  #ter = 'Galapagos' # ter = 'Colombia'; 
  ter   = territories[i]
  ter_f = str_replace_all(ter, ' ', '_')
  
  cat(sprintf('%03d of %d: %s - %s\n', i, length(territories), ter, Sys.time()))
  
  wkt_txt       = sprintf('%s/eez-%s_wkt.txt', dir_data, ter_f)
  obis_geo      = sprintf('%s/eez-%s_obis.geojson', dir_data, ter_f)
  obis_csv      = sprintf('%s/eez-%s_obis.csv', dir_data, ter_f)
  wdpa_geo      = sprintf('%s/eez-%s_wdpa.geojson', dir_data, ter_f)
  obis_wdpa_csv = sprintf('%s/eez-%s_obis_wdpa.csv', dir_data, ter_f)
  metrics_csv   = sprintf('%s/eez-%s_metrics.csv', dir_data, ter_f)
  
  # get eez for territory, original and simplified
  ter_sf = eez_all  %>%
    filter(
      pol_type == '200NM',
      territory1==ter)
  
  ter_smp_sf = eez_smp %>%
    filter(
      pol_type == '200NM',
      territory1==ter)
  
  # TEMP FIX
  if (file.exists(wkt_txt)){
    # redo wkt if outside world [-180, 180]
    # 1 of 230: Alaska - 2017-10-02 17:06:50
    # Error in robis2::occurrence(geometry = wkt) : 
    #   Internal Server Error (HTTP 500).
    wkt = readLines(wkt_txt)
    bb = readWKT(wkt) %>% st_as_sf() %>%
      st_bbox()
    if (bb['xmax'] > 180 | bb['xmin'] < -180){
      cat("  REDO wkt: bb['xmax'] > 180 | bb['xmin'] < -180!\n")
      file.remove(wkt_txt)
    }
  }
  
  if (!file.exists(wkt_txt)){
    wkt = ter_smp_sf %>%
      st_buffer(dist=0.1) %>%
      st_convex_hull() %>%
      st_intersection(world_sf) %>%
      st_geometry() %>%
      st_as_text()
    
    # simplify well-known text to size passable via URL for OBIS querying
    while (nchar(wkt) > 2048){
      wkt = readWKT(wkt) %>%
        geojson_json() %>%
        ms_simplify() %>%
        geojson_sp() %>%
        st_as_sf() %>%
        st_geometry() %>%
        st_as_text()
    }
    
    write_lines(wkt, wkt_txt)
  }
  wkt = readLines(wkt_txt)

  if (any(!file.exists(obis_geo), !file.exists(obis_csv))){
    # fetch OBIS occurrences
    obis_tbl = robis2::occurrence(geometry=wkt) %>%
      as_tibble()
    
    if (nrow(obis_tbl)==0){
      cat('   0 records\n')
      file.create(obis_csv) # touch file to indicate OBIS searched
      next()
    }
    
    for (fld in c('year','eventDate','depth','taxonRank')){
      if (!fld %in% names(obis_tbl)){
        obis_tbl[fld] = NA
      }
    }
    
    obis_tbl = obis_tbl %>%
      select(
        id, kingdom, taxonomicgroup, scientificName, 
        year, eventDate, depth,
        decimalLongitude, decimalLatitude,
        taxonRank, 
        worms_id, valid_id, originalscientificname,
        resource_id, catalogNumber, institutionCode, collectionCode,
        qc) %>%
      arrange(kingdom, taxonomicgroup, scientificName, year, id) %>%
      mutate(
        occurrence_id = sprintf('obis-eez:%s:%d', ter, row_number())) # TODO: unique ID in OBIS?!
    # View(obis_tbl)
    
    # drop unneeded OBIS columns: moot with select() above, retained for explicitness of dropped columns to revisit
    obis_tbl = obis_tbl %>%
      select_(.dots = setdiff(names(obis_tbl), obis_cols_drop))
    
    # fetch OBIS checklist for redlist status and taxonomic info
    cklist = robis::checklist(geometry=wkt) # cklist_0 = cklist # View(cklist_0)
    # get unique taxonomic name (tname), prioritizing with first non-NA of:
    #  valid, worms_id, redlist, status, phylum,...
    for (fld in c('redlist','status')){ # table(cklist$status)
      if (!fld %in% names(cklist)){
        cklist[fld] = NA
      }
    }
    cklist = cklist %>%
      rename(
        scientificName = tname) %>%
      mutate(
        invalid = valid_id != id) %>%
      arrange(scientificName,invalid,worms_id,redlist,status,phylum,class,order,family,genus,species) %>% # View(cklist)
      group_by(worms_id) %>%
      summarize(
        checklist_id   = first(id),
        scientificName = first(scientificName),
        valid_id       = first(valid_id),
        parent_id      = first(parent_id),
        rank_name      = first(rank_name),
        tauthor        = first(tauthor),
        phylum         = first(phylum),
        class          = first(class),
        order          = first(order),
        family         = first(family),
        genus          = first(genus),
        species        = first(species),
        redlist        = first(redlist),
        status         = first(status)) %>%
      mutate(
        invalid = valid_id != checklist_id) %>%
      arrange(invalid,scientificName,worms_id,redlist,status,phylum,class,order,family,genus,species)
    obis_tbl = obis_tbl %>%
      left_join(
        cklist %>%
          select(-scientificName, -valid_id), # already in obis_tbl
        by='worms_id')
    
    # turn table into points
    obis_sf = obis_tbl %>%
      st_as_sf(
        coords = c('decimalLongitude','decimalLatitude'),
        crs=4326, agr='constant')
    
    # rm points outside original unsimplified, unbuffered eez
    obis_sf = obis_sf %>%
      slice(
        st_intersects(
          ter_sf,
          obis_sf)[[1]])
    
    # write obis_geo with just id to save disk space, for later rejoining with obis_tbl
    if (sum(duplicated(obis_tbl$occurrence_id)) > 0) 
      stop('Whoah! Expecting all unique ids in OBIS occurrence table.')
    obis_sf %>%
      select(occurrence_id) %>%
      write_sf(obis_geo, delete_dsn=T)
    
    # write obis_tbl to csv
    obis_sf %>%
      st_set_geometry(NULL) %>%
      write_csv(obis_csv) # table(obis_sf$status)
  }
  
  # wdpa later
  #next()
  
  # read OBIS occurrences and join attributes
  obis_tbl = read_csv(obis_csv)
  if (nrow(obis_tbl) == 0){
    cat('  SKIPPING: 0 OBIS records\n')
    next()
  }
  obis_sf = read_sf(obis_geo) %>%
    left_join(
      obis_tbl,
      by='occurrence_id')
  
  # leaflet(obis_sf) %>%
  #   addProviderTiles(providers$Stamen.TonerLite) %>%
  #   addPolygons(
  #     data = eez_smp %>%
  #       filter(territory1==ter)) %>%
  #   addMarkers(
  #     clusterOptions = markerClusterOptions())
  
  # obis_sf %>%
  #   st_set_geometry(NULL) %>%
  #   head()
  
  # TODO: wdpa later
  #next()
  
  # extract-wdpa-by-eez ----
  if (!file.exists(wdpa_geo)){

    # filter by wdpa's within eez
    wdpa_sf = wdpa_all %>%
      slice(st_intersects(ter_sf, wdpa_all)[[1]])
    
    if (nrow(wdpa_sf) == 0){
      warning('no wdpa')
      write_sf(wdpa_sf, wdpa_geo, delete_dsn=T)
    } else {
      
      # calculate original area before extracting to eez
      wdpa_sf = wdpa_sf %>%
        mutate(
          area_wdpa_orig_km2 = st_area(st_geometry(wdpa_sf)))
      
      # extract intersection with eez
      # TODO: fix Alaska [i=1] post eez-Alaska__obis.csv
      #   001 of 230: Alaska - 2017-10-10 23:12:16
      #   Error in CPL_geos_op2(op, st_geometry(x), st_geometry(y)) : 
      #     Evaluation error: TopologyException: Input geom 0 is invalid: Ring Self-intersection at or near point -148.37773894399993 60.75520225300005 at -148.37773894399993 60.75520225300005.
      wdpa_sf = wdpa_sf %>% 
        st_intersection(ter_sf %>% select(territory1))
      
      # ensure proper geometry, eg line slivers with Micronesia (i=136)
      if (any(st_geometry_type(st_geometry(wdpa_sf)) != "MULTIPOLYGON")){
        cat('  CASTING to MULTIPOLYGON\n')
        wdpa_sf = wdpa_sf %>% 
          st_cast("MULTIPOLYGON")
      }
      
      # calculate area after extracting to eez
      wdpa_sf = wdpa_sf %>%
        mutate(
          area_wdpa_eez_km2 = st_area(st_geometry(wdpa_sf)))
      
      # rename
      if ('WDPA_PI' %in% names(wdpa_sf)){
        wdpa_sf = rename(wdpa_sf, WDPA_PID = WDPA_PI)
      }
      
      # only admit valid geometries
      #   TODO: fix with st_make_valid() but not on mbon server (need lwgeom), 
      #         eg line slivers with Micronesia (i=136)
      #         http://r-spatial.org/r/2017/03/19/invalid.html
      wdpa_sf = wdpa_sf[which(st_is_valid(wdpa_sf)),]
      
      write_sf(wdpa_sf, wdpa_geo, delete_dsn=T)
    }
  }
  wdpa_sf = read_sf(wdpa_geo)

  # if (nrow(wdpa_sf) == 0){
  #   warning('no wdpa')
  # } else {
    # labels <- with(
    #   wdpa_sf,
    #   sprintf(
    #     "<strong>%s</strong><br/>DESIG_ENG: %s<br/>DESIG_TYPE: %s<br/>IUCN CAT: %s<br/>GIS_M_AREA: %s",
    #     NAME, DESIG_ENG, DESIG_TYPE, IUCN_CAT, comma(round(GIS_M_AREA,2)))) %>% lapply(HTML)
    # 
    # leaflet(ter_sf) %>%
    #   addProviderTiles(providers$Stamen.TonerLite) %>%
    #   addPolygons() %>%
    #   addPolygons(
    #     data = wdpa_sf, #%>%
    #       #filter(IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI')),
    #     weight = 1,
    #     color = 'green', fillColor = 'green',
    #     label = labels,
    #     highlight = highlightOptions(
    #       color = 'yellow',
    #       weight = 5,
    #       bringToFront = T))
  # }
  
  # join-obis-wdpa ----
  if (!file.exists(obis_wdpa_csv)){
    
    if (nrow(wdpa_sf) == 0){
      
      # populate empty table with same fields
      o_tbl = tibble(
        occurrence_id = character(0),
        WDPA_PID      = character(0))
      
    } else {
    
      # join fields with prefix identifying source
      # "Evaluation error: IllegalArgumentException: Invalid number of points in LinearRing found 3 - must be 0 or >= 4."
      o_tbl = obis_sf %>%
        select(occurrence_id) %>%
        st_join(
          wdpa_sf %>%
            select(WDPA_PID),
          left=F) %>%
        st_set_geometry(NULL)
      # NOTE: multiple MPAs can occur at a single OBIS occurrence point
      
    }
    
    o_tbl %>%
      write_csv(obis_wdpa_csv)
  }
  obis_wdpa = read_csv(obis_wdpa_csv)
  
  # calc-metric ----
  if (!file.exists(metrics_csv)){
    # TODO: weight by level of protection? 
    IUCN_protection = c(
      Ia  = 'Strict Nature Reserve',
      Ib  = 'Wilderness area',
      II  = 'National Park',
      III = 'Natural Monument or feature',
      IV  = 'Habitat/species management area',
      V   = 'Protected landscape/seascape',
      VI  = 'Protected area with sustainable use of natural resources')
    # other to be discarded?: Not Applicable, Not Assigned, Not Reported
    #   excluding means no OBIS pts in MPAs, eg Galapagos, so NA or 1 metric?
    
    # summarize by single OBIS occurrence_id
    obis1_wdpa = obis_wdpa %>%
      mutate(
        WDPA_PID = as.character(WDPA_PID)) %>%
      left_join(
        wdpa_all, by = 'WDPA_PID') %>%
      #filter(IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI')) %>%
      group_by(occurrence_id) %>%
      summarize(
        wdpa_pids = paste(WDPA_PID, collapse=',')) # populate list of WDPA park ids
    
    # see Selig et al. - 2013 - Assessing Global Marine Biodiversity Status within a Coupled Socio-Ecological Perspective
    wts_status = c(LC=0, NT=0.2, VU=0.4, EN=0.6, CR=0.8, EX=1)
    
    d = obis_sf %>%
      st_set_geometry(NULL) %>% # drop spatial
      left_join(
        obis1_wdpa,
        by='occurrence_id') %>%
      #select(occurrence_id, taxonomicgroup, status, wdpa_pids) %>% View()
      filter(
        !is.na(status),
        status %in% names(wts_status)) %>% # View()
      mutate(
        wdpa_pids = ifelse(wdpa_pids=='NA', NA, wdpa_pids),  # weird 'NA', not NA
        in_mpa    = ifelse(!is.na(wdpa_pids), TRUE, FALSE),
        status_wt = wts_status[status]) %>% # View()
      group_by(
        taxonomicgroup, in_mpa) %>% # View() # table(d$taxonomicgroup, useNA='ifany') ; table(d$in_mpa, useNA='ifany')
      summarise(
        status_wt         = mean(status_wt), # NOTE: weighted by number of occurrences
        n_scientificNames = length(unique(scientificName)),
        n_occurrences     = n()) # %>% ungroup()

    if (length(setdiff(c(TRUE,FALSE), d$in_mpa)) > 0 ){
      # need in and out of MPA to come up with metric
      m = NA
      cat('  NA metrics_csv b/c need TRUE & FALSE in_mpa\n')
    } else {
      d_dif = d %>%
        select(taxonomicgroup, in_mpa, status_wt) %>%
        spread(in_mpa, status_wt) %>%
        rename(
          avg_status_wt_outside_mpa = `FALSE`,
          avg_status_wt_inside_mpa = `TRUE`) %>%
        mutate(
          status_dif = avg_status_wt_inside_mpa - avg_status_wt_outside_mpa,
          # fully  protected (in - out): 1   -   0 =  1
          # evenly protected (in - out): 0.5 - 0.5 =  0
          # NOT    protected (in - out): 0   -   1 = -1
          status     = rescale(status_dif, to=c(0, 1), from=c(-1, 1))
          # fully  protected:   1
          # evenly protected: 0.5
          # NOT    protected:   0
        ) %>%
        filter(!is.na(status))
      
      # assign weights to taxonomicgroup based on species richness regardless of having RedList status
      wts_taxa = d %>%
        group_by(taxonomicgroup) %>%
        summarize(
          n_scientificNames = sum(n_scientificNames)) %>% # NOTE: repeats in/out mpa
        mutate(
          wt = rescale(n_scientificNames))
      # wts_taxa
      
      m = weighted.mean(
        x = d_dif$status, 
        w = setNames(
          wts_taxa$wt,
          wts_taxa$taxonomicgroup)[
            d_dif$taxonomicgroup])
    }
    
    tbl_metrics = tibble(
      eez_territory1 = ter,
      eez_pol_type = '200NM',
      n_obs = nrow(obis_sf),
      n_spp = length(unique(obis_sf$scientificName)),
      idx_obis_wdpa = m)
      
    write_csv(tbl_metrics, metrics_csv)
  }
}

# aggregate data for shiny app ----

# metrics
d_metrics = list.files(dir_data, '^eez-.*_metrics\\.csv$', full.names=T) %>%
  map(
    ~ read_csv(.x) %>% 
      mutate(
        eez_territory1 = as.character(eez_territory1),
        eez_pol_type   = as.character(eez_pol_type),
        n_obs          = as.integer(n_obs),
        n_spp          = as.integer(n_spp),
        idx_obis_wdpa  = as.double(idx_obis_wdpa))) %>%
  bind_rows()
d_metrics = eez_smp %>% 
  st_set_geometry(NULL) %>%
  filter(pol_type == '200NM') %>% 
  mutate(
    eez_territory1 = as.character(territory1),
    eez_pol_type   = '200NM') %>%
  arrange(eez_territory1) %>%
  select(eez_territory1, eez_pol_type) %>%
  left_join(
    d_metrics,
    by=c('eez_territory1','eez_pol_type'))
write_csv(d_metrics, file.path(dir_data, '_eez_obis_metrics.csv'))

# obis
fun_taxa = function(csv){
  # csv = '/mbon/data_big/biodiversity/eez-Yemen_obis.csv'
  cat(paste(csv, '\n'))
  d = read_csv(csv)
  
  if (nrow(d) == 0){
    res = NULL
    cat('  NULL\n')
  } else {
    res = d %>%
      select(taxonomicgroup, kingdom, phylum, class, order, family, genus, species, scientificName) %>%
      mutate(
        eez_territory1 = str_replace(csv, '.*/eez-(.*)_obis.csv', '\\1'))
  }
  res
}

d_taxa = list.files(dir_data, '^eez-.*_obis\\.csv$', full.names=T) %>%
  map(fun_taxa) %>%
  bind_rows() %>%
  group_by(eez_territory1, taxonomicgroup, kingdom, phylum, class, order, family, genus, species) %>%
  summarize(
    n_obs = n(),
    n_spp = length(unique(scientificName))) %>%
  # TODO: calculate idx_obis_wdpa per group!
  left_join(
    d_metrics %>%
      select(eez_territory1, idx_obis_wdpa),
    by = 'eez_territory1')
write_csv(d_taxa, file.path(dir_data, '_eez_obis_taxa.csv'))

