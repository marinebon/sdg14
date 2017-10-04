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
wdpa_zip = file.path(wdpa_dir, 'WDPA_Sep2017-shapefile.zip')
wdpa_shp = file.path(wdpa_dir, 'WDPA_Sep2017-shapefile-polygons.shp')
wdpa_rds = file.path(wdpa_dir, 'WDPA_Sep2017-shapefile-polygons.rds')
wdpa_url = 'https://www.protectedplanet.net/downloads/WDPA_Sep2017?type=shapefile'

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

# fetch-wdpa
if (!file.exists(wdpa_rds)){
  download.file(wdpa_url, wdpa_zip)
  unzip(wdpa_zip, exdir=wdpa_dir)

  wdpa_all = read_sf(wdpa_shp) # WHOAH! 3.5 GB shp

  system.time({
    # [Tidying feature geometries with sf](http://r-spatial.org/r/2017/03/19/invalid.html#tidying-feature-geometries)
    #wdpa_n_invalid = sum(!st_is_valid(wdpa_all))
    #cat(sprintf('wdpa_n_invalid before: %d\n', wdpa_n_invalid))
    #if (wdpa_n_invalid > 0){
      if (!is.na(sf_extSoftVersion()["lwgeom"])) {
        wdpa_all = wdpa_all %>%
          st_make_valid() %>% 
          st_cast()
      } else {
        wdpa_all = wdpa_all %>%
          st_buffer(dist=0) %>%
          st_cast()
      }
      #wdpa_n_invalid = sum(!st_is_valid(wdpa_all))
      #cat(sprintf('wdpa_n_invalid after: %d\n', wdpa_n_invalid))
    #}
  })
  wdpa_rds = file.path(wdpa_dir, 'WDPA_Sep2017-shapefile-polygons.rds')
  saveRDS(wdpa_all, wdpa_rds)
}
if (!exists('wdpa_all')){
  #wdpa_all = readRDS(wdpa_rds)
}

# fetch-obis-by-eez ----
territories = eez_smp %>% filter(pol_type == '200NM') %>% .$territory1 %>% sort()
for (i in seq_along(territories)){
#for (ter in c('Galapagos','Colombia','Costa Rica','Ecuador','Panama')){
  
  #ter = 'Galapagos' # ter = 'Colombia' # sort(eez_smp$territory1)
  ter   = territories[i]
  ter_f = str_replace_all(ter, ' ', '_')
  
  cat(sprintf('%d of %d: %s - %s\n', i, length(territories), ter, Sys.time()))
  
  wkt_txt  = sprintf('%s/eez-%s_wkt.txt', dir_data, ter_f)
  obis_geo = sprintf('%s/eez-%s_obis.geojson', dir_data, ter_f)
  obis_csv = sprintf('%s/eez-%s_obis.csv', dir_data, ter_f)
  wdpa_geo = sprintf('%s/eez-%s_wdpa.geojson', dir_data, ter_f)
  obis_wdpa_csv        = sprintf('%s/eez-%s_obis_wdpa.csv', dir_data, ter_f)
  obis_wdpa_metric_txt = sprintf('%s/eez-%s_obis_wdpa_metric.txt', dir_data, ter_f)
  
  if (!file.exists(wkt_txt)){
    wkt = eez_smp %>%
      filter(
        pol_type == '200NM',
        territory1==ter) %>%
      st_buffer(dist=0.1) %>%
      st_convex_hull() %>%
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
    
    #old: "MULTIPOLYGON(((-81.54267595 12.0980556, -79.44154756 13.87745643, -73.45708733 15.13332554, -71.1315372 12.75828467, -75.39491029 10.51404426, -77.90466345 12.26324209, -81.54267595 12.0980556)), ((-78.6685557 1.42839193, -83.7965417 1.3566667, -84.3166667 5.1, -79.9217019 5.1, -77.93470224 7.31114837, -77.0248035 3.68909216, -78.6685557 1.42839193)))"
    # new w/ st_convex_hull(): POLYGON((-78.76095865 1.29069778, -83.79787516 1.35667559, -84.90682908 3.04724628, -84.41252901 5.02846783, -82.08530671 12.86513453, -79.24309222 15.06604623, -71.30800075 14.98762145, -70.67508796 11.64431718, -77.70543696 2.48822948, -78.76095865 1.29069778))
    
    write_lines(wkt, wkt_txt)
  }
  wkt = readLines(wkt_txt)
  
  # 1 of 230: Alaska - 2017-10-02 17:06:50
  # Error in robis2::occurrence(geometry = wkt) : 
  #   Internal Server Error (HTTP 500).
  bb = readWKT(wkt) %>% st_as_sf() %>%
    st_bbox()
  if (bb['xmax'] > 180 | bb['xmin'] < -180){
    warning("  bb['xmax'] > 180 | bb['xmin'] < -180")
    next()
  }
  
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
      obis_tbl[fld] = NA
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
        occurrence_id = sprintf('obis:%s:%d', ter, row_number())) # TODO: unique ID in OBIS?!
    
    # View(obis_tbl)
    
    # drop unneeded OBIS columns: moot with select() above, retained for explicitness of dropped columns to revisit
    obis_tbl = obis_tbl %>%
      select_(.dots = setdiff(names(obis_tbl), obis_cols_drop))
    
    # fetch OBIS checklist for redlist status and taxonomic info
    cklist = robis::checklist(geometry=wkt) # cklist_0 = cklist # View(cklist_0)
    # get unique taxonomic name (tname), prioritizing with first non-NA of:
    #  valid, worms_id, redlist, status, phylum,...
    for (fld in c('redlist','status')){
      cklist[fld] = NA
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
    eez = eez_all  %>%
      filter(
        pol_type == '200NM',
        territory1==ter)
    obis_sf = obis_sf %>%
      slice(
        st_intersects(
          eez,
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
      write_csv(obis_csv)
  }
  
  # wdpa later
  next()
  
  # read OBIS occurrences and join attributes
  obis_sf = read_sf(obis_geo) %>%
    left_join(
      read_csv(obis_csv),
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
  eez_sf = eez_all %>%
    filter(
      pol_type == '200NM',
      territory1==ter)
  
  if (!file.exists(wdpa_geo)){
    
    # filter by eez's within eez
    wdpa_sf = wdpa_all %>%
      slice(st_intersects(eez_sf, wdpa_all)[[1]])
    
    if (nrow(wdpa_sf) == 0){
      warning('no wdpa')
      write_sf(wdpa_sf, wdpa_geo, delete_dsn=T)
    } else {
      
      # calculate original area before extracting to eez
      wdpa_sf = wdpa_sf %>%
        mutate(
          area_wdpa_orig_km2 = st_area(st_geometry(wdpa_sf)))
      
      # extract intersection with eez
      wdpa_sf = wdpa_sf %>% 
        st_intersection(eez_sf %>% select(territory1))
      # TODO: Error in CPL_geos_op2(op, st_geometry(x), st_geometry(y)) : 
      #   attr classes has wrong size: please file an issue
      # st_geometry(wdpa_sf): GEOMETRY
      # st_geometry(eez_sf):  MULTIPOLYGON
      
      # calculate area after extracting to eez
      wdpa_sf = wdpa_sf %>%
        mutate(
          area_wdpa_eez_km2 = st_area(st_geometry(wdpa_sf)))
      
      write_sf(wdpa_sf, wdpa_geo, delete_dsn=T)
    }
  }
  wdpa_sf = read_sf(wdpa_geo)
  
  # if (nrow(wdpa_sf) == 0){
  #   warning('no wdpa')
  # } else {  
  #   labels <- with(
  #     wdpa_sf,
  #     sprintf(
  #       "<strong>%s</strong><br/>DESIG_ENG: %s<br/>DESIG_TYPE: %s<br/>IUCN CAT: %s<br/>GIS_M_AREA: %s",
  #       NAME, DESIG_ENG, DESIG_TYPE, IUCN_CAT, comma(round(GIS_M_AREA,2)))) %>% lapply(HTML)
  #   
  #   leaflet(eez_sf) %>%
  #     addProviderTiles(providers$Stamen.TonerLite) %>%
  #     addPolygons() %>%
  #     addPolygons(
  #       data = wdpa_sf,
  #       weight = 1,
  #       color = 'green', fillColor = 'green',
  #       label = labels,
  #       highlight = highlightOptions(
  #         color = 'yellow',
  #         weight = 5,
  #         bringToFront = T))
  # }
  
  # join-obis-wdpa ----
  if (!file.exists(obis_wdpa_csv)){
    
    # join fields with prefix identifying source
    o_sf = obis_sf %>%
      select(occurrence_id) %>%
      st_join(
        wdpa_sf %>%
          select(WDPA_PID))
    # NOTE: multiple MPAs can occur at a single OBIS occurrence point
    
    # TODO: continue...
    o_sf %>%
      st_set_geometry(NULL) %>%
      write_csv(obis_wdpa_csv)
  }
  obis_wdpa = read_csv(obis_wdpa_csv)
  
  # calc-metric ----
  if (!file.exists(obis_wdpa_metric_txt)){
    # summarize by single OBIS occurrence_id
    obis1_wdpa = obis_wdpa %>%
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
      filter(
        !is.na(status),
        status %in% names(wts_status)) %>%
      mutate(
        wdpa_pids = ifelse(wdpa_pids=='NA', NA, wdpa_pids),  # weird 'NA', not NA
        in_mpa    = ifelse(!is.na(wdpa_pids), TRUE, FALSE),
        status_wt = wts_status[status]) %>%
      group_by(
        taxonomicgroup, in_mpa) %>%
      summarise(
        status_wt         = mean(status_wt), # NOTE: weighted by number of occurrences
        n_scientificNames = length(unique(scientificName)),
        n_occurrences     = n()) # %>% ungroup()
    # d
    # sum(d$n_occurrences)
    # nrow(obis_sf)
    
    d_dif = d %>%
      select(taxonomicgroup, in_mpa, status_wt) %>%
      spread(in_mpa, status_wt) %>%
      rename(
        avg_status_wt_outside_mpa = `FALSE`,
        avg_status_wt_inside_mpa = `TRUE`) %>%
      mutate(
        status_dif = avg_status_wt_outside_mpa - avg_status_wt_inside_mpa)
    # fully unprotected (out - in): 1   -   0 =  1
    # evenly  protected (out - in): 0.5 - 0.5 =  0
    # fully   protected (out - in): 0   -   1 = -1
    # d_dif
    
    d_dif = d_dif %>%
      filter(
        !is.na(status_dif))
    
    wts_taxa = d %>%
      group_by(taxonomicgroup) %>%
      summarize(
        n_scientificNames = sum(n_scientificNames)) %>% # NOTE: repeats in/out mpa
      mutate(
        wt = rescale(n_scientificNames))
    # wts_taxa
    
    m = weighted.mean(
      x = d_dif$status_dif, 
      w = setNames(
        wts_taxa$wt,
        wts_taxa$taxonomicgroup)[
          d_dif$taxonomicgroup])
    
    write_lines(m, obis_wdpa_metric_txt)
  }
}
