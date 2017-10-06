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
territories = eez_smp %>% filter(pol_type == '200NM') %>% .$territory1 %>% sort()
#for (i in seq_along(territories)){
for (i in 2:length(territories)){
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
      wdpa_sf = wdpa_sf %>% 
        st_intersection(ter_sf %>% select(territory1))

      # calculate area after extracting to eez
      wdpa_sf = wdpa_sf %>%
        mutate(
          area_wdpa_eez_km2 = st_area(st_geometry(wdpa_sf)))
      
      # rename
      if ('WDPA_PI' %in% names(wdpa_sf)){
        wdpa_sf = rename(wdpa_sf, WDPA_PID = WDPA_PI)
      }
      
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
    
    # join fields with prefix identifying source
    o_sf = obis_sf %>%
      select(occurrence_id) %>%
      st_join(
        wdpa_sf %>%
          select(WDPA_PID),
        left=F)
    # NOTE: multiple MPAs can occur at a single OBIS occurrence point

    o_sf %>%
      st_set_geometry(NULL) %>%
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
