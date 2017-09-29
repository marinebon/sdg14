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

#roi              = 'cmar'
#roi_url          = 'https://chm.cbd.int/api/v2013/documents/FB73B0D1-64FB-367F-405A-51AAA4C060F7/attachments/ETTP_8_EBSA.geojson'
#roi              = 'clipperton'
#roi_url          = 'https://chm.cbd.int/api/v2013/documents/494D9489-5D08-524D-84AE-AD6817304655/attachments/ETTP_2_EBSA.geojson'
#roi_geo          = sprintf('cache/%s.geojson', roi)
# occ_csv          = sprintf('cache/%s_occ.csv', roi)
# occ_cklist_csv   = sprintf('cache/%s_occ_checklist.csv', roi)
# occ_txt          = sprintf('cache/%s_occ_nwkt.txt', roi)
# occ_geo          = sprintf('cache/%s_occ.geojson', roi)
dir_root = '/mbon/data_big'
dir_data = file.path(dir_root, 'biodiversity')
eez_all_rdata    = file.path(dir_data, 'eez_all.rdata')
eez_smp_rdata    = file.path(dir_data, 'eez_smp.rdata')
# eez_geo          = sprintf('cache/%s_eez.geojson', roi)
# wdpa_geo         = sprintf('cache/%s_wdpa.geojson', roi)
# wdpa_eez_geo     = sprintf('cache/%s_wdpa_eez.geojson', roi)
# occ_eez_wdpa_csv = sprintf('cache/%s_occ_eez_wdpa.csv', roi)

#if (!dir.exists('cache')) dir.create('cache')

obis_cols_drop = c(
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
  'occurrenceID','occurrenceRemarks','occurrenceStatus','originalNameUsage','originalNameUsageID','otherCatalogNumbers','ownerInstitutionCode',
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
eez_all = readRDS(eez_all_rdata) %>%
  st_set_crs(4326)

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
eez_smp = readRDS(eez_smp_rdata)

# leaflet(eez_smp) %>%
#   addTiles() %>%
#   addPolygons()

# fetch-obis-by-eez ----
#for (ter in sort(eez_smp$territory1)){
for (ter in c('Galapagos','Colombia','Costa Rica','Ecuador','Panama')){

  #ter = 'Galapagos' # sort(eez_smp$territory1)
  cat(sprintf('%s - %s\n', ter, Sys.time()))
  ter_f = str_replace_all(ter, ' ', '_')

  wkt_txt  = sprintf('%s/eez-%s_wkt.txt', dir_data, ter_f)
  obis_geo = sprintf('%s/eez-%s_obis.geojson', dir_data, ter_f)
  obis_csv = sprintf('%s/eez-%s_obis.csv', dir_data, ter_f)
  
  if (!file.exists(wkt_txt)){
    wkt = eez_smp %>%
      filter(
        pol_type == '200NM',
        territory1==ter) %>%
      st_buffer(dist=0.1) %>%
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
      as_tibble() %>%
      rename(
        occurrence_id = id) %>%
      select(
        kingdom, taxonomicgroup, scientificName, year, occurrence_id,
        eventDate, decimalLongitude, decimalLatitude, depth,
        taxonRank, worms_id, valid_id, originalscientificname,
        resource_id, catalogNumber, institutionCode, collectionCode,
        qc) %>%
      arrange(kingdom, taxonomicgroup, scientificName, year, occurrence_id) # View(obis_tbl)
    
    # drop unneeded OBIS columns: moot with select() above, retained for explicitness of dropped columns to revisit
    obis_tbl = obis_tbl %>%
      select_(.dots = setdiff(names(obis_tbl), obis_cols_drop))
    
    # fetch OBIS checklist for redlist status and taxonomic info
    cklist = robis::checklist(geometry=wkt) # cklist_0 = cklist # View(cklist_0)
    # get unique taxonomic name (tname), prioritizing with first non-NA of:
    #  valid, worms_id, redlist, status, phylum,...
    cklist = cklist %>%
      rename(
        scientificName = tname) %>%
      mutate(
        invalid = valid_id != id) %>%
      arrange(scientificName,invalid,worms_id,redlist,status,phylum,class,order,family,genus,species) %>% # View(cklist)
      group_by(scientificName) %>%
      summarize(
        checklist_id = first(id),
        worms_id     = first(worms_id),
        valid_id     = first(valid_id),
        parent_id    = first(parent_id),
        rank_name    = first(rank_name),
        tauthor      = first(tauthor),
        phylum       = first(phylum),
        class        = first(class),
        order        = first(order),
        family       = first(family),
        genus        = first(genus),
        species      = first(species),
        redlist      = first(redlist),
        status       = first(status)) %>%
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
    eez = eez_all %>%
      filter(territory1==ter)
    obis_sf = obis_sf %>%
      slice(
        st_intersects(
          eez,
          obis_sf)[[1]])
    
    # write obis_geo with just id to save disk space, for later rejoining with obis_tbl
    if (sum(duplicated(obis_tbl$occurrence_id)) > 0) stop('Whoah! Expecting all unique ids in OBIS occurrence table.')
    obis_sf %>%
      select(occurrence_id) %>%
      write_sf(obis_geo, delete_dsn=T)
    
    # write obis_tbl to csv
    obis_sf %>%
      st_set_geometry(NULL) %>%
      write_csv(obis_csv)
  }
  
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
  
  # fetch-wdpa ----
  wdpa_geo = sprintf('%s/eez-%s_wdpa.geojson', dir_data, ter_f)
  
  eez_sf = eez_all %>%
    filter(territory1==ter)
  
  if (!file.exists(wdpa_geo)){
    bb = st_bbox(eez_sf)
    
    q_url = 'http://ec2-54-204-216-109.compute-1.amazonaws.com:6080/arcgis/rest/services/wdpa/wdpa/MapServer/1/query'
    res = httr::GET(q_url, query = list(
      f            = 'json',
      geometry     = sprintf('%0.1f,%0.1f,%0.1f,%0.1f', bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]),
      geometryType = 'esriGeometryEnvelope',
      inSR         = 4326,
      outSR        = 4326,
      outFields    = '*'))
    
    wdpa_sf = httr::content(res) %>%
      read_sf() # wdpa0_sf = wdpa_sf; mapview(wdpa0_sf)
    
    if (nrow(wdpa_sf) == 0){
      warning('no wdpa')
      write_sf(wdpa_sf, wdpa_geo, delete_dsn=T)
    } else {
      
      # filter by eez's within eez
      wdpa_sf = wdpa_sf %>%
        slice(st_intersects(eez_sf, wdpa_sf)[[1]])
      
      # calculate original area before extracting to eez
      wdpa_sf = wdpa_sf %>%
        mutate(
          area_wdpa_orig_km2 = st_area(st_geometry(wdpa_sf)))
      
      # extract intersection with eez
      wdpa_sf = wdpa_sf %>%
        st_intersection(eez_sf %>% select())
      
      # calculate area after extracting to eez
      wdpa_sf = wdpa_sf %>%
        mutate(
          area_wdpa_eez_km2 = st_area(st_geometry(wdpa_sf)))
      
      write_sf(wdpa_sf, wdpa_geo, delete_dsn=T)
    }
  }
  wdpa_sf = read_sf(wdpa_geo)
  
  if (nrow(wdpa_sf) == 0){
    warning('no wdpa')
  } else {  
    labels <- with(
      wdpa_sf,
      sprintf(
        "<strong>%s</strong><br/>DESIG_ENG: %s<br/>DESIG_TYPE: %s<br/>IUCN CAT: %s<br/>GIS_M_AREA: %s",
        NAME, DESIG_ENG, DESIG_TYPE, IUCN_CAT, comma(round(GIS_M_AREA,2)))) %>% lapply(HTML)
    
    leaflet(eez_sf) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons() %>%
      addPolygons(
        data = wdpa_sf,
        weight = 1,
        color = 'green', fillColor = 'green',
        label = labels,
        highlight = highlightOptions(
          color = 'yellow',
          weight = 5,
          bringToFront = T))
  }

  # join-obis-wdpa ----
  obis_wdpa_csv = sprintf('%s/eez-%s_obis_wdpa.csv', dir_data, ter_f)
  
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
  
  # obis_wdpa %>%
  #   head()
  
  # calc-metric ----
  
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
  
  weighted.mean(
    x = d_dif$status_dif, 
    w = setNames(
      wts_taxa$wt,
      wts_taxa$taxonomicgroup)[
        d_dif$taxonomicgroup])
}
