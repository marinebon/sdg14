library(tidyverse)
library(RPostgreSQL)
library(sf)
library(leaflet)
library(RColorBrewer)

db = switch(
  Sys.info()[['sysname']],
  'Linux' = list(
    # host = read_delim('/etc/hosts', '\t', col_names=c('ip','host')) %>%
    #   separate(host, c('name','image'), extra='merge', fill='left') %>%
    #   filter(name=='postgis') %>%
    #   .$ip,
    # since docker rm --link /rstudio-shiny/postgis,
    #   docker inspect postgis | grep IPAddress
    host = '172.17.0.2',
    port = 5432,
    user = 'docker',
    pass = readLines('/mbon/.pgsql_pass_docker'),
    name = 'mbon_gis'),
  'Darwin' = list(
    host = '172.17.0.2',
    port = 5432,
    user = 'docker',
    pass = readLines('/mbon/.pgsql_pass_docker'),
    name = 'docker'))
db$dsn=sprintf(
  "PG:dbname=%s host=%s port=%s user=%s password=%s",
  db$name, db$host, db$port, db$user, db$pass)

dbDisconnect(con)
con = dbConnect(
  #dbDriver('PostgreSQL'),
  PostgreSQL(),
  dbname=db$name,
  user=db$user, password=db$pass, host=db$host, port=db$port)

dbGetQuery(con, "SELECT postgis_full_version();")
# POSTGIS="2.3.2 r15302" GEOS="3.4.2-CAPI-1.8.2 r3921" PROJ="Rel. 4.8.0, 6 March 2012" GDAL="GDAL 1.10.1, released 2013/08/26" LIBXML="2.9.1" LIBJSON="0.11.99" RASTER

qry = function(sql){
  r = dbSendStatement(con, sql)
  cat(sprintf('dbGetRowsAffected(): %d\n', dbGetRowsAffected(r)))
  dbClearResult(r)
}

# o = st_read_db(con, 'o')
# eez = st_read_db(con, query="SELECT * FROM eez WHERE pol_type='200NM'")

# obis table ----
# update obis table after loading from obis.csv (download.iobis.org) using pgfutter
qry("
    \timing
    ALTER TABLE obis ADD COLUMN lon FLOAT;
    ALTER TABLE obis ADD COLUMN lat FLOAT;
    VACUUM;
    UPDATE obis SET lon = CAST(decimallongitude AS FLOAT);
    UPDATE obis SET lat = CAST(decimallatitude AS FLOAT);
    ALTER TABLE obis ADD COLUMN geom geometry(POINT,4326);
    UPDATE obis SET geom = ST_SetSRID(ST_MakePoint(lon, lat), 4326);
    CREATE INDEX idx_obis_geom ON obis USING gist(geom);
    ALTER TABLE obis ADD COLUMN pkey SERIAL PRIMARY KEY;

ALTER TABLE obis ADD COLUMN depth_int INTEGER;
UPDATE obis SET depth_int = CASE WHEN depth='' THEN NULL else depth::numeric::integer END;
ALTER TABLE obis ADD COLUMN year_int INTEGER;
UPDATE obis SET year_int = CASE WHEN year='' THEN NULL else year::numeric::integer END;
CREATE INDEX CONCURRENTLY idx_obis_yr_d ON obis(year, depth);
CREATE INDEX CONCURRENTLY idx_obis_eez ON obis(eez_mrgid);
")

flds_keep = c('id','lon','lat','depth','eventDate','year','scientificName','worms_id','taxonomicgroup','kingdom','phylum','class','taxa_order','family','genus','species','qc','geom','rl_status','rl_weight','eez_mrgid')
flds_drop = setdiff(dbListFields(con, 'obis'), flds_keep)

qry(paste("ALTER TABLE obis", paste(sprintf('DROP COLUMN %s', flds_drop), collapse=', ')))
qry("CREATE INDEX CONCURRENTLY idx_taxa ON obis(taxonomicgroup, kingdom, phylum, class, taxa_order, family, genus, species);

    CREATE INDEX ON obis (taxonomicgroup);
    CREATE INDEX ON obis (kingdom);
    CREATE INDEX ON obis (phylum);
    CREATE INDEX ON obis (class);
    CREATE INDEX ON obis (taxa_order);
    CREATE INDEX ON obis (family);
    CREATE INDEX ON obis (genus);
    CREATE INDEX ON obis (species);
    ")
dbGetQuery(con, "SELECT COUNT(*) FROM obis;")

# obis redlist ----

# weights: http://www.iucnredlist.org/static/categories_criteria_2_3
wts_status = c(LC=0, `LR/lc`=0.05, `LR/cd`=0.1, NT=0.2, `LR/nt`=0.3, VU=0.4, EN=0.6, CR=0.8, EW=0.9, EX=1)

# load from robis::checklist()
cklist = read_csv('/mbon-local/postgresql/sqlite/obis_checklist_global.csv') %>%
  filter(!is.na(status)) %>%
  group_by(worms_id) %>%
  summarise(
    status = first(status)) %>%
  mutate(
    status_wt = wts_status[status])
#table(cklist[c('status','status_wt')])
dbWriteTable(con, 'obis_redlist', cklist, overwrite=T)
#dbDataType(con, cklist$status_wt)
#dbListFields(con, 'obis_redlist')

# join to obis
qry("ALTER TABLE obis ADD COLUMN rl_status TEXT, ADD COLUMN rl_weight float8;")
qry("UPDATE obis o SET rl_status = r.status, rl_weight=r.status_wt FROM obis_redlist r WHERE o.worms_id = r.worms_id;")

# obis eez ----
qry("ALTER TABLE obis ADD COLUMN eez_mrgid NUMERIC;")
# TODO: pol_type IN ('Joint regime','Disputed')
qry("UPDATE obis o SET eez_mrgid = e.mrgid FROM (SELECT mrgid, geom FROM eez WHERE pol_type='200NM') e WHERE ST_Within(o.geom, e.geom);")

qry("ALTER TABLE obis ADD COLUMN hex_gid INTEGER; UPDATE obis o SET hex_gid = e.mrgid FROM hex_287 h WHERE ST_Within(h.geom, e.geom);")


eez_smp = st_read_db(con, 'eez_s005')
names(eez_smp)
eez_smp %>%
  filter(pol_type == '200NM') %>%
  select(mrgid, geoname, territory1) %>%
  arrange(territory1, geoname) %>%
  st_set_geometry(NULL) %>%
  View()
# Russia: 5690
# Canada: 8493

qry("

SELECT o.pkey, e.mrgid AS eez_mrgid
INTO obis_russia
FROM obis AS o
JOIN eez_s005 AS e ON ST_Within(o.geom, e.geom)
WHERE e.pol_type='200NM' AND e.mrgid=5690;

SELECT o.pkey, e.mrgid AS eez_mrgid
INTO obis_canada
FROM obis AS o
JOIN eez_s005 AS e ON ST_Within(o.geom, e.geom)
WHERE e.pol_type='200NM' AND e.mrgid=8493;
    
SELECT o.pkey, e.mrgid AS eez_mrgid
INTO obis_alaska
FROM obis AS o
JOIN eez_s005 AS e ON ST_Within(o.geom, e.geom)
WHERE e.pol_type='200NM' AND e.mrgid=8463;
    
SELECT o.pkey, e.mrgid AS eez_mrgid
INTO obis_rest
FROM obis AS o
JOIN eez_s005 AS e ON ST_Within(o.geom, e.geom)
WHERE e.pol_type='200NM' AND e.mrgid NOT IN (5690, 8463, 8493);

UPDATE obis o SET eez_mrgid = e.mrgid FROM (SELECT mrgid, geom FROM eez_s005 WHERE pol_type='200NM') e WHERE ST_Within(o.geom, e.geom);")

sql = sprintf('SELECT COUNT(worms_id) AS n, COUNT(DISTINCT(worms_id)) n_spp FROM obis;')

# obis wdpa ----
qry("ALTER TABLE obis ADD COLUMN in_wdpa BOOLEAN;")
qry("UPDATE obis AS o SET in_wdpa = TRUE FROM wdpa AS w WHERE ST_Within(o.geom, w.wkb_geometry);")


# TODO: pool ----
library(pool)
library(dplyr)

my_db <- dbPool(
  
  RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)

group_by(
  taxonomicgroup, in_mpa) %>% # View() # table(d$taxonomicgroup, useNA='ifany') ; table(d$in_mpa, useNA='ifany')
  summarise(
    status_wt         = mean(status_wt), # NOTE: weighted by number of occurrences
    n_scientificNames = length(unique(scientificName)),
    n_occurrences     = n())


o %>%
  st_set_geometry(NULL) %>%
  left_join(
    eez %>%
      st_set_geometry(NULL) %>%
      select(mrgid, territory1),
    by=c('eez_mrgid'='mrgid')) %>%
  group_by(territory1) %>%
  summarize(
    n = n())


# OLD: occurrence table cleanup ----

db <- dbConnect(RSQLite::SQLite(), '/mbon-local/postgresql/sqlite/obis_test.sqlite')
dbListTables(db)

occurrence = dbGetQuery(db, 'SELECT * FROM occurrence LIMIT 5')
o = dbReadTable(db, 'o') %>% as_tibble()
o
# tibble(
#   fld0 = names(occurrence) %>% sort(),
#   fld1 = str_replace_all(fld0, '`', ''),
#   keep = F) %>%
#     write_csv('obis_sqlite_fldnames.csv')
flds = read_csv('obis_sqlite_fldnames.csv') %>%
  arrange(fld1) %>%
  filter(keep)
paste(flds$fld1, collapse=', ')


#sql = paste("DROP TABLE tmp; CREATE TABLE tmp AS SELECT ", paste(with(flds, sprintf('``%s`` AS %s', fld0, fld1)), collapse=','), "FROM occurrence;")
r = dbSendStatement(db, 'DROP TABLE o;')
dbClearResult(r)
dbListTables(db)

sql = paste("CREATE TABLE o AS SELECT", paste(with(flds, sprintf('``%s`` AS %s', fld0, fld1)), collapse=','), "FROM occurrence;")
r = dbSendStatement(db, sql)
dbClearResult(r)
dbDisconnect(db)
# tmp = dbReadTable(db, 'o')
# tmp %>%
#   summarise_each(funs(100*mean(is.na(.)) %>% round(3))) %>% sort()
# tmp %>%
#   as_tibble()
# tmp %>%
#   View()
