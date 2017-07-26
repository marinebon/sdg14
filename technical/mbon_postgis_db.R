library(tidyverse)
library(RPostgreSQL)
library(sf)
library(mapview)
library(RColorBrewer)

db = list(
  host = read_delim('/etc/hosts', '\t', col_names=c('ip','host')) %>%
    separate(host, c('name','image'), extra='merge', fill='left') %>%
    filter(name=='postgis') %>%
    .$ip,
  port = 5432,
  user = 'mbon',
  pass = readLines('/mbon/.pg_pass_mbon'),
  name = 'mbon')
db$dsn=sprintf(
  "PG:dbname=%s host=%s port=%s user=%s password=%s", 
  db$name, db$host, db$port, db$user, db$pass)

con = dbConnect(
  dbDriver('PostgreSQL'), 
  dbname=db$name, 
  user=db$user, password=db$pass, host=db$host, port=db$port)

#dbListTables(con)
#dbExistsTable(con, "obis_occ")

#dbGetQuery(con, 'select * from geometry_columns')
#dbGetQuery(con, 'select * from geography_columns')