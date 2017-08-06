
CREATE DATABASE mbon;
\connect mbon;
-- Enable PostGIS (includes raster)
CREATE EXTENSION postgis;
-- Enable Topology
CREATE EXTENSION postgis_topology;

DROP TABLE IF EXISTS obis_occ CASCADE;
CREATE TABLE obis_occ (
  "id" int,
  "decimalLongitude" float,
  "decimalLatitude" float,
  "institutionCode" varchar,
  "datasetName" varchar,
  "phylum" varchar,
  "class" varchar,
  "order" varchar,
  "family" varchar,
  "genus" varchar,
  "species" varchar,
  "scientificName" varchar,
  "aphiaID" int,
  "obisID" int,
  "resourceID" int,
  "year" int );

-- copy from csv
\copy obis_occ FROM '/mbon/data_big/technical/obis/obis_occ_head10k.csv' WITH DELIMITER ',' CSV HEADER;
-- \copy obis_occ FROM '/mbon/data_big/technical/obis/obis_occ.csv' WITH DELIMITER ',' CSV HEADER NULL AS 'NA';

-- add primary key
ALTER TABLE obis_occ ADD COLUMN gid serial PRIMARY KEY;

-- Add point geometry column to table
ALTER TABLE obis_occ ADD COLUMN geom geometry(Point,4326);

-- Populate column with point geometries
update obis_occ set geom = ST_SetSRID(ST_MakePoint( "decimalLongitude", "decimalLatitude" ), 4326);

-- Create a spatial index on points
CREATE INDEX idx_obis_occ_geom ON obis_occ USING GIST(geom);

