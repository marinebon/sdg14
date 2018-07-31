#!/bin/bash
#
# Post-startup config for geoserver docker container based on
# [kartoza/docker-geoserver]( https://github.com/kartoza/docker-geoserver ).
#
# Annotated notes here: [sdg14#4](https://github.com/marinebon/sdg14/issues/4).
#
# !!! NOTE !!! : these are more like notes and less like an actual runnable script
#
# startup of this container looks something like:
# ------------------------------
# docker run --name "geoserver" \
#  --restart unless-stopped \
#  -p 8080:8080 \
#  --link postgis:postgis \
#  -v /mbon:/mbon \
#  -v /mnt/mbon-supplement:/mbon-local \
#  -v /mnt/mbon-supplement/geoserver:/opt/geoserver/data_dir \
#  -d -t kartoza/geoserver

docker exec geoserver bash -c '\
  cd /usr/local/tomcat/webapps; \
  wget http://downloads.postgresqlstudio.org/2.0/pgstudio_2.0.zip; \
  unzip pgstudio_2.0.zip'

# GeoServer: Install Map Vector Tiles
# -----------------------------------
# Installing the Vector Tiles Extension — GeoServer 2.13.x User Manual
# http://docs.geoserver.org/latest/en/user/extensions/vectortiles/install.html
cd /usr/local/tomcat/webapps/geoserver/WEB-INF/lib
wget https://downloads.sourceforge.net/project/geoserver/GeoServer/2.12.0/extensions/geoserver-2.12.0-vectortiles-plugin.zip
unzip geoserver-2.12.0-vectortiles-plugin.zip

# Enable CORS in web.xml
# ----------------------
cd /usr/local/tomcat/webapps/geoserver/WEB-INF
apt-get install vim # install vi
vi web.xml

# Uncomment following sections in web.xml:
# <!-- Uncomment following filter to enable CORS -->
#  <filter>
#    <filter-name>cross-origin</filter-name>
#    <filter-class>org.eclipse.jetty.servlets.CrossOriginFilter</filter-class>
#  </filter>
#
#  <!-- Uncomment following filter to enable CORS -->
#  <filter-mapping>
#    <filter-name>cross-origin</filter-name>
#    <url-pattern>/*</url-pattern>
#  </filter-mapping>

# =============
# from https://github.com/marinebon/sdg14/blob/0960584fd7a8334ddf58be812446acdbbc87588e/satellite/satellite_prep.R
# =============
# TODO: create dir_out parameter files for GeoServer
# * [Using the ImageMosaic plugin for raster time-series data — GeoServer 2.12.x User Manual](http://docs.geoserver.org/stable/en/user/tutorials/imagemosaic_timeseries/imagemosaic_timeseries.html)
# * [Tile Caching with GeoWebCache — GeoServer Training](http://geoserver.geo-solutions.it/edu/en/enterprise/gwc.html)
