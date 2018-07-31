#!/bin/bash
#
# Post-startup config for postgis docker container based on
# [kartoza/docker-geoserver]( https://github.com/kartoza/docker-postgis ).
#
# Annotated notes here: [sdg14#4](https://github.com/marinebon/sdg14/issues/4).
#
# !!! NOTE !!! : these are more like notes and less like an actual runnable script
#
# startup of this container looks something like:
# ------------------------------
# docker run --name "postgis" \
#  --restart unless-stopped \
#  -p 5432:5432 \
#  -v /mbon:/mbon \
#  -v /mnt/mbon-supplement:/mbon-local \
#  -v /mnt/mbon-supplement/postgresql:/var/lib/postgresql \
#  -d -t kartoza/postgis

# copy postgis config allocating more RAM
docker exec postgis cp -f /mbon-local/postgresql.conf /etc/postgresql/9.5/.

# restart postgres to load config changes
docker exec postgis service postgresql restart

# install postgresql studio plugin
docker exec geoserver bash -c '\
  cd /usr/local/tomcat/webapps; \
  wget http://downloads.postgresqlstudio.org/2.0/pgstudio_2.0.zip; \
  unzip pgstudio_2.0.zip'
