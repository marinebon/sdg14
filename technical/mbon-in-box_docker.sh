# For background, see:
#  marinebon/sdg14#4: load server software, a la MBON in a Box, using docker, RStudio Server, Shiny...
#  https://github.com/marinebon/sdg14/issues/4

# configure docker to start on boot
sudo systemctl enable docker

# postgis
docker run --name "postgis" \
  --restart unless-stopped \
  -p 5432:5432 \
  -v /mbon:/mbon \
  -v /mbon-postgres:/var/lib/postgresql \
  -d -t kartoza/postgis

# geoserver
docker run --name "geoserver" \
  --restart unless-stopped \
  -p 8080:8080 \
  --link postgis:postgis \
  -v /mbon:/mbon \
  -v /mbon/geoserver:/opt/geoserver/data_dir \
  -d -t kartoza/geoserver

# pgstudio
docker exec geoserver bash -c '\
  cd /usr/local/tomcat/webapps; \
  wget http://downloads.postgresqlstudio.org/2.0/pgstudio_2.0.zip; \
  unzip pgstudio_2.0.zip'

# rstudio & shiny
docker run --name "rstudio-shiny" \
  --restart unless-stopped \
  -p 8787:8787 -p 3838:3838 \
  --link postgis:postgis \
  --link geoserver:geoserver \
  -v /mbon:/mbon \
  -v /mbon/shiny:/srv/shiny-server \
  -v /mbon/shiny-log:/var/log/shiny-server \
  -e ROOT=TRUE \
  -e USER=mbon -e PASSWORD=`cat ~/.mbon_passwd` \
  -d -t bdbest/rstudio-shiny:R-3.4-geospatial

# www
docker run --name "www" \
  --restart unless-stopped \
  -p 80:80 \
  --link postgis:postgis \
  --link geoserver:geoserver \
  --link rstudio-shiny:rstudio-shiny \
  -v /mbon:/mbon:ro \
  -v /mbon/www:/usr/share/nginx/html:ro \
  -v /mbon/www-conf:/etc/nginx:ro \
  -d -t nginx