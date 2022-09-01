FROM rocker/shiny:4.1.0
LABEL maintainer eric.burgueno@plantandfood.co.nz

COPY ./requirements.txt /tmp
RUN apt-get update && \
  apt-get -y install libgdal26 libgdal-dev libpq-dev && \
  install2.r -e -n $(nproc) -r 'http://cloud.r-project.org/' $(cat /tmp/requirements.txt) && \
  rm -rf /srv/shiny-server/* /tmp/requirements.txt && \
  apt-get -y remove libgdal-dev libpq-dev && \
  apt-get -y autoremove

COPY ./app/* /srv/shiny-server/

