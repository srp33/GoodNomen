FROM rocker/shiny-verse:3.6.1

# Install necessary R packages and prepare Shiny server dir.
RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
    lbzip2 \
    libv8-dev \
  && install2.r --error --deps TRUE \
    DT \
    RCurl \
    rhandsontable \
    rjson \
    RJSONIO \
    R.utils \
    shinyBS \
    shinycssloaders \
    shinyjs \
    shinyWidgets \
    writexl \
  && rm -rf /srv/shiny-server/*

COPY app.R /srv/shiny-server/
COPY ./www/ /srv/shiny-server/www/

USER shiny
