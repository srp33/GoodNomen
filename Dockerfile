FROM rocker/shiny-verse:4.0.3

# Install necessary R packages and prepare Shiny server dir.
RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
    lbzip2 \
    libv8-dev \
    r-cran-rjava \
  && install2.r --error --deps TRUE \
    DT \
    feather \
    future \
    httr \
    lubridate \
    promises \
    RCurl \
    readxl \
    rhandsontable \
    rjson \
    RJSONIO \
    R.utils \
    shinyBS \
    shinybusy \
    shinycssloaders \
    shinyjs \
    shinyWidgets \
    stringdist \
    tidytext \
    writexl \
  && rm -rf /srv/shiny-server/*

COPY *.R /srv/shiny-server/GoodNomen/
COPY ./www/ /srv/shiny-server/GoodNomen/www/
COPY BioPortalApiKey.txt /srv/shiny-server/GoodNomen/

#USER shiny
