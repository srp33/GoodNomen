FROM rocker/shiny-verse:3.6.1

# Install necessary R packages and prepare Shiny server dir.
RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
    lbzip2 \
    libv8-dev \
  && install2.r --error --deps TRUE \
    DT \
    RCurl \
    readxl \
    rhandsontable \
    rjson \
    RJSONIO \
    R.utils \
    shinyBS \
    shinycssloaders \
    shinyjs \
    shinyWidgets \
    writexl \
  && rm -rf /srv/shiny-server/* \
  && mkdir /srv/shiny-server/GoodNomen

COPY ./app.R /srv/shiny-server/GoodNomen/
COPY ./www/ /srv/shiny-server/GoodNomen/www/

USER shiny
