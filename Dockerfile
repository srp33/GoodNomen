FROM rocker/shiny-verse:3.6.1

# Prepare the Shiny server and install necessary R packages.
RUN rm -rf /srv/shiny-server/* \
 && mkdir /srv/shiny-server/GoodNomen \
 && R -e "install.packages(c('DT', 'RCurl', 'rhandsontable', 'rjson', 'RJSONIO', 'R.utils', 'shinyBS', 'shinycssloaders', 'shinyjs', 'tools', 'writexl'), repos='https://cloud.r-project.org')"

COPY ./app.R /srv/shiny-server/GoodNomen/
COPY ./www/ /srv/shiny-server/GoodNomen/www/

USER shiny
