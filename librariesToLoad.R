#Load libraries ----------------------------------------------------------
library(DT)
library(RCurl)
library(rhandsontable)
library(rjson)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(lubridate)
library(tools)
library(httr)
library(tidytext)
library(stringdist)
library(feather)
data(stop_words)

loadLibraries <- "# Load Libraries
library(tidyverse)"

eval(parse(text = loadLibraries))