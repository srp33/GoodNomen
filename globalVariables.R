
# "masterText" collects text for an Rscript that will replicate commands executed by Good Nomen
# Instances where masterText is edited are marked with "# ADD TEXT TO SCRIPT" followed by a description of what is being added
masterText <- NULL

eval(parse(text = loadLibraries))

# Set file upload limit to 50 MB
options(shiny.maxRequestSize = 50*1024^2, htmlwidgets.TOJSON_ARGS = list(na = 'string'))

# Global path variables -----------------------------------------------------------------

TEMP_DIR_PATH <- ""

# This will be true if the app is executed inside a Docker container.
if (dir.exists("/home/shiny"))
  TEMP_DIR_PATH <- "/tmp/"

API_KEY_FILE_PATH <- "BioPortalApiKey.txt"
ONTOLOGY_LIST_FILE_PATH <- paste0(TEMP_DIR_PATH, "OntologyList.txt")

# Global functions and Definitions --------------------------------------------------------

sURL <- NULL
readInputFileText <- NULL
API_KEY <- readChar(API_KEY_FILE_PATH, nchars = 36) # This gets the apikey from a txt file
DAYS_SINCE_DOWNLOAD <- 7
NUM_SAMPLE_ROWS <- 3 # Number of sample rows to send to Bioportal to get recommended ontologies. The larger it is, the slower the code will run
NUM_REC_ONTO <- 3 # Number of recommended ontologies to display to the user
NUM_REC_MANUAL <- 5 # Number of manual term recommendation to display to the user
MAX_HEADERS <- 5 # Make number of header rows uploaded data can have
NUM_TEST_TIMES <- 2 # If the URL doesn't work, test it again this many times.
SPINNER_TYPE <- 8 # Any number between 1 and 8. 8 is the circle spinner. (To see the different spinner options, go to https://projects.lukehaas.me/css-loaders/)
TIMEOUT_TIME <- 120 # Seconds

listOfLibrariesUsed <- c("DT", "RCurl", "rhandsontable", "rjson", "shiny", "shinyBS", "shinycssloaders", "shinyjs",
                         "tidyverse", "tools", "writexl")

# Define function for tooltips 
helpButton <- function(message = "content", placement = "right") {
  return(tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover"))
}

addLibrary <- function(librariesList) {
  installPackages <- ""
  for (libName in librariesList) {
    installPackages <- paste0(installPackages, "\nif (!suppressWarnings(require(", libName, ", quietly = TRUE))) {", 
                              '  install.packages("', libName, '")',"}")
  }
  return(installPackages)
  
}

# Define accepted file types and the read_ functions used to load them
extensionsMap <- c(".txt" = "tsv", ".tsv" = "tsv", ".csv" = "csv", ".xls" = "excel", ".xlsx" = "excel")

# Define function for collapsing a list with proper grammar
collapseText <- function(inputList) {
  lastIndex <- length(inputList)
  paste(paste(inputList[-1 * lastIndex], collapse = ", "), inputList[lastIndex], sep = ", and ")
}

getRecommendedTerms <- function(dataSet) {# Get a list of terms to standardize
  sampleRows  <- sample_n(dataSet, min(NUM_SAMPLE_ROWS, nrow(dataSet)))
  rowChar <- toString(unlist(unique(unlist(sampleRows, use.names = FALSE)))) # Change sample table to one string
  rowChar <- URLencode(rowChar, reserved = TRUE) #Why encode? Characters in a URL other than the English alphanumeric characters and - _ . ~ should be encoded as % plus a two-digit hexadecimal representation, and any single-byte character can be so encoded. The standard refers to this as 'percent-encoding'.
  rURL <- "http://data.bioontology.org/recommender?"
  response <- POST(rURL, body = list(input = rowChar, apikey = API_KEY, display_links = "false", display_context = "false"))
  return(response)
}

timeOutError <- function() {
  message("Timeout. Skipping.")
  showModal(modalDialog(title = "TimeOut Error",
                        p("The internet took too long to access and timed out. Try accessing BioPortal on a browser and see if it's working. If so, try running this app again."),
                        footer = modalButton("Dismiss"), easyClose = F))
}

autoMatchModule <- function(current, standard, booleanValue){
  ns <- NS(standard)
  tagList(
    fluidRow(
      column(width = 2, p(current, style = "padding:9px")),
      column(width = 3, p(standard, style = "padding:9px")),
      column(width = 2, checkboxInput(ns("checkBox"), value = booleanValue, label = NULL), style = "height:9px;")
    )
  )
}

