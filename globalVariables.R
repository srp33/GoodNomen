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

API_KEY <- readChar(API_KEY_FILE_PATH, nchars = 36) # This gets the apikey from a txt file
DAYS_SINCE_DOWNLOAD <- 30
NUM_SAMPLES_FROM_COLS <- 10 # Number of samples from each column to send to Bioportal to get recommended ontologies. The larger it is, the slower the code will run
MAX_TEST_VALUES <- 10000 # Max number of sample values to send to recommender
NUM_REC_ONTO <- 3 # Number of recommended ontologies to display to the user
NUM_REC_MANUAL <- 5 # Number of manual term recommendation to display to the user
MAX_HEADERS <- 5 # Max number of header rows uploaded data can have
NUM_TEST_TIMES <- 2 # If the URL doesn't work, test it again this many times
SPINNER_TYPE <- 8 # Any number between 1 and 8. 8 is the circle spinner. (To see the different spinner options, go to https://projects.lukehaas.me/css-loaders/)
TIMEOUT_TIME <- 300 # Seconds
LEFT_COLUMN_WIDTH <- 8 # Number between 1 and 12
RIGHT_COLUMN_WIDTH <- 4 # Number between 1 and 12 (left + right = 12)
AUTOMATCH_COLUMN_WIDTH <- 4 # Number between 1 and 12

listOfLibrariesUsed <- c("DT", "RCurl", "rhandsontable", "rjson", "shiny", "shinyBS", "shinycssloaders", "shinyjs",
                         "tidyverse", "tools", "writexl", "readxl", "httr", "tidytext", "dplyr", "readr", "tidyr", "stringdist", "feather")


