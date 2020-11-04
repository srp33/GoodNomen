#Load libraries ----------------------------------------------------------
loadLibraries <- "#Load Libraries
library(DT)
library(RCurl)
library(rhandsontable)
library(rjson)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(tidyverse)
library(tools)
library(writexl)
library(readxl)
library(httr)
library(tidytext) # https://www.tidytextmining.com/index.html
data(stop_words)
library(dplyr)
library(readr)
library(tidyr)
library(stringdist)"

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


# User Interface (UI) ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "Logo.png"),
    tags$style("body { word-wrap: break-word; }")
  ),
  includeScript("www/reactive_preferences.js"),
  useShinyjs(),
  navbarPage(title = "Good Nomen", id = 'tabs',
             # * Load Data ---------------------------------------------------------------
             tabPanel('Load Data', value = 'loadData', 
                      sidebarLayout(
                        sidebarPanel(tags$img(src = 'Logo.png', align = "right", height = "100px"),
                                     h4("Load Data"),
                                     p("Welcome to Good Nomen, an interface for mapping clinical data files based on standardized terminologies."),
                                     p(paste0(
                                       "Please upload a file containing patient data on each row and clinical variables in each column. ",
                                       "Accepted file types include ", collapseText(names(extensionsMap)), ".")
                                     ), 
                                     fileInput(inputId = "file1", label = "Choose Input File:", 
                                               multiple = FALSE, accept = names(extensionsMap), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                                     textOutput("inputError"), tags$head(tags$style("#inputError {color: red;}")),
                                     uiOutput("headerSelector"),
                                     uiOutput("colnamesSelector"), br(), br(),
                                     uiOutput("ontologySelector"),
                                     textOutput("error"), tags$head(tags$style("#error {color: red;}")), hr(),
                                     conditionalPanel(condition = "input.whichTerminology == 'Upload a terminology'",
                                                      uiOutput("uploadTerminologyButton")),
                                     uiOutput("page1Next")), 
                        # Data Preview 
                        mainPanel(
                          tags$em(textOutput("loadDataPreviewText")),
                          wellPanel(uiOutput("loadDataColNav"), shinycssloaders::withSpinner(DTOutput("uploadPreview"), color = "#112446"))
                        ))),
             
             # * Edit Data (Auto/Manual) ---------------------------------------------------------------
             tabPanel('Edit Data', value = 'editTable', 
                      sidebarPanel(width = 4,
                                   tags$img(src = 'Logo.png', align = "right", height = "100px"),
                                   h4("Edit Data"),
                                   p(
                                     paste(
                                       "Data may be standardized automatically or manually.",
                                       "First select the name of the column containing the data you wish to edit.",
                                       "If you would like to automate the matching process, press \"Auto-match.\"",
                                       "The data will be processed and then a pop-up window will appear and ask you to review the matches.",
                                       "If you would like to manually update the data, press \"Standardize Manually.\"",
                                       "A different pop-up window will appear with instructions on how to edit the data.",
                                       "When finished, press \"Next.\""
                                     )
                                   ), br(),
                                   htmlOutput("selectedOntology"),
                                   actionButton('changeOntology', label = div("Change Ontology", helpButton("Click to change which ontology you want to use")),
                                                style = "color: #fff; background-color: #6baed6; border-color: #6baed6;"), br(), br(),
                                   uiOutput("editThisColumnSelector"),
                                   conditionalPanel(condition = 'input.editThisColumn', 
                                                    tagList(
                                                      actionButton('automatch', label = div("Auto-match", helpButton("Matches will be found based on synonyms in the selected terminology."))),
                                                      actionButton('manual', label = div("Standardize Manually", helpButton("Update selected terms to manually chosen standardized term.")))
                                                    )),
                                   uiOutput("resetAndSave"), hr(),
                                   uiOutput("cancelChangeOntology"), hr(),
                                   div(
                                     actionButton('editBack', "Back", css.class = "back_button"),
                                     actionButton('editNext', "Next", css.class = "next_button"))
                      ),
                      mainPanel(
                        tags$em(textOutput("editDataPreviewText")),
                        wellPanel(uiOutput("editDataPreview"))
                      )
             ),
             
             # * Update Column Names -----------------------------------------------------
             tabPanel('Update Column Names', value = 'updateColumnNames',
                      sidebarPanel(width = 4,
                                   tags$img(src = 'Logo.png', align = "right", height = "100px"),
                                   h4("Update Column Names"),
                                   p(
                                     paste(
                                       "If desired, select a column to rename and a new column name. When a column to rename is selected,",
                                       "the new column name box will be autofilled with a suggested name if a matching term is found in the",
                                       "terminology. This term may be changed. Press \"Rename\" to update. Multiple columns may be renamed.",
                                       "When finished, press \"Next.\""
                                     )
                                   ), 
                                   uiOutput("editColumnSelector"),
                                   conditionalPanel(
                                     condition = 'input.editColumn',
                                     selectizeInput(
                                       'newColumn',
                                       label = "Select New Column Name:",
                                       choices = NULL,
                                       options = list(placeholder = 'Please select a column above...',
                                                      closeAfterSelect = TRUE)
                                     ),
                                     uiOutput("columnRenameButton")
                                   ), 
                                   bsModal(# Warning if user does not select column to rename and new column name
                                     'columnModal',
                                     title = "Error",
                                     trigger = 'input.newColumn',
                                     HTML(paste('<p color="black">You must select a column to rename and a new column name.", 
                                                "Please close this window and select these items.</p>')),
                                     tags$head(tags$style("#columnModal {color: red;}"))
                                   ), 
                                   bsModal(# Warning if user selects a new column name that is already being used as a column name
                                     'equalModal',
                                     title = "Error",
                                     trigger = 'input.newColumn',
                                     HTML(paste('<p color="black">The selected new column name is already being used as a column name.", 
                                                "Please close this window and select a different name.</p>')),
                                     tags$head(tags$style("#equalModal {color: red;}"))
                                   ), hr(), div(
                                     actionButton('columnBack', "Back", class = "back_button"),
                                     actionButton('columnSubmit', "Next", class = "next_button")
                                   )
                      ), 
                      # ** Data Preview 
                      mainPanel(
                        tags$em(textOutput("updateColNamesPreviewText")),
                        wellPanel(uiOutput("updateColNamesPreview"))
                      )
             ),
             
             # * Save Data ---------------------------------------------------------------
             tabPanel('Save Data', value = 'finalReport', 
                      sidebarPanel(
                        tags$img(src = 'Logo.png', align = "right", height = "100px"),
                        h4("Save Data"),
                        p("Enter a name for the output file and select an extension. Do not include the extension in the file name."),
                        uiOutput('outputFileNameUI'),
                        uiOutput('extensionSelector'),
                        uiOutput("downloadButtons"),
                        uiOutput("tab"), hr(),
                        actionButton('saveBack', "Back", class = "back_button")
                      ),
                      # ** Data Preview 
                      mainPanel(
                        tags$em(textOutput("saveDataPreviewText")),
                        uiOutput("saveDataColNav"),
                        wellPanel(shinycssloaders::withSpinner(DTOutput("saveDataPreview"), color = "#112446"))
                      )
             ),
             
             # * Contact -----------------------------------------------------------------
             tabPanel('Contact', value = 'contactPage',
                      fluidRow(
                        column(width = 2,
                               tags$img(src = 'Logo.png', height = "165px", align = "center")
                        ),
                        column(width = 9,
                               h4("Contact"),
                               HTML(paste('<div>For questions and comments, please visit', 
                                          '<a target="_blank", href="https://piccolo.byu.edu/Contact.aspx">https://piccolo.byu.edu/Contact.aspx</a>.',
                                          '<p>The source code for Good Nomen can be found at', 
                                          '<a target="_blank", href="https://github.com/srp33/GoodNomen">https://github.com/srp33/GoodNomen</a>.</p></div>'))
                        )
                      )
             )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) { 
  session$allowReconnect(TRUE)
  
  # Reactive Values
  values <- reactiveValues(datasetInput = NULL, dataset = NULL, extraHeaders = NULL, headerText = NULL,
                           extension = "", terminology = NULL, lastSelectedEditColumn = "", viewingSubset = c(1, 5),
                           matches = NULL, ontologyAcronym = "",
                           recommendedOntologies = NULL, listOfOntNames = NULL, ontName = "", TOTAL_TERM_LIST = NULL,
                           recTermsList = NULL, deselectedPushed = FALSE,
                           selectedPushed = FALSE, numTimesClicked = 0, synonyms = NULL, preferred = NULL)
  
  extension <- reactive({
    if (!is.null(input$file1)) {
      extSearch <- paste0(paste(str_replace(names(extensionsMap), "\\.", "\\\\."), collapse = "$|"), "$")
      ext <- str_extract(input$file1$datapath, extSearch)
    }
  })
  
  columns <- reactive({
    colnames(values$dataset)
  })
  
  dataPreview <- reactive({
    if (!is.null(values$dataset)) {
      datatable(
        values$dataset[, values$viewingSubset[1]:values$viewingSubset[2]], 
        rownames = FALSE, 
        options = list(dom = "tp", pageLength = 10,
                       columnDefs = list(list(
                         targets = "_all",
                         # Makes it so that the table will only display the first (colWidth()) chars.
                         # See https://rstudio.github.io/DT/options.html
                         # We want to display at least 30 chars.
                         render = JS(
                           paste0("function(data, type, row, meta) {",
                                  "return type === 'display' && typeof data === 'string' && data.length > ", max(floor(colWidth() / 2), 15), " ?",
                                  "'<span title=\"' + data + '\">' + data.substr(0,", max(floor(colWidth() / 2), 15), ") + '...</span>' : data;",
                                  "}"))
                       ))))
    }
  })
  
  lockedOntologyError <- function(){
    listOfOntNames <- readLines(ONTOLOGY_LIST_FILE_PATH)
    listOfOntNames <- listOfOntNames[!(listOfOntNames %in% values$ontName)]
    values$recommendedOntologies <<- values$recommendedOntologies[!(values$recommendedOntologies %in% values$ontName)]
    title <- "Error! Ontology locked for download!!"
    content <- tagList()
    content[[1]] <- p("Due to licensing restrictions, the ontology you selected to match terms with is locked for download from BioPortal. Please select a different ontology.")
    content[[2]] <- selectizeInput('newOntologySelector', label = "Select New Ontology:", 
                                   choices = list('Recommended Ontologies' = c("", values$recommendedOntologies),
                                                  'All Ontologies' = listOfOntNames),
                                   options = list(
                                     placeholder = "Please select an ontology...",
                                     closeAfterSelect = TRUE))
    content[[3]] <- actionButton('resetAndSave', label = "Save and Reset Ontology")
    showModal(
      modalDialog(
        content, title = title, footer = NULL, size = "l"
      )
    )
  }
  
  # Navigation button between pages of the app
  setColumnNavigation <- function(identifier) {
    if (!is.null(values$dataset)) {
      div(
        actionButton(paste0("backBtn_", identifier), icon("arrow-left"), class = "retract_view",
                     style = "margin-left: 20px;color: #fff;background-color: #2ca25f;border-color: #2ca25f"),
        actionButton(paste0("nextBtn_", identifier), icon("arrow-right"), class = "advance_view", 
                     style = "display: block;float:right;margin-right: 20px;color: #fff;background-color: #2ca25f;border-color: #2ca25f")
      )
    }
  }
  
  # The width of the columns, as determined by the width of the column names
  colWidth <- reactive({
    mean(nchar(colnames(values$dataset)))
  }) 
  
  # Makes sure that we view only the number of columns that will fit on the screen at any given time. A small screen fits about 75 
  # characters at a time. We don't want to display more than 5 columns at a time.
  moveBy <- reactive({
    min(max(floor(75 / colWidth()), 1), 5)
  })
  
  
  # The input argument is a vector of strings. These are terms (including synonyms)
  # from a controlled terminology (ontology). This function creates a "clean" version
  # of each term. The clean version is lowercase. "Stop words" are removed, as well
  # as some punctuation or extra whitespace.
  build_term_tibble = function(terms) {
    terms_df = tibble(term = terms) %>%
      unnest_tokens(word, term, drop=FALSE) %>%
      anti_join(stop_words) %>%
      dplyr::rename(originalTerm = term) %>%
      group_by(originalTerm) %>%
      summarize(cleanedTerm=paste(word, collapse=" ")) %>%
      ungroup() %>%
      group_by(cleanedTerm) %>%
      summarize(originalTerm = collapse_terms(originalTerm)) %>%
      ungroup()
    
    return(terms_df)
  }
  
  # When a cleaned term is the same for two original terms, 
  # collapse them as |-separated values.
  collapse_terms = function(terms) {
    paste0(unique(terms), collapse = "|")
  }
  
  # Finds matches between standardized terms and synonyms
  identifyMatches = function(originalTerms) {
    # Start a timer
    startTime = Sys.time()
    
    # The "withProgress" adds a progress bar. The functions "inc()" in this section increment the progress bar.
    withProgress(message = "Matching", {
       m <- 100
      
      incProgress(5/m, detail = "Cleaning terms")
    
    # Build a tibble with "clean" terms alongside the original terms.
    originalTerms = build_term_tibble(originalTerms)
    
    incProgress(5/m, detail = "Mapping to standardized terms")
    
    # Use the stringdist package to map the test (drug) terms to the ontology terms (including synonyms).
    sdm = stringdistmatrix(pull(values$TOTAL_TERM_LIST, cleanedTerm), pull(originalTerms, cleanedTerm), method = "jw", p = 0.1)
    
    incProgress(5/m, detail = "Organizing matches")
    
    # We get a matrix back. Convert it to a tibble and label columns descriptively.
    colnames(sdm) = pull(originalTerms, originalTerm)
    sdm = as_tibble(sdm)
    sdm = bind_cols(pull(values$TOTAL_TERM_LIST, originalTerm), sdm, .name_repair = "minimal")
    colnames(sdm)[1] = "OntologyTerm"
    
    incProgress(5/m, detail = "Tidying matches")
    
    # Create a tidy version of the results. It includes steps for un-collapsing terms that were combined in previous steps and just making the results easier to work with.
    sdm = pivot_longer(sdm, 2:ncol(sdm), names_to="TestTerm", values_to="Score") %>%
      group_by(TestTerm) %>%
      slice_min(n = 10, order_by = Score) %>%
      separate_rows(OntologyTerm, sep="\\|") %>%
      mutate(OntologyTerm = sapply(OntologyTerm, function(x) {
        index = which(values$synonyms == x)
        incProgress(1/m, detail = "Collecting matches")
        values$preferred[[index[1]]] })) %>%
      separate_rows(TestTerm, sep="\\|") %>%
      group_by(TestTerm, OntologyTerm) %>%
      summarize(Score = min(Score)) %>%
      select(TestTerm, OntologyTerm, Score) %>%
      arrange(TestTerm, Score)
    
    incProgress(5/m, detail = "Finishing")
    
    # Indicate how long the process took.
    duration = Sys.time() - startTime
    print("#############################")
    print(duration)
    print("#############################")
    sdm <- sdm[!duplicated(sdm$TestTerm),]
    
    })
    return(sdm)
  }
  
  observeEvent(input$advance_clicked, {
    start <- min(ncol(values$datasetInput), values$viewingSubset[1] + moveBy())
    end <- min(ncol(values$datasetInput), start + (moveBy() - 1))
    values$viewingSubset <- c(start, end)
    session$sendCustomMessage(type = "resetValue", "advance_clicked")
  }, ignoreNULL = TRUE)
  
  observeEvent(input$retract_clicked, {
    end <- max(1, values$viewingSubset[2] - moveBy())
    start <- max(1, end - (moveBy() - 1))
    values$viewingSubset <- c(start, end)
    session$sendCustomMessage(type = "resetValue", "retract_clicked")
  }, ignoreNULL = TRUE)
  
  ## * Load Data -----------------------------------------------------------------------------------------------------
  readInputFile <- function(inFile) {
    fileExt <- paste0(".", file_ext(gsub("\\\\", "/", inFile$datapath)))
    text <- paste0("# Please ensure that your terminology file (", inFile[1],") is in the same directory as this script before executing. Please also make sure that your R console is in the correct working terminal (use setwd() to change to the directory that your files are in).")
    installPackages <- addLibrary(listOfLibrariesUsed)
    readInputFileText <<- paste0("datasetInput <- read_", extensionsMap[[fileExt]], "('", inFile$name, "', col_names = FALSE)")
    
    # ADD TEXT TO SCRIPT for loading libraries
    masterText <<- NULL
    masterText <<- paste0(masterText,  installPackages, "\n\n", loadLibraries, "\n\n", text) 
    
    do.call(paste0("read_", extensionsMap[[fileExt]]), list(inFile$datapath, "col_names" = FALSE))
  }
  
  # After we upload the file, select the row we want to identify as the column names (and the number of header rows)
  setColNames <- function(startRow, colNameRow) {
    datasetInput <- values$datasetInput
    headerText <- paste0(
      "colNameRow <- ", colNameRow, "\n",
      "startRow <- ", startRow, "\n",
      "extraHeaders <- NULL\n\n",
      "if (startRow > 1) {\n",
        "\textraIndices <- 1:(startRow - 1)\n",
        "\textraIndices <- extraIndices[-colNameRow]\n",
        "\textraHeaders <- datasetInput[extraIndices,]\n",
      "}\n",
      "if (colNameRow == 0) {\n",
        "\tnewColsNames <- paste(\"Column\", 1:ncol(datasetInput), sep = \"_\")\n",
      "} else {\n",
        "\tnewColsNames <- datasetInput[",colNameRow,",]\n",
      "}\n",
      "colnames(datasetInput) <- newColsNames\n",
      "datasetInput <- datasetInput[",startRow,":nrow(datasetInput),]"
    )
    eval(parse(text = headerText))
    values$headerText <- headerText
    values$dataset <- datasetInput
    values$extraHeaders <- extraHeaders
    
    if (any(is.na(colnames(values$dataset)))) {
      colnames(values$dataset)[is.na(colnames(values$dataset))] <- "Null1"
    }
    values$viewingSubset <- c(1, min(moveBy(), ncol(values$datasetInput))) # This changes the UI view on the application
    return()
  }
  
  # Input File (header selector must only be set when user first uploads file; otherwise, if the user selects the number of header lines
  # before the table renders, the box flickers back and forth between selection and default)
  observeEvent(input$file1, ignoreInit = T, {
    withProgress(message = "Initializing Elements", {# Initialize variables so functionality is enabled and user can click between tabs without pushing "next"
      output$inputError <- tryCatch({
        values$datasetInput <<- readInputFile(input$file1)
        if (any(is.na(colnames(values$datasetInput)))) {
          colnames(values$datasetInput)[is.na(colnames(values$datasetInput))] <- "Null2"
        }
        renderText("")
      }, error = function(e) {
        renderText("An error has been detected. Please verify that the file type matches the extension.")
      })
      output$headerSelector <- renderUI({
        selectInput(
          "header",
          label = div(
            "Select Number of Header Lines:",
            helpButton(
              paste0("Select the number of header lines in the input file. ",
                     "Header lines will be ignored during the editing process.")
            )),
          choices = (0:min(MAX_HEADERS, nrow(values$datasetInput))), selected  = "1"
        )
      })
      
      incProgress(1/9, detail = "header selector")
      incProgress(1/9, detail = "terminology selector")
      incProgress(1/9, detail = "column selector")
      
      output$outputFileNameUI <- renderUI({
        textInput('outputFileName',
                  label = div(
                    "Output File Name (without extension):",
                    helpButton("Enter a name for the output file (do not include extension).")
                  ),
                  value = paste0(
                    substr(input$file1, 0, (nchar(input$file1) - if (extension() == ".xlsx") 5 else 4)), "_standardized"
                  )[1])
      })
      incProgress(1/9, detail = "file name selector")
      output$extensionSelector <- renderUI({
        selectExt <- if (grepl("xls", extension())) ".xlsx" else extension()
        selectInput('extension', label = div("Select Extension:", helpButton("Select an extension for the output file.")), choices = setdiff(names(extensionsMap), c(".xls", ".txt")), selected = selectExt) #.xls
      })
      incProgress(1/9, detail = "extension selector")
      incProgress(1/9, detail = "edit data selector")
    })
  })
  
  # When the header pops up after you upload the file
  observeEvent(input$header, {
    numericHeader <- as.numeric(input$header)
    if (numericHeader != 1) {
      disable("header")
    }
    setColNames(numericHeader + 1, if (numericHeader > 0) 1 else 0) 
  }, ignoreNULL = TRUE)
  
  output$colnamesSelector <- renderUI({
    if (!is.null(input$header) && as.numeric(input$header) > 1) {
      div(
        tags$b("Please select the header row you would like to use as the column names."),
        DTOutput("headerPreview")
      )
    }
  })
  
  output$headerPreview <- renderDT({
    datatable(
      values$datasetInput[1:as.numeric(input$header),],
      rownames = FALSE,
      colnames = rep("", ncol(values$datasetInput)),
      selection = list(mode = "single", selected = c(1)),
      options = list(dom = "t", scrollX = '300px', ordering = FALSE)
    )
  })
  
  # This changes the dataset if new rows are selected
  #"_rows_selected" is not part of the variable name-it is required to retrieve the row selected by the user
  observeEvent(input$headerPreview_rows_selected, {
    if (any(is.na(colnames(values$datasetInput)))) {
      colnames(values$datasetInput)[is.na(colnames(values$datasetInput))] <- "Null3"
    }
    setColNames(as.numeric(input$header) + 1, input$headerPreview_rows_selected)
  }, ignoreNULL = TRUE)
  
  # ** BioPortal Access (Download Ontologies)
  output$ontologySelector <- renderUI({
    
    if (!is.null(input$file1)) {
      # List of Ontology Names Recommender   
      # Pop up window informs the user that accessing info from BioPortal will take a while
      tryCatch({
        res <- R.utils::withTimeout(  { 
          show_modal_spinner(spin = "spring", color = "#112446",
                             text = p("To help you standardize your data, we are accessing the entire list of ontologies from ",
                             (a(href = 'https://bioportal.bioontology.org/', 'BioPortal')), " recommended for your dataset. 
                             Depending on your internet connection and the last time you used Good Nomen, this could take longer than a minute.
                             Thank you for your patience."))
          # Check to make sure OntologyList exists
          if (!file.exists(ONTOLOGY_LIST_FILE_PATH)) {
            file.create(ONTOLOGY_LIST_FILE_PATH)
            # Set an arbitrarily old date as the default
            Sys.setFileTime(ONTOLOGY_LIST_FILE_PATH, "2020-01-01")
          }
          # Get the last date modified from a file and see if it's been 7 days
          lastRunDate <- file.mtime(ONTOLOGY_LIST_FILE_PATH)
          dateToday <- Sys.Date()
          dateDif <- as.Date(strptime(dateToday, "%Y-%m-%d")) - as.Date(strptime(lastRunDate,"%Y-%m-%d"))
          diffNum <- as.numeric(dateDif)
          
          # If needed, download full ontology list from BioPortal. Else, read file
          bioportalOntologies <- NULL
          if (diffNum > DAYS_SINCE_DOWNLOAD) { 
            tryCatch({
              res <- R.utils::withTimeout({
                bioportalOntologiesResponse <- GET(paste0("http://data.bioontology.org/ontologies?apikey=", API_KEY))
                bioportalOntologies <- content(bioportalOntologiesResponse, "parsed")
              }, timeout = TIMEOUT_TIME)
            }, TimeoutException = function(ex) {
              timeOutError()
            }, finally = {
              if (is.null(bioportalOntologies)) {
                remove_modal_spinner()
                showModal(modalDialog(title = "BioPortal Unavailable for Access",
                                      p("BioPortal seems to be down, please check ",
                                        (a(href = 'https://bioportal.bioontology.org/', 'BioPortal')), " to see if it is working. You may need to try again later. We apologize for the inconvenience."),
                                      footer = NULL,
                                      easyClose = F))
              }
            })
            bioportalOntologiesDataFrame <- data.frame(t(sapply(bioportalOntologies,c)))
            bioportalOntologiesDataFrame$nameAndAcronymn = paste(bioportalOntologiesDataFrame$acronym, bioportalOntologiesDataFrame$name) # Makes a column with both acronym and name
            listOfOntNames <<- bioportalOntologiesDataFrame[, ncol(bioportalOntologiesDataFrame)] # This accesses the last column of the dataframe
            write.table(listOfOntNames, file = ONTOLOGY_LIST_FILE_PATH, append = FALSE, quote = FALSE,
                        row.names = FALSE, col.names = FALSE)
          }
          else {
            listOfOntNames <- readLines(ONTOLOGY_LIST_FILE_PATH)
            listOfOntNames <<- lapply(listOfOntNames, noquote)
          }
          
          # Make a tibble, so later on when you have the three recommended ontology acronyms, you can filter to find their full names. 
          ontologyTibble <- tibble(value = listOfOntNames)
          ontologyTibble <- separate(ontologyTibble, value, into =  c("Acronym", "FullName"), sep = "\\s", extra = "merge")
          
          # Access recommended ontologies through Bioportal
          if (any(is.na(values$datasetInput))) {
            values$datasetInput[is.na(values$datasetInput)] <- "Null"
          }
          rURL <- getRecommendedTerms(values$datasetInput)
          internetTester = curl::has_internet()
          if (!internetTester) {
            # If the URL is broken,try again a few times
            for (i in 1:NUM_TEST_TIMES) {
              rURL <- getRecommendedTerms(values$datasetInput)
              if (url.exists(rURL)) {
                break;
              }
            }
            recommendedOntologies <- ""; # Error and try again later
          }
          else {
            # Get the acronym for the top three recommended Ontologies 
            tryCatch({
              res <- R.utils::withTimeout(  {
                dataFrameRecommend <- content(rURL, "parsed")
                recommenderDF <- as.data.frame(t(sapply(dataFrameRecommend,c)))
              }, timeout = TIMEOUT_TIME)
            }, TimeoutException = function(ex) {
              timeOutError()
            })
            recTibble <- as_tibble(recommenderDF)
            if (ncol(recTibble) > 1) {
              recTibbleData <- sapply(unnest(select(recTibble, ontologies), ontologies), unlist)
              recTibble <- recTibbleData[seq(1, length(recTibbleData), by = 3)]

              # If there are fewer than three elements in the recommended ontology, set the NUM_RECOMMENDED_ONTOLOGIES to the size of the list created
              if (length(recTibble) < NUM_REC_ONTO) {NUM_REC_ONTO <<- length(recTibble)}
              
              recommendedOntologies <- recTibble[1:NUM_REC_ONTO] 
              for (i in 1:NUM_REC_ONTO) {
                thisTerm <- filter(ontologyTibble, Acronym == recommendedOntologies[i]) %>% select(FullName)
                recommendedOntologies <- replace(recommendedOntologies, i, paste(recommendedOntologies[i], unlist(unname(thisTerm)), " ", collapse = " "))
              }
            } else {
              recommendedOntologies <<- ""
            }
          }
          values$recommendedOntologies <<- recommendedOntologies
        }, timeout = TIMEOUT_TIME)
      }, TimeoutException = function(ex) {
        timeOutError()
      })
      remove_modal_spinner()
      selectizeInput('ontologySelector',
                     label = div(
                       "Select Ontology:",
                       helpButton("Select an ontology from BioPortal.")),
                     choices = list('Recommended Ontologies' = c("", recommendedOntologies),
                                    'All Ontologies' = listOfOntNames),
                     options = list(placeholder = "Select ontology or start typing...",
                                    closeAfterSelect = TRUE))
    } 
  })
  
  # ** Next Button (and Load Ontoloy) 
  output$page1Next <- renderUI({
    values$ontName <<- input$ontologySelector
    if (input$ontologySelector != "" && !is.null(input$ontologySelector)) {
      actionButton("buttonLoadThenNext", "Next", style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")
    }
  }) 
  
  # **Main Panel - Load Data 
  output$loadDataPreviewText <- renderText({
    if (is.null(values$dataset)) {
      "After you have uploaded a file, a preview of your data will appear here."
    } else {
      NULL
    }
  })
  
  output$loadDataColNav <- renderUI({
    setColumnNavigation("loadData")
  })
  
  output$uploadPreview <- renderDT({
    dataPreview()
  })
  
  # This observe event handles downloading the ontology, checking to see if it's locked, and moving forward to the next page
  observeEvent(input$buttonLoadThenNext, {
    
    # ADD TEXT TO SCRIPT for modifying headers 
    masterText <<- paste0(masterText, "\n\n# Set column names and format datasheet\n", readInputFileText, "\n", values$headerText)
    
    # Parse the acronym from the ontology name and show it
    values$ontologyAcronym <<- strsplit(values$ontName, " ")[[1]][1]
    
    values$manualSaveMessage <- NULL
    show_modal_spinner(spin = "spring", color = "#112446",
                       text = p("To help you standardize your data, we are pulling standardized terms from BioPortal.",
                                "If you would like to use this functionality on your own browser, please follow ",
                                (a(href = 'https://bioportal.bioontology.org/annotator', 'this link')), " and select the terminology you wish to use. 
                                Depending on your internet connection, this could take longer than a minute.", 
                                "Thank you for your patience."))
    
    # Get the last date modified from a file and see if it's been 7 days
    ontFileName <- paste0(TEMP_DIR_PATH, values$ontologyAcronym, "_Ontology.txt")
    allFileName <- paste0(TEMP_DIR_PATH, "All_Terms.txt")

    shouldDownload <- TRUE
    if (file.exists(ontFileName)) {
      lastRunDate <- file.mtime(ontFileName)
      dateDif <- as.Date(strptime(Sys.Date(), "%Y-%m-%d")) - as.Date(strptime(lastRunDate,"%Y-%m-%d"))

      if (as.numeric(dateDif) <= DAYS_SINCE_DOWNLOAD)
        shouldDownload <- FALSE
    }
    
    # If needed, download this ontology from BioPortal. Else, read cached file.
    if (shouldDownload) {
      downloadURL <- sprintf(paste("http://data.bioontology.org/ontologies/", values$ontologyAcronym, "/download?download_format=csv&display_links=false&apikey=", API_KEY, sep = ""))
      
      if (!url.exists(downloadURL)) {
        remove_modal_spinner()
        # SITUATION: ONTOLOGY IS LOCKED FOR DOWNLOAD & it's never been downloaded before
        lockedOntologyError()
      } else {
        tryCatch({
          res <- R.utils::withTimeout(  {
            tmpFilePath <- paste0(tempfile(), ".csv.gz")
            testDownloadURL <- GET(downloadURL, write_disk(tmpFilePath))
            ontologyFile <- suppressMessages(suppressWarnings(read_csv(tmpFilePath)))
            unlink(tmpFilePath)
          },  timeout = TIMEOUT_TIME)
        }, TimeoutException = function(ex) {
          print("timeout reached")
          timeOutError()
        })
        
        # Format column names to retrieve a list of preferred names stored in the ontology
        colnames(ontologyFile) <- sub("_", " ", colnames(ontologyFile))
        colnames(ontologyFile) <- toTitleCase(colnames(ontologyFile))
        ontologyFile <- ontologyFile[, !duplicated(colnames(ontologyFile))] # This was added because the MEDO ontology had duplicate columns and wouldn't pull the preferred name because of it

        # There are a few options of what the preferred name can be such as "preferred name" and "label"
        if ("Preferred Name" %in% colnames(ontologyFile)) {
          ontologyFile <- dplyr::rename(ontologyFile, Preferred = `Preferred Name`)
        } else if ("Label" %in% colnames(ontologyFile)) {
          ontologyFile <- dplyr::rename(ontologyFile, Preferred = `Label`)
        } else if ("Preferred Label" %in% colnames(ontologyFile)) {
          ontologyFile <- dplyr::rename(ontologyFile, Preferred = `Preferred Label`)
        }
        
        ontologyFile <- ontologyFile %>%
          filter(!Obsolete) %>%
          select(-Definitions, -Obsolete) %>%
          dplyr::rename(Synonym = Synonyms) %>%
          separate_rows(Synonym, sep=" ?\\| ?")
        
        values$synonyms <- pull(ontologyFile, Synonym)
        values$preferred <- pull(ontologyFile, Preferred)
        
        # Build a tibble with "clean" terms alongside the original terms.
        matchedTerms = build_term_tibble(pull(ontologyFile, Synonym))
        values$TOTAL_TERM_LIST <- matchedTerms
        
        write_csv(matchedTerms, file = ontFileName)
        write_csv(select(ontologyFile, c(Preferred, Synonym)), file = allFileName)
        
        remove_modal_spinner()
        updateTabsetPanel(session, 'tabs', selected = 'editTable')
      }
    } else {
      ontologyTerms <- read_csv(ontFileName)
      values$TOTAL_TERM_LIST <- ontologyTerms
      
      ontologyFile <- read_csv(allFileName)
      
      values$synonyms <- pull(ontologyFile, Synonym)
      values$preferred <- pull(ontologyFile, Preferred)
      
      remove_modal_spinner()
      updateTabsetPanel(session, 'tabs', selected = 'editTable')
    }
  })
  
  # * Edit Data ---------------------------------------------------------------
  
  output$selectedOntology <- renderUI({
    ontologyLstAcr <- strsplit(values$ontName, " ")
    values$ontologyAcronym <<- ontologyLstAcr[[1]][1]
    urlToOpen <- paste0("https://bioportal.bioontology.org/ontologies/",values$ontologyAcronym)
    HTML(paste("<b>Selected Ontology: </b>",  (a(href = urlToOpen, values$ontName)), collapse = "<BR>"))
  })
  
  observeEvent(input$changeOntology, {
    title <- "Change the Ontology"
    
    content <- tagList()
    content[[1]] <- selectizeInput('newOntologySelector', label = "Select New Ontology:", 
                                   choices = list('Recommended Ontologies' = c("", values$recommendedOntologies),
                                                  'All Ontologies' = listOfOntNames),
                                   options = list(
                                     placeholder = "Please select an ontology...",
                                     closeAfterSelect = TRUE))
    content[[3]] <- actionButton('resetAndSave', label = "Save and Reset Ontology")
    content[[4]] <- actionButton('cancelChangeOntology', label = "Cancel")
    showModal(
      modalDialog(
        content,
        title = title,
        footer = NULL,
        size = "m"
      )
    )
  })
  
  output$editThisColumnSelector <- renderUI({
    selectizeInput(
      'editThisColumn', 
      label = "Select Column to Standardize:", 
      choices = c("", columns()),
      selected = values$lastSelectedEditColumn,
      options = list(placeholder = 'Select column or start typing...',
                     closeAfterSelect = TRUE))
  })
  
  # ** Auto-match ---------------------------------------------------------------
  observeEvent(input$automatch, ignoreInit = T, {
      values$automatchResult <- list()
      
      # Disable the buttons while the matches are loading.
      disable("editThisColumn") # select box
      disable("automatch") # auto-match button
      disable("manual") # standardize manually button
      disable("editBack") # back button
      disable("editNext") # next button
      
      # If the column is not selected, throw an error. Otherwise, continue.
      if (is.null(input$editThisColumn) || input$editThisColumn == "") {
        title <- "Error"
        content <- columnNotSelectedMessage
      } else {
        title <- "Review Matches"
      }

      # Identify matches
      uniqueTerms <- unique(values$dataset[[input$editThisColumn]])
      sdm <- identifyMatches(uniqueTerms)
       
      matches <- as.data.frame(select(sdm, c(TestTerm, OntologyTerm)) %>%
         rename(`Current Term` = TestTerm) %>%
         rename(`Standardized Term` = OntologyTerm) %>%
         mutate(Accept = TRUE))
      
      values$matches <- matches[!(matches$`Current Term` == matches$`Standardized Term`),]
      # Sort the table alphabetically
      values$matches <- matches[order(matches$`Standardized Term`),]
        
      # Output the table
      if (nrow(values$matches) > 0) {
        if (length(matches) > 0) {
          content <- tagList()
          content[[1]] <- p(
            paste(
              "The following matches were found based on the terminology.",
              "Accept a match by checking the box in the row.",
              "Press \"Save\" to apply these changes to your data and close this window.",
              "If a match is accepted, all occurrences of the current term will be changed",
              "to the standardized term."
            )
          )
          content[[2]] <- actionButton('selectAll', label = "Select All")
          content[[3]] <- actionButton('deselectAll', label = "Deselect All")
          content[[4]] <- br()
          content[[5]] <- br()
          content[[6]] <- uiOutput("automatchTable")
          content[[7]] <- br()
          content[[8]] <- actionButton('automatchSave', label = "Save")
          content[[9]] <- actionButton('automatchClose', label = "Cancel", class = "secondary_button")
          content[[10]] <- tags$head(tags$style(
            "#automatchModal .modal-footer{ display:none}"
          ))
        } else {
          content <- p("No matches found.")
        }
      } else {
        content <- p("The terms in this column are already standardized or there were no terms to standardize.")
      }
    
    showModal(
      modalDialog(
        content,
        title = title,
        footer = modalButton("Close"),
        size = "l"
      )
    )
    
    renderAutoMatchTable()
    
    # Enable the buttons again.
    enable("editThisColumn") # select box
    enable("automatch") # auto-match button
    enable("manual") # standardize manually button
    enable("editBack") # back button
    enable("editNext") # next button
  })
  
  # ** ** Auto-match Supporting Events
  observeEvent(input$selectAll,{
    values$matches[,3] <- rep(TRUE, nrow(values$matches))
    values$selectedPushed <- TRUE
    renderAutoMatchTable()
  })
  
  observeEvent(input$deselectAll,  {
    values$matches[,3] <- rep(FALSE, nrow(values$matches))
    values$deselectedPushed <- TRUE
    renderAutoMatchTable()
  })
  
  renderAutoMatchTable <- function(){
    output$automatchTable <- renderUI({
      tagList(
        tagList(
          fluidRow(
            column(width = 2, align = "center", h4("Current Term")),
            column(width = 3, h4("Standardized Term")),
            column(width = 2, h4("Accept?"))
          )
        ),
        lapply(1:nrow(values$matches), function(i) {
          autoMatchModule(values$matches[i,1], values$matches[i,2], values$matches[i,3])
        }),
      )
    })
  }
  
  # Listeners for the automatch Table Module
  automatchTableListener <- function(input, output, session, modID){
    observeEvent(input$checkBox, {
      if (values$deselectedPushed == FALSE && values$selectedPushed == FALSE) {
        if (input$checkBox == FALSE) {
          values$matches[modID,3] <- FALSE
        } else if (input$checkBox == TRUE) {
          values$matches[modID,3] <- TRUE
        }
      } else {
        values$numTimesClicked = values$numTimesClicked + 1
      }
    })
    
    # This generates the automatch table Module and connects the listener (see automatchTableListener)
    observe({
      if (!is.null(values$matches)) {
        lapply(1:nrow(values$matches), function(i) {
          callModule(automatchTableListener, values$matches[i,1],i)
        })
      }
    })
    
    # Listener to control the select all and deselect all button
    observe({
      if (values$numTimesClicked >= nrow(values$matches)) {
        values$numTimesClicked <- 0
        values$selectedPushed = FALSE
        values$deselectedPushed = FALSE
      }
    })
  }
  
  # ** ** Auto-match Save 
  observeEvent(input$automatchSave, ignoreInit = T, {
    if (length(which(values$matches[,3])) > 0) {
      # Change dataset table values to reflect changes made by editor
      values$lastSelectedEditColumn <- input$editThisColumn
      accepted <- values$matches[,3]
      acceptedList <- values$matches$'Standardized Term'[accepted]
      
      # The if statement checks to make sure that at least 1 term has been selected to save. Else, don't change any terms.
      if (length(acceptedList) > 0 ) {
        names(acceptedList) <- paste0("^", values$matches$`Current Term`[accepted], "$")
        columnNameOfChangedTerms <- input$editThisColumn #The column from the actual datasheet that is to be changed
        datasetInput <- values$dataset
        
        # This tells the R Script which terms we want to change and what we want to change them to
        # It also changes the values in the dataset to their corrected value (if it was checked)
        names <- paste0("^", values$matches$`Current Term`[accepted], "$")
        
        # ADD TEXT TO SCRIPT for auto-matching
        masterText <<- paste0(masterText, "\n\n# Changing the dataset based on auto-match\n", "columnNameOfChangedTerms <- \"", columnNameOfChangedTerms, 
                              "\"\n", "acceptedList <- c(", paste0("'", unname(acceptedList), "'", collapse = ", "), ")",
                              "\n","namesAcceptedList <- c(", paste0("'", names, "'", collapse = ", "), ")",
                              "\nnames(acceptedList) <- namesAcceptedList")
        automatchingText <- "datasetInput[[columnNameOfChangedTerms]] <- str_replace_all(datasetInput[[columnNameOfChangedTerms]], acceptedList)"
        eval(parse(text = automatchingText))
        values$dataset <- datasetInput
        masterText <<- paste0(masterText, "\n", automatchingText)
      }
    }
    removeModal()
  })
  
  observeEvent(input$automatchClose, ignoreInit = T, {
    removeModal()
  })
  
  # ** Manual Standardization (Functions) ----------------------------------------
  
  # This is for the save/confirm button
  standardizeManually <- function() {
    newData <- if (is.null(input$newData) || input$newData == "" || input$saveConfirmBtn) NA else input$newData
    editThisColumn <- gsub("\"", "\\\\\"", input$editThisColumn)
    numItems <- length(input$editData)
    withProgress(message = "Standardizing", {
      values$lastSelectedEditColumn <- input$editThisColumn
      # ADD TEXT TO SCRIPT for manual standardization
      masterText <<- paste0(masterText, "\n\n", "# Manual standardization\neditThisColumn <- \"", editThisColumn, "\"\n")
      
      for (item in input$editData) {
        incProgress(1/numItems, detail = item)
        datasetInput <- values$dataset
        manualText <- paste0("datasetInput[[editThisColumn]][datasetInput[[editThisColumn]] == \"", item, "\"] <- \"", newData, "\"\n")
        eval(parse(text = manualText))
        values$dataset <- datasetInput
        masterText <<- paste0(masterText, manualText)
      }
    })
    values$manualSaveMessage <- paste0("Standardizations for column \"", input$editThisColumn, "\" have been saved. ",
                                       "Click \"Close\" to continue to the next step.")
    updateCheckboxInput(session, "makeNA", value = FALSE)
    updateSelectizeInput(session, "editData", selected = NULL)
    updateSelectizeInput(session, "newData", selected = NULL)
    return()
  }
  
  # Manual Modals
  saveConfirmModal <- bsModal(
    "saveConfirm",
    title = "Warning",
    trigger = "saveConfirmBtn",
    p(paste0('The selected terms will be overwritten as "NA". Please click "Continue"',
             ' to perform this operation or "Cancel" to return to the previous window.')),
    actionButton("saveConfirmContinue", label = "Continue"),
    actionButton("saveConfirmCancel", label = "Cancel", class = "secondary_button"),
    tags$head(tags$style(paste0("#saveConfirm .modal-footer{display:none}")))
  )
  
  columnNotSelectedMessage <- HTML(
    '<p style="color:red">You must select a column to edit before proceeding. Please close this window and select a column.</p>'
  )
  
  # ** MANUAL MODAL 1 
  startManual <- function() {
    withProgress(message = "Getting Manual Standardization Ready", value = 0, {
      incProgress(0.7)
      downloadURL <- sprintf(paste("http://data.bioontology.org/ontologies/", values$ontologyAcronym, "/download?download_format=csv&display_links=false&apikey=", API_KEY, sep = ""))
      title <- "Manual Standardization"
      content <- tagList()
      content[[1]] <- p(
        paste(
          "In the box below, enter terms that have a common meaning or terms that you wish to change. ",
          "The options in the box are unique values from the selected column.",
          "To move on, press \"Next.\" "
        )
      )
      content[[2]] <- uiOutput("editDataSelector")
      content[[3]] <- checkboxInput("makeNA", "These terms represent missing values")
      content[[4]] <- br()
      incProgress(0.1)
      content[[5]] <- div(actionButton('nextManualModal', 'Next'), style = "float:right")
      content[[6]] <- br()
      content[[7]] <- br()
      incProgress(0.1)
      showModal(
        modalDialog(
          content,
          title = title,
          footer = tagList(modalButton("Cancel")),
          size = "l",
          easyClose = TRUE)
      )
    })
  }
  
  observeEvent(input$manual, {
    startManual()
  })
  
  observeEvent(input$manualAnother, {
    startManual()
  })
  
  observeEvent(input$manualNAAnother, {
    startManual()
  })
  
  observeEvent(input$nextManualModal, {
    termsToStandardizeManually <- paste0(unlist(input$editData), collapse = ', ')
    values$manualSaveMessage <- NULL
    
    if (input$makeNA == 0) {
    
      # ** MANUAL MODAL 2 
      # Download data and recommendations from BioPortal
      show_modal_spinner(spin = "spring", color = "#112446",
                       text = p("To help you standardize your data, we are pulling all standardized terms from BioPortal.",
                                "If you would like to use this functionality on your own browser, please follow ",
                                (a(href = 'https://bioportal.bioontology.org/annotator', 'this link')), " and select the terminology you wish to use.
                                Depending on your internet connection, this could take longer than a minute.",
                                "Thank you for your patience."))
      
      values$recTermsList <- NULL
      
      sdm <- identifyMatches(input$editData)
      matches <- as.data.frame(sdm)
        
      if (NUM_REC_MANUAL > nrow(matches)) {
        recTermsList <- matches$OntologyTerm
        values$recTermsList <- recTermsList
          
      }
      else {
        matchesSubset <- head(matches, NUM_REC_MANUAL)
        recTermsList <- matchesSubset$OntologyTerm
        values$recTermsList <- recTermsList
      }
        
      names(values$recTermsList) <- NULL
    }
    
    # ** MANUAL MODAL 3 
    # Modal #3 Let the user select the standardized terms they want to represent their data
    title <- "Standardizing Selected Terms"
    
    content <- tagList()
    content[[1]] <- conditionalPanel(
      condition = "!input.makeNA", p(
      paste(
        "Pick a term that represents all the terms you selected. ",
        "The options in the second box are from the selected terminology. By default, the program will pick the top rated term that connects all the data. 
         To apply changes to your data, press \"Save.\"",
        "All occurrences of the terms in the first box will be replaced with the selected terminology term."
      )
    ))
    content[[2]] <- br()
    content[[3]] <- HTML(paste('Terms that you selected with a common meaning: ', termsToStandardizeManually))
    content[[4]] <- br()
    content[[5]] <- br()
    content[[6]] <- conditionalPanel(
      condition = "!input.makeNA",
      selectizeInput('newData', label = "Select terminology term or enter a new term:",
                     choices = "", selected = NULL, options = list(placeholder = 'Select term or start typing...', 
                                                                   closeAfterSelect = TRUE, create = TRUE))
    )
    content[[7]] <- br()
    content[[8]] <- conditionalPanel(
      condition = "!input.makeNA && input.editData && input.newData",
      style = "display: inline-block;",
      actionButton("manualSave", "Save")
    )
    content[[9]] <- conditionalPanel(
      condition = "input.editData && input.makeNA",
      style = "display: inline-block;",
      actionButton("saveConfirmBtn", "Save as NA")
    )
    content[[10]] <- conditionalPanel(
      condition = "input.editData && input.makeNA && input.saveConfirmBtn",
      style = "display: inline-block;",
      actionButton("manualNAAnother", "Standardize Another Group of Terms")
    )
    content[[11]] <- conditionalPanel(
      condition = "input.editData && input.manualSave",
      style = "display: inline-block;",
      actionButton("manualAnother", "Standardize Another Group of Terms")
    )
    content[[12]] <- actionButton('manualClose', label = "Close", class = "secondary_button")
    content[[13]] <- saveConfirmModal
    content[[14]] <- br()
    content[[15]] <- textOutput("savedMessage")
    content[[16]] <- tags$head(tags$style("#savedMessage {color:green}"))
    
    showModal(
      modalDialog(
        content,
        title = title,
        footer = NULL,
        size = "l"
      )
    )
  })
  
  # Update the selectize input from the server's end (this is for ALL the terms in the Ontology, sometimes as large as 150,000 terms)
  observeEvent(input$nextManualModal, {
    if (length(values$recTermsList) > 0) {
      updateSelectizeInput(session, 'newData', choices = list("", 'Recommended Terms' = c(values$recTermsList, ""),
                                                              'All Terms' = c("", sort(unique(values$preferred)))), server = TRUE)
    } else {
      updateSelectizeInput(session, 'newData', choices = list(values$preferred), server = TRUE)
    }
  })

  # Populating Manual Modal
  output$editDataSelector <- renderUI({
    selectizeInput('editData', label = "Enter terms that have a common meaning:", choices = unique(str_trim(values$dataset[[input$editThisColumn]][order(values$dataset[[input$editThisColumn]])])), multiple = T,
                   options = list(placeholder = "Select a term or start typing..."))
  })
  
  # Manual Listeners 
  observeEvent(input$saveConfirmContinue, {
    toggleModal(session, "saveConfirm", toggle = "close")
    standardizeManually()
  })
  
  observeEvent(input$saveConfirmCancel, {
    toggleModal(session, "saveConfirm", toggle = "close")
  })
  
  observeEvent(input$manualSave, {
    standardizeManually()
  })
  
  output$savedMessage <- renderText({
    values$manualSaveMessage
  })
  
  observeEvent(input$resetAndSave, {
    shinyjs::reset('manual')
    values$ontName <<- input$newOntologySelector
    ontologyLstAcr <- strsplit(input$newOntologySelector, " ")
    values$ontologyAcronym <<- ontologyLstAcr[[1]][1]
    updateSelectizeInput(session, 'ontologySelector', selected = values$ontName) #This updates the first drop down menu with your new selection
    
    show_modal_spinner(spin = "spring", color = "#112446",
                       text = p("Loading new ontology. Please be patient, this shouldn't take long."),)
    
    downloadURL <- sprintf(paste("http://data.bioontology.org/ontologies/", values$ontologyAcronym, "/download?download_format=csv&display_links=false&apikey=", API_KEY, sep = ""))
    
    if (!url.exists(downloadURL)) { # THE ONTOLOGY IS LOCKED FOR DOWNLOAD
      removeModal()
      lockedOntologyError()
    } else {
      removeModal()
    }
  })
  
  observeEvent(input$cancelChangeOntology, {
    removeModal()
  }, ignoreInit = T)
  
  observeEvent(input$manualClose, {
    removeModal()
  }, ignoreInit = T)
  
  # Main Panel - Edit Data 
  output$editDataPreviewText <- renderText({
    if (is.null(values$dataset)) {
      "After you have uploaded a file, a preview of your data will appear here."
    } else {
      NULL
    }
  })
  
  output$editDataPreview <- renderUI({
    if (!is.null(values$dataset)) {
      if (!is.null(input$editThisColumn) && input$editThisColumn %in% colnames(values$dataset)) {
        DTOutput("singleColumn")
      } else {
        output <- tagList()
        output[[1]] <- setColumnNavigation("editData")
        output[[2]] <- DTOutput("editDataAll")
        output
      }
    }
  })
  
  output$singleColumn <- renderDT({
    datatable(values$dataset[, input$editThisColumn], options = list(pageLength = 10), rownames = F)
  })
  
  output$editDataAll <- renderDT({
    dataPreview()
  })
  
  # *Update Column Names -----------------------------------------------------
  
  output$editColumnSelector <- renderUI({
    selectizeInput(
      'editColumn', 
      label = "Specify Column to Rename:", 
      choices = c("", columns()),
      options = list(placeholder = 'Select column or start typing...',
                     closeAfterSelect = TRUE, create = TRUE))
  })
  
  output$columnRenameButton <- renderUI({
    if (!is.null(input$newColumn) && input$newColumn != "") {
      actionButton('columnRename', "Rename")
    }
  })
  
  observe({
    if (!is.null(input$editColumn) && nchar(input$editColumn) > 0) {
      disable("newColumn")
      
      show_modal_spinner(spin = "spring", color = "#112446",
                         text = paste0("To help you standardize your data, we are finding recommended column names from the ontology you 
                                       selected. Feel free to write your own column names as well. Thank you for your patience."))

      sdm <- identifyMatches(input$editColumn)
      newColNames <- sdm$OntologyTerm
      names(newColNames) <- NULL
      
      removeModal()
      
      updateSelectizeInput(session, inputId = 'newColumn', choices = list('Recommended Terms' = c(newColNames, ""),
                                                                          'All Terms' = c("", values$preferred)), server = TRUE,
                           options = list(placeholder = 'Select a term or start typing...', 
                                          create = TRUE, maxItems = 5, maxOptions = 100,
                                          closeAfterSelect = TRUE))
      enable("newColumn")
    }
  })
  
  # Rename Column 
  observeEvent(input$columnRename, ignoreInit = T, {
    # Display warning if user does not select column to rename and new column name
    if (input$editColumn == "" | input$newColumn == "") {
      toggleModal(session, 'columnModal', toggle = "open")
    }
    # Display warning if new name is already the name of a column
    else if (any(input$newColumn %in% columns())) {
      toggleModal(session, 'equalModal', toggle = "open")
    }
    else {
      datasetInput <- values$dataset
      newColumn <- gsub("\"", "\\\\\"", input$newColumn)
      changeColumnText <- paste0("\n# Edit Column Name\n",
                     "editColumn <- \"", input$editColumn, "\"\n",
                     "newColumn <- \"", newColumn, "\"",
                     "\ncolnames(datasetInput)[which(colnames(datasetInput) == editColumn)] <- newColumn")
      eval(parse(text = changeColumnText))
      values$dataset <- datasetInput
      
      # ADD TEXT TO SCRIPT for modifying column names
      masterText <<- paste0(masterText, "\n", changeColumnText)
    }
    showNotification(paste0("Column \"", input$editColumn, "\" has been renamed to \"", input$newColumn, ".\""))
  })
  
  # Main Panel - Update Column Names 
  output$updateColNamesPreviewText <- renderText({
    if (is.null(values$dataset)) {
      "After you have uploaded a file, a preview of your data will appear here."
    } else {
      NULL
    }
  })
  
  output$updateColNamesPreview <- renderUI({
    if (!is.null(values$dataset)) {
      if (!is.null(input$editColumn) && input$editColumn %in% colnames(values$dataset)) {
        DTOutput("updateSingleColumn")
      } else {
        output <- tagList()
        output[[1]] <- setColumnNavigation("updateColNames")
        output[[2]] <- DTOutput("updateColNamesAll")
        output
      }
    }
  })
  
  output$updateSingleColumn <- renderDT({
    if (input$editColumn %in% colnames(values$dataset)) {
      datatable(values$dataset[, input$editColumn], options = list(pageLength = 10), rownames = F)
    }
  })
  
  output$updateColNamesAll <- renderDT({
    dataPreview()
  })
  
  # *Save Data ---------------------------------------------------------------
  
  # Name Output File 
  output$downloadButtons <- renderUI({
    if (!is.null(values$dataset)) {
      output <- tagList()
      output[[1]] <- downloadButton('editReport', width = "100%", label = div("Download Output File", 
                                                                              helpButton("Output file contains updated data.")),
                                    style = "display: block; color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 9px;")
      output[[2]] <- downloadButton('script1', width = "100%", label = div("Download R Script",
                                                                           helpButton(paste("RScript contains all of the commands", 
                                                                                            "necessary to create the output file from", 
                                                                                            "the original file."))),
                                    style = "display: block; color: #fff; background-color: #2ca25f; border-color: #2ca25f; margin-bottom: 9px;")
      output # This ensures that the buttons will appear
    }
  })
  
  # Download Output File
  output$editReport <- downloadHandler(
    filename = function() {
      fileName <- if (input$outputFileName == "") "shiny_output" else input$outputFileName
      thisExtension <- if (nchar(input$extension) == 0) extension() else input$extension
      return(paste0(fileName, thisExtension))
    }, 
    content = function(file) {
      thisExtension <- if (nchar(input$extension) == 0) extension() else input$extension
      toWrite <- if (grepl("xls", thisExtension)) "xlsx" else extensionsMap[[thisExtension]]
      if (!is.null(values$extraHeaders)) {
        values$dataset <- rbind(setNames(values$extraHeaders, names(values$dataset)), values$dataset)
      }
      return(do.call(paste0("write_", toWrite), list(values$dataset, file, "col_names" = (input$header != "0"))))
    }
  )
  
  # Download R script. Format is downloadHandler(filename, content)
  output$script1 <- downloadHandler(
    filename = function() {
      paste0(input$outputFileName, "_R_script.R")
    }, content = function(file) {
      fileName <- if (input$outputFileName == "") "shiny_output" else input$outputFileName
      thisExtension <- if (nchar(input$extension) == 0) extension() else input$extension
      fullFileName <- paste0(fileName, ".R")
      
      # ADD TEXT TO SCRIPT for saving the file
      masterText <<- paste0(masterText, "\n", "\n# Save file\n",
                            "if (!is.null(extraHeaders)) {\n",
                            "\tdatasetInput <- rbind(setNames(extraHeaders, names(datasetInput)), datasetInput)\n",
                            "}\n",
                            "file <- '", paste0(input$outputFileName, thisExtension),
                            "'\n", paste0("write_", substring(thisExtension, 2)), "(datasetInput, file)",
                            "\nprint('Your file has been successfully saved and modified with the name: ",input$outputFileName, "')")
      write.table(masterText, file, row.names = F, col.names = F, quote = F)
      showNotification(paste0("Your file was successfully saved."))
    })
  
  output$saveDataPreviewText <- renderText({
    if (is.null(values$dataset)) {
      "After you have uploaded a file, a preview of your data will appear here."
    } else {NULL}
  })
  
  output$saveDataColNav <- renderUI({
    if (!is.null(values$dataset)) {
      setColumnNavigation("saveData")
    }
  })
  
  output$saveDataPreview <- renderDT({
    dataPreview()
  })
  
  # Change Tab --------------------------------------------------------------
  observeEvent(input$button, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'editTable')
  })
  observeEvent(input$terminologyButton, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'editTable')
  })
  observeEvent(input$editBack, {
    updateTabsetPanel(session, 'tabs', selected = 'loadData')
  })
  observeEvent(input$editNext, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'updateColumnNames')
  })
  observeEvent(input$columnBack, {
    updateTabsetPanel(session, 'tabs', selected = 'editTable')
  })
  observeEvent(input$columnSubmit, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'finalReport')
  })
  observeEvent(input$saveBack, {
    updateTabsetPanel(session, 'tabs', selected = 'updateColumnNames')
  })
}
shinyApp(ui, server)
