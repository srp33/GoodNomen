## Good Nomen Shiny App (Version 2.0)

#Load libraries ----------------------------------------------------------
librariesTxt <<- "#Load Libraries
library(DT)
library(RCurl)
library(rhandsontable)
library(rjson)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(tools)
library(writexl)"

masterText <- NULL
eval(parse(text=librariesTxt))

# Set file upload limit to 50 MB
options(shiny.maxRequestSize=50*1024^2, htmlwidgets.TOJSON_ARGS = list(na = 'string'))

# Global path variables
TEMP_DIR_PATH <- "/tmp"
API_KEY_FILE_PATH <- paste0(TEMP_DIR_PATH, "/BioPortalApiKey.txt")
ONTOLOGY_LIST_FILE_PATH <- paste0(TEMP_DIR_PATH, "/OntologyList.txt")

# Global functions and Definitions --------------------------------------------------------

libraryText <- c("DT", "RCurl", "rhandsontable", "rjson", "shiny", "shinyBS", "shinycssloaders", "shinyjs",
                 "tidyverse", "tools", "writexl")
RDFFile <- NULL
sURL <- NULL
readInputFileText <- NULL
API_KEY <- readChar(API_KEY_FILE_PATH, nchars = 36) #This gets the apikey from a txt file
DAYS_SINCE_DOWNLOAD <- 7
NUM_SAMPLE_ROWS <- 3 # number of sample rows to send to Bioportal to get recommended ontologies. The larger it is, the slower the code will run
NUM_REC_ONTO <- 3 #number of recommended ontologies to display to the user
NUM_REC_MANUAL <- 5 #num of manual term recommendation to display to the user
MAX_HEADERS <- 5 #Make number of header rows uploaded data can have
NUM_TEST_TIMES <-2 #If the URL doesn't work, test it again this many times.
SPINNER_TYPE <- 8 #any number between 1 and 8. 8 is the circle spinner. (To see the different spinner options, go to https://projects.lukehaas.me/css-loaders/)
TIMEOUT_TIME <- 120 # seconds

initializeScript <- function() {
  libraryText <<- c("dplyr", "stringr", "readxl", "writexl", "shinyBS", "shinycssloaders", "rhandsontable", "shinyjs", "RCurl", "rjson", "httr", "tidyverse")
}

initializeScript()

#define function for tooltips 
helpButton <- function(message = "content", placement = "right") {
  return(tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover"))
}

addLibrary <- function(librariesList) {
  installPackages <- ""
  for(libName in librariesList){
    installPackages <- paste0(installPackages, "\nif (!suppressWarnings(require(", libName, ", quietly = TRUE))) {", 
                              '  install.packages("', libName, '")',"}")
  }
  return(installPackages)
  
}

#define accepted file types and the read_ functions used to load them
extensionsMap <- c(".txt" = "tsv", ".tsv" = "tsv", ".csv" = "csv", ".xls" = "excel", ".xlsx" = "excel")

#define function for collapsing a list with proper grammar
collapse_text <- function(my_list) {
  last_index <- length(my_list)
  paste(paste(my_list[-1 * last_index], collapse = ", "), my_list[last_index], sep = ", and ")
}

getRecommendedTerms <- function(dataSet) {  # Get a list of terms to standardize
  sampleRows  <- sample_n(dataSet, min(NUM_SAMPLE_ROWS, nrow(dataSet)))
  rowChar <- toString(unlist(unique(unlist(sampleRows, use.names = FALSE)))) #Change sample table to one string
  rowChar <- URLencode(rowChar, reserved = TRUE) #Why encode? Characters in a URL other than the English alphanumeric characters and - _ . ~ should be encoded as % plus a two-digit hexadecimal representation, and any single-byte character can be so encoded. The standard refers to this as 'percent-encoding'.
  rURL <<- sprintf("http://data.bioontology.org/recommender?input=%s&apikey=%s&display_links=false&display_context=false", rowChar, API_KEY)
  
  # I had the error foudn here (https://stackoverflow.com/questions/49173967/trouble-using-jsonlites-fromjson-with-url-in-r) when I didn't include next three lines of code. Ignore the warning they generate
  res <- readLines(rURL)
  class(res) <- "json"
  return(res)
}

timeOutError <- function() {
  message("Timeout. Skipping.")
  showModal(modalDialog(title = "TimeOut Error",
                        p("The internet took too long to access and timed out. Try accessing BioPortal on a browser and see if it's working. If so, try running this app again."),
                        footer = modalButton("Dismiss"), easyClose = F))
}

# User Interface (UI) ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "Logo.png")
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
                                       "Accepted file types include ", collapse_text(names(extensionsMap)), ".")
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
                          wellPanel( uiOutput("loadDataColNav"), withSpinner(DTOutput("upload_preview")))
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
                                   div(
                                     actionButton('editBack', "Back", class = "back_button"),
                                     actionButton('editNext', "Next", class = "next_button"))
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
                                       label = "Select new column name:",
                                       choices = NULL,
                                       options = list(placeholder = 'Please select a column above...',
                                                      closeAfterSelect = TRUE)
                                     ),
                                     uiOutput("columnRenameButton")
                                   ), 
                                   bsModal(#warning if user does not select column to rename and new column name
                                     'columnModal',
                                     title = "Error",
                                     trigger = 'input.newColumn',
                                     HTML(paste('<p color="black">You must select a column to rename and a new column name.", 
                                                "Please close this window and select these items.</p>')),
                                     tags$head(tags$style("#columnModal {color: red;}"))
                                   ), 
                                   bsModal( #warning if user selects a new column name that is already being used as a column name
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
                        uiOutput('outputFileName'),
                        uiOutput('extensionSelector'),
                        uiOutput("downloadButtons"),
                        uiOutput("tab"), hr(),
                        actionButton('saveBack', "Back", class = "back_button")
                      ),
                      # ** Data Preview 
                      mainPanel(
                        tags$em(textOutput("saveDataPreviewText")),
                        uiOutput("saveDataColNav"),
                        wellPanel(withSpinner(DTOutput("saveDataPreview")))
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
  
  #Reactive Values
  values <- reactiveValues(datasetInput = NULL, dataset = NULL, headerDisabled = FALSE, 
                           extension = "", terminology = NULL, lastSelectedEditColumn = "", viewingSubset = c(1, 5),
                           manualSavedMessage = NULL, synonyms = NULL, myDF = NULL, OntologyAcronym = "",
                           RecommendedOntologies = NULL, listOfOntNames = NULL, ontName = "", TOTAL_TERM_LIST = NULL,
                           recTermsList = NULL)#, manualSelection = NULL)
  
  extension <- reactive({
    if (!is.null(input$file1)) {
      ext_search <- paste0(paste(str_replace(names(extensionsMap), "\\.", "\\\\."), collapse = "$|"), "$")
      ext <- str_extract(input$file1$datapath, ext_search)
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
                         ##Makes it so that the table will only display the first (col_width()) chars.
                         ##See https://rstudio.github.io/DT/options.html
                         ##We want to display at least 30 chars.
                         render = JS(
                           paste0("function(data, type, row, meta) {",
                                  "return type === 'display' && typeof data === 'string' && data.length > ", max(floor(col_width() / 2), 15), " ?",
                                  "'<span title=\"' + data + '\">' + data.substr(0,", max(floor(col_width() / 2), 15), ") + '...</span>' : data;",
                                  "}"))
                       ))))
    }
  })
  
  lockedOntologyError <- function(){
    listOfOntNames <- readLines(ONTOLOGY_LIST_FILE_PATH)
    listOfOntNames <- listOfOntNames[!(listOfOntNames %in% values$ontName)]
    values$RecommendedOntologies <<- values$RecommendedOntologies[!(values$RecommendedOntologies %in% values$ontName)]
    title <- "Error! Ontology locked for download!!"
    content <- tagList()
    content[[1]] <- p("Due to licensing restrictions, the ontology you selected to match terms with is locked for download from BioPortal. Please select a different ontology.")
    content[[2]] <- selectizeInput('newOntologySelector', label = "Select new Ontology:", 
                                   choices = list('Recommended Ontologies' = c("", values$RecommendedOntologies),
                                                  'All Ontologies' = listOfOntNames),
                                   options = list(
                                     placeholder = "Please choose terms above...",
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
  
  # the width of the columns, as determined by the width of the column names
  col_width <- reactive({
    mean(nchar(colnames(values$dataset)))
  }) 
  
  # makes sure that we view only the number of columns that will fit on the screen at any given time. a small screen fits about 75 
  # characters at a time. we don't want to display more than 5 columns at a time.
  move_by <- reactive({
    min(max(floor(75 / col_width()), 1), 5)
  })
  
  observeEvent(input$advance_clicked, {
    start <- min(ncol(values$datasetInput), values$viewingSubset[1] + move_by())
    end <- min(ncol(values$datasetInput), start + (move_by() - 1))
    values$viewingSubset <- c(start, end)
    session$sendCustomMessage(type = "resetValue", "advance_clicked")
  }, ignoreNULL = TRUE)
  
  observeEvent(input$retract_clicked, {
    end <- max(1, values$viewingSubset[2] - move_by())
    start <- max(1, end - (move_by() - 1))
    values$viewingSubset <- c(start, end)
    session$sendCustomMessage(type = "resetValue", "retract_clicked")
  }, ignoreNULL = TRUE)
  
  ## * Load Data -----------------------------------------------------------------------------------------------------
  readInputFile <- function(inFile) {
    fileExt <- paste0(".", file_ext(gsub("\\\\", "/", inFile$datapath)))
    text <- paste0("# Please ensure that your terminology file (", inFile[1],") is in the same directory as this script before executing. Please also make sure that your R console is in the correct working terminal (use setwd() to change to the directory that your files are in).")
    installPackages <- addLibrary(libraryText)
    readInputFileText <<- paste0("datasetInput <- read_", extensionsMap[[fileExt]], "('", inFile$name, "', col_names=FALSE)")
    masterText <<- NULL
    masterText <<- paste0(masterText,  installPackages, "\n\n", librariesTxt, "\n\n", text) 
    do.call(paste0("read_", extensionsMap[[fileExt]]), list(inFile$datapath, "col_names" = FALSE))
  }
  
  # After we upload the file, select the row we want to identify as the column names (and the number of header rows)
  setColNames <- function(startRow, colnameRow) {
    datasetInput <- values$datasetInput
    txt<- paste0(
      "colnameRow <-", colnameRow, "\n",
      "if (colnameRow == 0) {
      newColsNames <- paste(\"Column\", 1:ncol(datasetInput), sep = \"_\")
    } else {
      newColsNames <- datasetInput[",colnameRow,",]
    }
    colnames(datasetInput) = newColsNames
    datasetInput <- datasetInput[",startRow,":nrow(datasetInput),]"
    )
    eval(parse(text=txt))
    masterText <<- paste0(masterText, "\n\n# Set column names and format datasheet\n", readInputFileText, "\n", txt)
    values$dataset <- datasetInput
    
    if(any(is.na(colnames(values$dataset)))){
      colnames(values$dataset)[is.na(colnames(values$dataset))] <- "Null1"
    }
    values$viewingSubset <- c(1, min(move_by(), ncol(values$datasetInput))) #this changes the UI view on the application
    return()
  }
  
  # Input File(header selector must only be set when user first uploads file; otherwise, if the user selects the number of header lines
  #before the table renders, the box flickers back and forth between selection and default)
  observeEvent(input$file1, ignoreInit = T, {
    withProgress(message = "Initializing elements", { #initialize variables so functionality is enabled and user can click between tabs without pushing "next"
      output$inputError <- tryCatch({
        initializeScript()
        values$datasetInput <<- readInputFile(input$file1)
        if(any(is.na(colnames(values$datasetInput)))){
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
      
      if (!is.null(values$dataset)) { # we had previously loaded a file and need to display the new file
        setColNames(2, 1) 
      }
      incProgress(1/9, detail = "header selector")
      incProgress(1/9, detail = "terminology selector")
      incProgress(1/9, detail = "column selector")
      
      output$outputFileName <- renderUI({
        textInput('outputFileName',
                  label = div(
                    "Output File Name (without extension):",
                    helpButton("Enter a name for the output file (do not include extension).")
                  ),
                  value = paste0(
                    substr(input$file1, 0, (nchar(input$file1) - if (extension() == ".xlsx") 5 else 4)), "_standardized"
                  ))
      })
      incProgress(1/9, detail = "file name selector")
      output$extensionSelector <- renderUI({
        select_ext <- if (grepl("xls", extension())) ".xlsx" else extension()
        selectInput('extension', label = div("Select Extension:", helpButton("Select an extension for the output file.")), choices = setdiff(names(extensionsMap), c(".xls", ".txt")), selected = select_ext) #.xls
      })
      incProgress(1/9, detail = "extension selector")
      incProgress(1/9, detail = "edit data selector")
    })
  })
  
  # When the header pops up after you upload the file
  observeEvent(input$header, {
    numeric_header <- as.numeric(input$header)
    if (numeric_header != 1) {
      disable("header")
      values$headerDisabled <- TRUE
    }
    setColNames(numeric_header + 1, if (numeric_header > 0) 1 else 0) 
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
      selection = list(mode="single", selected=c(1)),
      options = list(dom = "t", scrollX = '300px', ordering = FALSE)
    )
  })
  
  # This changes the dataset if new rows are selected
  observeEvent(input$headerPreview_rows_selected, {
    if(any(is.na(colnames(values$datasetInput)))){
      colnames(values$datasetInput)[is.na(colnames(values$datasetInput))] <- "Null3"
    }
    setColNames(as.numeric(input$header) + 1, input$headerPreview_rows_selected)
  }, ignoreNULL = TRUE)
  
  # ** BioPortal Access (Download Ontologies)
  output$ontologySelector <- renderUI ({
    if(!is.null(input$file1)) {
      ## List of Ontology Names Recommender   
      # Pop up window informs the user that accessing info from BioPortal will take awhile
      tryCatch({
        res <- R.utils::withTimeout(  { 
          showModal(modalDialog(title = "Loading Ontologies from BioPortal",
                                p("To help you standardize your data, we are accessing the entire list of ontologies and specific ontologies from ",
                                  (a(href = 'https://bioportal.bioontology.org/', 'BioPortal')), " recommended for your dataset. 
                                  Depending on your internet connection and the last time you used Good Nomen, this could take longer than a minute.
                                  Thank you for your patience."),
                                withSpinner(" ", type = SPINNER_TYPE, proxy.height = "150px"), footer = NULL, easyClose = F))
          
          # Check to make sure OntologyList exists
          if(!file.exists(ONTOLOGY_LIST_FILE_PATH)){
            file.create(ONTOLOGY_LIST_FILE_PATH)
            # Set an arbitrarily old date as the default
            Sys.setFileTime(ONTOLOGY_LIST_FILE_PATH, "2020-01-01")
          }
          # Get the last date modified from a file and see if it's been 7 days
          lastRunDate <- file.mtime(ONTOLOGY_LIST_FILE_PATH)
          dateToday <- Sys.Date()
          dateDif <- as.Date(strptime(dateToday, "%Y-%m-%d"))-as.Date(strptime(lastRunDate,"%Y-%m-%d"))
          diffNum <- as.numeric(dateDif)
          
          # If more than 7 days, download full ontology list from BioPortal. Else, read file
          myContent <- NULL
          if (diffNum > DAYS_SINCE_DOWNLOAD){ 
            tryCatch({
              res <- R.utils::withTimeout({
                myContent <- RJSONIO::fromJSON(paste0("http://data.bioontology.org/ontologies?apikey=", API_KEY))
              }, timeout = TIMEOUT_TIME)
            }, TimeoutException = function(ex) {
              timeOutError()
            }, finally = {
              if(is.null(myContent)){
                removeModal()
                showModal(modalDialog(title = "BioPortal Unavailable for Access",
                                      p("BioPortal seems to be down, please check ",
                                        (a(href = 'https://bioportal.bioontology.org/', 'BioPortal')), " to see if it is working. You may need to try again later. We apologize for the inconvenience."),
                                      footer = NULL,
                                      easyClose = F))
              }
            })
            
            ContentDataFrame <<- data.frame(t(sapply(myContent,c)))
            ContentDataFrame$nameAndAcronymn = paste(ContentDataFrame$acronym, ContentDataFrame$name) #Makes a column with both acronym and name
            listOfOntNames <<- ContentDataFrame[, ncol(ContentDataFrame)] # This accesses the last column of the dateframe
            write.table(listOfOntNames, file = ONTOLOGY_LIST_FILE_PATH, append= FALSE, quote = FALSE,
                        row.names = FALSE, col.names = FALSE)
          }
          else{
            listOfOntNames <- readLines(ONTOLOGY_LIST_FILE_PATH)
            listOfOntNames <<- lapply(listOfOntNames, noquote)
          }
          
          # Make a tibble, so later on when you have the three recommended ontology acronyms, you can filter to find their full names. 
          print("got here1")
          ontologyTibble <- as_tibble(listOfOntNames)
          ontologyTibble <- separate(ontologyTibble, value, into =  c("Acronym", "FullName"), sep="\\s", extra = "merge")
          
          #Access recommended ontologies through Bioportal
          if(any(is.na(values$datasetInput))){
            values$datasetInput[is.na(values$datasetInput)] <- "Null"
          }
          rURL <- getRecommendedTerms(values$datasetInput)
          internetTester = curl::has_internet()
          if(!internetTester){
            # If the URL is broken,try again a few times
            for(i in 1:NUM_TEST_TIMES){
              rURL <- getRecommendedTerms(values$datasetInput)
              if (url.exists(rURL)){
                break;
              }
            }
            RecommendedOntologies <- ""; #Error and try again later
          }
          else {
            # Get the acronym for the top three recommended Ontologies 
            tryCatch({
              res <- R.utils::withTimeout(  { 
                DataFrameRecommend <- jsonlite::fromJSON(rURL) 
                Recommenderdf <- as.data.frame(t(sapply(DataFrameRecommend,c)))
              }, timeout = TIMEOUT_TIME)
            }, TimeoutException = function(ex) {
              timeOutError()
            })
          print("got here2")
            recTibble <- as_tibble(Recommenderdf)
            if(ncol(recTibble) > 1){
          print("got here3")
              recTibble <- as_tibble(Recommenderdf)
              recTibble <- recTibble %>% 
                select(ontologies) %>% 
                unnest(ontologies) %>% 
                unnest(ontologies) %>% 
                pull(acronym) 
              
              # If there are fewer than three elements in the recommended ontology, set the NUM_RECOMMENDED_ONTOLOGIES to the size of the list created
              if (length(recTibble) < NUM_REC_ONTO){NUM_REC_ONTO <<- length(recTibble)}
              
              RecommendedOntologies <- recTibble[1:NUM_REC_ONTO] 
              for (i in 1:NUM_REC_ONTO){
                thisTerm <- filter(ontologyTibble, Acronym == RecommendedOntologies[i]) %>% select(FullName)
                RecommendedOntologies <- replace(RecommendedOntologies, i, paste(RecommendedOntologies[i], unlist(unname(thisTerm)), " ", collapse = " "))
              }
            } else{
              RecommendedOntologies <<- ""
            }
          }
          values$RecommendedOntologies <<- RecommendedOntologies
        }, timeout = TIMEOUT_TIME)
      }, TimeoutException = function(ex) {
        timeOutError()
      })
      removeModal()
      
      selectizeInput('ontologySelector',
                     label = div(
                       "Select Ontology:",
                       helpButton("Select an ontology from BioPortal.")),
                     choices = list('Recommended Ontologies' = c("", RecommendedOntologies),
                                    'All Ontologies' = listOfOntNames),
                     options = list(placeholder = "Select ontology or start typing...",
                                    closeAfterSelect = TRUE))
    } 
  })
  
  # ** Next Button (and Load Ontoloy) 
  output$page1Next <- renderUI({
    values$ontName <<- input$ontologySelector
    if(input$ontologySelector != "" && !is.null(input$ontologySelector)) {
      actionButton("nextbutton", "Next", style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")
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
  
  output$upload_preview <- renderDT({
    dataPreview()
    myDataTable <- dataPreview()
  })
  
  #This observe event handles downloading the ontology, checking to see if it's locked, and moving forward to the next page
  observeEvent(input$buttonLoadThenNext, {
    OntologyLstAcr <- strsplit(values$ontName, " ")
    values$OntologyAcronym <<- OntologyLstAcr[[1]][1]
    
    values$manualSaveMessage <- NULL
    showModal(modalDialog(title = "Loading Standardized Terms from BioPortal.",
                          p("To help you standardize your data, we are pulling all standardized terms from BioPortal so you have the choice to select them.",
                            "If you would like to use this functionality on your own browser, please follow ",
                            (a(href = 'https://bioportal.bioontology.org/annotator', 'this link')), " and input the terminology you wish to change. 
                            Depending on your internet connection, this could take longer than a minute.", 
                            "Thank you for your patience."),
                          withSpinner(" ", type = SPINNER_TYPE, proxy.height = "150px"),
                          footer = NULL,
                          easyClose = F))
    
    # Get the last date modified from a file and see if it's been 7 days
    fileName <- paste0(TEMP_DIR_PATH, "/", values$OntologyAcronym, "_Ontology.txt")
    if (file.exists(fileName)){
      lastRunDate <- file.mtime(fileName)
      dateToday <- Sys.Date()
      dateDif <- as.Date(strptime(dateToday, "%Y-%m-%d"))-as.Date(strptime(lastRunDate,"%Y-%m-%d"))
      diffNum <- as.numeric(dateDif)
    }
    else{
      diffNum <- 9
    }
    
    # If more than 7 days, download full ontology list from BioPortal. Else, read file
    if (diffNum > DAYS_SINCE_DOWNLOAD){
      
      downloadURL <- sprintf(paste("http://data.bioontology.org/ontologies/", values$OntologyAcronym, "/download?download_format=csv&display_links=false&apikey=", API_KEY, sep=""))
      if (!url.exists(downloadURL)) {
        removeModal()
        ## SITUATION: ONTOLOGY IS LOCKED FOR DOWNLOAD & it's never been downloaded before
        lockedOntologyError()
      }else{
        tryCatch({
          res <- R.utils::withTimeout(  {
            tmpFilePath <- paste0(tempfile(), ".csv.gz")
            myFile <- download.file(downloadURL, tmpFilePath, quiet = FALSE, mode = "wb")
            myFile <- read_csv(tmpFilePath)
            unlink(tmpFilePath)
          },  timeout = TIMEOUT_TIME)
        }, TimeoutException = function(ex) {
          timeOutError()
        })
 
        #format column names to retrieve a list of preferred names stored in the ontology
        colnames(myFile) <- sub("_", " ", colnames(myFile))
        colnames(myFile) <- tolower(colnames(myFile))
        myFile <- myFile[, !duplicated(colnames(myFile))] #This was added because the MEDO ontology had duplicate columns and wouldn't pull the preferred name because of it
        # There are a few options of what the preferred name can be such as "preferred name" and "label"
        if ("preferred name" %in% colnames(myFile)){
          values$TOTAL_TERM_LIST <- sort(pull(myFile, var = "preferred name"))
        } else if ("label" %in% colnames(myFile)){
          values$TOTAL_TERM_LIST <- sort(pull(myFile, var = "label"))
        } else {
          values$TOTAL_TERM_LIST <- sort(pull(myFile, var = "preferred label"))
        }
        #TODO make an error message to show them inconsistencies in downloaded data
        write.table(values$TOTAL_TERM_LIST, file = fileName, append= FALSE, quote = FALSE,
                    row.names = FALSE, col.names = FALSE)
        removeModal()
        updateTabsetPanel(session, 'tabs', selected = 'editTable')
      }
    }else{
      myList <- readLines(fileName)
      myList <- unlist(lapply(myList, noquote))
      values$TOTAL_TERM_LIST <<- myList
      removeModal()
      updateTabsetPanel(session, 'tabs', selected = 'editTable')
    }
  })
  
  # * Edit Data ---------------------------------------------------------------
  
  output$selectedOntology <- renderUI({
    OntologyLstAcr <- strsplit(values$ontName, " ")
    values$OntologyAcronym <<- OntologyLstAcr[[1]][1]
    urlToOpen <- paste0("https://bioportal.bioontology.org/ontologies/",values$OntologyAcronym)
    HTML(paste("<b>Selected Ontology: </b>",  (a(href = urlToOpen, values$ontName)), collapse="<BR>"))
  })
  
  observeEvent(input$changeOntology, {
    title <- "Change the Ontology"
    
    content <- tagList()
    content[[1]] <- selectizeInput('newOntologySelector', label = "Select new Ontology:", 
                                   choices = list('Recommended Ontologies' = c("", values$RecommendedOntologies),
                                                  'All Ontologies' = listOfOntNames),
                                   options = list(
                                     placeholder = "Please choose terms above...",
                                     closeAfterSelect = TRUE))
    content[[3]] <- actionButton('resetAndSave', label = "Save and Reset Ontology")
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
      label = "Select column to standardize:", 
      choices = c("", columns()),
      selected = values$lastSelectedEditColumn,
      options = list(placeholder = 'Select column or start typing...',
                     closeAfterSelect = TRUE))
  })
  
  # ** Auto-match ---------------------------------------------------------------
  observeEvent(input$automatch, ignoreInit = T, {
    # The "withProgress" adds a progress bar while automatch is getting ready. The functions "inc()" in this section increment the progress bar
    withProgress(message = 'Automatching Data', value = 0, {
      values$automatchResult <- list()
      
      # Disable the buttons while the matches are loading.
      disable("editThisColumn") # select box
      disable("automatch") # auto-match button
      disable("manual") # standardize manually button
      disable("editBack") # back button
      disable("editNext") # next button
      
      #if the column is not selected, through an error. Otherwise, continue
      if (is.null(input$editThisColumn) || input$editThisColumn == "") {
        title <- "Error"
        content <- columnNotSelectedMessage
      } else {
        title <- "Review Matches"
        
        # Create API Url to run BioPortal's Annotator
        incProgress(.1)
        uniqueTerms <<- unique(values$dataset[[input$editThisColumn]])
        uniqueTerms <- paste(unlist(uniqueTerms), collapse = ' ')
        sURL <<- sprintf("http://data.bioontology.org/annotator?text=%s&apikey=%s",gsub(" ", "", URLencode(uniqueTerms, reserved = TRUE)), API_KEY)
        sURL <<- paste0(sURL, "&ontologies=",values$OntologyAcronym, "&display_links=false&display_context=false&include=prefLabel")
        
        # Make a data frame from the JSON generated by Annotator (with a timer for timeout)
        if (url.exists(sURL) == TRUE) {
          tryCatch({
            res <- R.utils::withTimeout(  {
              incProgress(.1)
              RDFFile <<- getURL(sURL)
              DataFrameAnnotator <- RJSONIO::fromJSON(sURL)}, timeout = TIMEOUT_TIME)
          }, TimeoutException = function(ex) {
            timeOutError()
          })
          
          AutoMatchdf <- as.data.frame(t(sapply(DataFrameAnnotator,c)))
          myDF <- data.frame("Current Term" = NA, "Standardized Term" = NA, "Accept" = TRUE, check.names = FALSE)
          
          n <- nrow(AutoMatchdf) + 2
          # Loop through the dataframe (df) and extract Standardized and Current Names
          for (i in 1:nrow(AutoMatchdf)){
            incProgress(1/n)
            StandardName <- unlist(lapply(AutoMatchdf[i, 1], function(l) l[[1]]), recursive = FALSE) #grab the standardized name
            CurrentName <- unlist(unlist(AutoMatchdf[i,3], recursive=FALSE), recursive = FALSE)
            myListofCurrentNames <- unique(CurrentName[grepl("text", names(CurrentName))])
            myListofCurrentNames <- stringi::stri_trans_totitle(myListofCurrentNames)
            
            #Check to make sure there are names to standardize
            if(is.na(myListofCurrentNames[1])){
              break;
            }
            
            #Sometimes bioportal checks and returns substrings of elements. If this is the case, this conditional changes the substring back to it's full form
            if (length(myListofCurrentNames) != 0 || is.na(pmatch(tolower(myListofCurrentNames), tolower(values$dataset[[input$editThisColumn]]))) == FALSE) {
              index <- pmatch(tolower(myListofCurrentNames), tolower(values$dataset[[input$editThisColumn]]))
              myListofCurrentNames = values$dataset[[input$editThisColumn]][index]
            }
            
            # If the term is already in the data frame, don't add it (must come after the code above) Maybe later, let the user pick which term they would rather pick
            if(myListofCurrentNames %in% myDF[,1]){
              next;
            }
            
            if (length(myListofCurrentNames) > 1){
              lapply(myListofCurrentNames, function(x) {
                myDF <<- rbind(myDF, list(x, StandardName, "TRUE"))
              })
            }
            else{
              myDF <- rbind(myDF, list(myListofCurrentNames, stringi::stri_trans_totitle(StandardName), "TRUE"), stringsAsFactors = FALSE)
            }
          }
          myDF <- myDF[-1,] #there is a random column made in the last step so this strips that back down
          values$myDF <- myDF[!(myDF$`Current Term`==myDF$`Standardized Term`),]
          
          # Output the table
          if (length(AutoMatchdf) > 0) {
            
            if (length(myDF) > 0) {
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
              content[[6]] <- rHandsontableOutput("reviewTable", width = "100%")
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
        }
        else{
          content <- p("Server Error 404: Automatch couldn't generate. Try using a smaller file or just using manual selection.")
        }
      }
    })
    showModal(
      modalDialog(
        content,
        title = title,
        footer = modalButton("Close"),
        size = "l"
      )
    )
    
    # Enable the buttons again.
    enable("editThisColumn") # select box
    enable("automatch") # auto-match button
    enable("manual") # standardize manually button
    enable("editBack") # back button
    enable("editNext") # next button
  })
  
  # ** ** Auto-match Supporting Events 
  observeEvent(input$selectAll, ignoreInit = T, {
    values$myDF$Accept <- rep(TRUE, nrow(myDF))
  })
  
  observeEvent(input$deselectAll, ignoreInit = T, {
    values$myDF$Accept <- rep(FALSE, nrow(myDF))
  })
  
  output$reviewTable <- renderRHandsontable({
    if (!is.null(values$myDF)) {
      # these lines make the table display responsive to the select all and deselect all buttons
      input$selectAll
      input$deselectAll
      rhandsontable(values$myDF, useTypes = F, selectCallback = T, stringsAsFactors = F, 
                    rowHeaders = NULL) %>%
        hot_col(col = "Current Term", readOnly = T, type = "text") %>%
        hot_col(col = "Standardized Term", readOnly = T, type = "text") %>%
        hot_col(col = "Accept", type = "checkbox")
    }
  })
  
  # ** ** Auto-match Save 
  observeEvent(input$automatchSave, ignoreInit = T, {
    if (length(which(as.logical(values$myDF$Accept))) > 0) {
      # Change dataset table values to reflect changes made by editor
      values$lastSelectedEditColumn <- input$editThisColumn
      accepted <- hot_to_r(input$reviewTable)
      accepted_list <- accepted$'Standardized Term'[as.logical(accepted$Accept)]
      
      # the if statement checks to make sure that at least 1 term has been selected to save. Else, don't change any terms.
      if(length(accepted_list) > 0 ) {
        names(accepted_list) <- paste0("^", accepted$`Current Term`[as.logical(accepted$Accept)], "$")
        columnNameOfChangedTerms <- input$editThisColumn
        datasetInput <- values$dataset
        
        #This tells the R Script which terms we want to change and what we want to change them to 
        names <- paste0("^", accepted$`Current Term`[as.logical(accepted$Accept)], "$")
        masterText <<- paste0(masterText, "\n\n# Changing the dataset based on AutoMatch\n", "columnNameOfChangedTerms <- \"", columnNameOfChangedTerms, 
                              "\"\n", "accepted_list <- c(", paste0("'", unname(accepted_list), "'", collapse = ", "), ")",
                              "\n","names_accepted_list <- c(", paste0("'", names, "'", collapse=", "), ")",
                              "\nnames(accepted_list) <- names_accepted_list")
        txt <- "datasetInput[[columnNameOfChangedTerms]] <- str_replace_all(datasetInput[[columnNameOfChangedTerms]], accepted_list)"
        eval(parse(text = txt))
        values$dataset <- datasetInput
        masterText <<- paste0(masterText, "\n", txt)
      }
    }
    removeModal()
  })
  
  observeEvent(input$automatchClose, ignoreInit = T, {
    removeModal()
  })
  
  # ** Manual Standardization (Functions) ----------------------------------------
  
  #This is for the save/confirm button
  standardizeManually <- function() {
    newData <- if (is.null(input$newData) || input$newData == "") NA else input$newData
    editThisColumn <- gsub("\"", "\\\\\"", input$editThisColumn)
    m <- length(input$editData)
    withProgress(message = "Standardizing", {
      values$lastSelectedEditColumn <- input$editThisColumn
      for (item in input$editData) {
        incProgress(1/m, detail = item)
        datasetInput <- values$dataset
        txt <- paste0("datasetInput[[editThisColumn]][datasetInput[[editThisColumn]] == \"", item, "\"] <- \"", newData, "\"")
        eval(parse(text = txt))
        values$dataset <- datasetInput
        masterText <<- paste0(masterText, "\n\n", "#Manual Standardization\neditThisColumn <- \"", editThisColumn, "\"\n", txt)
      }
    })
    values$manualSaveMessage <- paste0("Standardizations for column \"", input$editThisColumn, "\" have been saved. ",
                                       "Click \"Close\" to continue to the next step.")
    updateCheckboxInput(session, "makeNA", value = FALSE)
    updateSelectizeInput(session, "editData", selected = NULL)
    updateSelectizeInput(session, "newData", selected = "")
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
  
  ## ** MANUAL MODAL 1 
  observeEvent(input$manual, {
    withProgress(message = "Getting Manual Standardization ready", value = 0, {
      incProgress(0.7)
      downloadURL <- sprintf(paste("http://data.bioontology.org/ontologies/", values$OntologyAcronym, "/download?download_format=csv&display_links=false&apikey=", API_KEY, sep=""))
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
      content[[5]] <- div(actionButton('nextManualModal', 'Next'), style="float:right")
      content[[6]] <- br()
      content[[7]] <- br()
      incProgress(0.1)
      showModal(
        modalDialog(
          content,
          title = title,
          footer = NULL,
          size = "l",
          easyClose = TRUE)
      )
    })
  })
  
  observeEvent(input$nextManualModal, {
    
    ## ** MANUAL MODAL 2 
    #Download data and recommendations from BioPortal
    values$manualSaveMessage <- NULL
    showModal(modalDialog(title = "Loading Standardized Terms from BioPortal.",
                          p("To help you standardize your data, we are pulling all standardized terms from BioPortal so you have the choice to select them.",
                            "If you would like to use this functionality on your own browser, please follow ",
                            (a(href = 'https://bioportal.bioontology.org/annotator', 'this link')), " and input the terminology you wish to change.
                            Depending on your internet connection, this could take longer than a minute.",
                            "Thank you for your patience."),
                          withSpinner(" ", type = SPINNER_TYPE, proxy.height = "150px"),
                          footer = NULL,
                          easyClose = F))
    
    # Recommender(recommend three terms)
    myList <- paste0(unlist(input$editData), collapse = ', ')
    recommendedTerms <- URLencode(myList, reserved = TRUE)
    annURL <- sprintf("http://data.bioontology.org/annotator?text=%s&ontologies=%s&apikey=%s&display_links=false&exclude_synonyms=false&display_context=false&include=prefLabel", recommendedTerms,values$OntologyAcronym, API_KEY)
    
    values$recTermsList <<- NULL
    if (url.exists(annURL) == TRUE) {
      tryCatch({
        res <- R.utils::withTimeout(  {
          DataFrameAnn <- jsonlite::fromJSON(annURL)
        },  timeout = TIMEOUT_TIME)
      }, TimeoutException = function(ex) {
        timeOutError()
      })
      frequencyTable <- table(DataFrameAnn$annotatedClass$prefLabel) #convert this list to a frequency table
      frequencyTable <- frequencyTable[order(frequencyTable, decreasing=T)]
      
      if (NUM_REC_MANUAL > length(frequencyTable)) {
        recTermsList <- names(head(frequencyTable, n=length(frequencyTable)))
        recTermsList <- toString(recTermsList)
        values$recTermsList <<- tools::toTitleCase(recTermsList)
      }
      else {
        recTermsList <- names(head(frequencyTable, n=NUM_REC_MANUAL))
        values$recTermsList <<- tools::toTitleCase(recTermsList)
      }
    }
    
    ## ** MANUAL MODAL 3 
    # Modal #3, Let the user select the standardized terms they want to represent their data
    title <- "Standardizing Selected Terms"
    
    content <- tagList()
    content[[1]] <- p(
      paste(
        "Pick a term that represents all the terms you selected. ",
        "The options in the second box are from the selected terminology. By default, the program will pick the top rated term that connects all the data. 
         To apply changes to your data, press \"Save.\"",
        "All occurrences of the terms in the first box will be replaced with the selected terminology term."
      )
    )
    content[[2]] <- br()
    content[[3]] <- HTML(paste('Terms that you selected with a common meaning: ', myList))
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
      actionButton("saveConfirmBtn", "Save")
    )
    content[[10]] <- actionButton('manualClose', label = "Close", class = "secondary_button")
    content[[11]] <- saveConfirmModal
    content[[12]] <- br()
    content[[13]] <- textOutput("savedMessage")
    content[[14]] <- tags$head(tags$style("#savedMessage {color:green}"))
    
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
    if (length(values$recTermsList) > 0){
      updateSelectizeInput(session, 'newData', choices = list('Recommended Terms' = c(values$recTermsList, ""),
                                                              'All Terms' = c("", values$TOTAL_TERM_LIST)), server = TRUE)
    } else {
      updateSelectizeInput(session, 'newData', choices = list(values$TOTAL_TERM_LIST), server = TRUE)
    }
  })
  
  # Populating Manual  Modal
  output$editDataSelector <- renderUI({
    selectizeInput('editData', label = "Enter terms that have a common meaning:", choices = unique(str_trim(values$dataset[[input$editThisColumn]][order(values$dataset[[input$editThisColumn]])])), multiple = T,
                   options = list(placeholder = "Select a term or start typing..."))
  })
  
  # Use a bounding variable to make sure we only get the suggestions when we need them.
  getSuggestions <- function(toFind, boundingVar = "TRUE") {
    allSuggestions <- c("i", "k")
    
    if (eval(parse(text = boundingVar))) {
      if (length(allSuggestions) > 0) {
        list('All Terms from the Selected Ontology' = TOTAL_TERM_LIST) 
      } else {
        list("All Terms from the Selected Ontology" = TOTAL_TERM_LIST)
      }
    }
  }
  
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
    OntologyLstAcr <- strsplit(input$newOntologySelector, " ")
    values$OntologyAcronym <<- OntologyLstAcr[[1]][1]
    updateSelectizeInput(session, 'ontologySelector', selected = values$ontName) #This updates the first drop down menu with your new selection
    
    showModal(modalDialog(title = "Loading New Ontology.",
                          p("Please be patient, this shouldn't take long."),
                          withSpinner(" ", type = SPINNER_TYPE, proxy.height = "150px"),
                          footer = NULL,
                          easyClose = F))
    downloadURL <- sprintf(paste("http://data.bioontology.org/ontologies/", values$OntologyAcronym, "/download?download_format=csv&display_links=false&apikey=", API_KEY, sep=""))
    
    if (!url.exists(downloadURL)) { ## THE ONTOLOGY IS LOCKED FOR DOWNLOAD
      removeModal()
      lockedOntologyError()
    } else{
      removeModal()
    }
  })
  
  observeEvent(input$manualClose, {
    #toggleModal(session, "saveConfirm", toggle = "close") I don't know why this has to be commented out, but when it's commented out the app works
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
      label = "Specify column to rename:", 
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
      
      showModal(modalDialog(title = "Loading Recommended Column Names.",
                            p("To help you standardize your data, we are accessing recommended Column Names from ",
                              (a(href = 'https://bioportal.bioontology.org/annotator', 'BioPortal')), ". Feel free to write your own column names as well.",  
                              "Depending on your internet connection and the last time you used Good Nomen, this could take longer than a minute.", 
                              "Thank you for your patience."),
                            withSpinner(" ", type = SPINNER_TYPE, proxy.height = "150px"),
                            footer = NULL,
                            easyClose = F))
      
      # Get recommended column names from BioPortal
      aURL <<- sprintf("http://data.bioontology.org/annotator?text=%s&apikey=%s", input$editColumn, API_KEY)
      aURL <<- paste0(aURL, "&display_links=false&display_context=false&include=prefLabel")
      newColNames <- c("")
      
      if (url.exists(aURL) == TRUE) {
        tryCatch({
          res <- R.utils::withTimeout(  {
            DataFrameAnnotator <- RJSONIO::fromJSON(aURL)
          },  timeout = TIMEOUT_TIME)
        }, TimeoutException = function(ex) {
          timeOutError()
        })
        newColNamesdf <- as.data.frame(t(sapply(DataFrameAnnotator,c)))
        myDF <- data.frame("Current Term" = NA, "Standardized Term" = NA, "Accept" = TRUE, check.names = FALSE)
        
        # Loop through the dataframe (df) and extract the new recommended column names
        for (i in 1:3){
          StandardName <- unlist(lapply(newColNamesdf[i, 1], function(l) l[[1]]), recursive = FALSE) #grab the standardized name
          newColNames <- append(newColNames, stringi::stri_trans_totitle(StandardName))
        }
      }
      removeModal()
      
      updateSelectizeInput(session, inputId = 'newColumn', choices = list('Recommended Terms' = c(newColNames, ""),
                                                                          'All Terms' = c("", values$TOTAL_TERM_LIST)), server = TRUE,
                           options = list(placeholder = 'Select a term or start typing...', 
                                          create = TRUE, maxItems = 5, maxOptions = 100,
                                          closeAfterSelect = TRUE))
      enable("newColumn")
    }
  })
  
  # Update the selectize input from the server's end (this is for ALL the terms in the Ontology, sometimes as large as 150,000 terms)
  observeEvent(input$nextManualModal, {
    if (length(values$recTermsList) > 0){
      updateSelectizeInput(session, 'newData', choices = list('Recommended Terms' = c(values$recTermsList, ""),
                                                              'All Terms' = c("", values$TOTAL_TERM_LIST)), server = TRUE)
    } else {
      updateSelectizeInput(session, 'newData', choices = list(values$TOTAL_TERM_LIST), server = TRUE)
    }
  })
  
  #Rename Column 
  observeEvent(input$columnRename, ignoreInit = T, {
    #display warning if user does not select column to rename and new column name
    if (input$editColumn == "" | input$newColumn == "") {
      toggleModal(session, 'columnModal', toggle = "open")
    }
    #display warning if new name is already the name of a column
    else if (any(input$newColumn %in% columns())) {
      toggleModal(session, 'equalModal', toggle = "open")
    }
    else {
      datasetInput <- values$dataset
      newColumn <- gsub("\"", "\\\\\"", input$newColumn)
      txt <<- paste0("\n#Edit Column Name\n",
                     "editColumn <- \"", input$editColumn, "\"\n",
                     "newColumn <- \"", newColumn, "\"",
                     "\ncolnames(datasetInput)[which(colnames(datasetInput) == editColumn)] <- newColumn")
      eval(parse(text = txt))
      values$dataset <- datasetInput
      masterText <<- paste0(masterText, "\n", txt)
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
      output #this ensures that the buttons will appear
    }
  })
  
  #Download Output File
  output$editReport <- downloadHandler(
    filename = function() {
      fileName <- if (input$outputFileName == "") "shiny_output" else input$outputFileName
      this_extension <- if (nchar(input$extension) == 0) extension() else input$extension
      return(paste0(fileName, this_extension))
    }, 
    content = function(file) {
      this_extension <- if (nchar(input$extension) == 0) extension() else input$extension
      to_write <- if (grepl("xls", this_extension)) "xlsx" else extensionsMap[[this_extension]]
      return(do.call(paste0("write_", to_write), list(values$dataset, file, "col_names" = (input$header != "0"))))
    }
  )
  
  #Download R script. Format is downloadHandler(filename, content)
  output$script1 <- downloadHandler(
    filename = function() {
      paste0(input$outputFileName, "_R_script.R")
    }, content = function(file) {
      fileName <- if (input$outputFileName == "") "shiny_output" else input$outputFileName
      this_extension <- if (nchar(input$extension) == 0) extension() else input$extension
      fullFileName <- paste0(fileName, ".R")
      
      to_write <- if (grepl("xls", this_extension)) {
        libraryText <<- c(libraryText, "writexl")
        "xlsx"
      } else {
        libraryText <<- libraryText[!str_detect(libraryText, "writexl")]
        extensionsMap[[this_extension]]
      }
      masterText <<- paste0(masterText, "\n", "\n# Save File\n",
                            "file <- '", paste0(input$outputFileName, this_extension),
                            "'\n", paste0("write_", substring(this_extension, 2)), "(datasetInput, file)",
                            "\nprint('Your file has been successfully saved and modified with the name: ",input$outputFileName, "')")
      write.table(masterText, file, row.names = F, col.names = F, quote = F)
      showNotification(paste0("Your file was successfully saved."))
    })
  
  output$saveDataPreviewText <- renderText({
    if (is.null(values$dataset)) {
      "After you have uploaded a file, a preview of your data will appear here."
    } else { NULL }
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
