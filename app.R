library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(shinyBS)
library(shinycssloaders)
library(rhandsontable)

#set file upload limit to 50 MB
options(shiny.maxRequestSize=50*1024^2)

#define function for writing Rscript 
outputText <- NULL
writeToScript <- function(text) {
  outputText <<- rbind(outputText, text)
}

#define function for commenting Rscript 
commentify <- function(myStr, header = F) {
  numChars <- 100
  comment <- paste0("# ", myStr, " ")
  comment <- paste0(comment, paste(rep("-", numChars - nchar(comment)), collapse = ""))
  if (header == F) {
    writeToScript("\n")
  }
  writeToScript(comment)
}

commentify("Good Nomen", T)
writeToScript("# Please ensure that your data file and thesaurus file (if using an uploaded thesaurus) are in the same directory as this script before executing.")
writeToScript("library(readr)")
writeToScript("library(dplyr)")
writeToScript("library(stringr)")
writeToScript("library(readxl)")
writeToScript("library(writexl)")

#define function for tooltips 
helpButton <- function(message = "content", placement = "right") {
  return(tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover"))
}

#define function for spinner (loading widget) 
spinner <- function(table) {
  withSpinner(tableOutput(table), color = "#bdbdbd", size = 0.5)
}


# UI ----------------------------------------------------------------------
ui <- navbarPage(title = 'Good Nomen', id = 'tabs',
                 

# Load Data ---------------------------------------------------------------
                 tabPanel('Load Data', sidebarLayout(
                           sidebarPanel(textOutput("uploadInstructions"),
                                        br(),
                                        fileInput(inputId = "file1", label = "Choose Input File:", 
                                                  multiple = FALSE, accept = c(".tsv", ".txt", ".csv", ".xls", ".xlsx"), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                                        textOutput("inputError"), tags$head(tags$style("#inputError {color: red;}")),
                                        uiOutput("headerSelector"),
                                        uiOutput("thesaurusSelector"), 
                                        conditionalPanel(condition = "input.whichThesaurus == 'Upload a thesaurus'", uiOutput("inputUploadedThesaurus")),
                                        #warning if user uploads a thesaurus with less than 2 columns
                                        bsModal('badThesaurusModal', title = "Error", trigger = 'input.inputUploadedThesaurus',
                                                textOutput('badThesaurusNote'), tags$head(tags$style("#badThesaurusModal {color: red;}")), tags$head(tags$style("#badThesaurusNote {color: black;}"))),
                                        #warning if user does not upload a thesaurus
                                        bsModal('noThesaurusModal', title = "Error", trigger = 'input.inputUploadedThesaurus',
                                                textOutput('noThesaurusNote'), tags$head(tags$style("#noThesaurusModal {color: red;}")), tags$head(tags$style("#noThesaurusNote {color: black;}"))),
                                        #'input.header' relies on the condition that a file has been loaded, so this conditional panel prevents the action button from appearing before a file has been loaded
                                        conditionalPanel(condition = "(input.header && input.whichThesaurus != 'Upload a thesaurus')", #  || (input.inputUploadedThesaurus) && input.whichThesaurus == 'Upload a thesaurus' (input.uploadedThesaurus)
                                                         actionButton("button", "Next", style="color: #fff; background-color: #2ca25f; border-color: #2ca25f")),
                                        textOutput("error"), tags$head(tags$style("#error {color: red;}")),
                                        conditionalPanel(condition = "input.whichThesaurus == 'Upload a thesaurus'", 
                                                         uiOutput("uploadThesaurusButton"))),
                           mainPanel(div(id = "startTable", conditionalPanel(condition = 'input.header', spinner(('table'))))))),
                 

# Update Column Names -----------------------------------------------------
                 tabPanel('Update Column Names', value = 'updateColumnNames',
                          sidebarPanel(width = 4,
                                       textOutput('columnNote'),
                                       br(),
                                       uiOutput("editColumnSelector"),
                                       conditionalPanel(condition = 'input.editColumn', selectizeInput('newColumn', label = "Select new column name:", choices = NULL, options = list(placeholder = 'Please wait for options to load...', create = T))),
                                       #warning if user does not select column to rename and new column name
                                       bsModal('columnModal', title = "Error", trigger = 'input.newColumn',
                                               textOutput('columnErrorNote'), tags$head(tags$style("#columnModal {color: red;}")), tags$head(tags$style("#columnErrorNote {color: black;}"))),
                                       #warning if user selects a new column name that is already being used as a column name
                                       bsModal('equalModal', title = "Error", trigger = 'input.newColumn',
                                               textOutput('equalNote'), tags$head(tags$style("#equalModal {color: red;}")), tags$head(tags$style("#equalNote {color: black;}"))),
                                       actionButton('columnRename', "Rename", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       actionButton('columnSubmit', "Next", style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")),
                          mainPanel(spinner('secondViewTable'))),
                 

# Edit Data ---------------------------------------------------------------
                 tabPanel('Edit Data', value = 'editTable', 
                          sidebarPanel(width = 4, 
                                       textOutput('editNote'),
                                       br(),
                                       uiOutput("columnSelector"),
                                       uiOutput("editThisColumnSelector"),
                                       bsModal('matchColumnModal', title = "Error", trigger = 'input.editThisColumn',
                                               textOutput('matchColumnNote'), tags$head(tags$style("#matchColumnModal {color: red;}")), tags$head(tags$style("#matchColumnNote {color: black;}"))),
                                       actionButton('automatch', label = div("Automatch", helpButton("Matches will be found based on synonyms in the NCI Thesaurus.")), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       actionButton('manual', label = div("Standardize Manually", helpButton("Update selected terms to manually chosen standardized term.")), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       actionButton('editNext', "Next", style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")),
                          mainPanel(bsModal('automatchModal', title = "Review Matches", trigger = "input.automatch", 
                                            textOutput('automatchNote'),
                                            br(),
                                            actionButton('selectAll', label = "Select All"),
                                            actionButton('deselectAll', label = "Deselect All"),
                                            br(),
                                            br(),
                                            rHandsontableOutput("reviewTable"), 
                                            br(), 
                                            actionButton('automatchSave', label = "Save", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                            actionButton('automatchClose', label = "Cancel", style = "color: #fff; background-color: #6baed6; border-color: #6baed6"),
                                            tags$head(tags$style("#automatchModal .modal-footer{ display:none}"))),
                                    bsModal('noMatchModal', title = "Review Matches", trigger = "input.automatch", 
                                            textOutput('noMatchNote')), 
                                    bsModal('manualModal', title = "Manual Standardization", trigger = "input.manual", 
                                            textOutput('manualNote'),
                                            br(),
                                            uiOutput("editDataSelector"),
                                            checkboxInput("makeNA", "These terms represent missing values"),
                                            conditionalPanel(condition = "input.makeNA == false", selectizeInput('newData', label = "Select thesaurus term:", choices = c(""), options = list(placeholder = 'Please wait for options to load...', create = T))),
                                            actionButton('manualSave', label = "Save", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                            actionButton('manualClose', label = "Cancel", style = "color: #fff; background-color: #6baed6; border-color: #6baed6"),
                                            tags$head(tags$style("#manualModal .modal-footer{ display:none}"))),
                                    bsModal('noNewModal', title = "Error", trigger = 'input.manualSave',
                                            textOutput('noNewNote'), tags$head(tags$style("#noNewModal {color: red;}")), tags$head(tags$style("#noNewNote {color: black;}"))),
                                    bsModal('makeNAModal', title = "Warning", trigger = 'input.manualSave',
                                            textOutput('makeNANote'), tags$head(tags$style("#makeNAModal {color: red;}")), tags$head(tags$style("#makeNANote {color: black;}")),
                                            br(),
                                            actionButton('makeNAContinue', label = "Continue", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                            actionButton('makeNACancel', label = "Cancel", style = "color: #fff; background-color: #6baed6; border-color: #6baed6"),
                                            tags$head(tags$style("#makeNAModal .modal-footer{ display:none}"))),
                                    spinner('viewTable'))),
                 

# Save Data ---------------------------------------------------------------
                 tabPanel('Save Data', value = 'finalReport', 
                          sidebarPanel(
                            textOutput('outputNote'),
                            br(),
                            uiOutput('outputFileName'),
                            uiOutput('extensionSelector'),
                            downloadButton('editReport', label = div("Download Output File", helpButton("Output file contains updated data.")), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            downloadButton('script1', label = div("Download RScript", helpButton("RScript contains all of the commands necessary to create the output file from the original file.")), style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")),
                          mainPanel(
                            spinner('finalTable'),
                            br(),
                            downloadButton('editReport2', label = div("Download Output File", helpButton("Output file contains updated data.")), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            downloadButton('script2', label = div("Download RScript", helpButton("RScript contains all of the commands necessary to create the output file from the original file.")), style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f"))),

# Contact -----------------------------------------------------------------
                tabPanel('Contact', value = 'contactPage',
                         textOutput("contactNote")))



# Server ------------------------------------------------------------------
server <- function(input, output, session) { 
  
  session$allowReconnect(TRUE)
  
  #provide an initial table so that spinners do not cause confusion
  initialTable <- as.data.frame("No data available")
  names(initialTable) <- "Please upload a data file"
  output$table <- renderTable(initialTable)
  output$viewTable <- renderTable(initialTable)
  output$secondViewTable <- renderTable(initialTable)
  output$finalTable <- renderTable(initialTable)
  
  values <- reactiveValues(datasetInput = NULL, datasetInputHeaders = NULL, header = 1, start = 1, extension = "", inputName = "", editColumn = "", 
                           firstEditData = "", manualSuggestion = "", newColumn = "", thesaurus = NULL, suggestion = "", letterList = c(),
                           columns = c(), selectedThesaurus = NULL, nameSelectedThesaurus = "", unmatched = c(), standardized = c())

  # Initialize Instructions -------------------------------------------------
  output$uploadInstructions <- renderText("Please upload a file containing unique patient data on each row and clinical information in each column. 
                                          Accepted file types include .txt, .tsv, .csv, .xls, and .xlsx.")
  output$badThesaurusNote <- renderText("The uploaded thesaurus must contain at least 2 columns. Please upload a new file or select one of the default options.")
  output$noThesaurusNote <- renderText("No thesaurus selected. Please upload a file or select one of the default options.")
  output$columnNote <- renderText("If desired, select a column to rename and a new column name. When a column to rename is selected, 
                                  the new column name box will be autofilled with a suggested name if a matching term is found in the 
                                  thesaurus. This term may be changed. Press \"Rename\" to update. Multiple columns may be renamed. 
                                  When finished, press \"Next.\"")
  output$outputNote <- renderText("Enter a name for the output file and select an extension. Do not include the extension in the file name.")
  output$columnErrorNote <- renderText("You must select a column to rename and a new column name. Please close this window and select these items.")
  output$equalNote <- renderText("The selected new column name is already being used as a column name. Please close this window and select a different name.")
  output$editNote <- renderText("Data may be standardized automatically or manually. First select the name of the column containing the data you wish to edit.
                                If you would like to automate the matching process, press \"Automatch.\" The data will be processed and then a pop-up window 
                                will appear and ask you to review the matches. If you would like to manually update the data, press \"Standardize Manually.\" 
                                A different pop-up window will appear with instructions on how to edit the data. When finished, press \"Next.\"")
  output$automatchNote <- renderText("The following matches were found based on the thesaurus. Accept a match by checking the box in the row. 
                                     Press \"Save\" to apply these changes to your data and close this window. If a match is accepted, all occurrences 
                                     of the current term will be changed to the standardized term.")
  output$matchColumnNote <- renderText("You must select a column to edit before proceeding. Please close this window and select a column.")
  output$noMatchNote <- renderText("No matches found.")
  output$manualNote <- renderText("In the box below, enter terms that have a common meaning then select a thesaurus term that has the same meaning. The options 
                                  in the first box are terms that do not already match a preferred term in the thesaurus. To apply a change to your data, press 
                                  \"Save.\" All occurrences of the terms in the first box will be replaced with the selected thesaurus term.")
  output$noNewNote <- renderText("No thesaurus term selected. Please close this window and select a thesaurus term.")
  output$makeNANote <- renderText("The selected terms will be stored as \"NA.\"")
  output$contactNote <- renderText("Questions and comments can be directed to stephen_piccolo@byu.edu.")

  #download thesaurus files
  n <- 4
  withProgress(message = "Initializing terminologies", {
    system("wget -O Modified_NCI.rds \"https://osf.io/dmwv6/download\"")
    incProgress(1/n, detail = "NCI")
    system("wget -O Modified_ICD.rds \"https://osf.io/a8hmt/download\"")
    incProgress(1/n, detail = "ICD")
    system("wget -O Modified_HGNC.rds \"https://osf.io/rcdq6/download\"")
    incProgress(1/n, detail = "HGNC")
    system("wget -O Modified_MeSH.rds \"https://osf.io/v82m9/download\"")
    incProgress(1/n, detail = "MeSH")
  })
  
  
# Define Functions --------------------------------------------------------
  #reading input file
  readInputFile <- function(inFile, useColNames = T) { 
    extension <- substr(inFile$datapath, (nchar(inFile$datapath) - 3), nchar(inFile$datapath))
    functionExtension <- ""
    #use inFile[1] to get the file name because the variable inFile has 4 elements that refer to name, size, file type, and location
    if ((".txt" == extension) | (".tsv" == extension)) {
      inputData <- read_tsv(inFile$datapath, col_names = useColNames)
      functionExtension <- "tsv"
    }
    else if (".csv" == extension){
      inputData <- read_csv(inFile$datapath, col_names = useColNames)
      functionExtension <- "csv"
    }
    else if ((".xls" == extension) | ("xlsx" == extension)) {
      inputData <- read_excel(inFile$datapath, col_names = useColNames)
      values$extension = ".xlsx"
      functionExtension <- "excel"
    }
    #WRITE TO SCRIPT
    if (inFile[1] == input$file1[1] & any(grepl("Read input file", outputText))) {
      outputText <<- gsub("datasetInput <- read.+", paste0("datasetInput <- read_", functionExtension, "(\"", inFile[1], "\", col_names = ", useColNames, ")"), outputText)
    }
    else if (inFile[1] == input$file1[1]) {
      commentify("Read input file")
      writeToScript(paste0("datasetInput <- read_", functionExtension, "(\"", inFile[1], "\", col_names = ", useColNames, ")"))
    }
    #adds text to outputText for uploaded thesaurus file
    else if (any(grepl("Load thesaurus file", outputText)) & input$columnRename == 0 & input$automatch == 0 & input$manual == 0) {
      outputText <<- gsub("system.+", paste0("openThesaurus <- read_", functionExtension, "(\"", inFile[1], "\", col_names = ", useColNames, ")"), outputText)
      outputText <<- gsub("thesaurus <- readRDS.+", "thesaurus <- openThesaurus", outputText)
    }
    else {
      commentify("Load thesaurus file")
      writeToScript(paste0("openThesaurus <- read_", functionExtension, "(\"", inFile[1], "\", col_names = ", useColNames, ")"))
      writeToScript("thesaurus <- openThesaurus")
      writeToScript("names(thesaurus) <- c(\"PreferredName\", \"Synonyms\")")
    }
    return(inputData)
  }
  #writing output file based on extension
  writeOutputFile <- function(outputName, extension, outputDataFrame) { 
    inFile <- input$file1
    if (outputName == "") {
      outputName <- "shiny_output"
    }
    if (length(extension) == 0) {
      extension <- substr(inFile$datapath, (nchar(inFile$datapath) - 3), nchar(inFile$datapath))
    }
    useCols <- T
    if (input$header == 0) {
      useCols <- F
    }
    if (".txt" == extension) {
      returnList <- c("filename" = paste0(outputName, ".txt"), "scriptText" = paste0("write_tsv(datasetInput, file, col_names = ", useCols, ")"), "content" = function(file){
        write_tsv(outputDataFrame, file, col_names = useCols)
      })
    }
    else if (".tsv" == extension) {
      returnList <- c("filename" = paste0(outputName, ".tsv"), "scriptText" = paste0("write_tsv(datasetInput, file, col_names = ", useCols, ")"), "content" = function(file) {
        write_tsv(outputDataFrame, file, col_names = useCols)
      })
    }
    else if (".csv" == extension) {
      returnList <- c("filename" = paste0(outputName, ".csv"), "scriptText" = paste0("write_csv(datasetInput, file, col_names = ", useCols, ")"), "content" = function(file) {
        write_csv(outputDataFrame, file, col_names = useCols)
      })
    }
    else if (".xlsx" == extension) {
      returnList <- c("filename" = paste0(outputName, ".xlsx"), "scriptText" = paste0("write_xlsx(datasetInput, file, col_names = ", useCols, ")"), "content" = function(file) {
        write_xlsx(outputDataFrame, file, col_names = useCols)
      })
    }
    return(returnList)
  }
  #standardizing data
  standardizeManually <- function(newData) {
    m <- length(input$editData)
    withProgress(message = "Standardizing", {
      for (item in input$editData) {
        incProgress(1/m, detail = item)
        #must have sys.sleep or progress bar does not increment properly
        Sys.sleep(0.1)
        values$datasetInput[[input$editThisColumn]][values$datasetInput[[input$editThisColumn]] == item] = newData
      }
    })
    #WRITE TO SCRIPT 
    command <- input$editData[1]
    if (length(input$editData) > 1) {
      for (i in 2:length(input$editData)) {
        command <- paste0(command, "\", \"", input$editData[i])
      }
    }
    newData <- gsub("\"", "\\\\\"", newData)
    writeToScript(paste0("editData <- c(\"", command, "\")"))
    writeToScript(paste0("newData <- \"", newData, "\""))
    writeToScript("for (item in editData) {")
    writeToScript("    datasetInput[[editThisColumn]][datasetInput[[editThisColumn]] == item] = newData")
    writeToScript("}")
    updateSelectizeInput(session, 'newData', label = "Select thesaurus term:", choices = c("", newData, as.character(values$manualSuggestion), as.character(values$letterList)), 
                         options = list(placeholder = 'Select thesaurus term or start typing...', create = T), selected = "", server = T)
    values$unmatched <- setdiff(values$datasetInput[[input$editThisColumn]][values$start:nrow(values$datasetInput)], values$thesaurus$PreferredName)
    values$unmatched <- setdiff(values$unmatched, NA)
    return()
  }


# Change Tab --------------------------------------------------------------
  observeEvent(input$button, ignoreInit = T, {
    if (input$whichThesaurus == "Upload a thesaurus" & is.null(input$uploadedThesaurus)) {
      toggleModal(session, 'noThesaurusModal', toggle = "open")
    }
    else {
      updateTabsetPanel(session, 'tabs', selected = 'updateColumnNames')
    }
  })
  observeEvent(input$thesaurusButton, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'updateColumnNames')
  })
  observeEvent(input$columnSubmit, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'editTable')
  })
  observeEvent(input$editNext, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'finalReport')
  })
  
# Input File1 -------------------------------------------------------------
  #header selector must only be set when user first uploads file; otherwise, if the user selects the number of header lines
  #before the table renders, the box flickers back and forth between selection and default
  observeEvent(input$file1, once = T, {
    output$headerSelector <- renderUI({
        selectInput("header", label = div("Select Number of Header Lines:", helpButton("Select the number of header lines in the input file. Header lines will be ignored during the editing process.")), choices = (0:nrow(values$datasetInput)), selected = values$header)
    })
  })
  observeEvent(input$file1, ignoreInit = T, {
    withProgress(message = "Initializing elements", {
      #initialize variables so functionality is enabled and user can click between tabs without pushing "next"
      output$inputError <- tryCatch({
        values$datasetInput <- readInputFile(input$file1)
        renderText("")
      }, error = function(e) {
        renderText("An error has been detected. Please verify that the file type matches the extension.")
      })
      values$extension <- substr(input$file1$datapath, (nchar(input$file1$datapath) - 3), nchar(input$file1$datapath))
      values$inputName = paste0(substr(input$file1, 0, (nchar(input$file1) - 4)), "_modified")
      if ("xlsx" == values$extension) {
        values$inputName = paste0(substr(input$file1, 0, (nchar(input$file1) - 5)), "_modified")
      }
      values$datasetInputHeaders <- names(values$datasetInput)
      incProgress(1/13, detail = "header")
      values$columns <- names(values$datasetInput)
      incProgress(1/13, detail = "column")
      output$table <- renderTable(values$datasetInput)
      incProgress(1/13, detail = "table")
      output$viewTable <- renderTable(values$datasetInput)
      incProgress(1/13, detail = "table")
      output$secondViewTable <- renderTable(values$datasetInput)
      incProgress(1/13, detail = "table")
      output$finalTable <- renderTable(values$datasetInput)
      incProgress(1/13, detail = "table")
    
      output$thesaurusSelector <- renderUI({
        radioButtons("whichThesaurus", label = div("Select Thesaurus:", helpButton("NCI Thesaurus: cancer terms, ICD-10-CM: disease classification, HGNC: gene names, MeSH: medical subject headings")), choices = c("NCI Thesaurus", "ICD-10-CM", "HGNC", "MeSH", "Upload a thesaurus"))
      })
      incProgress(1/13, detail = "thesaurus selector")
    
      output$editColumnSelector <- renderUI({
        selectizeInput('editColumn', label = "Select column to rename:", choices = c("", names(values$datasetInput)), options = list(placeholder = 'Select column or start typing...'))
      })
      incProgress(1/13, detail = "column selector")
      
      output$outputFileName <- renderUI({
        textInput('outputFileName', label = div("Output File Name (without extension):", helpButton("Enter a name for the output file (do not include extension).")), value = values$inputName)
      })
      incProgress(1/13, detail = "file name selector")
      
      output$extensionSelector <- renderUI({
        selectInput('extension', label = div("Select Extension:", helpButton("Select an extension for the output file.")), choices = c(".txt", ".tsv", ".csv", ".xlsx"), selected = values$extension) #.xls
      })
      incProgress(1/13, detail = "extension selector")
      
      output$editThisColumnSelector <- renderUI({
        selectizeInput('editThisColumn', label = "Select column to standardize:", choices = c("", values$columns), options = list(placeholder = 'Select column or start typing...'))
      })
      incProgress(1/13, detail = "edit column selector")
      
      output$editDataSelector <- renderUI({
        selectizeInput('editData', label = "Enter terms that have a common meaning:", choices = values$unmatched, multiple = T)
      })
      incProgress(1/13, detail = "edit data selector")
      
      output$inputUploadedThesaurus <- renderUI({
        fileInput(inputId = "uploadedThesaurus", label = div("Choose Thesaurus File:", helpButton("Uploaded thesauri must have 2 columns. The first should contain preferred terms. The second should contain synonymns separated by vertical bars (\"|\").")), 
                  multiple = FALSE, accept = c(".tsv", ".txt", ".csv", ".xls", ".xlsx"), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected")
      })
      incProgress(1/13, detail = "thesaurus selector")
    })
  })

# Input Header ------------------------------------------------------------
  observeEvent(input$header, ignoreInit = T, {
    #WRITE TO SCRIPT 
    if (any(grepl("header", outputText))) {
      outputText <<- gsub("header <- [[:digit:]]+", paste0("header <- ", input$header), outputText)
    }
    else {
      commentify("Define number of header lines")
      writeToScript(paste0("header <- ", input$header))
      #WRITE TO SCRIPT 
      commentify("Set line number for start of analysis")
      writeToScript("if (header > 1) {")
      writeToScript("    start <- header")
      writeToScript("} else {")
      writeToScript("    start <- 1")
      writeToScript("}")
    }
    #set starting line to be used when iterating through data (ignores extra header lines)
    if (input$header > 1) {
      values$start <- as.numeric(input$header)
      values$header <- 1
    }
    #if user selects 0 header lines, rename the columns with generic names
    if (input$header == 0) {
      values$datasetInput <- readInputFile(input$file1, useColNames = F)
      values$header <- 0
    }
    else {
      if (!all(names(values$datasetInput) == values$datasetInputHeaders)) {
        values$datasetInput <- readInputFile(input$file1, useColNames = T)
        values$header <- input$header
      }
    }
    values$columns <- names(values$datasetInput)
  })

# Input Thesaurus (load file) ---------------------------------------------------------
  observeEvent(input$whichThesaurus, ignoreInit = T, ignoreNULL = T, {
    values$nameSelectedThesaurus <- input$whichThesaurus
    thesaurusName <- ""
    download <- ""
    if (input$whichThesaurus == "NCI Thesaurus") {
      values$thesaurus <- readRDS("Modified_NCI.rds")
      names(values$thesaurus) <- c("PreferredName", "Synonyms")
      thesaurusName <- "NCI"
      download <- "system(\"wget -O Modified_NCI.rds \\\"https://osf.io/dmwv6/download\\\"\")"
    }
    else if (input$whichThesaurus == "ICD-10-CM") {
      values$thesaurus <- readRDS("Modified_ICD.rds")
      names(values$thesaurus) <- c("PreferredName", "Synonyms")
      thesaurusName <- "ICD"
      download <- "system(\"wget -O Modified_ICD.rds \\\"https://osf.io/a8hmt/download\\\"\")"
    }
    else if (input$whichThesaurus == "HGNC") {
      values$thesaurus <- readRDS("Modified_HGNC.rds")
      names(values$thesaurus) <- c("PreferredName", "Synonyms")
      thesaurusName <- "GeneNames"
      download <- "system(\"wget -O Modified_HGNC.rds \\\"https://osf.io/rcdq6/download\\\"\")"
    }
    else if (input$whichThesaurus == "MeSH") {
      values$thesaurus <- readRDS("Modified_MeSH.rds")
      names(values$thesaurus) <- c("PreferredName", "Synonyms")
      thesaurusName <- "MeSH"
      download <- "system(\"wget -O Modified_MeSH.rds \\\"https://osf.io/v82m9/download\\\"\")"
    }
    else if (input$whichThesaurus == "Upload a thesaurus") {
      #handled by observeEvent for input$uploadedThesaurus
    }
    #WRITE TO SCRIPT
    #if thesaurus is uploaded, writing to outputText is handled in the readInputFile function
    if (input$whichThesaurus != "Upload a thesaurus" & any(grepl("Load thesaurus file", outputText)) & input$columnRename == 0 & input$automatch == 0 & input$manual == 0) {
      outputText <<- gsub("system.+", download, outputText)
      outputText <<- gsub("thesaurus <- readRDS.+", paste0("thesaurus <- readRDS(\"Modified_", thesaurusName, ".rds\")"), outputText)
    }
    else if (input$whichThesaurus != "Upload a thesaurus") {
      commentify("Load thesaurus file")
      writeToScript(download)
      writeToScript(paste0("thesaurus <- readRDS(\"Modified_", thesaurusName, ".rds\")"))
      writeToScript("names(thesaurus) <- c(\"PreferredName\", \"Synonyms\")")
    }
    values$thesaurus <- as.data.frame(lapply(values$thesaurus, unlist))
    values$thesaurus <- values$thesaurus[order(values$thesaurus$PreferredName),]
    values$suggestion <- ""
    values$manualSuggestion <- ""
    values$letterList <- c()
  })
  
# Input Thesaurus (update standardized options) --------------------------------------------------------- 
  observeEvent({
    input$whichThesaurus
    input$uploadedThesaurus}, {
    values$letterList <- c()
    values$suggestion <- ""
    for (j in 1:nchar(values$editColumn)) {
      #replace underscores with spaces before searching for matches in thesaurus
      if (substr(values$editColumn, j, j) == '_') {
        values$editColumn = paste0(substr(values$editColumn, 0, j - 1), ' ', substr(values$editColumn, j + 1, nchar(values$editColumn)))
      }
    }
    searchName <- paste0("(?i)(\\||^)(", values$editColumn, ")(\\||$)")
    preferredName <- values$thesaurus$PreferredName[grepl(searchName, values$thesaurus$Synonyms)]
    if (length(preferredName) == 1 && values$editColumn != preferredName) {
      values$suggestion <- preferredName
    }
    if (!is.null(input$newColumn)) {
      values$letterList <- values$thesaurus$PreferredName[substr(values$thesaurus$PreferredName, 0, nchar(input$newColumn)) == input$newColumn]
    }
    updateSelectizeInput(session, 'newColumn', label = "Select new column name:", choices = c("", as.character(values$suggestion), as.character(values$letterList)), 
                         options = list(placeholder = 'Select thesaurus term or start typing...', create = T), selected = input$newColumn, server = T)
    updateSelectizeInput(session, 'newData', label = "Select thesaurus term:", choices = c("", as.character(values$manualSuggestion), as.character(values$letterList)), 
                         options = list(placeholder = 'Select thesaurus term or start typing...', create = T), selected = input$newData, server = T)
  })

# Upload Thesaurus --------------------------------------------------------
  observeEvent(input$uploadedThesaurus, ignoreInit = T, {
    output$error <- tryCatch({
      values$thesaurus <- readInputFile(input$uploadedThesaurus)
      renderText("")
    }, error = function(e) {
      renderText("An error has been detected. Please verify that the file type matches the extension.")
    })
    if (ncol(values$thesaurus) >= 2) {
      names(values$thesaurus) <- c("PreferredName", "Synonyms")
      values$thesaurus <- values$thesaurus[order(values$thesaurus$PreferredName),]
      output$uploadThesaurusButton <- renderUI({
        actionButton("thesaurusButton", "Next", style="color: #fff; background-color: #2ca25f; border-color: #2ca25f")
      })
    }
    else {
       output$inputUploadedThesaurus <- renderUI({
         fileInput(inputId = "uploadedThesaurus", label = div("Choose Thesaurus File:", helpButton("Uploaded thesauri must have 2 columns. The first should contain preferred terms. The second should contain synonymns separated by vertical bars (\"|\").")),
                   multiple = FALSE, accept = c(".tsv", ".txt", ".csv", ".xls", ".xlsx"), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected")
       })
      toggleModal(session, 'badThesaurusModal', toggle = "open")
    }
  })

# Update Options for Standardized Columns ---------------------------------
  observeEvent({
    input$newColumn
    input$editColumn}, ignoreNULL = T, {
      if (input$newColumn == "") {
        values$letterList <- c()
        values$suggestion <- ""
        values$editColumn <- input$editColumn
        for (j in 1:nchar(values$editColumn)) {
          #replace underscores with spaces before searching for matches in thesaurus
          if (substr(values$editColumn, j, j) == '_') {
            values$editColumn <- paste0(substr(values$editColumn, 0, j - 1), ' ', substr(values$editColumn, j + 1, nchar(values$editColumn)))
          }
        }
        searchName <- paste0("(?i)(\\||^)(", values$editColumn, ")(\\||$)")
        preferredName <- values$thesaurus$PreferredName[grepl(searchName, values$thesaurus$Synonyms)]
        if (length(preferredName) == 1 && values$editColumn != preferredName) {
          values$suggestion <- preferredName
        }
        if (!is.null(input$newColumn)) {
          values$letterList <- values$thesaurus$PreferredName[substr(values$thesaurus$PreferredName, 0, nchar(input$newColumn)) == input$newColumn]
        }

        updateSelectizeInput(session, 'newColumn', label = "Select new column name:", choices = c("", input$newColumn, as.character(values$suggestion), as.character(values$letterList)), 
                             options = list(placeholder = 'Select thesaurus term or start typing...', create = T), selected = input$newColumn, server = T)
      }
    })

# Rename Column -----------------------------------------------------------
  observeEvent(input$columnRename, ignoreInit = T, {
    values$columns <- names(values$datasetInput)
    #display warning if user does not select column to rename and new column name
    if (input$editColumn == "" | input$newColumn == ""){
      toggleModal(session, 'columnModal', toggle = "open")
    }
    #display warning if new name is already the name of a column
    else if (input$newColumn %in% values$columns) {
      toggleModal(session, 'equalModal', toggle = "open")
    }
    else {
      names(values$datasetInput)[which(names(values$datasetInput) == input$editColumn)] <<- input$newColumn
      #WRITE TO SCRIPT 
      if (!any(grepl("Rename column", outputText))) {
        commentify("Rename column")
      }
      newColumn <- gsub("\"", "\\\\\"", input$newColumn)
      writeToScript(paste0("editColumn <- \"", input$editColumn, "\""))
      writeToScript(paste0("newColumn <- \"", newColumn, "\""))
      writeToScript("datasetInput <- as.data.frame(datasetInput)")
      writeToScript(paste0("colnames(datasetInput)[which(names(datasetInput) == editColumn)] <- newColumn"))
      values$columns <- names(values$datasetInput)
      updateSelectizeInput(session, 'newColumn', label = "Select new column name:", choices = c("", input$newColumn, as.character(values$suggestion), as.character(values$letterList)), 
                           options = list(placeholder = 'Select thesaurus term or start typing...', create = T), selected = "", server = T)
    }
  })


# Automatch ---------------------------------------------------------------
  observeEvent(input$automatch, ignoreInit = T, {
    #WRITE TO SCRIPT 
    if (input$editThisColumn != "") {
      commentify("Automatching")
    }
    writeToScript(paste0("editThisColumn <- \"", input$editThisColumn, "\""))
    if (input$editThisColumn == "") {
      toggleModal(session, 'matchColumnModal', toggle = "open")
    }
    else {
      values$automatchTable <- NULL
      #WRITE TO SCRIPT 
      writeToScript("automatchTable <- NULL")
      n <- nrow(values$datasetInput)
      #only search for matches of terms that are not already standardized
      names <- setdiff(values$datasetInput[[input$editThisColumn]][values$start:nrow(values$datasetInput)], values$thesaurus$PreferredName)
      m <- length(unique(names))
      withProgress(message = "Automatching", {
        for (name in unique(names)) {
          incProgress(1/m, detail = name)
          #Sys.sleep must be included here or else progress bar does not increment properly
          Sys.sleep(0.01)
          if (is.na(name)) {
            next
          }
          for (j in 1:nchar(name)) {
            #replace underscores with spaces before searching for matches in thesaurus
            if (substr(name, j, j) == '_') {
              name = paste0(substr(name, 0, j - 1), ' ', substr(name, j + 1, nchar(name)))
            }
          }
          searchName <- paste0("(?i)(\\||^)(", name, ")(\\||$)")
          preferredName <- values$thesaurus$PreferredName[grepl(searchName, values$thesaurus$Synonyms)]
          if (length(preferredName) == 1) {
            if (name != preferredName) {
              values$automatchTable <- rbind(values$automatchTable, c(name, as.character(preferredName), TRUE))
            }
          }
        }
        #WRITE TO SCRIPT 
        writeToScript("n <- nrow(datasetInput)")
        writeToScript("names <- setdiff(datasetInput[[editThisColumn]][start:nrow(datasetInput)], thesaurus$PreferredName)")
        writeToScript("for (name in unique(names)) {")
        writeToScript("    if (is.na(name)) {")
        writeToScript("        next")
        writeToScript("    }")
        writeToScript("    for (j in 1:nchar(name)) {")
        writeToScript("        if (substr(name, j, j) == '_') {")
        writeToScript("            name = paste0(substr(name, 0, j - 1), ' ', substr(name, j + 1, nchar(name)))")
        writeToScript("        }")
        writeToScript("    }")
        writeToScript("    searchName <- paste0(\"(?i)(\\\\||^)(\", name, \")(\\\\||$)\")")
        writeToScript("    preferredName <- thesaurus$PreferredName[which(grepl(searchName, thesaurus$Synonyms))]")
        writeToScript("    if (length(preferredName) == 1) {")
        writeToScript("        if (name != preferredName) {")
        writeToScript("            automatchTable <- rbind(automatchTable, c(name, preferredName, TRUE))")
        writeToScript("        }")
        writeToScript("    }")
        writeToScript("}")
      })
      #convert automatchTable to data frame so information can be used in rows and columns
      values$automatchTable <- as.data.frame(values$automatchTable)
      #fixes this error: Warning: Error in order: unimplemented type 'list' in 'orderVector1'
      values$automatchTable <- as.data.frame(lapply(values$automatchTable, unlist))
      if (length(values$automatchTable) > 0) {
        newData <- values$automatchTable[order(values$automatchTable[,2]),]
        names(newData) <- c("Current Term", "Standardized Term", "Accept")
        row.names(newData) <- 1:nrow(newData)
        output$reviewTable <- renderRHandsontable({
          rhandsontable(newData, useTypes = F, selectCallback = T, stringsAsFactors = F) %>%
            hot_col(col = "Current Term", readOnly = T, type = "text") %>%
            hot_col(col = "Standardized Term", readOnly = T, type = "text") %>%
            hot_col(col = "Accept", type = "checkbox")
        })
        toggleModal(session, 'automatchModal', toggle = "open")
      }
      else {
        
        toggleModal(session, 'noMatchModal', toggle = "open")
      }
      output$viewTable <- renderTable(values$datasetInput)
    }
  })
  

# Automatch Supporting Events ---------------------------------------------
  observeEvent(input$selectAll, ignoreInit = T, {
    values$automatchTable <- hot_to_r(input$reviewTable)
    values$automatchTable$Accept = T
    
    output$reviewTable <- renderRHandsontable({
      rhandsontable(values$automatchTable, useTypes = F, selectCallback = T, stringsAsFactors = F) %>%
        hot_col(col = "Current Term", readOnly = T, type = "text") %>%
        hot_col(col = "Standardized Term", readOnly = T, type = "text") %>%
        hot_col(col = "Accept", type = "checkbox")
    })
  })
  
  observeEvent(input$deselectAll, ignoreInit = T, {
    values$automatchTable <- hot_to_r(input$reviewTable)
    values$automatchTable$Accept = F
    output$reviewTable <- renderRHandsontable({
      rhandsontable(values$automatchTable, useTypes = F, selectCallback = T, stringsAsFactors = F) %>%
        hot_col(col = "Current Term", readOnly = T, type = "text") %>%
        hot_col(col = "Standardized Term", readOnly = T, type = "text") %>%
        hot_col(col = "Accept", type = "checkbox")
    })
  })

# Automatch Save ----------------------------------------------------------
  observeEvent(input$automatchSave, ignoreInit = T, {
    if (length(values$automatchTable) > 0) {
      accepted <- hot_to_r(input$reviewTable)
      m <- nrow(accepted)
      withProgress(message = "Standardizing", {
        for (i in 1:nrow(accepted)) {
          incProgress(1/m, detail = accepted$"Current Term"[i])
          #Sys.sleep is necessary for progress bar to increment properly
          Sys.sleep(0.1)
          if (accepted$Accept[i] == T) {
            values$datasetInput[[input$editThisColumn]][values$datasetInput[[input$editThisColumn]] == as.character(accepted$"Current Term"[i])] = as.character(accepted$"Standardized Term"[i])
          }
        }
      })
    }
    
    #WRITE TO SCRIPT 
    commentify("Save automatches")
    writeToScript("automatchTable <- as.data.frame(automatchTable)")
    writeToScript("colnames(automatchTable) <- c(\"Current Term\", \"Standardized Term\", \"Accept\")")
    accepted <- hot_to_r(input$reviewTable)
    for (i in 1:nrow(accepted)) {
      if (accepted$Accept[i] == F) {
        writeToScript(paste0("automatchTable$Accept[", i, "] <- FALSE"))
      }
    }
    writeToScript("for (i in 1:nrow(automatchTable)) {")
    writeToScript("    if (automatchTable$Accept[i] == TRUE) {")
    writeToScript("        datasetInput[[editThisColumn]][datasetInput[[editThisColumn]] == as.character(automatchTable$\"Current Term\"[i])] = as.character(automatchTable$\"Standardized Term\"[i])")
    writeToScript("    }")
    writeToScript("}")
    toggleModal(session, 'automatchModal', toggle = "close")
  })
  
  observeEvent(input$automatchClose, ignoreInit = T, {
    toggleModal(session, 'automatchModal', toggle = "close")
  })


# Manual Standardization --------------------------------------------------
  observeEvent(input$manual, ignoreInit = T, {
    if (input$editThisColumn == "") {
      toggleModal(session, 'matchColumnModal', toggle = "open")
    }
    else {
      values$unmatched <- setdiff(values$datasetInput[[input$editThisColumn]][values$start:nrow(values$datasetInput)], values$thesaurus$PreferredName)
      values$unmatched <- setdiff(values$unmatched, NA)
      #WRITE TO SCRIPT 
      commentify("Manual standardization")
      writeToScript(paste0("editThisColumn <- \"", input$editThisColumn, "\""))
      toggleModal(session, 'manualModal', toggle = "open")
    }
  })


# Update Standardized Terms Options ---------------------------------------
  observeEvent({
    input$newData
    input$editData}, {
    if (input$newData == "") {
      values$firstEditData <<- input$editData[1]      
      values$letterList <- c()
      values$manualSuggestion <- ""
      for (j in 1:nchar(values$firstEditData)) {
        #replace underscores with spaces before searching for matches in thesaurus
        if (substr(values$firstEditData, j, j) == '_') {
          values$firstEditData = paste0(substr(values$firstEditData, 0, j - 1), ' ', substr(values$firstEditData, j + 1, nchar(values$firstEditData)))
        }
      }
      searchName <- paste0("(?i)(\\||^)(", values$firstEditData, ")(\\||$)")
      preferredName <- values$thesaurus$PreferredName[grepl(searchName, values$thesaurus$Synonyms)]
      if (length(preferredName) == 1 && values$firstEditData != preferredName) {
        values$manualSuggestion <- preferredName
      }
      values$letterList <- values$thesaurus$PreferredName[substr(values$thesaurus$PreferredName, 0, nchar(input$newData)) == input$newData]
      updateSelectizeInput(session, 'newData', label = "Select thesaurus term:", choices = c("", input$newData, as.character(values$manualSuggestion), as.character(values$letterList)), 
                           options = list(placeholder = 'Select thesaurus term or start typing...', create = T), selected = input$newData, server = T)
    }
  })

# Manual Save -------------------------------------------------------------
  observeEvent(input$manualSave, ignoreInit = T, {
    if (input$newData == "" & input$makeNA == F) {
      toggleModal(session, 'noNewModal', toggle = "open")
    }
    else if (input$makeNA == F) {
      newData <- input$newData
      standardizeManually(newData)
    }
    else {
      toggleModal(session, 'makeNAModal', toggle = "open")
    }
  })

# Standarize to NA --------------------------------------------------------
  observeEvent(input$makeNAContinue, {
    newData <- NA
    standardizeManually(newData)
    toggleModal(session, 'makeNAModal', toggle = "close")
  })
  
  observeEvent(input$makeNACancel, {
    toggleModal(session, 'makeNAModal', toggle = "close")
  })
  
  observeEvent(input$manualClose, ignoreInit = T, {
    toggleModal(session, 'manualModal', toggle = "close")
  })

# Name Output File --------------------------------------------------------
  observeEvent({
    input$outputFileName 
    input$extension}, {
    outputName <- input$outputFileName
    extension <- input$extension
    outputDataFrame <- values$datasetInput
    returnList <- writeOutputFile(outputName, extension, outputDataFrame)
    returnList$filename <- gsub("[/#%&{}\\<>*? $!'\":@|]", "_", returnList$filename)
    
    #compiles text to write to script when user downloads script; saved so that download code is not put in the middle of the script if the user edits something after naming the output file
    saveUntilEnd <- "\n"
    numChars <- 100
    comment <- "# Save edited data "
    completeComment <- paste0(comment, paste(rep("-", numChars - nchar(comment)), collapse = ""))
    saveUntilEnd <- rbind(saveUntilEnd, completeComment)
    saveUntilEnd <- rbind(saveUntilEnd, paste0("file <- \"", returnList$filename, "\""))
    saveUntilEnd <- rbind(saveUntilEnd, paste0(returnList$scriptText))

    #compile output file    
    output$editReport <- downloadHandler(filename = returnList$filename, content = returnList$content)
    output$editReport2 <- downloadHandler(filename = returnList$filename, content = returnList$content)
    
    #script output
    names(outputText) <- NULL
    outputText <- as.data.frame(outputText)
    output$script1 <- downloadHandler(filename = function() {
        paste("good_nomen_R_script", ".txt", sep = "")}, content = function(file) {
        write.table(rbind(outputText, saveUntilEnd), file, row.names = F, col.names = F, quote = F)
        })
    output$script2 <- downloadHandler(filename = function() {
      paste("good_nomen_R_script", ".txt", sep = "")}, content = function(file) {
      write.table(outputText, file, row.names = F, col.names = F, quote = F)
    })
  })
}

shinyApp(ui, server)
  