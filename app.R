library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(shinyBS)
library(shinycssloaders)
library(rhandsontable)
library(DT)

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
writeToScript("# Please ensure that your data file and terminology file (if using an uploaded terminology) are in the same directory as this script before executing.")
writeToScript("library(readr)")
writeToScript("library(dplyr)")
writeToScript("library(stringr)")
writeToScript("library(readxl)")
writeToScript("library(writexl)")

#define function for tooltips 
helpButton <- function(message = "content", placement = "right") {
  return(tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover"))
}


# UI ----------------------------------------------------------------------
ui <- navbarPage(title = "Good Nomen", id = 'tabs',
#ui <- navbarPage(titlePanel(title=div(img(src="logo.png", height = 50), "Good Nomen")), id = 'tabs',

# Load Data ---------------------------------------------------------------
                 tabPanel('Load Data', sidebarLayout(
                   #sidebarPanel(
                           sidebarPanel(img(src='Logo.png', align = "right", height = 100),
                                        textOutput("welcomeMessage"),
                                        br(),
                                        textOutput("uploadInstructions"),
                                        br(),
                                        fileInput(inputId = "file1", label = "Choose Input File:", 
                                                  multiple = FALSE, accept = c(".tsv", ".txt", ".csv", ".xls", ".xlsx"), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                                        textOutput("inputError"), tags$head(tags$style("#inputError {color: red;}")),
                                        uiOutput("headerSelector"),
                                        uiOutput("terminologySelector"), 
                                        conditionalPanel(condition = "input.whichTerminology == 'Upload a terminology'", uiOutput("inputUploadedTerminology")),
                                        #warning if user uploads a terminology with less than 2 columns
                                        bsModal('badTerminologyModal', title = "Error", trigger = 'input.inputUploadedTerminology',
                                                textOutput('badTerminologyNote'), tags$head(tags$style("#badTerminologyModal {color: red;}")), tags$head(tags$style("#badTerminologyNote {color: black;}"))),
                                        #warning if user does not upload a terminology
                                        bsModal('noTerminologyModal', title = "Error", trigger = 'input.inputUploadedTerminology',
                                                textOutput('noTerminologyNote'), tags$head(tags$style("#noTerminologyModal {color: red;}")), tags$head(tags$style("#noTerminologyNote {color: black;}"))),
                                        #'input.header' relies on the condition that a file has been loaded, so this conditional panel prevents the action button from appearing before a file has been loaded
                                        conditionalPanel(condition = "(input.header && input.whichTerminology != 'Upload a terminology')", #  || (input.inputUploadedTerminology) && input.whichTerminology == 'Upload a terminology' (input.uploadedTerminology)
                                                         actionButton("button", "Next", style="color: #fff; background-color: #2ca25f; border-color: #2ca25f")),
                                        textOutput("error"), tags$head(tags$style("#error {color: red;}")),
                                        conditionalPanel(condition = "input.whichTerminology == 'Upload a terminology'", 
                                                         uiOutput("uploadTerminologyButton"))),
                            mainPanel())),
                
# Edit Data ---------------------------------------------------------------
                 tabPanel('Edit Data', value = 'editTable', 
                          sidebarPanel(width = 4, 
                                       textOutput('editNote'),
                                       br(),
                                       uiOutput("columnSelector"),
                                       uiOutput("editThisColumnSelector"),
                                       bsModal('matchColumnModal', title = "Error", trigger = 'input.editThisColumn',
                                               textOutput('matchColumnNote'), tags$head(tags$style("#matchColumnModal {color: red;}")), tags$head(tags$style("#matchColumnNote {color: black;}"))),
                                       actionButton('automatch', label = div("Auto-match", helpButton("Matches will be found based on synonyms in the selected terminology.")), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
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
                                            conditionalPanel(condition = "input.makeNA == false", selectizeInput('newData', label = "Select terminology term:", choices = c(""), options = list(placeholder = 'Please wait for options to load...', create = T))),
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
                                    DTOutput('singleColumn'))),
                 
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
                      actionButton('columnSubmit', "Next", style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f"))),

# Save Data ---------------------------------------------------------------
                 tabPanel('Save Data', value = 'finalReport', 
                          sidebarPanel(
                            textOutput('outputNote'),
                            br(),
                            uiOutput('outputFileName'),
                            uiOutput('extensionSelector'),
                            actionButton('generateOutput', label = div("Generate Output", helpButton("Press to generate output files. If any changes are made after the button has been pressed, 
                                                                                                     it must be pressed again to update the output.")), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            conditionalPanel(condition = 'input.generateOutput',
                              downloadButton('editReport', label = div("Download Output File", helpButton("Output file contains updated data.")), 
                                             style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                              downloadButton('script1', label = div("Download RScript", helpButton("RScript contains all of the commands necessary to create the output file from the original file.")), 
                                             style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")))),
                         
# Contact -----------------------------------------------------------------
                tabPanel('Contact', value = 'contactPage',
                         textOutput("contactNote")))



# Server ------------------------------------------------------------------
server <- function(input, output, session) { 
  
  session$allowReconnect(TRUE)
  
  values <- reactiveValues(datasetInput = NULL, datasetInputHeaders = NULL, header = 1, start = 1, extension = "", inputName = "", editColumn = "", 
                           firstEditData = "", manualSuggestion = "", newColumn = "", terminology = NULL, suggestion = "", letterList = c(),
                           columns = c(), nameSelectedTerminology = "", unmatched = c(), standardized = c(), column = c(), editThisColumn = "")

  #define function for rendering single column
  renderColumn <- function() {
    # if (is.null(input$editThisColumn)) {
    #   values$column <- as.data.frame(values$datasetInput[1,][values$start:nrow(values$datasetInput)])
    # }
    # else {
    #   #values$column <- as.data.frame(values$datasetInput[[input$editThisColumn]][values$start:nrow(values$datasetInput)])
       values$column <- as.data.frame(values$datasetInput[[values$editThisColumn]][values$start:nrow(values$datasetInput)])
    # }
    if (ncol(values$column) > 0) {
      names(values$column) <- input$editThisColumn
    }
    output$singleColumn <- renderDT(values$column, options = list(pageLength = 20), rownames = F)
  }
  
  # Initialize Instructions -------------------------------------------------
  output$welcomeMessage <- renderText("Welcome to Good Nomen, an interface for mapping clinical data files based on standardized terminologies.")
  output$uploadInstructions <- renderText("Please upload a file containing patient data on each row and clinical variables in each column. 
                                          Accepted file types include .txt, .tsv, .csv, .xls, and .xlsx.")
  output$badTerminologyNote <- renderText("The uploaded terminology must contain at least 2 columns. Please upload a new file or select one of the default options.")
  output$noTerminologyNote <- renderText("No terminology selected. Please upload a file or select one of the default options.")
  output$columnNote <- renderText("If desired, select a column to rename and a new column name. When a column to rename is selected, 
                                  the new column name box will be autofilled with a suggested name if a matching term is found in the 
                                  terminology. This term may be changed. Press \"Rename\" to update. Multiple columns may be renamed. 
                                  When finished, press \"Next.\"")
  output$outputNote <- renderText("Enter a name for the output file and select an extension. Do not include the extension in the file name.")
  output$columnErrorNote <- renderText("You must select a column to rename and a new column name. Please close this window and select these items.")
  output$equalNote <- renderText("The selected new column name is already being used as a column name. Please close this window and select a different name.")
  output$editNote <- renderText("Data may be standardized automatically or manually. First select the name of the column containing the data you wish to edit.
                                If you would like to automate the matching process, press \"Auto-match.\" The data will be processed and then a pop-up window 
                                will appear and ask you to review the matches. If you would like to manually update the data, press \"Standardize Manually.\" 
                                A different pop-up window will appear with instructions on how to edit the data. When finished, press \"Next.\"")
  output$automatchNote <- renderText("The following matches were found based on the terminology. Accept a match by checking the box in the row. 
                                     Press \"Save\" to apply these changes to your data and close this window. If a match is accepted, all occurrences 
                                     of the current term will be changed to the standardized term.")
  output$matchColumnNote <- renderText("You must select a column to edit before proceeding. Please close this window and select a column.")
  output$noMatchNote <- renderText("No matches found.")
  output$manualNote <- renderText("In the box below, enter terms that have a common meaning then select a terminology term that has the same meaning. The options 
                                  in the first box are terms that do not already match a preferred term in the terminology. To apply a change to your data, press 
                                  \"Save.\" All occurrences of the terms in the first box will be replaced with the selected terminology term.")
  output$noNewNote <- renderText("No terminology term selected. Please close this window and select a terminology term.")
  output$makeNANote <- renderText("The selected terms will be stored as \"NA.\"")
  output$contactNote <- renderText("For questions and comments, please visit piccolo.byu.edu/Contact.aspx. The source code for Good Nomen can be found at https://github.com/srp33/GoodNomen.")

  #download terminology files
  n <- 4
  withProgress(message = "Initializing terminologies", {
    system("wget -O Modified_NCI.rds \"https://osf.io/dmwv6/download\"")
    incProgress(1/n, detail = "NCI")
    system("wget -O Modified_ICD.rds \"https://osf.io/a8hmt/download\"")
    incProgress(1/n, detail = "ICD")
    system("wget -O Modified_HGNC.rds \"https://osf.io/q4snj/download\"")
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
    #adds text to outputText for uploaded terminology file
    else if (any(grepl("Load terminology file", outputText)) & input$columnRename == 0 & input$automatch == 0 & input$manual == 0) {
      outputText <<- gsub("system.+", paste0("openTerminology <- read_", functionExtension, "(\"", inFile[1], "\", col_names = ", useColNames, ")"), outputText)
      outputText <<- gsub("terminology <- readRDS.+", "terminology <- openTerminology", outputText)
    }
    else {
      commentify("Load terminology file")
      writeToScript(paste0("openTerminology <- read_", functionExtension, "(\"", inFile[1], "\", col_names = ", useColNames, ")"))
      writeToScript("terminology <- openTerminology")
      writeToScript("names(terminology) <- c(\"PreferredName\", \"Synonyms\")")
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
    updateSelectizeInput(session, 'newData', label = "Select terminology term:", choices = c("", newData, as.character(values$manualSuggestion), as.character(values$letterList)), 
                         options = list(placeholder = 'Select terminology term or start typing...', create = T), selected = "", server = T)
    values$unmatched <- setdiff(values$datasetInput[[input$editThisColumn]][values$start:nrow(values$datasetInput)], values$terminology$PreferredName)
    values$unmatched <- setdiff(values$unmatched, NA)
    return()
  }


# Change Tab --------------------------------------------------------------
  observeEvent(input$button, ignoreInit = T, {
    if (input$whichTerminology == "Upload a terminology" & is.null(input$uploadedTerminology)) {
      toggleModal(session, 'noTerminologyModal', toggle = "open")
    }
    else {
      updateTabsetPanel(session, 'tabs', selected = 'editTable')
    }
  })
  observeEvent(input$terminologyButton, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'editTable')
  })
  observeEvent(input$columnSubmit, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'finalReport')
  })
  observeEvent(input$editNext, ignoreInit = T, {
    updateTabsetPanel(session, 'tabs', selected = 'updateColumnNames')
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
      incProgress(1/9, detail = "header")
      values$columns <- names(values$datasetInput)
      incProgress(1/9, detail = "column")
      values$editThisColumn <- names(values$datasetInput)[1] 
      output$terminologySelector <- renderUI({
        radioButtons("whichTerminology", label = div("Select terminology:", helpButton("NCI Thesaurus: cancer terms, ICD-10-CM: disease classification, HGNC: gene names, MeSH: medical subject headings")), 
                     choices = c("NCI Thesaurus", "ICD-10-CM", "HGNC", "MeSH", "Upload a terminology"))
      })
      incProgress(1/9, detail = "terminology selector")
    
      output$editColumnSelector <- renderUI({
        selectizeInput('editColumn', label = "Select column to rename:", choices = c("", names(values$datasetInput)), options = list(placeholder = 'Select column or start typing...'))
      })
      incProgress(1/9, detail = "column selector")
      
      output$outputFileName <- renderUI({
        textInput('outputFileName', label = div("Output File Name (without extension):", helpButton("Enter a name for the output file (do not include extension).")), value = values$inputName)
      })
      incProgress(1/9, detail = "file name selector")
      
      output$extensionSelector <- renderUI({
        selectInput('extension', label = div("Select Extension:", helpButton("Select an extension for the output file.")), choices = c(".txt", ".tsv", ".csv", ".xlsx"), selected = values$extension) #.xls
      })
      incProgress(1/9, detail = "extension selector")
      
      output$editThisColumnSelector <- renderUI({
        #default is to display first column of dataset
        selectizeInput('editThisColumn', label = "Select column to standardize:", choices = c("", values$columns), options = list(placeholder = 'Select column or start typing...'), selected = values$editThisColumn)#names(values$datasetInput)[1])
      })
      incProgress(1/9, detail = "edit column selector")
      
      output$editDataSelector <- renderUI({
        selectizeInput('editData', label = "Enter terms that have a common meaning:", choices = values$unmatched, multiple = T)
      })
      incProgress(1/9, detail = "edit data selector")
      
      output$inputUploadedTerminology <- renderUI({
        fileInput(inputId = "uploadedTerminology", label = div("Choose File:", helpButton("Uploaded terminologies must have 2 columns. The first should contain preferred terms. 
                                                                                          The second should contain a list of synonyms separated by vertical bars (\"|\").")), 
                  multiple = FALSE, accept = c(".tsv", ".txt", ".csv", ".xls", ".xlsx"), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected")
      })
      incProgress(1/9, detail = "terminology selector")
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
      values$start <- 1
    }
    else {
      if (!all(names(values$datasetInput) == values$datasetInputHeaders)) {
        values$datasetInput <- readInputFile(input$file1, useColNames = T)
        values$header <- input$header
        values$start <- 1
      }
    }
    values$columns <- names(values$datasetInput)
    renderColumn()
  })

# Input terminology (load file) ---------------------------------------------------------
  observeEvent(input$whichTerminology, ignoreInit = T, ignoreNULL = T, {
    values$nameSelectedTerminology <- input$whichTerminology
    terminologyName <- ""
    download <- ""
    if (input$whichTerminology == "NCI Thesaurus") {
      values$terminology <- readRDS("Modified_NCI.rds")
      names(values$terminology) <- c("PreferredName", "Synonyms")
      terminologyName <- "NCI"
      download <- "system(\"wget -O Modified_NCI.rds \\\"https://osf.io/dmwv6/download\\\"\")"
    }
    else if (input$whichTerminology == "ICD-10-CM") {
      values$terminology <- readRDS("Modified_ICD.rds")
      names(values$terminology) <- c("PreferredName", "Synonyms")
      terminologyName <- "ICD"
      download <- "system(\"wget -O Modified_ICD.rds \\\"https://osf.io/a8hmt/download\\\"\")"
    }
    else if (input$whichTerminology == "HGNC") {
      values$terminology <- readRDS("Modified_HGNC.rds")
      names(values$terminology) <- c("PreferredName", "Synonyms")
      terminologyName <- "GeneNames"
      download <- "system(\"wget -O Modified_HGNC.rds \\\"https://osf.io/q4snj/download\\\"\")"
    }
    else if (input$whichTerminology == "MeSH") {
      values$terminology <- readRDS("Modified_MeSH.rds")
      names(values$terminology) <- c("PreferredName", "Synonyms")
      terminologyName <- "MeSH"
      download <- "system(\"wget -O Modified_MeSH.rds \\\"https://osf.io/v82m9/download\\\"\")"
    }
    else if (input$whichTerminology == "Upload a terminology") {
      #handled by observeEvent for input$uploadedTerminology
    }
    #WRITE TO SCRIPT
    if (input$whichTerminology != "Upload a terminology" & any(grepl("Load terminology file", outputText)) & input$columnRename == 0 & input$automatch == 0 & input$manual == 0) {
      outputText <<- gsub("system.+", download, outputText)
      outputText <<- gsub("terminology <- readRDS.+", paste0("terminology <- readRDS(\"Modified_", terminologyName, ".rds\")"), outputText)
    }
    else if (input$whichTerminology != "Upload a terminology") {
      commentify("Load terminology file")
      writeToScript(download)
      writeToScript(paste0("terminology <- readRDS(\"Modified_", terminologyName, ".rds\")"))
      writeToScript("names(terminology) <- c(\"PreferredName\", \"Synonyms\")")
    }
    #there is no else statement for an uploaded terminology because writing to outputText is handled in the readInputFile function
    values$terminology <- as.data.frame(lapply(values$terminology, unlist))
    values$terminology <- values$terminology[order(values$terminology$PreferredName),]
    values$suggestion <- ""
    values$manualSuggestion <- ""
    values$letterList <- c()
  })
  
# Input terminology (update standardized options) --------------------------------------------------------- 
  observeEvent({
    input$whichTerminology
    input$uploadedTerminology}, {
    values$letterList <- c()
    values$suggestion <- ""
    for (j in 1:nchar(values$editColumn)) {
      #replace underscores with spaces before searching for matches in terminology
      if (substr(values$editColumn, j, j) == '_') {
        values$editColumn = paste0(substr(values$editColumn, 0, j - 1), ' ', substr(values$editColumn, j + 1, nchar(values$editColumn)))
      }
    }
    searchName <- paste0("(?i)(\\||^)(", values$editColumn, ")(\\||$)")
    preferredName <- values$terminology$PreferredName[grepl(searchName, values$terminology$Synonyms)]
    if (length(preferredName) == 1 && values$editColumn != preferredName) {
      values$suggestion <- preferredName
    }
    if (!is.null(input$newColumn)) {
      values$letterList <- values$terminology$PreferredName[substr(values$terminology$PreferredName, 0, nchar(input$newColumn)) == input$newColumn]
    }
    updateSelectizeInput(session, 'newColumn', label = "Select new column name:", choices = c("", as.character(values$suggestion), as.character(values$letterList)), 
                         options = list(placeholder = 'Select terminology term or start typing...', create = T), selected = input$newColumn, server = T)
    updateSelectizeInput(session, 'newData', label = "Select terminology term:", choices = c("", as.character(values$manualSuggestion), as.character(values$letterList)), 
                         options = list(placeholder = 'Select terminology term or start typing...', create = T), selected = input$newData, server = T)
  })

# Upload terminology --------------------------------------------------------
  observeEvent(input$uploadedTerminology, ignoreInit = T, {
    output$error <- tryCatch({
      values$terminology <- readInputFile(input$uploadedTerminology)
      renderText("")
    }, error = function(e) {
      renderText("An error has been detected. Please verify that the file type matches the extension.")
    })
    if (ncol(values$terminology) >= 2) {
      names(values$terminology) <- c("PreferredName", "Synonyms")
      values$terminology <- values$terminology[order(values$terminology$PreferredName),]
      output$uploadTerminologyButton <- renderUI({
        actionButton("terminologyButton", "Next", style="color: #fff; background-color: #2ca25f; border-color: #2ca25f")
      })
    }
    else {
       output$inputUploadedTerminology <- renderUI({
         fileInput(inputId = "uploadedTerminology", label = div("Choose terminology File:", helpButton("Uploaded Terminologies must have 2 columns. The first should contain preferred terms. The second should contain synonymns separated by vertical bars (\"|\").")),
                   multiple = FALSE, accept = c(".tsv", ".txt", ".csv", ".xls", ".xlsx"), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected")
       })
      toggleModal(session, 'badTerminologyModal', toggle = "open")
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
          #replace underscores with spaces before searching for matches in terminology
          if (substr(values$editColumn, j, j) == '_') {
            values$editColumn <- paste0(substr(values$editColumn, 0, j - 1), ' ', substr(values$editColumn, j + 1, nchar(values$editColumn)))
          }
        }
        searchName <- paste0("(?i)(\\||^)(", values$editColumn, ")(\\||$)")
        preferredName <- values$terminology$PreferredName[grepl(searchName, values$terminology$Synonyms)]
        if (length(preferredName) == 1 && values$editColumn != preferredName) {
          values$suggestion <- preferredName
        }
        if (!is.null(input$newColumn)) {
          values$letterList <- values$terminology$PreferredName[substr(values$terminology$PreferredName, 0, nchar(input$newColumn)) == input$newColumn]
        }

        updateSelectizeInput(session, 'newColumn', label = "Select new column name:", choices = c("", input$newColumn, as.character(values$suggestion), as.character(values$letterList)), 
                             options = list(placeholder = 'Select terminology term or start typing...', create = T), selected = input$newColumn, server = T)
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
                           options = list(placeholder = 'Select terminology term or start typing...', create = T), selected = "", server = T)
    }
    showNotification(paste0("Column \"", input$editColumn, "\" has been renamed to \"", input$newColumn, ".\""))
  })

# Render Column -----------------------------------------------------------
  observeEvent(input$editThisColumn, {
    if (is.null(input$editThisColumn)) {
      values$editThisColumn <- names(values$datasetInput)[1]
    }
    else {
      values$editThisColumn <- input$editThisColumn
    }
    renderColumn()
  })

# Auto-match ---------------------------------------------------------------
  observeEvent(input$automatch, ignoreInit = T, {
    #WRITE TO SCRIPT 
    if (input$editThisColumn != "") {
      commentify("Auto-matching")
    }
    editThisColumn <- gsub("\"", "\\\\\"", input$editThisColumn)
    writeToScript(paste0("editThisColumn <- \"", editThisColumn, "\""))
    if (input$editThisColumn == "") {
      toggleModal(session, 'matchColumnModal', toggle = "open")
    }
    else {
      values$automatchTable <- NULL
      #WRITE TO SCRIPT 
      writeToScript("automatchTable <- NULL")
      n <- nrow(values$datasetInput)
      #only search for matches of terms that are not already standardized
      names <- setdiff(values$datasetInput[[input$editThisColumn]][values$start:nrow(values$datasetInput)], values$terminology$PreferredName)
      m <- length(unique(names))
      withProgress(message = "Auto-matching", {
        for (name in unique(names)) {
          incProgress(1/m, detail = name)
          #Sys.sleep must be included here or else progress bar does not increment properly
          Sys.sleep(0.01)
          if (is.na(name)) {
            next
          }
          for (j in 1:nchar(name)) {
            #replace underscores with spaces before searching for matches in terminology
            if (substr(name, j, j) == '_') {
              name = paste0(substr(name, 0, j - 1), ' ', substr(name, j + 1, nchar(name)))
            }
          }
          searchName <- paste0("(?i)(\\||^)(", name, ")(\\||$)")
          preferredName <- values$terminology$PreferredName[grepl(searchName, values$terminology$Synonyms)]
          if (length(preferredName) == 1) {
            if (name != preferredName) {
              values$automatchTable <- rbind(values$automatchTable, c(name, as.character(preferredName), TRUE))
            }
          }
        }
        #WRITE TO SCRIPT 
        writeToScript("n <- nrow(datasetInput)")
        writeToScript("names <- setdiff(datasetInput[[editThisColumn]][start:nrow(datasetInput)], terminology$PreferredName)")
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
        writeToScript("    preferredName <- terminology$PreferredName[which(grepl(searchName, terminology$Synonyms))]")
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
    }
  })
  

# Auto-match Supporting Events ---------------------------------------------
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

# Auto-match Save ----------------------------------------------------------
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
    commentify("Save auto-matches")
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
    renderColumn()
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
      values$unmatched <- setdiff(values$datasetInput[[input$editThisColumn]][values$start:nrow(values$datasetInput)], values$terminology$PreferredName)
      values$unmatched <- setdiff(values$unmatched, NA)
      #WRITE TO SCRIPT 
      commentify("Manual standardization")
      editThisColumn <- gsub("\"", "\\\\\"", input$editThisColumn)
      writeToScript(paste0("editThisColumn <- \"", editThisColumn, "\""))
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
        #replace underscores with spaces before searching for matches in terminology
        if (substr(values$firstEditData, j, j) == '_') {
          values$firstEditData = paste0(substr(values$firstEditData, 0, j - 1), ' ', substr(values$firstEditData, j + 1, nchar(values$firstEditData)))
        }
      }
      searchName <- paste0("(?i)(\\||^)(", values$firstEditData, ")(\\||$)")
      preferredName <- values$terminology$PreferredName[grepl(searchName, values$terminology$Synonyms)]
      if (length(preferredName) == 1 && values$firstEditData != preferredName) {
        values$manualSuggestion <- preferredName
      }
      values$letterList <- values$terminology$PreferredName[substr(values$terminology$PreferredName, 0, nchar(input$newData)) == input$newData]
      updateSelectizeInput(session, 'newData', label = "Select terminology term:", choices = c("", input$newData, as.character(values$manualSuggestion), as.character(values$letterList)), 
                           options = list(placeholder = 'Select terminology term or start typing...', create = T), selected = input$newData, server = T)
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
    renderColumn()
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
    input$generateOutput}, {
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
    
    #script output
    names(outputText) <- NULL
    outputText <- as.data.frame(outputText)
    output$script1 <- downloadHandler(filename = function() {
        paste("good_nomen_R_script", ".txt", sep = "")}, content = function(file) {
        write.table(rbind(outputText, saveUntilEnd), file, row.names = F, col.names = F, quote = F)
    })
  })
}

shinyApp(ui, server)
  