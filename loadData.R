# Load Data -----------------------------------------------------------------------------------------------------
readInputFile <- function(inFile) {
  fileExt <- paste0(".", file_ext(gsub("\\\\", "/", inFile$datapath)))
  text <- paste0("# Please ensure that your terminology file (", inFile[1],") is in the same directory as this script before executing. Please also make sure that your R console is in the correct working terminal (use setwd() to change to the directory that your files are in).")
  
  if (fileExt == ".xls" | fileExt == ".xlsx") {
    library(readxl)
    library(writexl)
    listOfLibrariesUsed <- c(listOfLibrariesUsed, "readxl", "writexl")
    loadLibraries <- paste0(loadLibraries, "\nlibrary(readxl)\nlibrary(writexl)")
  }
  
  installPackages <- addLibrary(listOfLibrariesUsed)
  readInputFileText <<- paste0("datasetInput <- read_", extensionsMap[[fileExt]], "('", inFile$name, "', col_names = FALSE)")
  
  # ADD TEXT TO SCRIPT for loading libraries
  masterText <<- NULL
  masterText <<- paste0(masterText,  installPackages, "\n\n", loadLibraries, "\n\n", text) 
  
  values$datasetWithColTypes <- do.call(paste0("read_", extensionsMap[[fileExt]]), list(inFile$datapath))
  datasetWithoutColTypes <- as.data.frame(values$datasetWithColTypes) %>%
    mutate_all(as.character)
  datasetWithoutColTypes <- rbind(colnames(datasetWithoutColTypes), datasetWithoutColTypes)
  return(as_tibble(datasetWithoutColTypes))
}

# After uploading file, select row to use as column names (and number of header rows)
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
    "if (colNameRow > startRow) {\n",
    "\tstartRow <- startRow - 1\n",
    "\tdatasetInput <- datasetInput[-",colNameRow,",]\n",
    "\tdatasetInput <- rbind(extraHeaders[1,], datasetInput)\n",
    "\textraHeaders <- extraHeaders[-1,]\n",
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
  return()
}


# Initialize variables so functionality is enabled and user can click between tabs without pushing "next"
observeEvent(input$userFile, ignoreInit = T, {
  req(input$userFile)
  session$sendCustomMessage("upload_msg", "YOUR TEXT")
  withProgress(message = "Initializing Elements", {
    output$inputError <- tryCatch({
      values$datasetInput <- readInputFile(input$userFile)
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
    incProgress(1/9, detail = "file name selector")
    output$extensionSelector <- renderUI({
      selectExt <- if (grepl("xls", extension())) ".xlsx" else extension()
      selectInput('extension', label = div("Select Extension:", helpButton("Select an extension for the output file.")), 
                  choices = setdiff(names(extensionsMap), c(".xls", ".txt")), selected = selectExt) #.xls
    })
    incProgress(1/9, detail = "extension selector")
    incProgress(1/9, detail = "edit data selector")
  })
})

# Update column names when number of header rows is selected
observeEvent(input$header, {
  numericHeader <- as.numeric(input$header)
  setColNames(numericHeader + 1, if (numericHeader > 0) 1 else 0) 
}, ignoreNULL = TRUE)

# Show widget for selecting which row to use as header
output$colnamesSelector <- renderUI({
  if (!is.null(input$userFile)) {
    div(
      tags$b("Please select the row you would like to use as the column names."),
      DTOutput("headerPreview")
    )
  }
})

# Display options to use for column names
output$headerPreview <- renderDT({
  datatable(
    values$datasetInput[1:NUM_DISPLAY_ROWS,],
    rownames = FALSE,
    colnames = rep("", ncol(values$datasetInput)),
    selection = list(mode = "single", selected = c(1)),
    style = "bootstrap",
    options = list(dom = "t", scrollX = '300px', ordering = FALSE,
                   columnDefs = list(list(visible = TRUE, targets = 0:(NUM_DISPLAY_COLS - 1)), list(visible = FALSE, targets = "_all"))),
  )
})

# Update the dataset if a new row is selected to use for column names
# "_rows_selected" is not part of the variable name-it is required to retrieve the row selected by the user
observeEvent(input$headerPreview_rows_selected, {
  if (any(is.na(colnames(values$datasetInput)))) {
    colnames(values$datasetInput)[is.na(colnames(values$datasetInput))] <- "Null3"
  }
  setColNames(as.numeric(input$header) + 1, input$headerPreview_rows_selected)
}, ignoreNULL = TRUE)

# Before moving to the next page, check that an ontology has been selected
output$firstPageNext <- renderUI({
  # values$ontName <<- input$ontologySelector ## TODO move this line to the appropriate place in standardize columns
  if (input$headerPreview_rows_selected != "" && !is.null(input$headerPreview_rows_selected)) {
    actionButton("buttonLoadThenNext", "Next", style = "float: right; color: #fff; background-color: #2ca25f; border-color: #2ca25f")
  }
}) 

# Handle downloading the ontology, check to see if it's locked, and move forward to the next page  ## TODO pull apart and put appropriate parts in standardize columns
# observeEvent(input$buttonLoadThenNext, {
  
#   # ADD TEXT TO SCRIPT for modifying headers 
#   masterText <<- paste0(masterText, "\n\n# Set column names and format datasheet\n", 
#                         readInputFileText, "\n", values$headerText)
  
#   # Parse the acronym from the ontology name and show it
#   values$ontologyAcronym <<- strsplit(values$ontName, " ")[[1]][1]
  
#   values$manualSaveMessage <- NULL
#   show_modal_spinner(spin = "spring", color = "#112446",
#                      text = p("To help you standardize your data, we are pulling standardized terms from ", 
#                               (a(href = 'https://bioportal.bioontology.org/annotator', 'BioPortal.')),
#                               "Depending on your internet connection, this could take a while", 
#                               "Thank you for your patience."))
  
#   if (loadOntology()) {
#     remove_modal_spinner()
#     updateTabsetPanel(session, 'tabs', selected = 'editTable')
#   }
# })


# Display a summary of the data when file is uploaded
output$uploadPreview <- renderText({
  paste0("Thank you for uploading a file! We detected ", nrow(values$datasetInput), " rows and ", ncol(values$datasetInput), " columns. ",
           "Please use the controls on the left to continue.")
})
