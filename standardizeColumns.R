# Standardize Columns ---------------------------------------------------------------

# Build widget for selecting column to standardize
output$editThisColumnSelector <- renderUI({
  browser()
  selectizeInput(
    'editThisColumn', 
    label = "Select Column to Standardize:", 
    choices = columns(), selected = values$lastSelectedEditColumn)
})

# Get recommended ontologies from BioPortal
getRecommendedOntologies <- function(dataset) {
  browser()
  testValues <- c()
  sampleDataset <- dataset[, !sapply(dataset, function(x) length(x) == length(unique(x)))]
  sampleDataset <- sampleDataset[, !sapply(sampleDataset, is.numeric) & !sapply(sampleDataset, is.logical) & !sapply(sampleDataset, is.Date) & !sapply(sampleDataset, is.POSIXct)] 
  for (columnName in colnames(sampleDataset)) {
    columnData <- c(columnName, sampleDataset[[columnName]])
    testValues <- c(testValues, sample(unique(columnData), if (length(unique(columnData)) >= NUM_SAMPLES_FROM_COLS) NUM_SAMPLES_FROM_COLS else length(unique(columnData))))
  }
  if (length(testValues) > 1000) {
    testValues <- testValues[1:1000]
  }
  testString <- toString(testValues)
  if (testString == "") {
    return(NULL)
  }
  testString <- URLencode(testString, reserved = TRUE) # Why encode? Characters in a URL other than the English alphanumeric characters and - _ . ~ should be encoded as % plus a two-digit hexadecimal representation, and any single-byte character can be so encoded. The standard refers to this as 'percent-encoding'.
  rURL <- "http://data.bioontology.org/recommender?"
  response <- POST(rURL, body = list(input = testString, apikey = API_KEY, display_links = "false", display_context = "false"))
  return(response)
}

# BioPortal Access (Download Ontologies)
output$ontologySelector <- renderUI({
  recommendedOntologies <- NULL
  if (!is.null(input$userFile)) {
    # List of Ontology Names Recommender   
    # Pop up window informs the user that accessing info from BioPortal will take a while
    tryCatch({
      res <- R.utils::withTimeout(  { 
        show_modal_spinner(spin = "spring", color = "#112446",
                           text = p("To help you standardize your data, we are accessing the entire list of ontologies from ",
                                    (a(href = 'https://bioportal.bioontology.org/', 'BioPortal')), " recommended for your dataset. 
                             Depending on your internet connection and the last time you used Good Nomen, this could take a while.
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
          }, error = function(er) {
          }, finally = {
            if (is.null(bioportalOntologies)) {
              remove_modal_spinner()
              showModal(modalDialog(title = "BioPortal Unavailable for Access",
                                    p("BioPortal seems to be down, please check ",
                                      (a(href = 'https://bioportal.bioontology.org/', 'BioPortal')), " to see if it is working. 
                                      You may need to try again later. We apologize for the inconvenience."),
                                    footer = NULL,
                                    easyClose = F))
            }
          })
          bioportalOntologiesDataFrame <- data.frame(t(sapply(bioportalOntologies,c)))
          bioportalOntologiesDataFrame$nameAndAcronymn = paste0(bioportalOntologiesDataFrame$acronym, " - ", bioportalOntologiesDataFrame$name) # Makes a column with both acronym and name
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
        rURL <- getRecommendedOntologies(values$datasetWithColTypes)
        internetTester = curl::has_internet()
        if (!internetTester) {
          # If the URL is broken,try again a few times
          for (i in 1:NUM_TEST_TIMES) {
            rURL <- getRecommendedOntologies(values$datasetWithColTypes)
            if (url.exists(rURL)) {
              break;
            }
          }
          # Error and try again later
          recommendedOntologies <- ""; 
        }
        else {
          # Get the acronym for the top three recommended ontologies 
          recommenderDF <- NULL
          if (!is.null(rURL)) {
            tryCatch({
              res <- R.utils::withTimeout(  {
                dataFrameRecommend <- content(rURL, "parsed")
                recommenderDF <- as.data.frame(t(sapply(dataFrameRecommend,c)))
              }, timeout = TIMEOUT_TIME)
            }, error = function(er) {
            }, finally = {
              if (is.null(recommenderDF)) {
                timeOutError()
              }
            })
            recTibble <- as_tibble(recommenderDF)
            if (ncol(recTibble) > 1) {
              recTibbleData <- sapply(unnest(select(recTibble, ontologies), ontologies), unlist)
              recTibble <- recTibbleData[seq(1, length(recTibbleData), by = 3)]
              
              # If there are fewer than three elements in the recommended ontology, set the NUM_RECOMMENDED_ONTOLOGIES to the size of the list created
              if (length(recTibble) < NUM_REC_ONTO) {NUM_REC_ONTO <<- length(recTibble)}
              
              recommendedOntologies <- recTibble[1:NUM_REC_ONTO] 
              for (i in 1:NUM_REC_ONTO) {
                thisTerm <- filter(ontologyTibble, Acronym == recommendedOntologies[i]) %>% 
                  select(FullName)
                recommendedOntologies <- replace(recommendedOntologies, i, paste(recommendedOntologies[i], unlist(unname(thisTerm)), 
                                                                                 " ", collapse = " "))
              }
            }
          }
        }
        values$recommendedOntologies <<- recommendedOntologies
      }, timeout = TIMEOUT_TIME)
    })
    
    
    # If no recommended ontologies were found, tell the user
    if (is.null(recommendedOntologies)) {
      showModal(modalDialog(title = "No recommended ontologies found",
                            p("We could not find any information in your file to use for recommending an ontology.",
                              "Please select an ontology manually."),
                            footer = modalButton("Close"),
                            easyClose = TRUE))
    }
    remove_modal_spinner()
    selectizeInput('ontologySelector',
                   label = div(
                     "Select Ontology:",
                     helpButton("Select an ontology from BioPortal.")),
                   choices = list('Recommended Ontologies' = c("", recommendedOntologies),
                                  'All Ontologies' = listOfOntNames),
                   options = list(placeholder = "Select ontology or start typing...",
                                  closeAfterSelect = TRUE),
                                  )
  }
})

# Handle downloading the ontology, check to see if it's locked
observeEvent(input$ontologySelector, {

  values$ontName <<- input$ontologySelector

  # ADD TEXT TO SCRIPT for modifying headers 
  masterText <<- paste0(masterText, "\n\n# Set column names and format datasheet\n", 
                        readInputFileText, "\n", values$headerText)
  
  # Parse the acronym from the ontology name and show it
  values$ontologyAcronym <<- strsplit(values$ontName, " ")[[1]][1]
  
  values$manualSaveMessage <- NULL
  show_modal_spinner(spin = "spring", color = "#112446",
                     text = p("To help you standardize your data, we are pulling standardized terms from ", 
                              (a(href = 'https://bioportal.bioontology.org/annotator', 'BioPortal.')),
                              "Depending on your internet connection, this could take a while", 
                              "Thank you for your patience."))
  
  if (loadOntology()) {
    remove_modal_spinner()
    updateTabsetPanel(session, 'tabs', selected = 'editTable')
  }
})

# Include source code for automatching and manual matching
source('automatch.R', local = TRUE)
source('manualMatch.R', local = TRUE)

# Render message if the user has not uploaded data
output$standardizeColumnsPreviewText <- renderText({
  if (is.null(values$dataset)) {
    "After you have uploaded a file, a preview of your data will appear here."
  } else {
    NULL
  }
})

# Prepare a single column to display
output$singleColumn <- renderDT({
  datatable(values$dataset[, input$editThisColumn], options = list(pageLength = 10), rownames = F)
})



# Display "rename" button when a column and new name have been selected
output$columnRenameButton <- renderUI({
  if (!is.null(input$newColumn) && input$newColumn != "") {
    actionButton('columnRename', "Rename", width = "100%")
  }
})

# Listen for when the user selects a column to rename then suggest a standardized name
observe({
  if (!is.null(input$editThisColumn) && nchar(input$editThisColumn) > 0 && values$ontName != NULL) {  ## TODO make sure this doesn't try to access ontology before it's selected
    disable("newColumn")
    
    show_modal_spinner(spin = "spring", color = "#112446",
                       text = paste0("To help you standardize your data, we are finding recommended column names from the ontology you 
                                       selected. Thank you for your patience."))
    
    sdm <- identifyMatches(input$editThisColumn)
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

# Edit the column names
observeEvent(input$columnRename, ignoreInit = T, {
  # Display warning if user does not select column to rename and new column name
  if (input$editThisColumn == "" | input$newColumn == "") {
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
                               "editThisColumn <- \"", input$editThisColumn, "\"\n",
                               "newColumn <- \"", newColumn, "\"",
                               "\ncolnames(datasetInput)[which(colnames(datasetInput) == editThisColumn)] <- newColumn")
    eval(parse(text = changeColumnText))
    values$dataset <- datasetInput
    
    # ADD TEXT TO SCRIPT for modifying column names
    masterText <<- paste0(masterText, "\n", changeColumnText)
    
    # Keep track of changes
    originalTerm <- input$editThisColumn
    source <- "Column name"
    ontologyTerm <- newColumn
    uri <- values$ids[which(values$preferred == ontologyTerm)]
    if (length(uri) == 0) {
      uri <- NA
    } else {
      uri <- uri[1]
    }
    row <- c(originalTerm, source, NA, ontologyTerm, uri)
    names(row) <- c("Original_Term", "Source", "Column_Name", "Ontology_Term", "Ontology_Term_URI")
    masterChanges <<- rbind(masterChanges, row)
  }
  showNotification(paste0("Column \"", input$editThisColumn, "\" has been renamed to \"", input$newColumn, ".\""))
})