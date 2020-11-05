# Load Data -----------------------------------------------------------------------------------------------------
readInputFile <- function(inFile) {
  fileExt <- paste0(".", file_ext(gsub("\\\\", "/", inFile$datapath)))
  text <- paste0("# Please ensure that your terminology file (", inFile[1],") is in the same directory as this script 
                 before executing. Please also make sure that your R console is in the correct working terminal (use 
                 setwd() to change to the directory that your files are in).")
  installPackages <- addLibrary(listOfLibrariesUsed)
  readInputFileText <<- paste0("datasetInput <- read_", extensionsMap[[fileExt]], "('", inFile$name, "', col_names = FALSE)")
  
  # ADD TEXT TO SCRIPT for loading libraries
  masterText <<- NULL
  masterText <<- paste0(masterText,  installPackages, "\n\n", loadLibraries, "\n\n", text) 
  
  do.call(paste0("read_", extensionsMap[[fileExt]]), list(inFile$datapath, "col_names" = FALSE))
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


# Initialize variables so functionality is enabled and user can click between tabs without pushing "next"
observeEvent(input$file1, ignoreInit = T, {
  withProgress(message = "Initializing Elements", {
    output$inputError <- tryCatch({
      values$datasetInput <<- readInputFile(input$file1)
      if (any(is.na(colnames(values$datasetInput)))) {
        colnames(values$datasetInput)[is.na(colnames(values$datasetInput))] <- "Null2"
      }
      renderText("")
    }, error = function(e) {
      renderText("An error has been detected. Please verify that the file type matches the extension.")
    })
    
    # Header selector must only be set when user first uploads file; otherwise, if the user selects the number of header 
    # lines before the table renders, the box flickers back and forth between selection and default.
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
  if (numericHeader != 1) {
    disable("header")
  }
  setColNames(numericHeader + 1, if (numericHeader > 0) 1 else 0) 
}, ignoreNULL = TRUE)

# Show widget for selecting which row to use as header
output$colnamesSelector <- renderUI({
  if (!is.null(input$header) && as.numeric(input$header) > 1) {
    div(
      tags$b("Please select the header row you would like to use as the column names."),
      DTOutput("headerPreview")
    )
  }
})

# Display options to use for column names
output$headerPreview <- renderDT({
  datatable(
    values$datasetInput[1:as.numeric(input$header),],
    rownames = FALSE,
    colnames = rep("", ncol(values$datasetInput)),
    selection = list(mode = "single", selected = c(1)),
    options = list(dom = "t", scrollX = '300px', ordering = FALSE)
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

# BioPortal Access (Download Ontologies)
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
                                      (a(href = 'https://bioportal.bioontology.org/', 'BioPortal')), " to see if it is working. 
                                      You may need to try again later. We apologize for the inconvenience."),
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
          # Error and try again later
          recommendedOntologies <- ""; 
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
              thisTerm <- filter(ontologyTibble, Acronym == recommendedOntologies[i]) %>% 
                select(FullName)
              recommendedOntologies <- replace(recommendedOntologies, i, paste(recommendedOntologies[i], unlist(unname(thisTerm)), 
                                                                               " ", collapse = " "))
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

# When moving to the page, check that an ontology has been selected
output$page1Next <- renderUI({
  values$ontName <<- input$ontologySelector
  if (input$ontologySelector != "" && !is.null(input$ontologySelector)) {
    actionButton("buttonLoadThenNext", "Next", style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")
  }
}) 

# Display message until a file is uploaded
output$loadDataPreviewText <- renderText({
  if (is.null(values$dataset)) {
    "After you have uploaded a file, a preview of your data will appear here."
  } else {
    NULL
  }
})

# Navigate to page
output$loadDataColNav <- renderUI({
  setColumnNavigation("loadData")
})

# Display data when file is uploaded
output$uploadPreview <- renderDT({
  dataPreview()
})

# Handle downloading the ontology, check to see if it's locked, and move forward to the next page
observeEvent(input$buttonLoadThenNext, {
  
  # ADD TEXT TO SCRIPT for modifying headers 
  masterText <<- paste0(masterText, "\n\n# Set column names and format datasheet\n", 
                        readInputFileText, "\n", values$headerText)
  
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
    downloadURL <- sprintf(paste("http://data.bioontology.org/ontologies/", values$ontologyAcronym, 
                                 "/download?download_format=csv&display_links=false&apikey=", API_KEY, sep = ""))
    
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