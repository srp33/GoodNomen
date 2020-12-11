# Install packages if necessary
addLibrary <- function(librariesList) {
  installPackages <- ""
  for (libName in librariesList) {
    installPackages <- paste0(installPackages, "\nif (!suppressWarnings(require(", libName, ", quietly = TRUE))) {", 
                              '  install.packages("', libName, '")',"}")
  }
  return(installPackages)
}

# Error when ontology is unavailable
lockedOntologyError <- function(){
  listOfOntNames <- readLines(ONTOLOGY_LIST_FILE_PATH)
  listOfOntNames <- listOfOntNames[!(listOfOntNames %in% values$ontName)]
  values$recommendedOntologies <<- values$recommendedOntologies[!(values$recommendedOntologies %in% values$ontName)]
  title <- "Error: Ontology locked for download."
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

# Error when ontology is missing preferred terms column
missingPreferredError <- function(){
  listOfOntNames <- readLines(ONTOLOGY_LIST_FILE_PATH)
  listOfOntNames <- listOfOntNames[!(listOfOntNames %in% values$ontName)]
  values$recommendedOntologies <<- values$recommendedOntologies[!(values$recommendedOntologies %in% values$ontName)]
  title <- "Error: Ontology missing preferred terms column."
  content <- tagList()
  content[[1]] <- p("The ontology you selected does not have a preferred terms column. Please select a different ontology.")
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

# Error when process takes too long
timeOutError <- function() {
  message("Timeout reached.")
  showModal(modalDialog(title = "Timeout Error", "The internet took too long to access and timed out.
                                    Try accessing BioPortal on a browser and see if it's working.
                                    If so, try running this app again.",
                        footer = modalButton("Dismiss"), easyClose = F))
}

# Load ontology and save information about preferred terms and synonyms
# Other parts of the app depend on what happens in this function, so it returns a boolean indicating whether loading the ontology succeeded
loadOntology <- function() {
  # Get the last date modified from a file and see if it's been 7 days
  ontFileName <- paste0(TEMP_DIR_PATH, values$ontologyAcronym, "_Ontology.feather")
  allFileName <- paste0(TEMP_DIR_PATH, values$ontologyAcronym, "_All_Terms.feather")
  
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
      ontologyFile <- NULL
      tryCatch({
        tmpFilePath <- paste0(tempfile(), ".csv.gz")
        res <- R.utils::withTimeout({
          testDownloadURL <- GET(downloadURL, write_disk(tmpFilePath))
          ontologyFile <- suppressMessages(suppressWarnings(read_csv(tmpFilePath)))
        }, timeout = TIMEOUT_TIME)
      }, error = function(er) {
      }, finally = {
        unlink(tmpFilePath)
        if (is.null(ontologyFile)) {
          remove_modal_spinner()
          timeOutError()
          return(FALSE)
        }
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
      } else {
        missingPreferredError()
      }
      
      # We want the column of synonyms to be named "Synonym" but do not want to overwrite the synonyms if they are already in a column called "Synonym"
      if (!("Synonym" %in% colnames(ontologyFile))) {
        ontologyFile <- dplyr::rename(ontologyFile, Synonym = Synonyms)
      }
      
      ontologyFile <- ontologyFile %>%
        filter(!Obsolete) %>%
        select(-Definitions, -Obsolete) %>%
        separate_rows(Synonym, sep=" ?\\| ?")
      
      # The ID column contains URIs for each of the preferred terms
      # These are collected during the matching process so the user can reference all of the changes made to the data
      if ("Class ID" %in% colnames(ontologyFile)) {
        ontologyFile <- dplyr::rename(ontologyFile, ID = `Class ID`)
      } else if (!("ID" %in% colnames(ontologyFile))) {
        ontologyFile <- mutate(ontologyFile, ID = rep(NA, nrow(ontologyFile)))
      }
      
      values$synonyms <- pull(ontologyFile, Synonym)
      values$preferred <- pull(ontologyFile, Preferred)
      values$ids <- pull(ontologyFile, ID)
      
      # Build a tibble with "clean" terms alongside the original terms. If there are no synonyms then use the preferred terms.
      if (all(is.na(values$synonyms))) {
        matchedTerms = buildTermTibble(pull(ontologyFile, Preferred))
      } else {
        matchedTerms = buildTermTibble(pull(ontologyFile, Synonym))
      }
      values$totalTermList <- matchedTerms
      
      write_feather(matchedTerms, ontFileName)
      write_feather(select(ontologyFile, c(Preferred, Synonym, ID)), allFileName)
    }
  } else {
    ontologyTerms <- read_feather(ontFileName)
    values$totalTermList <- ontologyTerms
    
    ontologyFile <- read_feather(allFileName)
    values$synonyms <- pull(ontologyFile, Synonym)
    values$preferred <- pull(ontologyFile, Preferred)
    values$ids <- pull(ontologyFile, ID)
  }
  return(TRUE)
}

# Change the ontology
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
    loadOntology()
  }
})