# Manual Standardization ----------------------------------------

# Update data based on selections
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

# Warn the user that terms will be overwritten with NA
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

# Warning that column has not been selected
columnNotSelectedMessage <- HTML(
  '<p style="color:red">You must select a column to edit before proceeding. Please close this window and select a column.</p>'
)

# Modal for selecting a group of terms to standardize
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
    
    # Get standardized terms to choose from
    show_modal_spinner(spin = "spring", color = "#112446",
                       text = p("To help you standardize your data, we are pulling all standardized terms from the ontology you selected.",
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
    
    # Remove the names or else they will be displayed instead of the data in the selector
    names(values$recTermsList) <- NULL
  }
  
  # Modal for selecting standardized terms to replace unstandardized terms
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

# Populate manual modal
output$editDataSelector <- renderUI({
  selectizeInput('editData', label = "Enter terms that have a common meaning:", choices = unique(str_trim(values$dataset[[input$editThisColumn]][order(values$dataset[[input$editThisColumn]])])), multiple = T,
                 options = list(placeholder = "Select a term or start typing..."))
})

# Close modal on "save" and update data
observeEvent(input$saveConfirmContinue, {
  toggleModal(session, "saveConfirm", toggle = "close")
  standardizeManually()
})

# Close modal on "cancel"
observeEvent(input$saveConfirmCancel, {
  toggleModal(session, "saveConfirm", toggle = "close")
})

# Update data on "save"
observeEvent(input$manualSave, {
  standardizeManually()
})

# Display message indicating if changes were successful
output$savedMessage <- renderText({
  values$manualSaveMessage
})

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
  }
})

# Close modal on "cancel"
observeEvent(input$cancelChangeOntology, {
  removeModal()
}, ignoreInit = T)

# Close modal on "close"
observeEvent(input$manualClose, {
  removeModal()
}, ignoreInit = T)
