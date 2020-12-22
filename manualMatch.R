# Manual Standardization ----------------------------------------

# Before letting the user manual match, ensure that an onotology has been selected and a column to edit
output$manual <- renderUI({
  if (input$ontologySelector != "" && !is.null(input$ontologySelector) && input$editThisColumn != "" && !is.null(input$editThisColumn)) {
    actionButton('manual', label = div("Manual", helpButton("Update selected terms to a manually chosen ontology term.")), width = "100%")
  }
})

# Before letting the user apply changes to the data, ensure that they have selected terms to modify
output$nextManualModal <- renderUI({
  if (input$editData != "" && !is.null(input$editData)) {
    actionButton('nextManualModal', 'Next')
  }
})

# Update data based on selections
standardizeManually <- function() {
  newData <- if (is.null(input$newData) || input$newData == "" || input$saveConfirmBtn) NA else input$newData
  editThisColumn <- gsub("\"", "\\\\\"", input$editThisColumn)
  numItems <- length(input$editData)
  
  withProgress(message = "Standardizing", {
    values$lastSelectedEditColumn <- input$editThisColumn
    # ADD TEXT TO SCRIPT for manual standardization
    masterText <<- paste0(masterText, "\n\n", "# Manual standardization\neditThisColumn <- \"", editThisColumn, "\"\n")
    rows <- NULL
    
    for (item in input$editData) {
      incProgress(1/numItems, detail = item)
      
      # Update dataset
      datasetInput <- values$dataset
      manualText <- paste0("datasetInput[[editThisColumn]][datasetInput[[editThisColumn]] == \"", item, "\"] <- \"", newData, "\"\n")
      eval(parse(text = manualText))
      values$dataset <- datasetInput
      masterText <<- paste0(masterText, manualText)
      
      # Keep track of changes
      originalTerm <- item
      source <- "Data value"
      ontologyTerm <- newData
      uri <- values$ids[which(values$preferred == ontologyTerm)]
      if (length(uri) == 0) {
        uri <- NA
      } else {
        uri <- uri[1]
      }
      row <- c(originalTerm, source, editThisColumn, ontologyTerm, uri)
      names(row) <- c("Original_Term", "Source", "Column_Name", "Ontology_Term", "Ontology_Term_URI")
      rows <- rbind(rows, row)
    }
  })
  
  masterChanges <<- rbind(masterChanges, rows)
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
    content[[5]] <- div(uiOutput("nextManualModal"), style = "float:right")
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

# Start standardization when the user presses "Manual Standardization"
observeEvent(input$manual, {
  startManual()
})

# Start standardization when the user presses "Standardize Another Group of Terms"
observeEvent(input$manualAnother, {
  startManual()
})

# Start standardization when the user presses "Standardize Another Group of Terms" within NA standardization
observeEvent(input$manualNAAnother, {
  startManual()
})

# After user selects terms to standardize, get standardized term suggestions and build widget
observeEvent(input$nextManualModal, {
  termsToStandardizeManually <- paste0(unlist(input$editData), collapse = ', ')
  values$manualSaveMessage <- NULL
  
  if (input$makeNA == 0) {
    
    # Show information modal
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
        "The options in box are from the selected terminology. By default, the program will pick the top rated term that connects all the data. 
         To apply changes to your data, press \"Save.\"",
        "All occurrences of the terms you selected on the previous page will be replaced with the selected terminology term."
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
    updateSelectizeInput(session, 'newData', choices = list('Recommended Terms' = c(values$recTermsList, ""),
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

# Close modal on "cancel"
observeEvent(input$cancelChangeOntology, {
  removeModal()
}, ignoreInit = T)

# Close modal on "close"
observeEvent(input$manualClose, {
  removeModal()
}, ignoreInit = T)
