# Update Column Names -----------------------------------------------------

# Build widget for selecting column to rename
output$editColumnSelector <- renderUI({
  selectizeInput(
    'editColumn', 
    label = "Specify Column to Rename:", 
    choices = columns())
})

# Display "rename" button when a column and new name have been selected
output$columnRenameButton <- renderUI({
  if (!is.null(input$newColumn) && input$newColumn != "") {
    actionButton('columnRename', "Rename", width = "100%")
  }
})

# Listen for when the user selects a column to rename then suggest a standardized name
observe({
  if (!is.null(input$editColumn) && nchar(input$editColumn) > 0) {
    disable("newColumn")
    
    show_modal_spinner(spin = "spring", color = "#112446",
                       text = paste0("To help you standardize your data, we are finding recommended column names from the ontology you 
                                       selected. Thank you for your patience."))
    
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

# Edit the column names
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
    
    # Keep track of changes
    originalTerm <- input$editColumn
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
  showNotification(paste0("Column \"", input$editColumn, "\" has been renamed to \"", input$newColumn, ".\""))
})

# Display message if user has not uploaded a file
output$updateColNamesPreviewText <- renderText({
  if (is.null(values$dataset)) {
    "After you have uploaded a file, a preview of your data will appear here."
  } else {
    NULL
  }
})

# Display the column the user has selected to rename
output$updateSingleColumn <- renderDT({
  if (!is.null(input$editColumn)) {
    if (input$editColumn %in% colnames(values$dataset)) {
      datatable(values$dataset[, input$editColumn], options = list(pageLength = 10), rownames = F)
    }
  }
})