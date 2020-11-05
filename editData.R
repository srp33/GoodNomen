# Edit Data ---------------------------------------------------------------

# Display text showing which ontology is selected
output$selectedOntology <- renderUI({
  ontologyLstAcr <- strsplit(values$ontName, " ")
  values$ontologyAcronym <<- ontologyLstAcr[[1]][1]
  urlToOpen <- paste0("https://bioportal.bioontology.org/ontologies/",values$ontologyAcronym)
  HTML(paste("<b>Selected Ontology: </b>",  (a(href = urlToOpen, values$ontName)), collapse = "<BR>"))
})

# Show widget for changing ontology
observeEvent(input$changeOntology, {
  title <- "Change the Ontology"
  
  content <- tagList()
  content[[1]] <- selectizeInput('newOntologySelector', label = "Select New Ontology:", 
                                 choices = list('Recommended Ontologies' = c("", values$recommendedOntologies),
                                                'All Ontologies' = listOfOntNames),
                                 options = list(
                                   placeholder = "Please select an ontology...",
                                   closeAfterSelect = TRUE))
  content[[3]] <- actionButton('resetAndSave', label = "Save and Reset Ontology")
  content[[4]] <- actionButton('cancelChangeOntology', label = "Cancel")
  showModal(
    modalDialog(
      content,
      title = title,
      footer = NULL,
      size = "m"
    )
  )
})

# Show widget for selecting column to standardize
output$editThisColumnSelector <- renderUI({
  selectizeInput(
    'editThisColumn', 
    label = "Select Column to Standardize:", 
    choices = c("", columns()),
    selected = values$lastSelectedEditColumn,
    options = list(placeholder = 'Select column or start typing...',
                   closeAfterSelect = TRUE))
})

source('automatch.R', local = TRUE)
source('manualMatch.R', local = TRUE)

# Render message if the user has not uploaded data
output$editDataPreviewText <- renderText({
  if (is.null(values$dataset)) {
    "After you have uploaded a file, a preview of your data will appear here."
  } else {
    NULL
  }
})

# Display data (either all of the data or a single column if the user has selected a column to update)
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

# Display a single column
output$singleColumn <- renderDT({
  datatable(values$dataset[, input$editThisColumn], options = list(pageLength = 10), rownames = F)
})

# Display all of the data
output$editDataAll <- renderDT({
  dataPreview()
})