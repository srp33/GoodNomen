# Edit Data ---------------------------------------------------------------

# Display text showing which ontology is selected
output$selectedOntology <- renderUI({
  ontologyLstAcr <- strsplit(values$ontName, " ")
  values$ontologyAcronym <<- ontologyLstAcr[[1]][1]
  urlToOpen <- paste0("https://bioportal.bioontology.org/ontologies/",values$ontologyAcronym)
  HTML(paste("<b>Selected Ontology: </b>",  (a(href = urlToOpen, values$ontName, style = "color:#252525")), collapse = "<BR>"))
})

# Build widget for changing ontology
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

# Build widget for selecting column to standardize
output$editThisColumnSelector <- renderUI({
  selectizeInput(
    'editThisColumn', 
    label = "Select Column to Standardize:", 
    choices = columns(), selected = values$lastSelectedEditColumn)
})

# Include source code for automatching and manual matching
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

# Prepare a single column to display
output$singleColumn <- renderDT({
  datatable(values$dataset[, input$editThisColumn], options = list(pageLength = 10), rownames = F)
})