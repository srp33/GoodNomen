# Auto-match ---------------------------------------------------------------

# Before letting the user automatch, ensure that an onotology has been selected and a column to edit
output$automatch <- renderUI({
  if (input$ontologySelector != "" && !is.null(input$ontologySelector) && input$editThisColumn != "" && !is.null(input$editThisColumn)) {
    actionButton('automatch', label = div("Auto-match", helpButton("Matches will be found based on synonyms in the selected ontology")), width = "100%")
  }
})

# Search for matches and build widget
observeEvent(input$automatch, ignoreInit = T, {
  values$automatchResult <- list()
  
  # If the column is not selected, throw an error. Otherwise, continue.
  if (is.null(input$editThisColumn) || input$editThisColumn == "") {
    title <- "Error"
    content <- columnNotSelectedMessage
  } else {
    title <- "Review Matches"
  }
  
  # Show information modal
  show_modal_spinner(spin = "spring", color = "#112446",
                     text = p("To identify matching terms, we are pulling all standardized terms from the ontology you selected.",
                              "Thank you for your patience."))
  
  # Identify matches
  uniqueTerms <- unique(values$dataset[[input$editThisColumn]])
  sdm <- as.data.frame(identifyMatches(uniqueTerms))
  
  values$matches <- dplyr::select(sdm, -Score) %>%
    dplyr::rename(`Current Term` = TestTerm) %>%
    dplyr::rename(`Ontology Term` = OntologyTerm) %>%
    dplyr::filter(`Current Term` != `Ontology Term`) %>%
    dplyr::mutate(Accept = TRUE)
    
 
  # Sort the table alphabetically
  values$matches <- values$matches[order(values$matches$`Ontology Term`),]
  
  # Output the table
  if (nrow(values$matches) > 0) {
    content <- tagList()
    content[[1]] <- p(
      paste(
        "The following matches were found based on the terminology.",
        "Accept a match by checking the box in the row.",
        "Press \"Save\" to apply these changes to your data and close this window.",
        "If a match is accepted, all occurrences of the current term will be changed",
        "to the ontology term."
      )
    )
    content[[2]] <- actionButton('selectAll', label = "Select All", style = "color: #fff; background-color: #6baed6; border-color: #6baed6;")
    content[[3]] <- actionButton('deselectAll', label = "Deselect All", style = "color: #fff; background-color: #6baed6; border-color: #6baed6;")
    content[[4]] <- br()
    content[[5]] <- br()
    content[[6]] <- uiOutput("automatchTable")
    content[[7]] <- br()
    content[[8]] <- actionButton('automatchSave', label = "Save")
    content[[9]] <- actionButton('automatchClose', label = "Cancel", class = "secondary_button")
    content[[10]] <- tags$head(tags$style(
      "#automatchModal .modal-footer{ display:none}"
    ))
  } else {
    content <- tagList()
    content[[1]] <- p("The terms in this column are already standardized or there were no matches found.")
    content[[2]] <- actionButton('automatchClose', label = "Cancel", class = "secondary_button")
  }
  
  showModal(
    modalDialog(
      content,
      title = title,
      footer = NULL,
      size = "l"
    )
  )
  
  renderAutoMatchTable()
})

# Select all matches
observeEvent(input$selectAll,{
  values$matches[,3] <- rep(TRUE, nrow(values$matches))
  renderAutoMatchTable()
})

# Deselect all matches
observeEvent(input$deselectAll,  {
  values$matches[,3] <- rep(FALSE, nrow(values$matches))
  renderAutoMatchTable()
})


# Generate one row at a time for the auto-match table
autoMatchModule <- function(current, standard, booleanValue){
  currentString <- str_replace_all(current, regex("\\W+"), "") 
  ns <- NS(currentString)
  tagList(
    fluidRow(
      column(width = AUTOMATCH_COLUMN_WIDTH, p(current, style = "padding:9px")),
      column(width = AUTOMATCH_COLUMN_WIDTH, p(standard, style = "padding:9px")),
      column(width = AUTOMATCH_COLUMN_WIDTH, checkboxInput(ns("checkBox"), value = booleanValue, label = NULL), style = "height:9px;")
    )
  )
}

# Show all matches found
renderAutoMatchTable <- function(){
  output$automatchTable <- renderUI({
    tagList(
      tagList(
        fluidRow(
          column(width = AUTOMATCH_COLUMN_WIDTH, h4("Current Term", style = "padding:9px")),
          column(width = AUTOMATCH_COLUMN_WIDTH, h4("Ontology Term", style = "padding:9px")),
          column(width = AUTOMATCH_COLUMN_WIDTH, h4("Accept?"), style = "padding:9px")
        )
      ),
      lapply(1:nrow(values$matches), function(i) {
        autoMatchModule(values$matches[i,1], values$matches[i,2], values$matches[i,3])
      }),
    )
  })
}

# Update data based on matches selected 
observeEvent(input$automatchSave, ignoreInit = T, {
  # Find which matches were accepted by the user
  # Reactive variables have some limitations, one being that you cannot perform functions on a vector of reactive variables
  # For loops can be used here instead to go through the rows of the matches table and see if the box was checked
  checks <- c()
  nameSpaces <- str_replace_all(values$matches[,1], regex("\\W+"), "")
  
  for (name in nameSpaces) { 
    checkBox <- input[[paste0(name, "-checkBox")]] # with the large test file the input for the checkboxes is always empty...
    checks <- c(checks, checkBox)
  }

  values$matches[,3] <- checks

  if (sum(checks) > 0) {
    # Change dataset table values to reflect changes made by editor
    values$lastSelectedEditColumn <- input$editThisColumn
    accepted <- values$matches[,3]
    acceptedList <- values$matches$'Ontology Term'[accepted]
    
    # The if statement checks to make sure that at least 1 term has been selected to save. Else, don't change any terms.
    if (length(acceptedList) > 0) {
      names(acceptedList) <- values$matches$`Current Term`[accepted]
      columnNameOfChangedTerms <- input$editThisColumn # The column from the actual datasheet that is to be changed
      datasetInput <- values$dataset
      
      # This tells the R Script which terms we want to change and what we want to change them to
      # It also changes the values in the dataset to their corrected value (if it was checked)
      names <- names(acceptedList)
      
      # ADD TEXT TO SCRIPT for auto-matching
      masterText <<- paste0(masterText, "\n\n# Changing the dataset based on auto-match\n", "columnNameOfChangedTerms <- \"", columnNameOfChangedTerms, 
                            "\"\n", "acceptedList <- c(", paste0("'", unname(acceptedList), "'", collapse = ", "), ")",
                            "\n","namesAcceptedList <- c(", paste0("'", names, "'", collapse = ", "), ")",
                            "\nnames(acceptedList) <- namesAcceptedList")
      automatchingText <- "datasetInput[[columnNameOfChangedTerms]] <- str_replace_all(datasetInput[[columnNameOfChangedTerms]], fixed(acceptedList))"
      eval(parse(text = automatchingText))
      values$dataset <- datasetInput
      masterText <<- paste0(masterText, "\n", automatchingText)
      
      originalTerms <- values$matches$`Current Term`[accepted]
      ontologyTerms <- values$matches$`Ontology Term`[accepted]
      rows <- NULL
      # Some ontology terms map to multiple URIs, so a for loop is necessary instead of more condensed statements
      for (i in 1:length(originalTerms)) {
        originalTerm <- originalTerms[i]
        source <- "Data value"
        ontologyTerm <- ontologyTerms[i]
        uri <- values$ids[which(values$preferred == ontologyTerm)][1]
        row <- c(originalTerm, source, columnNameOfChangedTerms, ontologyTerm, uri)
        names(row) <- c("Original_Term", "Source", "Column_Name", "Ontology_Term", "Ontology_Term_URI")
        rows <- rbind(rows, row)
      }
      masterChanges <<- rbind(masterChanges, rows)
    }
  }
  removeModal()
})

# Close modal on "close"
observeEvent(input$automatchClose, ignoreInit = T, {
  removeModal()
})