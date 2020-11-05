# Automatch ---------------------------------------------------------------
observeEvent(input$automatch, ignoreInit = T, {
  values$automatchResult <- list()
  
  # Disable the buttons while the matches are loading.
  disable("editThisColumn") # select box
  disable("automatch") # automatch button
  disable("manual") # standardize manually button
  disable("editBack") # back button
  disable("editNext") # next button
  
  # If the column is not selected, throw an error. Otherwise, continue.
  if (is.null(input$editThisColumn) || input$editThisColumn == "") {
    title <- "Error"
    content <- columnNotSelectedMessage
  } else {
    title <- "Review Matches"
  }
  
  # Identify matches
  uniqueTerms <- unique(values$dataset[[input$editThisColumn]])
  sdm <- identifyMatches(uniqueTerms)
  
  matches <- as.data.frame(select(sdm, c(TestTerm, OntologyTerm)) %>%
                             rename(`Current Term` = TestTerm) %>%
                             rename(`Standardized Term` = OntologyTerm) %>%
                             mutate(Accept = TRUE))
  
  values$matches <- matches[!(matches$`Current Term` == matches$`Standardized Term`),]
 
  # Sort the table alphabetically
  values$matches <- matches[order(matches$`Standardized Term`),]
  
  # Output the table
  if (nrow(values$matches) > 0) {
    if (length(matches) > 0) {
      content <- tagList()
      content[[1]] <- p(
        paste(
          "The following matches were found based on the terminology.",
          "Accept a match by checking the box in the row.",
          "Press \"Save\" to apply these changes to your data and close this window.",
          "If a match is accepted, all occurrences of the current term will be changed",
          "to the standardized term."
        )
      )
      content[[2]] <- actionButton('selectAll', label = "Select All")
      content[[3]] <- actionButton('deselectAll', label = "Deselect All")
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
      content <- p("No matches found.")
    }
  } else {
    content <- p("The terms in this column are already standardized or there were no terms to standardize.")
  }
  
  showModal(
    modalDialog(
      content,
      title = title,
      footer = modalButton("Close"),
      size = "l"
    )
  )
  
  renderAutoMatchTable()
  
  # Enable the buttons again.
  enable("editThisColumn") # select box
  enable("automatch") # automatch button
  enable("manual") # standardize manually button
  enable("editBack") # back button
  enable("editNext") # next button
})

# Select all matches
observeEvent(input$selectAll,{
  values$matches[,3] <- rep(TRUE, nrow(values$matches))
  values$selectedPushed <- TRUE
  renderAutoMatchTable()
})

# Deselect all matches
observeEvent(input$deselectAll,  {
  values$matches[,3] <- rep(FALSE, nrow(values$matches))
  values$deselectedPushed <- TRUE
  renderAutoMatchTable()
})

# Show all matches found
renderAutoMatchTable <- function(){
  output$automatchTable <- renderUI({
    tagList(
      tagList(
        fluidRow(
          column(width = 2, align = "center", h4("Current Term")),
          column(width = 3, h4("Standardized Term")),
          column(width = 2, h4("Accept?"))
        )
      ),
      lapply(1:nrow(values$matches), function(i) {
        autoMatchModule(values$matches[i,1], values$matches[i,2], values$matches[i,3])
      }),
    )
  })
}

# Listeners for the automatch table module
automatchTableListener <- function(input, output, session, modID){
  observeEvent(input$checkBox, {
    if (values$deselectedPushed == FALSE && values$selectedPushed == FALSE) {
      if (input$checkBox == FALSE) {
        values$matches[modID,3] <- FALSE
      } else if (input$checkBox == TRUE) {
        values$matches[modID,3] <- TRUE
      }
    } else {
      values$numTimesClicked = values$numTimesClicked + 1
    }
  })
  
  # Generate the automatch table module and connect the listener (see automatchTableListener)
  observe({
    if (!is.null(values$matches)) {
      lapply(1:nrow(values$matches), function(i) {
        callModule(automatchTableListener, values$matches[i,1],i)
      })
    }
  })
  
  # Listener to control the select all and deselect all button
  observe({
    if (values$numTimesClicked >= nrow(values$matches)) {
      values$numTimesClicked <- 0
      values$selectedPushed = FALSE
      values$deselectedPushed = FALSE
    }
  })
}

# Update data based on matches selected 
observeEvent(input$automatchSave, ignoreInit = T, {
  if (length(which(values$matches[,3])) > 0) {
    # Change dataset table values to reflect changes made by editor
    values$lastSelectedEditColumn <- input$editThisColumn
    accepted <- values$matches[,3]
    acceptedList <- values$matches$'Standardized Term'[accepted]
    
    # The if statement checks to make sure that at least 1 term has been selected to save. Else, don't change any terms.
    if (length(acceptedList) > 0 ) {
      names(acceptedList) <- paste0("^", values$matches$`Current Term`[accepted], "$")
      columnNameOfChangedTerms <- input$editThisColumn #The column from the actual datasheet that is to be changed
      datasetInput <- values$dataset
      
      # This tells the R Script which terms we want to change and what we want to change them to
      # It also changes the values in the dataset to their corrected value (if it was checked)
      names <- paste0("^", values$matches$`Current Term`[accepted], "$")
      
      # ADD TEXT TO SCRIPT for automatching
      masterText <<- paste0(masterText, "\n\n# Changing the dataset based on automatch\n", "columnNameOfChangedTerms <- \"", columnNameOfChangedTerms, 
                            "\"\n", "acceptedList <- c(", paste0("'", unname(acceptedList), "'", collapse = ", "), ")",
                            "\n","namesAcceptedList <- c(", paste0("'", names, "'", collapse = ", "), ")",
                            "\nnames(acceptedList) <- namesAcceptedList")
      automatchingText <- "datasetInput[[columnNameOfChangedTerms]] <- str_replace_all(datasetInput[[columnNameOfChangedTerms]], acceptedList)"
      eval(parse(text = automatchingText))
      values$dataset <- datasetInput
      masterText <<- paste0(masterText, "\n", automatchingText)
    }
  }
  removeModal()
})

# Close modal on "close"
observeEvent(input$automatchClose, ignoreInit = T, {
  removeModal()
})