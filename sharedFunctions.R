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

# Navigation button between pages of the app
setColumnNavigation <- function(identifier) {
  if (!is.null(values$dataset)) {
    div(
      actionButton(paste0("backBtn_", identifier), icon("arrow-left"), class = "retract_view",
                   style = "margin-left: 20px;color: #fff;background-color: #2ca25f;border-color: #2ca25f"),
      actionButton(paste0("nextBtn_", identifier), icon("arrow-right"), class = "advance_view", 
                   style = "display: block;float:right;margin-right: 20px;color: #fff;background-color: #2ca25f;border-color: #2ca25f")
    )
  }
}