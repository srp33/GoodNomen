# masterText collects text for an Rscript that will replicate commands executed by Good Nomen
# Instances where masterText is edited are marked with "# ADD TEXT TO SCRIPT" followed by a description of what is being added
masterText <- NULL


# masterChanges collects all of the changes made to the user's data
# After standardization is complete the user has the option to save a tsv file containing all of the changes
masterChanges <- NULL

# readInputFileText will be modified based on whether or not the user chooses to use a row other than the first row as headers
readInputFileText <- NULL

listOfLibrariesUsed <- c("tidyverse")

# Reactive values
values <- reactiveValues(datasetInput = NULL, dataset = NULL, extraHeaders = NULL, headerText = NULL,
                         extension = "", terminology = NULL, lastSelectedEditColumn = "",
                         matches = NULL, ontologyAcronym = "", recommendedOntologies = NULL, 
                         listOfOntNames = NULL, ontName = "", ontVersion = "", totalTermList = NULL, recTermsList = NULL, 
                         synonyms = NULL, preferred = NULL)

# Save the extension of the uploaded file (will be used as the default for how to save the output file)
extension <- reactive({
  if (!is.null(input$userFile)) {
    extSearch <- paste0(paste(str_replace(names(extensionsMap), "\\.", "\\\\."), collapse = "$|"), "$")
    ext <- str_extract(input$userFile$datapath, extSearch)
  }
})

# Column names of dataset updated as they are modified
columns <- reactive({
  colnames(values$dataset)
})