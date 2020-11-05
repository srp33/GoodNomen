# Reactive Values
values <- reactiveValues(datasetInput = NULL, dataset = NULL, extraHeaders = NULL, headerText = NULL,
                         extension = "", terminology = NULL, lastSelectedEditColumn = "", viewingSubset = c(1, 5),
                         matches = NULL, ontologyAcronym = "",
                         recommendedOntologies = NULL, listOfOntNames = NULL, ontName = "", TOTAL_TERM_LIST = NULL,
                         recTermsList = NULL, deselectedPushed = FALSE,
                         selectedPushed = FALSE, numTimesClicked = 0, synonyms = NULL, preferred = NULL)

extension <- reactive({
  if (!is.null(input$file1)) {
    extSearch <- paste0(paste(str_replace(names(extensionsMap), "\\.", "\\\\."), collapse = "$|"), "$")
    ext <- str_extract(input$file1$datapath, extSearch)
  }
})

columns <- reactive({
  colnames(values$dataset)
})

dataPreview <- reactive({
  if (!is.null(values$dataset)) {
    datatable(
      values$dataset[, values$viewingSubset[1]:values$viewingSubset[2]], 
      rownames = FALSE, 
      options = list(dom = "tp", pageLength = 10,
                     columnDefs = list(list(
                       targets = "_all",
                       # Makes it so that the table will only display the first (colWidth()) chars.
                       # See https://rstudio.github.io/DT/options.html
                       # We want to display at least 30 chars.
                       render = JS(
                         paste0("function(data, type, row, meta) {",
                                "return type === 'display' && typeof data === 'string' && data.length > ", max(floor(colWidth() / 2), 15), " ?",
                                "'<span title=\"' + data + '\">' + data.substr(0,", max(floor(colWidth() / 2), 15), ") + '...</span>' : data;",
                                "}"))
                     ))))
  }
})

# The width of the columns, as determined by the width of the column names
colWidth <- reactive({
  mean(nchar(colnames(values$dataset)))
}) 

# Makes sure that we view only the number of columns that will fit on the screen at any given time. A small screen fits about 75 
# characters at a time. We don't want to display more than 5 columns at a time.
moveBy <- reactive({
  min(max(floor(75 / colWidth()), 1), 5)
})