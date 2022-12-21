source('librariesToLoad.R', local = TRUE)
source('globalVariables.R', local = TRUE)
source('UI.R', local = TRUE)

server <- function(input, output, session) { 
  session$allowReconnect(TRUE)
  source('sharedVariables.R', local = TRUE)
  source('sharedFunctions.R', local = TRUE)
  source('matchingFunctions.R', local = TRUE)
  source('loadData.R', local = TRUE)
  source('standardizeColumns.R', local = TRUE)
  source('saveData.R', local = TRUE)
  source('tabNavigation.R', local = TRUE)
}

shinyApp(ui, server)
