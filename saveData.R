# Save Data ---------------------------------------------------------------

# Name Output File 
output$downloadButtons <- renderUI({
  if (!is.null(values$dataset)) {
    output <- tagList()
    output[[1]] <- downloadButton('editReport', width = "100%", label = div("Download Output File", 
                                                                            helpButton("Output file contains updated data.")),
                                  style = "display: block; color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 9px;")
    output[[2]] <- downloadButton('script1', width = "100%", label = div("Download R Script",
                                                                         helpButton(paste("RScript contains all of the commands", 
                                                                                          "necessary to create the output file from", 
                                                                                          "the original file."))),
                                  style = "display: block; color: #fff; background-color: #2ca25f; border-color: #2ca25f; margin-bottom: 9px;")
    output # This ensures that the buttons will appear
  }
})

# Download Output File
output$editReport <- downloadHandler(
  filename = function() {
    fileName <- if (input$outputFileName == "") "shiny_output" else input$outputFileName
    thisExtension <- if (nchar(input$extension) == 0) extension() else input$extension
    return(paste0(fileName, thisExtension))
  }, 
  content = function(file) {
    thisExtension <- if (nchar(input$extension) == 0) extension() else input$extension
    toWrite <- if (grepl("xls", thisExtension)) "xlsx" else extensionsMap[[thisExtension]]
    if (!is.null(values$extraHeaders)) {
      values$dataset <- rbind(setNames(values$extraHeaders, names(values$dataset)), values$dataset)
    }
    return(do.call(paste0("write_", toWrite), list(values$dataset, file, "col_names" = (input$header != "0"))))
  }
)

# Download R script. Format is downloadHandler(filename, content)
output$script1 <- downloadHandler(
  filename = function() {
    paste0(input$outputFileName, "_R_script.R")
  }, content = function(file) {
    fileName <- if (input$outputFileName == "") "shiny_output" else input$outputFileName
    thisExtension <- if (nchar(input$extension) == 0) extension() else input$extension
    fullFileName <- paste0(fileName, ".R")
    
    # ADD TEXT TO SCRIPT for saving the file
    masterText <<- paste0(masterText, "\n", "\n# Save file\n",
                          "if (!is.null(extraHeaders)) {\n",
                          "\tdatasetInput <- rbind(setNames(extraHeaders, names(datasetInput)), datasetInput)\n",
                          "}\n",
                          "file <- '", paste0(input$outputFileName, thisExtension),
                          "'\n", paste0("write_", substring(thisExtension, 2)), "(datasetInput, file)",
                          "\nprint('Your file has been successfully saved and modified with the name: ",input$outputFileName, "')")
    write.table(masterText, file, row.names = F, col.names = F, quote = F)
    showNotification(paste0("Your file was successfully saved."))
  })

# Message before data is ready to be displayed
output$saveDataPreviewText <- renderText({
  if (is.null(values$dataset)) {
    "After you have uploaded a file, a preview of your data will appear here."
  } else {NULL}
})

# Navigate to page
output$saveDataColNav <- renderUI({
  if (!is.null(values$dataset)) {
    setColumnNavigation("saveData")
  }
})

# Display data
output$saveDataPreview <- renderDT({
  dataPreview()
})
