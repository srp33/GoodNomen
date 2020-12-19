# Save Data ---------------------------------------------------------------
# Widget for output file name
output$outputFileNameUI <- renderUI({
  textInput('outputFileName',
            label = div(
              "Output File Name (without extension):",
              helpButton("Enter a name for the output file (do not include extension).")
            ),
            value = paste0(
              substr(input$userFile, 0, (nchar(input$userFile) - if (extension() == ".xlsx") 5 else 4)), "_GoodNomen"
            )[1])
})

# Build the buttons for downloading output files
output$downloadButtons <- renderUI({
  if (!is.null(values$dataset)) {
    output <- tagList()
    output[[1]] <- downloadButton('editReport', width = "100%", label = div("Download Output File", 
                                                                            helpButton("Output file contains updated data.")),
                                  style = "display: block; color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 9px;")
    output[[2]] <- downloadButton('script', width = "100%", label = div("Download R Script",
                                                                         helpButton(paste("R Script contains all of the commands", 
                                                                                          "necessary to create the output file from", 
                                                                                          "the original file."))),
                                  style = "display: block; color: #fff; background-color: #2ca25f; border-color: #2ca25f; margin-bottom: 9px;")
    output[[3]] <- downloadButton('changes', width = "100%", label = div("Download Changes",
                                                                         helpButton(paste("File contains a record of all of the changes", 
                                                                                          "made to the data."))),
                                  style = "display: block; color: #fff; background-color: #6baed6; border-color: #6baed6;")
    output # This ensures that the buttons will appear
  }
})

# Download output file
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

# Download R script
output$script <- downloadHandler(
  filename = function() {
    paste0(input$outputFileName, "_R_script.R")
  }, content = function(file) {
    thisExtension <- if (nchar(input$extension) == 0) extension() else input$extension
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

# Download changes
output$changes <- downloadHandler(
  filename = function() {
    paste0(input$outputFileName, "_Changes.tsv")
  }, content = function(file) {
    write_tsv(as.data.frame(masterChanges), file)
    showNotification(paste0("Your file was successfully saved."))
  })

# Message before data is ready to be displayed
output$saveDataPreviewText <- renderText({
  if (is.null(values$dataset)) {
    "After you have uploaded a file, a preview of your data will appear here."
  } else {NULL}
})
  
# Display data
output$saveDataPreview <- renderText(paste0("Thank you for using Good Nomen! You made ", if (!is.null(masterChanges)) nrow(masterChanges) else "no",  " unique changes to your data. ",
                                            "Please use the controls on the left to download the output files."))

