# User Interface (UI) ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "Logo.png"),
    tags$style("body { word-wrap: break-word; }")
  ),
  includeScript("www/reactive_preferences.js"),
  useShinyjs(),
  navbarPage(title = "Good Nomen", id = 'tabs',
             # Load Data ---------------------------------------------------------------
             tabPanel('Load Data', value = 'loadData', 
                      sidebarLayout(
                        sidebarPanel(tags$img(src = 'Logo.png', align = "right", height = "100px"),
                                     h4("Load Data"),
                                     p("Welcome to Good Nomen, an interface for mapping clinical data files based on standardized terminologies."),
                                     p(paste0(
                                       "Please upload a file containing patient data on each row and clinical variables in each column. ",
                                       "Accepted file types include ", collapseText(names(extensionsMap)), ".")
                                     ), 
                                     fileInput(inputId = "file1", label = "Choose Input File:", 
                                               multiple = FALSE, accept = names(extensionsMap), width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                                     textOutput("inputError"), tags$head(tags$style("#inputError {color: red;}")),
                                     uiOutput("headerSelector"),
                                     uiOutput("colnamesSelector"), br(), br(),
                                     uiOutput("ontologySelector"),
                                     textOutput("error"), tags$head(tags$style("#error {color: red;}")), hr(),
                                     conditionalPanel(condition = "input.whichTerminology == 'Upload a terminology'",
                                                      uiOutput("uploadTerminologyButton")),
                                     uiOutput("page1Next")), 
                        # Data Preview 
                        mainPanel(
                          tags$em(textOutput("loadDataPreviewText")),
                          wellPanel(uiOutput("loadDataColNav"), shinycssloaders::withSpinner(DTOutput("uploadPreview"), color = "#112446"))
                        ))),
             
             # Edit Data (Auto/Manual) ---------------------------------------------------------------
             tabPanel('Edit Data', value = 'editTable', 
                      sidebarPanel(width = 4,
                                   tags$img(src = 'Logo.png', align = "right", height = "100px"),
                                   h4("Edit Data"),
                                   p(
                                     paste(
                                       "Data may be standardized automatically or manually.",
                                       "First select the name of the column containing the data you wish to edit.",
                                       "If you would like to automate the matching process, press \"Auto-match.\"",
                                       "The data will be processed and then a pop-up window will appear and ask you to review the matches.",
                                       "If you would like to manually update the data, press \"Standardize Manually.\"",
                                       "A different pop-up window will appear with instructions on how to edit the data.",
                                       "When finished, press \"Next.\""
                                     )
                                   ), br(),
                                   htmlOutput("selectedOntology"),
                                   actionButton('changeOntology', label = div("Change Ontology", helpButton("Click to change which ontology you want to use")),
                                                style = "color: #fff; background-color: #6baed6; border-color: #6baed6;"), br(), br(),
                                   uiOutput("editThisColumnSelector"),
                                   conditionalPanel(condition = 'input.editThisColumn', 
                                                    tagList(
                                                      actionButton('automatch', label = div("Auto-match", helpButton("Matches will be found based on synonyms in the selected terminology."))),
                                                      actionButton('manual', label = div("Standardize Manually", helpButton("Update selected terms to manually chosen standardized term.")))
                                                    )),
                                   uiOutput("resetAndSave"), hr(),
                                   uiOutput("cancelChangeOntology"), hr(),
                                   div(
                                     actionButton('editBack', "Back", css.class = "back_button"),
                                     actionButton('editNext', "Next", css.class = "next_button"))
                      ),
                      mainPanel(
                        tags$em(textOutput("editDataPreviewText")),
                        wellPanel(uiOutput("editDataPreview"))
                      )
             ),
             
             # Update Column Names -----------------------------------------------------
             tabPanel('Update Column Names', value = 'updateColumnNames',
                      sidebarPanel(width = 4,
                                   tags$img(src = 'Logo.png', align = "right", height = "100px"),
                                   h4("Update Column Names"),
                                   p(
                                     paste(
                                       "If desired, select a column to rename and a new column name. When a column to rename is selected,",
                                       "the new column name box will be autofilled with a suggested name if a matching term is found in the",
                                       "terminology. This term may be changed. Press \"Rename\" to update. Multiple columns may be renamed.",
                                       "When finished, press \"Next.\""
                                     )
                                   ), 
                                   uiOutput("editColumnSelector"),
                                   conditionalPanel(
                                     condition = 'input.editColumn',
                                     selectizeInput(
                                       'newColumn',
                                       label = "Select New Column Name:",
                                       choices = NULL,
                                       options = list(placeholder = 'Please select a column above...',
                                                      closeAfterSelect = TRUE)
                                     ),
                                     uiOutput("columnRenameButton")
                                   ), 
                                   bsModal(# Warning if user does not select column to rename and new column name
                                     'columnModal',
                                     title = "Error",
                                     trigger = 'input.newColumn',
                                     HTML(paste('<p color="black">You must select a column to rename and a new column name.", 
                                                "Please close this window and select these items.</p>')),
                                     tags$head(tags$style("#columnModal {color: red;}"))
                                   ), 
                                   bsModal(# Warning if user selects a new column name that is already being used as a column name
                                     'equalModal',
                                     title = "Error",
                                     trigger = 'input.newColumn',
                                     HTML(paste('<p color="black">The selected new column name is already being used as a column name.", 
                                                "Please close this window and select a different name.</p>')),
                                     tags$head(tags$style("#equalModal {color: red;}"))
                                   ), hr(), div(
                                     actionButton('columnBack', "Back", class = "back_button"),
                                     actionButton('columnSubmit', "Next", class = "next_button")
                                   )
                      ), 
                      # ** Data Preview 
                      mainPanel(
                        tags$em(textOutput("updateColNamesPreviewText")),
                        wellPanel(uiOutput("updateColNamesPreview"))
                      )
             ),
             
             # Save Data ---------------------------------------------------------------
             tabPanel('Save Data', value = 'finalReport', 
                      sidebarPanel(
                        tags$img(src = 'Logo.png', align = "right", height = "100px"),
                        h4("Save Data"),
                        p("Enter a name for the output file and select an extension. Do not include the extension in the file name."),
                        uiOutput('outputFileNameUI'),
                        uiOutput('extensionSelector'),
                        uiOutput("downloadButtons"),
                        uiOutput("tab"), hr(),
                        actionButton('saveBack', "Back", class = "back_button")
                      ),
                      # ** Data Preview 
                      mainPanel(
                        tags$em(textOutput("saveDataPreviewText")),
                        uiOutput("saveDataColNav"),
                        wellPanel(shinycssloaders::withSpinner(DTOutput("saveDataPreview"), color = "#112446"))
                      )
             ),
             
             # Contact -----------------------------------------------------------------
             tabPanel('Contact', value = 'contactPage',
                      fluidRow(
                        column(width = 2,
                               tags$img(src = 'Logo.png', height = "165px", align = "center")
                        ),
                        column(width = 9,
                               h4("Contact"),
                               HTML(paste('<div>For questions and comments, please visit', 
                                          '<a target="_blank", href="https://piccolo.byu.edu/Contact.aspx">https://piccolo.byu.edu/Contact.aspx</a>.',
                                          '<p>The source code for Good Nomen can be found at', 
                                          '<a target="_blank", href="https://github.com/srp33/GoodNomen">https://github.com/srp33/GoodNomen</a>.</p></div>'))
                        )
                      )
             )
  )
)