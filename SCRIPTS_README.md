# R script organization and explanation

*Good Nomen* is a shiny app built from a collection of R scripts. *app.R* contains all of the code for the project through "source" calls. These import code from the other scripts and make it accessible in the local environment. One of the distinguishing features of *Good Nomen* is its ability to generate an R script that can replicate the steps taken in the app. You will find several "eval" statements through out the code. These allow us to both collect and execute the code. You will also find several statements that build to "masterText." This is the variable collects the commands. You will also find several statements that build to "masterChanges." This variable keeps track of all of the changes made to the data, which can be downloaded for reference.

*librariesToLoad.R* contains code for loading all of the libraries needed for *Good Nomen.*

*globalVariables.R* contains variables that will remain constant throughout use of the app. This includes elements such as the API key, the number of terms to suggest, etc.

*UI.R* stands for "user interface" and contains all the code necessary for structuring *Good Nomen's* interface. The user interface is composed of several tabs that have unique purposes. This script controls only the physical appearance of the tabs; the logic for each of the tabs is contained in separate scripts.

*sharedVariables.R* contains variables that are needed across several of the tabs. Many of these variables are "reactive" variables, meaning that they are modified immediately when the user submits input.

*sharedFunctions.R* contains functions that are used by two or more of the tabs. This includes things such as error throwing, displaying data, loading ontologies, etc.

*matchingFunctions.R* contains the functions necessary to perform Jaro-Winkler matching. These functions are used for automatching, manual matching, and suggesting new column names.

The following four scripts each contain code for one of the tabs on the app.

*loadData.R* controls the first page of the app. On this page, there are several widgets for uploading a file, downloading an ontology, specifying the number of header rows, etc. All of these widgets are managed in this script.

*editData.R* controls the second page of the app. On this page, the user will make changes to their data. For this reason, a preview of the data is displayed so that the user can be aware of what is happening behind the scenes. Similar to *app.R,* loadData.R contains "source" commands. These reference the code for automatching and manual matching. 

*automatch.R* contains code for automatching. The actual matching functions are in *matchingFunctions.R,* but this script covers getting a column of data from the user's file, sending it to the matching functions, retrieving the results, and sorting them in a way that the user can view and interact with. A table of results is shown to the user. This table contains only unique matches. The user can choose to accept or reject each of these matches. After they have done so, code from *automatch.R* is executed that updates the user's data accordingly.

*manualMatch.R* contains code for manual matching. In order to manual match data, the user is asked to select unstandardized terms from their data that have a common meaning. They are then able to either mark these values as NA or choose a standardized value. *Good Nomen* provides up to 5 suggestions for a standardized term to use. There will be one standardized suggestion per unstandardized term selected. For example, if the user says that "x," "y," and "z" have the same meaning, *Good Nomen* will suggest 3 possible standardized terms. However, the user has the option to select any of the standardized terms from the ontology or input their own term. The results from manual matching are incorporated into the user's data in the same way as in *automatch.R.*

*updateColumnNames.R* controls the third page of the app. On this page the user can change column names in their data, if desired. Jaro-Winkler matching is used to suggest a standardized column name for the selected column.

*saveData.R* controls the last page of the app. On this page the user can download their edited data, the R script that has been built throughout the process, and a TSV file containing a record of all of the changes made to the data. 

*tabNavigation.R* is a short script containing "observeEvent" statements that listen for when the user presses buttons that take them back and forth between pages.



