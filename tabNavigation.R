# Change Tab --------------------------------------------------------------
observeEvent(input$buttonLoadThenNext, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'standardizeColumns')
})
observeEvent(input$terminologyButton, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'standardizeColumns')
})
observeEvent(input$editBack, {
  updateTabsetPanel(session, 'tabs', selected = 'loadData')
})
observeEvent(input$editNext, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'updateColumnNames')
})
observeEvent(input$columnBack, {
  updateTabsetPanel(session, 'tabs', selected = 'editTable')
})
observeEvent(input$columnSubmit, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'finalReport')
})
observeEvent(input$saveBack, {
  updateTabsetPanel(session, 'tabs', selected = 'updateColumnNames')
})