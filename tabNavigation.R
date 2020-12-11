# Change Tab --------------------------------------------------------------
observeEvent(input$button, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'editTable')
})
observeEvent(input$terminologyButton, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'editTable')
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