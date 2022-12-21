# Change Tab --------------------------------------------------------------
observeEvent(input$buttonLoadThenNext, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'standardizeColumns')
})
observeEvent(input$terminologyButton, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'standardizeColumns')
})
observeEvent(input$standardizeBack, {
  updateTabsetPanel(session, 'tabs', selected = 'loadData')
})
observeEvent(input$standardizeNext, ignoreInit = T, {
  updateTabsetPanel(session, 'tabs', selected = 'finalReport')
})
observeEvent(input$saveBack, {
  updateTabsetPanel(session, 'tabs', selected = 'standardizeColumns')
})