library(shiny)
library(shinyIncubator)
library(googleVis)
suppressPackageStartupMessages(library(googleVis))

shinyServer(function(input, output, session){
  #call find_recs function from the global file
  observe({
    if (input$submit == 0)
      return()
    
    out = isolate({
      withProgress(session, {
        setProgress(message = 'Finding recommendations...')
        find_recs(input$title, input$author)
      })
    })
    
    output$msg = renderUI({HTML('<h6>', out$msg, '<h6>')})
    output$table = renderGvis({
      if (length(out) > 1) {
        gvisTable(out$recs, options = list(allowHtml = T, width = 900, height = 600))
      }
    })
  })
})