library(shiny)

shinyServer(function(input, output){
  #call find_recs function from the global file
  recs = reactive({
    validate(
      need(nchar(input$title) > 0, 'Enter a book to see recommendations.')
    )
    
    withProgress(message = 'Finding recommendations...', {
      out = find_recs(input$title, input$author)
    })
    
    validate(
      need(nrow(out$recs_wv) > 0, 'Sorry, not enough information is gathered to make recommendations with.')
    )
    
    out
  })
  
  output$table = renderDataTable({
    input$submit
    out = isolate(recs())
    
    if (input$method == 'word_vec') {
      out$recs_wv
    }
    else {
      out$recs_cos
    }
  }, options = list(searching = F, pageLength = 5, lengthMenu = c(5, 10), autoWidth = F))
})