library(shiny)

shinyUI(fluidPage(theme='bootstrap.min.css',
                  tags$style(type='text/css',
                             'label {font-size: 12px;}',
                             '.recalculating {opacity: 1.0;}'
                  ),
                  
                  tags$h2("Recommender for New York Times and NPR best-sellers, 
                          and Goodreads 'books that everyone should read at least once'"),
                  
                  wellPanel(
                    fluidRow(
                      column(5, textInput('title', 'Please enter the title of a book you like', value = '')),
                      column(5, textInput('author', 'Please enter the author of the book', value = '')),
                      column(2, actionButton('submit', 'Submit'))
                    ),
                    
                    fluidRow(
                      column(12, radioButtons('method', 'Based on genres and similarity metrics calculated using',
                                             c('Word vectors' = 'word_vec',
                                               'Term frequencies' = 'cosine'),
                                             inline = T))
                    ),
                    
                    fluidRow(column(12, dataTableOutput('table'))),
                    fluidRow(h6(a('www.runzemc.com', href='http://www.runzemc.com')),
                             h6(a('github.com/Runze', href='https://github.com/Runze/book_recommender')))
                  )
))