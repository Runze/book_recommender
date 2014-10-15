library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(theme='bootstrap.min.css',
                  tags$style(type='text/css',
                             'label {font-size: 12px;}',
                             '.recalculating {opacity: 1.0;}'
                  ),
                  
                  titlePanel('Recommender for New York Times and NPR best-seller, and Goodreads\'  most recommended books'),
                  
                  sidebarPanel(
                    textInput('title', 'Please enter the title of a book you like', value = ''),
                    textInput('author', 'Please enter the author of the book', value = ''),
                    actionButton('submit', 'Submit'),
                    h6(a('www.runzemc.com', href='http://www.runzemc.com')),
                    h6(a('github.com/Runze', href='https://github.com/Runze/book_recommender'))
                  ),
                  
                  mainPanel(
                    progressInit(),
                    htmlOutput('msg'),
                    htmlOutput('table')
                  )
))