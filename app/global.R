library(tm)
library(topicmodels)
library(rvest)

load('data/books.RData')
load('data/lda_genre.RData')
load('data/lda_desc.RData')
load('data/lda_desc_topics.RData')

rm_space = function(x) {
  x = gsub('^ +', '', x)
  x = gsub(' +$', '', x)
  x = gsub(' +', ' ', x)  
}

find_recs = function(title, author) {
  #first look the book up on goodreads
  #using book title and author name
  title = tolower(rm_space(title))
  author = tolower(rm_space(author))
  link = gsub(' ', '%20', paste0('https://www.goodreads.com/search?&query=', paste(title, author)))
  ps = html(link)
  
  #check if anything is found
  #it appears that shiny doesn't work well with css selectors. hence xpath is used
  #xpath derived from SelectorGadget: http://selectorgadget.com
  found = html_text(html_nodes(ps, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "searchSubNavContainer", " " ))]'))
  
  if(length(found) == 0 | length(grep('0 of 0 results', found)) > 0) {
    out = list('Sorry, the book is not found on goodreads.')
    names(out) = 'msg'
  }
  else {
    #analyze returned results
    titles = html_text(html_nodes(ps, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "bookTitle", " " ))]//span'))
    authors = html_text(html_nodes(ps, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "authorName", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//span')) #only selecting 1st authors
    
    #find the first result that match the title and author pair
    for (i in 1:length(titles)) {
      if(length(grep(title, rm_space(titles[i]), ignore.case = T)) > 0 &
           length(grep(author, rm_space(authors[i]), ignore.case = T)) > 0) {
        break
      }
    }
    
    #find the link to the chosen book
    link_found = html_attrs(html_nodes(ps, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "bookTitle", " " ))]'))[i]
    link_found = paste0('https://www.goodreads.com', link_found[[1]][1])
    
    #extract genre and description
    ps_found = html(link_found)
    genres = tryCatch(html_text(html_nodes(ps_found, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "elementList", " " ))]')),
                      error = function(cond) return(NA))
    descs = tryCatch(html_text(html_nodes(ps_found, xpath = '//*[(@id = "description")]')), 
                     error = function(cond) return(NA))
    if (!is.na(genres[1]) & !is.na(descs[1])) {
      genre = paste(genres, collapse = '')
      genre = gsub('-', '', genre)
      genre = gsub('[^[:alpha:]]', ' ', genre)
      genre = gsub('users|Add |a |comment |comments ', ' ', genre)
      genre = gsub('Non Fiction', 'Nonfiction', genre)
      
      #goodreads may present 2 versions of descriptions, one of which is truncated
      #hence look for the longer version
      desc = descs[which.max(sapply(descs, nchar))]
      desc = gsub('-', '', desc)
      desc = gsub('[^[:alpha:]]', ' ', desc)
      
      #first find the matching genre
      #using the genre lda model
      c_genre_new = Corpus(VectorSource(genre))
      dtm_genre_new = DocumentTermMatrix(c_genre_new)
      lda_genre_new = posterior(lda_genre, dtm_genre_new)$topics
      
      #find the top 2 genre
      topic_new = order(lda_genre_new, decreasing = T)[1:2]
      
      #decide on the number of neighbors to find from the 2 topics based on the posterior probability
      wgt = lda_genre_new[topic_new[1]] / (lda_genre_new[topic_new[1]] + lda_genre_new[topic_new[2]])
      neighbors = c(ceiling(10 * wgt), floor(10 * (1 - wgt)))
      
      #then find the nearest neighbors from the matching topics
      #using the description lda models
      c_desc_new = Corpus(VectorSource(desc))
      dtm_desc_new = DocumentTermMatrix(c_desc_new)
      
      recs = data.frame(matrix(nrow = 0, ncol = ncol(books[[1]])))
      for (i in 1:2) {
        if(neighbors[i] > 0) {
          #predict posterior probabilities
          lda_desc_new = posterior(lda_desc[[topic_new[i]]], dtm_desc_new)
          lda_desc_topics_new = lda_desc_new$topics
          
          #remove the book from the training set if it is already included
          b = books[[topic_new[i]]]
          t = lda_desc_topics[[topic_new[i]]]
          
          title_author = tolower(rm_space(paste(b$title, b$author)))
          title_author_new = tolower(rm_space(paste(title, author)))
          if(length(grep(title_author_new, title_author)) > 0) {
            b = b[-grep(title_author_new, title_author), ]
            t = t[-grep(title_author_new, title_author), ]
          }
          
          #calculate distances between the book and the training set using the posterior probabilities
          dists = apply(lda_desc_topics[[topic_new[i]]], 1, function(x) dist(rbind(x, lda_desc_topics_new)))
          
          #pick the closest neighbors up to the determined neighbor size
          recs = rbind(recs, b[order(dists), ][1:neighbors[i], ])
        }
      }
      out = list(recs, 'Results are based on descriptions on goodreads.')
      names(out) = c('recs', 'msg')
    }
    else {
      out = list('Sorry, the book, or some version of it, is found but there is insufficient description on goodreads to find recommendations with.')
      names(out) = 'msg'
    }
  }
  return(out)
}
