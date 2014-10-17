library(tm)
library(NLP)
library(openNLP)
library(topicmodels)
library(rvest)
source('helper_functions.R')

load('data/lda_genre.RData')
load('data/books.RData')
load('data/descs.RData')

find_recs = function(title, author) {
  title = tolower(rm_space(title))
  author = tolower(rm_space(author))
  link = gsub(' ', '%20', paste0('https://www.goodreads.com/search?&query=', paste(title, author)))
  ps = html(link)
  
  #check if anything is found
  #it appears that shiny doesn't work well with css selectors. hence xpath is used
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
    
    #get genres
    genres = extract_genre(ps_found, 'xpath')
    
    #get descriptions
    desc = extract_desc(ps_found, 'xpath')
    
    if (!is.na(genres) & !is.na(desc)) {
      #first find the matching genres
      c_genre_new = Corpus(VectorSource(genres))
      dtm_genre_new = DocumentTermMatrix(c_genre_new)
       
      #apply the lda model
      lda_genre_new = posterior(lda_genre, dtm_genre_new)$topics
      
      #find the top 2 genre
      topic_new = order(lda_genre_new, decreasing = T)[1:2]
      
      #decide on the number of neighbors to find from the 2 topics based on the posterior probability
      wgt = lda_genre_new[topic_new[1]] / (lda_genre_new[topic_new[1]] + lda_genre_new[topic_new[2]])
      neighbors = c(ceiling(10 * wgt), floor(10 * (1 - wgt)))
      
      #then find the nearest neighbors from the matching topics based on descriptions
      #first clean the description
      desc = gsub('-|nbsp', '', desc)
      desc = gsub('[^A-Za-z]', ' ', desc)
      
      c_desc = Corpus(VectorSource(desc))
      c_desc = tm_map(c_desc, removeWords, c(stopwords('SMART'), stopwords('english')))
      c_desc = lapply(c_desc, rm_space)
      c_desc = lapply(c_desc, pos_tag)
      
      #gather recommendations
      recs = data.frame(matrix(nrow = 0, ncol = ncol(books[[1]])))
      for (i in 1:2) {
        if(neighbors[i] > 0) {
          #remove the book from the training set if it is already included
          title_author_train = tolower(paste(books[[topic_new[i]]]$title, books[[topic_new[i]]]$author))
          title_author_new = tolower(paste(title, author))
          title_author_train = gsub('[^a-z0-9]', '', title_author_train)
          title_author_new = gsub('[^a-z0-9]', '', title_author_new)
          
          descs_train = descs[[topic_new[i]]][!grepl(title_author_new, title_author_train)]
          books_train = books[[topic_new[i]]][!grepl(title_author_new, title_author_train), ]
          
          #append the description to all the other descriptions within this genre
          desc_all = c(c_desc, descs_train)
          
          #create dtm
          dtm_all = create_dtm(Corpus(VectorSource(desc_all)))
          
          #calculate cosine similarities between the new book and the training set
          sim = cosine_similarity(dtm_all)
          
          #find the most similar books up to the amount defined by neighbors[i]
          recs = rbind(recs, books_train[order(sim, decreasing = T)[1:neighbors[i]], ])
        }
      }
      out = list(recs, 'Results are based on descriptions on Goodreads.')
      names(out) = c('recs', 'msg')
    }
    else {
      out = list('Sorry, the book, or some version of it, is found but there is insufficient description on goodreads to find recommendations with.')
      names(out) = 'msg'
    }
  }
  return(out)
}