library(tm)
library(topicmodels)
library(rvest)
source('helper_functions.R')

load('data/lda_genre.RData')
load('data/books.RData')
load('data/word2vec_matrix.RData')

#calculate correlation matrix between words using word vectors
word2vec_cor = cor(t(word2vec_matrix))

find_recs = function(title, author) {
  title = tolower(rm_space(title))
  author = rm_space(gsub('[^a-z]+', ' ', tolower(author)))
  link = gsub(' ', '%20', paste0('https://www.goodreads.com/search?&query=', paste(title, author)))
  ps = html(link)
  
  #create skeleton tables to store recommendations
  recs_wv = data.frame(matrix(nrow = 0, ncol = ncol(books[[1]])))
  recs_cos = data.frame(matrix(nrow = 0, ncol = ncol(books[[1]])))
  names(recs_wv) = names(books[[1]])
  names(recs_cos) = names(books[[1]])
  
  #check if anything is found
  #it appears that shiny doesn't work well with css selectors. hence xpath is used
  found = html_text(html_nodes(ps, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "searchSubNavContainer", " " ))]'))
  
  if (length(found) > 0 & !grepl('0 of 0 results', found)) {
    #analyze returned results
    titles = html_text(html_nodes(ps, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "bookTitle", " " ))]//span'))
    authors = html_text(html_nodes(ps, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "authorName", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//span')) #only selecting 1st authors
    
    #find the first result that match the title and author pair
    for (i in 1:length(titles)) {
      if (length(grep(title, rm_space(titles[i]), ignore.case = T)) > 0 &
           length(grep(author, rm_space(gsub('[^a-z]+', ' ', tolower(authors[i]))), ignore.case = T)) > 0) {
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
      
      #parse descriptions into words
      desc = gsub('[^a-z]', ' ', tolower(desc))
      desc_wd = unique(unlist(strsplit(desc, ' +')))
      
      #find the nearest neighbors from the matching topics based on descriptions
      for (i in 1:2) {
        if (neighbors[i] > 0) {
          #remove the book from the training set if it is already included
          title_author_train = tolower(paste(books[[topic_new[i]]]$title, books[[topic_new[i]]]$author))
          title_author_new = tolower(paste(title, author))
          title_author_train = gsub('[^a-z0-9]', '', title_author_train)
          title_author_new = gsub('[^a-z0-9]', '', title_author_new)
          
          books_train = books[[topic_new[i]]][!grepl(title_author_new, title_author_train), ]          
          descs_train = books_train$desc
          descs_train = lapply(descs_train, function(x) gsub('[^a-z]', ' ', tolower(x)))
          
          #OPTION 1: using word vectors
          #parse descriptions into words
          descs_train_wd = lapply(descs_train, function(x) unique(unlist(strsplit(x, ' +'))))
          
          #process the word vector correlation matrix
          #subsest to the words used by the new book only
          word2vec_cor_new = word2vec_cor[row.names(word2vec_cor) %in% desc_wd, ]
          
          #compute similarities
          sim = lapply(descs_train_wd, function(x) book_similarity(x, desc_wd, word2vec_cor_new))
          
          #find the most similar books up to the amount defined by neighbors[i]
          recs_wv = rbind(recs_wv, books_train[order(unlist(sim), decreasing = T)[1:neighbors[i]], ])
        
          #OPTION 2: using cosine similarities
          #create corpura
          desc_c = Corpus(VectorSource(desc))
          descs_train_c = Corpus(VectorSource(unlist(descs_train)))
          
          #create dtm using combined corpus and
          #only the words used in the word2vec model
          dict = colnames(word2vec_cor_new)
          dtm_descs = DocumentTermMatrix(c(desc_c, descs_train_c),
                                         control = list(dictionary = dict))
          
          #calculate cosine similarities between the new book and the training set
          sim = cosine_similarity(dtm_descs)
          
          #find the most similar books
          recs_cos = rbind(recs_cos, books_train[order(sim, decreasing = T)[1:neighbors[i]], ])
        }
      }
    }
  }
  out = list(recs_wv, recs_cos)
  names(out) = c('recs_wv', 'recs_cos')
  return(out)
}