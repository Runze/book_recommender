library(tm)
library(topicmodels)
library(rvest)
source('helper_functions.R')

load('data/genre_lda.rda')
load('data/books.rda')
load('data/word2vec_matrix.rda')

# calculate correlation matrix between words using word vectors
word2vec_cor = cor(t(word2vec_matrix))

# function to find recommendations for the book a user entered
find_recs = function(title, author) {
  # create skeleton tables to store recommendations
  recs_wv = data.frame(matrix(nrow = 0, ncol = ncol(books[[1]])))
  recs_cos = data.frame(matrix(nrow = 0, ncol = ncol(books[[1]])))
  names(recs_wv) = names(books[[1]])
  names(recs_cos) = names(books[[1]])
  
  # find the goodreads link to the book using the title and author
  title = tolower(rm_space(title))
  author = rm_space(gsub('[^a-z]+', ' ', tolower(author)))
  link = find_books_gr(title, author, 'xpath')
  # it appears that shiny doesn't work well with css selectors. hence xpath is used
  
  if (!is.null(link)) {
    # extract genre and description
    pg_src = html(link)
    
    # get genres
    genres_new = extract_genre(pg_src, 'xpath')
    
    # get descriptions
    desc_new = extract_desc(pg_src, 'xpath')
    
    if (genres_new != '' & desc_new != '') {
      # first find the matching genres using the trained lda model
      genre_corpus_new = Corpus(VectorSource(genres_new))
      genre_dtm_new = DocumentTermMatrix(genre_corpus_new)
      
      # apply the lda model
      genre_topics_prob = posterior(genre_lda, genre_dtm_new)$topics
      
      # find the top 2 genres
      genre_topics = order(genre_topics_prob, decreasing = T)[1:2]
      
      # find the number of books to extract from each of the 2 genres
      wgt = genre_topics_prob[genre_topics[1]] / (genre_topics_prob[genre_topics[1]] + genre_topics_prob[genre_topics[2]])
      neighbors = c(ceiling(10 * wgt), floor(10 * (1 - wgt)))
      
      # parse descriptions into words
      desc_new = gsub('[^a-z]', ' ', tolower(desc_new))
      desc_new_wd = unique(unlist(strsplit(desc_new, ' +')))
      
      # find the nearest neighbors from the matching topics based on descriptions
      for (i in 1:2) {
        if (neighbors[i] > 0) {
          # remove the book from the training set if it is already included
          title_author_train = tolower(paste(books[[genre_topics[i]]]$title, books[[genre_topics[i]]]$author))
          title_author_new = tolower(paste(title, author))
          title_author_train = gsub('[^a-z0-9]', '', title_author_train)
          title_author_new = gsub('[^a-z0-9]', '', title_author_new)
          
          books_train = books[[genre_topics[i]]][!agrepl(title_author_new, title_author_train), ]
          descs_train = books_train$desc
          descs_train = lapply(descs_train, function(x) gsub('[^a-z]', ' ', tolower(x)))
          
          # OPTION 1: using word vectors
          # parse descriptions into words
          descs_train_wd = lapply(descs_train, function(x) unique(unlist(strsplit(x, ' +'))))
          
          # process the word vector correlation matrix
          # subsest to the words used by the new book only
          word2vec_cor_new = word2vec_cor[row.names(word2vec_cor) %in% desc_new_wd, ]
          
          # compute similarities
          sim = lapply(descs_train_wd, function(x) book_similarity(x, desc_new_wd, word2vec_cor_new))
          
          # find the most similar books up to the amount defined by neighbors[i]
          recs_wv = rbind(recs_wv, books_train[order(unlist(sim), decreasing = T)[1:neighbors[i]], ])
          
          # OPTION 2: using cosine similarities
          # create corpus using the book's description
          desc_corpus_new = Corpus(VectorSource(desc_new))
          descs_corpus_train = Corpus(VectorSource(unlist(descs_train)))
          
          # create dtm using combined corpus and only the words used in the word2vec model
          dict = colnames(word2vec_cor_new)
          combined_descs_dtm = DocumentTermMatrix(c(desc_corpus_new, descs_corpus_train),
                                                  control = list(dictionary = dict))
          
          # calculate cosine similarities between the new book and the training set
          sim = cosine_similarity(combined_descs_dtm)
          
          # find the most similar books
          recs_cos = rbind(recs_cos, books_train[order(sim, decreasing = T)[1:neighbors[i]], ])
        }
      }
    }
  }

  out = list(recs_wv, recs_cos)
  names(out) = c('recs_wv', 'recs_cos')
  return(out)
}