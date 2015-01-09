library(rvest)
library(plyr)
library(stringr)
library(slam)
options(stringsAsFactors = F)

#remove extra white spaces
rm_space = function(x) {
  x = gsub('^ +', '', x)
  x = gsub(' +$', '', x)
  x = gsub(' +', ' ', x)  
}

#extract genres from goodreads
extract_genre = function(pg_src, method) {
  if (method == 'css') {
    genres = tryCatch(html_text(html_nodes(pg_src, '.elementList')),
                      error = function(cond) return(NA))  
  }
  else {
    genres = tryCatch(html_text(html_nodes(pg_src, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "elementList", " " ))]')),
                      error = function(cond) return(NA))
  }
  
  genres = gsub('\n| +', '', genres)
  genres = genres[!(grepl('comment', genres))]
  
  #get genre votes
  genre_votes = as.integer(gsub(',', '', str_extract(genres, '[0-9,]+')))
  genre_votes = floor(genre_votes/min(genre_votes, na.rm = T))
  genre_votes[is.na(genre_votes)] = 0
  
  #repeat genre tags per votes
  genres = gsub('[0-9,]+user.*', '', genres)
  genres = rep(genres, genre_votes)
  genres = paste(genres, collapse = ' ')
  genres = gsub('>', ' ', genres)
  genres = gsub('-', '', genres)
  genres = rm_space(genres)
  
  genres = ifelse(nchar(genres) == 0, NA, genres)
  return(genres)
}

#extract descriptions from goodreads
extract_desc = function(pg_src, method) {
  if (method == 'css') {
    descs = tryCatch(html_text(html_nodes(pg_src, '#description span')),
                     error = function(cond) return(NA))
  }
  else {
    descs = tryCatch(html_text(html_nodes(pg_src, xpath = '//*[(@id = "description")]//span')),
                     error = function(cond) return(NA))  
  }
  
  #get the longer description
  desc = descs[which.max(sapply(descs, nchar))]
  
  desc = gsub('-', '', desc)
  desc = rm_space(desc)
  return(desc)
}

#function to loop over ldas with k different copies
lda_loop = function(d, t_min, t_max, t_break) {
  lda_eval = data.frame(matrix(nrow = 0, ncol = 2))
  for (t in seq(t_min, t_max, t_break)) {
    cat(t, '\n')
    lda = LDA(d, t, control = list(seed = 2014))
    lda_eval = rbind(lda_eval, c(t, perplexity(lda)))
  }
  
  names(lda_eval) = c('topic', 'perplex')
  return(lda_eval)
}

#function to calculate similarities between books based on word vectors' correlation matrix
book_similarity = function(desc_train, desc_new, cor_matrix) {
  cor_matrix = cor_matrix[, colnames(cor_matrix) %in% desc_train]
  
  #for each word in the new book, find the most similar word in the training book
  cor_maxes = tryCatch(apply(cor_matrix, 1, max),
                       error = function(cond) return(NA))
  
  #using this method, the final score is the sum of the correlation coefficients
  #between the most similar words found in the 2 books
  return(sum(cor_maxes))
}

#function to calculate cosine similarities between the first row and the rest of a dtm matrix
cosine_similarity = function(m) {
  m1 = as.vector(m[1, ])
  m2 = as.matrix(m[-1, ])
  sim = apply(m2, 1, function(x) x %*% m1 / sqrt(sum(x^2) * sum(m1^2)))
  return(sim)
}
