library(rvest)
library(plyr)
library(stringr)
library(slam)
options(stringsAsFactors = F)

# remove extra white spaces
rm_space = function(x) {
  x = gsub('^ +', '', x)
  x = gsub(' +$', '', x)
  x = gsub(' +', ' ', x)  
}

# extract genres from goodreads
extract_genre = function(pg_src, method = 'css') {
  if (method == 'css') {
    genres = tryCatch(html_text(html_nodes(pg_src, '.elementList')),
                      error = function(cond) return(''))  
  }
  else {
    genres = tryCatch(html_text(html_nodes(pg_src, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "elementList", " " ))]')),
                      error = function(cond) return(''))
  }
  
  genres = gsub('\n| +', '', genres)
  genres = genres[!(grepl('comment', genres))]
  
  # get genre votes
  genre_votes = as.integer(gsub(',', '', str_extract(genres, '[0-9,]+')))
  genre_votes = floor(genre_votes/min(genre_votes, na.rm = T))
  genre_votes[is.na(genre_votes)] = 0
  
  # repeat genre tags per votes
  genres = gsub('[0-9,]+user.*', '', genres)
  genres = rep(genres, genre_votes)
  genres = paste(genres, collapse = ' ')
  genres = gsub('>', ' ', genres)
  genres = gsub('-', '', genres)
  genres = rm_space(genres)
  
  genres = ifelse(nchar(genres) == 0, '', genres)
  return(genres)
}

# extract descriptions from goodreads
extract_desc = function(pg_src, method = 'css') {
  if (method == 'css') {
    descs = tryCatch(html_text(html_nodes(pg_src, '#description span')),
                     error = function(cond) return(''))
  }
  else {
    descs = tryCatch(html_text(html_nodes(pg_src, xpath = '//*[(@id = "description")]//span')),
                     error = function(cond) return(''))  
  }
  
  # get the longer description
  desc = descs[which.max(sapply(descs, nchar))]
  desc = rm_space(desc)
  return(desc)
}

# wrapper function to extract all relevant information from goodreads
extract_gr = function(link, get_title = T, get_author = T, get_genre = T, 
                      get_desc = T, get_img = T, get_amazon = T, get_rating = T) {
  pg_src = tryCatch(html(link),  error = function(cond) return(NA))
  
  # in case of an error, try 2 more times
  t = 0
  while (is.na(pg_src) & t < 2) {
    cat('error; try again in 10 seconds.\n')
    Sys.sleep(10)
    pg_src = tryCatch(html(link), error = function(cond) return(NA))
    t = t + 1
  }
  
  if(!is.na(pg_src)) {
    gr = list()
    
    # get title
    if (get_title == T) {
      title = tryCatch(html_text(html_nodes(pg_src, '#bookTitle')), error = function(cond) return(''))
      gr$title = rm_space(gsub('\n', '', title))
    }
    
    # get author
    if (get_title == T) {
      author = tryCatch(html_text(html_nodes(pg_src, '.authorName')), error = function(cond) return(''))
      gr$author = paste(author, collapse = ', ')
    }
    
    # get genres
    if (get_genre == T) {
      gr$genres = extract_genre(pg_src)  
    }
    
    # get descriptions
    if (get_desc == T) {
      gr$desc = extract_desc(pg_src)  
    }
    
    # get image
    if (get_img == T) {
      gr$img = tryCatch(html_attr(html_nodes(pg_src, '#coverImage'), 'src'), error = function(cond) return(''))
    }
    
    # get amazon link
    if (get_amazon == T) {
      amazon = tryCatch(html_attr(html_nodes(pg_src, '.firstBuyButton .buttonBar'), 'href'), error = function(cond) return(''))
      gr$amazon = paste0('https://www.goodreads.com', amazon)
    }
    
    # get rating
    if (get_rating == T) {
      gr$rating = as.numeric(tryCatch(html_text(html_nodes(pg_src, '.average')), error = function(cond) return('')))
    }
    
    return(gr)
  }
}

# function to look up books on goodreads based on the title and author pair
find_books_gr = function(title, author, method = 'css') {
  title = rm_space(gsub('[^A-Za-z0-9]', ' ', title))
  author = rm_space(gsub('[^A-Za-z0-9]', ' ', author))
  link = gsub(' ', '%20', paste0('https://www.goodreads.com/search?&query=', paste(title, author)))
  pg_src = html(link)
  
  # check if the book is found
  if (method == 'css') {
    found = html_text(html_nodes(pg_src, '.searchSubNavContainer'))  
  }
  else {
    found = html_text(html_nodes(pg_src, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "searchSubNavContainer", " " ))]'))
  }
  
  if (length(found) > 0 && !grepl('0 of 0 results', found)) {
    # analyze returned results
    if (method == 'css') {
      titles = html_text(html_nodes(pg_src, '.bookTitle span'))
      authors = html_text(html_nodes(pg_src, '.authorName:nth-child(1) span')) #only selecting 1st authors
    }
    else {
      titles = html_text(html_nodes(pg_src, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "bookTitle", " " ))]//span'))
      authors = html_text(html_nodes(pg_src, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "authorName", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//span'))
    }
    
    # find the first result that match the title and author pair
    for (i in 1:length(titles)) {
      if (grepl(title, titles[i], ignore.case = T) &
            grepl(author, authors[i], ignore.case = T)) {
        break
      }
    }
    
    # find the link to the chosen book
    if (method == 'css') {
      link_found = html_attr(html_nodes(pg_src, '.bookTitle'), 'href')[i]
    }
    else {
      link_found = html_attr(html_nodes(pg_src, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "bookTitle", " " ))]'), 'href')[i]
    }
    link_found = paste0('https://www.goodreads.com', link_found)

    return(link_found)
  }
}

# function to loop over ldas with k different copies
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

# function to calculate similarities between books based on word vectors' correlation matrix
book_similarity = function(desc_train, desc_new, cor_matrix) {
  cor_matrix = cor_matrix[, colnames(cor_matrix) %in% desc_train]
  
  # for each word in the new book, find the most similar word in the training book
  cor_maxes = tryCatch(apply(cor_matrix, 1, max),
                       error = function(cond) return(NA))
  
  # using this method, the final score is the sum of the correlation coefficients
  # between the most similar words found in the 2 books
  return(sum(cor_maxes))
}

# function to calculate cosine similarities between the first row and the rest of a dtm matrix
cosine_similarity = function(m) {
  m1 = as.vector(m[1, ])
  m2 = as.matrix(m[-1, ])
  sim = apply(m2, 1, function(x) x %*% m1 / sqrt(sum(x^2) * sum(m1^2)))
  return(sim)
}
