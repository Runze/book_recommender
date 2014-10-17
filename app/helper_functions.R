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
  if(method == 'css') {
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
  
  return(genres)
}

#extract descriptions from goodreads
extract_desc = function(pg_src, method) {
  if(method == 'css') {
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

#tag all words to only keep adjectives, nouns, and verbs
pos_tag = function(x) {
  gc()
  cleaned = ''
  
  if (nchar(x) > 0) {
    sent_token_annotator = Maxent_Sent_Token_Annotator()
    word_token_annotator = Maxent_Word_Token_Annotator()  
    a = NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
    
    pos_tag_annotator = Maxent_POS_Tag_Annotator()
    a = NLP::annotate(x, pos_tag_annotator, a)
    w = subset(a, type == 'word')
    
    #keep adjectives, nouns, and verbs
    to_keep = grep('JJ|NN|VB', unlist(w$features))
    
    #but not pronouns
    pn = grep('NNP', unlist(w$features))
    to_keep = setdiff(to_keep, pn)
    
    start = w$start[to_keep]
    end = w$end[to_keep]
    
    for (i in 1:length(to_keep)) {
      cleaned = rm_space(paste(cleaned, str_sub(x, start[i], end[i])))
    }
  }
  return(cleaned)
}

#create dtm with tf-idf trimming
create_dtm = function(x) {
  dtm = DocumentTermMatrix(x, control = list(minWordLength = 3))
  
  #trim based on tf-idf
  tf_all = as.matrix(dtm / row_sums(dtm))
  tf_all[tf_all == 0] = NA
  tf = apply(tf_all, 2, mean, na.rm = T)
  
  idf = log2(nrow(dtm) / col_sums(dtm > 0))
  tf_idf = tf * idf
  
  #keep the terms with tf-idf above 25th percentile
  dtm = dtm[, tf_idf >= quantile(tf_idf, .25)]
  to_exclude = row_sums(dtm) == 0
  dtm = dtm[!to_exclude, ]
  
  return(dtm)
}

#function to calculate cosine similarities between the first row and the rest of a matrix
cosine_similarity = function(m) {
   m1 = as.matrix(m[1, ])
   m2 = as.matrix(m[-1, ])
   sim = apply(m2, 1, function(x) x %*% t(m1) / sqrt(sum(x^2) * sum(m1^2)))
   return(sim)
}
