library(tm)
library(NLP)
library(openNLP)
library(stringr)
library(slam)
library(topicmodels)
library(ggplot2)
library(rvest)
library(plyr)
library(googleVis)

load('nyt_npr_gr.RData')

#first mine the genre
nyt_npr_gr$genre_gr = tolower(gsub('comment', '', nyt_npr_gr$genre_gr))
c_genre = Corpus(VectorSource(nyt_npr_gr$genre_gr))
c_genre = tm_map(c_genre, removeWords, c(stopwords('SMART'), stopwords('english')))
dtm_genre = DocumentTermMatrix(c_genre)

to_excl = row_sums(dtm_genre) == 0
dtm_genre = dtm_genre[!to_excl, ]
nyt_npr_gr = nyt_npr_gr[!to_excl, ]
save(dtm_genre, file = 'app/data/dtm_genre.RData')
save(nyt_npr_gr, file = 'nyt_npr_gr.RData')

#function to loop over ldas with k different copies
lda_loop = function(d, t_min, t_max, t_break) {
  lda_eval = data.frame(matrix(nrow = 0, ncol = 2))
  for (t in seq(t_min, t_max, t_break)) {
    cat(t, '\n')
    set.seed(2014)
    lda = LDA(d, t, control = list(seed = 2014))
    lda_eval = rbind(lda_eval, c(t, perplexity(lda)))
  }
  
  names(lda_eval) = c('topic', 'perplex')
  return(lda_eval)
}

lda_eval_genre = lda_loop(dtm_genre, 4, 20, 2)
pp_genre =
  ggplot(lda_eval_genre, aes(x = topic, y = perplex)) + geom_line()
ggsave(pp_genre, file = 'perplex_genre.jpg')

#14 appears to be the optimal split
lda_genre = LDA(dtm_genre, 14, control = list(seed = 2014))
save(lda_genre, file = 'app/data/lda_genre.RData')

nyt_npr_gr$lda_genre = topics(lda_genre)
save(nyt_npr_gr, file = 'nyt_npr_gr.RData')

#create html table illustrating the number of books per genre
books_per_genre = ddply(nyt_npr_gr, .(lda_genre), summarize, count = length(lda_genre))
genre_terms = t(terms(lda_genre, 5))
books_per_genre = data.frame(cbind(books_per_genre, genre_terms))
names(books_per_genre) = c('topic', 'count', 'term1', 'term2', 'term3', 'term4', 'term5')

genre_html = gvisTable(books_per_genre, options = list(width = '800', height = '400'))
plot(genre_html)

#then mine the description
#combine descriptions and process them as a corpus
desc = paste(nyt_npr_gr$desc, nyt_npr_gr$desc_gr)
desc = gsub('NA|nbsp', '', desc)
desc = gsub('-', '', desc)
desc = gsub('[^A-Za-z]', ' ', desc)

rm_space = function(x) {
  x = gsub('^ +', '', x)
  x = gsub(' +$', '', x)
  x = gsub(' +', ' ', x)  
}

c_desc = Corpus(VectorSource(desc))
c_desc = tm_map(c_desc, removeWords, c(stopwords('SMART'), stopwords('english')))
c_desc = lapply(c_desc, rm_space)

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

c_desc = lapply(c_desc, pos_tag)
to_excl = sapply(c_desc, nchar) == 0 | c_desc == 'NA'
c_desc[to_excl] = NULL
save(c_desc, file = 'c_desc.RData')

nyt_npr_gr = nyt_npr_gr[!to_excl, ]
save(nyt_npr_gr, file = 'nyt_npr_gr.RData')

#combine descriptions and genres
c_desc_gr = paste(c_desc, nyt_npr_gr$genre_gr)

#create corpus (one for each topic)
c_desc_gr_t = list()
for (i in 1:max(nyt_npr_gr$lda_genre)) {
  c = c_desc_gr[nyt_npr_gr$lda_genre == i]
  c_desc_gr_t[i] = list(Corpus(VectorSource(c)))
}

#split the book table as well
books = list()
for (i in 1:max(nyt_npr_gr$lda_genre)) {
  b = subset(nyt_npr_gr, lda_genre == i, select = c(img, title, author, desc, amazon))
  b$img = sprintf("<img src='%s' width='100' height='133'></img>", b$img)
  b$amazon = sprintf("<a href='%s' target='_blank'>%s</a>", b$amazon, 'amazon')
  b$desc = gsub('<U.+?>|\\(less\\)|[^[:print:]]', ' ', b$desc)
  b$desc = rm_space(b$desc)
  Encoding(b$desc) = 'UTF-8'
  
  books[i] = list(b)
}

save(books, file = 'app/data/books.RData')

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
  
  return(list(dtm, to_exclude))
}

dtm_desc_gr_t_w_excl = lapply(c_desc_gr_t, create_dtm)

dtm_desc_gr_t = lapply(dtm_desc_gr_t_w_excl, function(x) x[[1]])
to_exclude = lapply(dtm_desc_gr_t_w_excl, function(x) x[[2]])

for (i in 1:length(books)) {
  books[[i]] = books[[i]][!to_exclude[i][[1]], ]
}

save(dtm_desc_gr_t, file = 'app/data/dtm_desc_gr_t.RData')
save(books, file = 'app/data/books.RData')

#lda
lda_eval_desc_t = lapply(dtm_desc_gr_t, function(x) lda_loop(x, 20, 200, 20))

for (i in 1:length(lda_eval_desc_t)) {
  pp_desc =
    ggplot(lda_eval_desc_t[[i]], aes(x = topic, y = perplex)) + geom_line()
  ggsave(pp_desc, file = sprintf('perplex_desc_%s.jpg', i))
}

#define optimal split based on cv results
split = c(160, 120, 200, 180, 180, 200, 180, 200, 180, 180, 140, 200, 180, 180)

#perform lda
lda_desc = list()
lda_desc_topics = list()
for (i in 1:length(lda_eval_desc_t)) {
  cat(i, '\n')
  set.seed(2014)
  lda_desc[i] = LDA(dtm_desc_gr_t[[i]], split[i])
  lda_desc_topics[i] = list(posterior(lda_desc[[i]])$topics)
}

save(lda_desc, file = 'app/data/lda_desc.RData')
save(lda_desc_topics, file = 'app/data/lda_desc_topics.RData')
