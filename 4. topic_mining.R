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

#combine nyt with npr
load('bs_ex_uni.RData')
load('npr_gr.RData')
nyt_npr = data.frame(rbind.fill(bs_ex_uni, npr_gr))

#first mine the genre
c_genre = Corpus(VectorSource(nyt_npr$genre_gr))
c_genre = tm_map(c_genre, removeWords, c(stopwords('SMART'), stopwords('english')))
dtm_genre = DocumentTermMatrix(c_genre)

#function to perform k-fold lda cv
lda_cv = function(d, k, t_min, t_max, t_break) {
  #create k folds
  ind = 1:nrow(d)
  set.seed(2014)
  ind = ind[order(rnorm(length(ind)))]
  nf = ceiling(length(ind) / k)
  f = {}
  for (i in 1:k) {
    f[[i]] = ind[seq((i-1)*nf+1, min(i*nf, length(ind)))]
  }
  
  #remove folds with NA in them (i.e., too small to slice any more)
  f[which(sapply(f, function(x) max(is.na(x))) == 1)] = NULL
  
  #k-fold lda cv
  lda_eval = data.frame(fold = integer(), topic = integer(), perplex = numeric())
  for (i in 1:length(f)) {
    d_train = d[-f[[i]], ]
    d_test = d[f[[i]], ]
    
    for (t in seq(t_min, t_max, t_break)) {
      cat(i, t, '\n')
      set.seed(2014)
      lda_train = LDA(d_train, t)
      lda_test = LDA(d_test, model = lda_train)
      
      lda_eval = rbind(lda_eval, c(i, t, perplexity(lda_test)))
    }
  }
  
  names(lda_eval) = c('fold', 'topic', 'perplex')
  return(lda_eval)
}

lda_eval_genre = lda_cv(dtm_genre, 10, 4, 20, 2)
pp_genre =
  ggplot(lda_eval_genre, aes(x = topic, y = perplex, colour = as.factor(fold), group = as.factor(fold))) + geom_line()
ggsave(pp_genre, file = 'perplex_genre.jpg')

#8 appears to be the optimal split
set.seed(2014)
lda_genre = LDA(dtm_genre, 8)
save(lda_genre, file = 'app/data/lda_genre.RData')

nyt_npr$lda_genre = topics(lda_genre)
save(nyt_npr, file = 'nyt_npr.RData')

#create html table illustrating the number of books per genre
books_per_genre = ddply(nyt_npr, .(lda_genre), summarize, count = length(lda_genre))
genre_terms = t(terms(lda_genre, 5))
books_per_genre = data.frame(cbind(books_per_genre, genre_terms))
names(books_per_genre) = c('topic', 'count', 'term1', 'term2', 'term3', 'term4', 'term5')

genre_html = gvisTable(books_per_genre, options = list(width = '800', height = '400'))
plot(genre_html)

#then mine the description
#combine descriptions and process them as a corpus
desc = paste(nyt_npr$desc, nyt_npr$desc_gr)
desc = gsub('NA|nbsp', '', desc)
desc = gsub('-', '', desc)
desc = gsub('[^[:alpha:]]', ' ', desc)
desc = gsub('Â|â', '', desc)

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

nyt_npr = nyt_npr[!to_excl, ]
save(nyt_npr, file = 'nyt_npr.RData')

#create corpus (one for each topic)
c_desc_t = list()
for (i in 1:max(nyt_npr$lda_genre)) {
  c = c_desc[nyt_npr$lda_genre == i]
  c_desc_t[i] = list(Corpus(VectorSource(c)))
}

#split the book table as well
books = list()
for (i in 1:max(nyt_npr$lda_genre)) {
  b = subset(nyt_npr, lda_genre == i, select = c(img, title, author, desc, amazon))
  b$img = sprintf("<img src='%s' width='100' height='133'></img>", b$img)
  b$amazon = sprintf("<a href='%s' target='_blank'>%s</a>", b$amazon, 'amazon')
  b$desc = gsub('<U.+?>|\\(less\\)|[^[:print:]]|Â|â', ' ', b$desc)
  b$desc = rm_space(b$desc)
  
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

dtm_desc_t_w_excl = lapply(c_desc_t, create_dtm)

dtm_desc_t = lapply(dtm_desc_t_w_excl, function(x) x[[1]])
to_exclude = lapply(dtm_desc_t_w_excl, function(x) x[[2]])

for (i in 1:length(to_exclude)) {
  books[[i]] = books[[i]][!to_exclude[i][[1]], ]
}

save(dtm_desc_t, file = 'dtm_desc.RData')
save(books, file = 'app/data/books.RData')

#lda
lda_eval_desc_t = lapply(dtm_desc_t, function(x) lda_cv(x, 5, 10, 50, 5))

for (i in 1:length(lda_eval_desc_t)) {
  pp_desc =
    ggplot(lda_eval_desc_t[[i]], aes(x = topic, y = perplex, colour = as.factor(fold), group = as.factor(fold))) + geom_line()
  ggsave(pp_desc, file = sprintf('perplex_desc_%s.jpg', i))
}

#define optimal split based on cv results
split = c(50, 45, 20, 20, 40, 45, 50, 40)

#perform lda
lda_desc = list()
lda_desc_topics = list()
for (i in 1:length(lda_eval_desc_t)) {
  set.seed(2014)
  lda_desc[i] = LDA(dtm_desc_t[[i]], split[i])
  lda_desc_topics[i] = list(posterior(lda_desc[[i]])$topics)
}

save(lda_desc, file = 'app/data/lda_desc.RData')
save(lda_desc_topics, file = 'app/data/lda_desc_topics.RData')
