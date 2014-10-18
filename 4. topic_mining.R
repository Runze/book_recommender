library(tm)
library(NLP)
library(openNLP)
library(stringr)
library(slam)
library(topicmodels)
library(ggplot2)
library(plyr)
library(googleVis)
source('app/helper_functions.R')

load('nyt_npr_gr.RData')

#first mine the genre
nyt_npr_gr$genre_gr = tolower(nyt_npr_gr$genre_gr)
c_genre = Corpus(VectorSource(nyt_npr_gr$genre_gr))
c_genre = tm_map(c_genre, removeWords, c(stopwords('SMART'), stopwords('english')))
dtm_genre = DocumentTermMatrix(c_genre)

#exclude rows with 0 terms (after the cleaning above)
to_excl = row_sums(dtm_genre) == 0
dtm_genre = dtm_genre[!to_excl, ]
nyt_npr_gr = nyt_npr_gr[!to_excl, ]
save(dtm_genre, file = 'app/data/dtm_genre.RData')
save(nyt_npr_gr, file = 'nyt_npr_gr.RData')

lda_eval_genre = lda_loop(dtm_genre, 10, 30, 2)
pp_genre =
  ggplot(lda_eval_genre, aes(x = topic, y = perplex)) + geom_line()
ggsave(pp_genre, file = 'perplex_genre.jpg')

#20 appears to be a good split (any more split may result in too small a sample size within certain groups)
lda_genre = LDA(dtm_genre, 20, control = list(seed = 2014))
save(lda_genre, file = 'app/data/lda_genre.RData')

nyt_npr_gr$lda_genre = topics(lda_genre)
save(nyt_npr_gr, file = 'nyt_npr_gr.RData')

#create html table illustrating the number of books per genre
books_per_genre = ddply(nyt_npr_gr, .(lda_genre), summarize, count = length(lda_genre))
genre_terms = t(terms(lda_genre, 5))
books_per_genre = data.frame(cbind(books_per_genre, genre_terms))
names(books_per_genre) = c('topic', 'count', 'term1', 'term2', 'term3', 'term4', 'term5')

genre_html = gvisTable(books_per_genre, options = list(width = 800, height = 500))
plot(genre_html)

#then mine the description
#combine descriptions and process them as a corpus
nyt_npr_gr$desc = paste(nyt_npr_gr$desc, nyt_npr_gr$desc_gr)
nyt_npr_gr$desc = gsub('NA|nbsp', '', nyt_npr_gr$desc)
nyt_npr_gr$desc = gsub('-', '', nyt_npr_gr$desc)
desc = gsub('[^A-Za-z]', ' ', nyt_npr_gr$desc)

c_desc = Corpus(VectorSource(desc))
c_desc = tm_map(c_desc, removeWords, c(stopwords('SMART'), stopwords('english')))
c_desc = lapply(c_desc, rm_space)

#pos tag
c_desc = lapply(c_desc, pos_tag)
to_excl = sapply(c_desc, nchar) == 0 | c_desc == 'NA'
c_desc[to_excl] = NULL
save(c_desc, file = 'c_desc.RData')

nyt_npr_gr = nyt_npr_gr[!to_excl, ]
save(nyt_npr_gr, file = 'nyt_npr_gr.RData')

#split descriptions per topic
descs = list()
for (i in 1:max(nyt_npr_gr$lda_genre)) {
  descs[i] = list(c_desc[nyt_npr_gr$lda_genre == i])
}

#split the book table as well
books = list()
for (i in 1:max(nyt_npr_gr$lda_genre)) {
  b = subset(nyt_npr_gr, lda_genre == i, select = c(img, title, author, desc, amazon))
  b$img = sprintf("<img src='%s' width='100' height='133'></img>", b$img)
  b$amazon = sprintf("<a href='%s' target='_blank'>%s</a>", b$amazon, 'amazon')
  b$desc = gsub('<U.+?>|\\(less\\)|[^[:print:]]|Â|â', ' ', b$desc)
  b$desc = rm_space(b$desc)
  Encoding(b$desc) = 'UTF-8'
  
  books[i] = list(b)
}

save(descs, file = 'app/data/descs.RData')
save(books, file = 'app/data/books.RData')

#find the most frequent terms used in each genre
dtms = lapply(descs, function(x) create_dtm(Corpus(VectorSource(x))))
tfs = lapply(dtms, col_sums)
tfs = lapply(tfs, function(x) x[order(x, decreasing = T)][1:10])
terms = lapply(tfs, names)

#create html table illustrating the number of books per genre
terms_df = data.frame(matrix(unlist(terms), nrow = 20, byrow = T))
terms_df = data.frame(cbind(1:20, terms_df))
names(terms_df) = c('topic', 'term1', 'term2', 'term3', 'term4', 'term5',
                    'term6', 'term7', 'term8', 'term9', 'term10')

terms_html = gvisTable(terms_df, options = list(width = 800, height = 500))
plot(terms_html)
