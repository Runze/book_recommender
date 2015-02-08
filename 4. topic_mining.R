library(tm)
library(slam)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(googleVis)

library(stringr)
options(stringsAsFactors = F)
source('app/helper_functions.R')

load('nyt_npr_gr.RData')

# first mine the genre
genre_corpus = Corpus(VectorSource(tolower(nyt_npr_gr$genres)))
genre_dtm = DocumentTermMatrix(genre_corpus)

# exclude rows with 0 terms (after the cleaning above)
to_excl = row_sums(genre_dtm) == 0
genre_dtm = genre_dtm[!to_excl, ]
nyt_npr_gr = nyt_npr_gr[!to_excl, ]

save(genre_dtm, file = 'intermediary/genre_dtm.rda')
save(nyt_npr_gr, file = 'intermediary/nyt_npr_gr.rda')

# run lda on genres
genre_lda_eval = lda_loop(genre_dtm, 10, 30, 2)
genre_lda_preplex =
  ggplot(genre_lda_eval, aes(x = topic, y = perplex)) + geom_line()
ggsave(genre_lda_preplex, file = 'intermediary/genre_lda_preplex.png')

# 22 appears to be a good split
genre_lda = LDA(genre_dtm, 22, control = list(seed = 2014))
save(genre_lda, file = 'app/data/genre_lda.rda')

# create genre assignment for each book
nyt_npr_gr$genre_topic = topics(genre_lda)
save(nyt_npr_gr, file = 'intermediary/nyt_npr_gr.rda')

# create html table illustrating the number of books per genre
books_per_genre = nyt_npr_gr %>%
  group_by(genre_topic) %>%
  summarise(count = n())
genre_terms = t(terms(genre_lda, 5))
books_per_genre = cbind(books_per_genre, genre_terms)
names(books_per_genre) = c('topic', 'count', 'term1', 'term2', 'term3', 'term4', 'term5')

genre_html = gvisTable(books_per_genre, options = list(width = 800, height = 500))
plot(genre_html)

# split and format the book table per topic
books = list()
for (i in 1:max(nyt_npr_gr$genre_topic)) {
  book = nyt_npr_gr %>%
    filter(genre_topic == i) %>%
    select(img, title, author, desc, rating, amazon) %>%
    mutate(img = sprintf("<img src='%s' width='100' height='133'></img>", img),
           amazon = sprintf("<a href='%s' target='_blank'>%s</a>", amazon, 'amazon'))
  
  books[i] = list(book)
}

save(books, file = 'app/data/books.rda')

# off to python to build word vectors!
write.csv(nyt_npr_gr, file = 'intermediary/nyt_npr_gr.csv', row.names = F)

# ...after executing the python code, resave to rdata (for faster loading)
word2vec_matrix = read.csv('intermediary/word2vec_matrix.csv', header = T, row.names = 1)
save(word2vec_matrix, file = 'app/data/word2vec_matrix.rda')
