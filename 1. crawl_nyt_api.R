library(httr)
library(plyr)
library(dplyr)
source('app/helper_functions.R')

key = 'f8597b398ce96a249ef0cf81a074fe87:5:65450565'
link = sprintf('http://api.nytimes.com/svc/books/v2/lists/names.json?api-key=%s', key)
lists = content(GET(link))
lists_name = sapply(lists$results, function(x) x$list_name_encoded)
lists_name = lists_name[c(3:5, 7:9)]

dates = seq(as.Date('2008-06-08'), as.Date('2015-02-15'), by = 'week')
nyt_books = list()
k = 0

# retrieve books
for (i in 1:length(lists_name)) {
  for (j in 1:length(dates)) {
    cat(lists_name[i], dates[j], '\n')
    link = sprintf('http://api.nytimes.com/svc/books/v2/lists/%s/%s.json?&api-key=%s', lists_name[i], dates[j], key)
    lists = content(GET(link))
    if(length(lists$results) > 0) {
      nyt_books[k] = lists$results
      k = k + 1
    }
    Sys.sleep(.5)
  }
}

save (nyt_books, file = 'intermediary/nyt_books.rda')

# extract relevant information
extract_info = function(x) {
  y = list()
  
  y$isbn1 = x$book_details[[1]]$primary_isbn13
  y$img = x$book_details[[1]]$book_image
  y$amazon = x$book_details[[1]]$amazon_product_url
  
  return(y)
}

nyt_books = lapply(nyt_books, extract_info)
nyt_books = ldply(nyt_books, data.frame)
nyt_books = nyt_books[!duplicated(nyt_books$isbn1), ]
save (nyt_books, file = 'intermediary/nyt_books.rda')

# retrieve titles, authors, genres, descriptions, and ratings from goodreads
extract_gr_nyt = function(isbn) {
  link = paste0('https://www.goodreads.com/search?&query=', isbn)
  return(extract_gr(link, get_img = F, get_amazon = F))
}

gr = list()
for (i in 1:length(nyt_books$isbn1)) {
  cat(i, 'of', length(nyt_books$isbn1), '\n')
  gr[i] = list(extract_gr_nyt(nyt_books$isbn1[i]))
  gr[[i]]$isbn1 = nyt_books$isbn1[i]
}

# filter out unfound or unhelpful books
gr_filtered = gr[sapply(gr, function(x) x$genres != '' & x$desc != '')]
gr_filtered = ldply(gr_filtered, data.frame)

# analyze distribution of ratings
summary(gr_filtered$rating) # 3.04 - 4.76

# keep only books with ratings >= median rating
gr_filtered = filter(gr_filtered, rating >= median(gr_filtered$rating))

# merge with the rest of the information
nyt_books = inner_join(nyt_books, gr_filtered, by = 'isbn1')
nyt_books = select(nyt_books, -isbn1)

save(nyt_books, file = 'intermediary/nyt_books.rda')
