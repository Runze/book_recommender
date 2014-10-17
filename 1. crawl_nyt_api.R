library(httr)
library(plyr)
source('app/helper_functions.R')

key = '<key>'
link = sprintf('http://api.nytimes.com/svc/books/v2/lists/names.json?api-key=%s', key)
lists = content(GET(link))
lists_n = sapply(lists$results, function(x) x$list_name_encoded)
lists_n_sub = lists_n[c(3:5, 7:9)]

dates = seq(as.Date('2008-06-08'), as.Date('2014-10-05'), by = 'week')
bs = list()
k = 0

for (i in 1:length(lists_n_sub)) {
  for (j in 1:length(dates)) {
    cat(lists_n_sub[i], dates[j], '\n')
    link = sprintf('http://api.nytimes.com/svc/books/v2/lists/%s/%s.json?&api-key=%s', lists_n_sub[i], dates[j], key)
    lists = content(GET(link))
    if(length(lists$results) > 0) {
      bs[k] = lists$results
      k = k + 1
    }
    Sys.sleep(.5)
  }
}

save (bs, file = 'bs.RData')

extract_info = function(x) {
  y = list()
  
  y$list_name = x$list_name
  y$title = x$book_details[[1]]$title
  y$desc = x$book_details[[1]]$description
  y$author = x$book_details[[1]]$author
  y$isbn1 = x$book_details[[1]]$primary_isbn13
  y$isbn2 = x$book_details[[1]]$primary_isbn10
  y$img = x$book_details[[1]]$book_image
  y$amazon = x$book_details[[1]]$amazon_product_url
  
  return(y)
}

bs_ex = lapply(bs, extract_info)
bs_ex = ldply(bs_ex, data.frame)
bs_ex = data.frame(sapply(bs_ex, as.character))
save (bs_ex, file = 'bs_ex.RData')

#dedupe
title_author = tolower(rm_space(paste(bs_ex$title, bs_ex$author)))
bs_ex_uni = bs_ex[!duplicated(title_author), ]

#retrieve genres and descriptions from goodreads
extract_gr = function(x) {
  link = paste0('https://www.goodreads.com/search?&query=', x)
  ps = html(link)
  y = list()
  
  #get genres
  y$genre = extract_genre(ps, 'css')
  
  #get descriptions
  y$desc = extract_desc(ps, 'css')
  
  return(y)
}

gr = lapply(bs_ex_uni$isbn1, extract_gr)
bs_ex_uni$desc_gr = sapply(gr, function(x) x$desc)
bs_ex_uni$genre_gr = sapply(gr, function(x) x$genre)

#remove entries without a genre
bs_ex_uni = subset(bs_ex_uni, genre_gr != '')
save(bs_ex_uni, file = 'bs_ex_uni.RData')
