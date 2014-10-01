library(httr)
library(plyr)

#first get a list of available bestseller lists (e.g., hardcover fiction)
key = '<api_key>'
link = sprintf('http://api.nytimes.com/svc/books/v2/lists/names.json?api-key=%s', key)
lists = content(GET(link))
lists_n = sapply(lists$results, function(x) x$list_name_encoded)
lists_n_sub = lists_n[c(3:5, 7:9)] #only pick fictions and non-fictions

#then get all the books appeared on the chosen bestseller lists
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

#extract relevant information
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

save (bs_ex, file = 'bs_ex.RData')
