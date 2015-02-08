library(rvest)
library(plyr)
library(dplyr)
library(stringr)
options(stringsAsFactors = F)
source('app/helper_functions.R')

base_url = 'https://www.goodreads.com/list/show/264.Books_That_Everyone_Should_Read_At_Least_Once'
base_ps = html(base_url)

#find the total number of pages
pgs = as.integer(html_text(html_node(base_ps, 'a:nth-child(13)')))

#loop over all the pages
gr_books = data.frame(matrix(nrow = 0, ncol = 5))
for (p in 1:pgs) {
  cat(p, 'of', as.integer(pgs), '\n')
  url = sprintf('%s?page=%d', base_url, p)
  ps = html(url)
  
  title = html_text(html_nodes(ps, '.bookTitle span'))
  author = html_text(html_nodes(ps, '.authorName span'))
  url = html_attr(html_nodes(ps, '.bookTitle'), 'href')
  rating =
    as.numeric(str_extract(html_text(html_nodes(ps, '.minirating')),
                           '[0-9\\.]+'))
  vote =
    as.integer(gsub(',', '', str_extract(html_text(html_nodes(ps, '#all_votes div .uitext a:nth-child(3)')),
                                         '[0-9,]+')))
  
  gr_books = data.frame(rbind(gr_books, cbind(title, author, url, rating, vote)))
}

save(gr_books, file = 'intermediary/gr_books.rda')

# analyze distribution of ratings
summary(gr_books$rating) # 0 - 5

# keep only books with ratings >= median rating
gr_books = filter(gr_books, rating >= median(gr_books$rating))

# check if these books are included in the nyt and npr list already
load('intermediary/nyt_npr.rda')
nyt_npr_authors_titles = rm_space(gsub('[^a-z]', ' ', tolower(paste(nyt_npr$author, nyt_npr$title))))
gr_authors_titles = rm_space(gsub('[^a-z]', ' ', tolower(paste(gr_books$author, gr_books$title))))
dups = grep(paste(nyt_npr_authors_titles, collapse = '|'), gr_authors_titles)

gr_books = gr_books[-dups, ]
save(gr_books, file = 'intermediary/gr_books.rda')

# retrieve genres, descriptions, ratings, image, and amazon links from goodreads
extract_gr_gr = function(title, author) {
  # first find the link to the book based on the title and author pair
  link = find_books_gr(title, author)
  
  # then retrieve relevant information
  if (!is.null(link)) {
    return(extract_gr(link))    
  }
}

gr = list()
for (i in 1:nrow(gr_books)) {
  cat(i, 'of', nrow(gr_books), '\n')
  link = paste0('https://www.goodreads.com', gr_books$url[i])
  gr[i] = list(extract_gr(link, get_title = F, get_author = F, get_rating = F))
  gr[[i]]$url = gr_books$url[i]
}

# filter out unfound or unhelpful books
gr_filtered = gr[sapply(gr, function(x) x$genres != '' & x$desc != '')]
gr_filtered = lapply(gr_filtered, lapply, function(x) ifelse(length(x) == 0, '', x))
gr_filtered = ldply(gr_filtered, data.frame)

# merge with the rest of the information
gr_books = inner_join(gr_books, gr_filtered, by = 'url')
gr_books = select(gr_books, -url, -vote)

save(gr_books, file = 'intermediary/gr_books.rda')

# combine with nyt and npr books
nyt_npr_gr = rbind(nyt_npr, gr_books)
nyt_npr_gr = nyt_npr_gr[!duplicated(paste(nyt_npr_gr$title, nyt_npr_gr$author)), ]
save(nyt_npr_gr, file = 'intermediary/nyt_npr_gr.rda')
