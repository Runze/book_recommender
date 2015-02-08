library(rvest)
library(plyr)
library(dplyr)
options(stringsAsFactors = F)
source('app/helper_functions.R')

npr = list()
for (year in 2012:2014) {
  for (week in 1:52) {
    cat(year, week, '\n')
    link = sprintf('http://www.npr.org/books/bestsellers/%s/week%s', year, week)
    
    pg_src = tryCatch(html(link), error = function(cond) return(NA))
    if (!is.na(pg_src)) {
      titles = tryCatch(html_text(html_nodes(pg_src, '.title')), 
                        error = function(cond) return(''))
      titles = titles[titles != 'Title']
      authors = tryCatch(html_text(html_nodes(pg_src, '.author')), 
                         error = function(cond) return(''))
      authors = authors[authors != 'Author']
      
      npr$titles = c(npr$titles, titles)
      npr$authors = c(npr$authors, authors) 
    }
  }
}

# convert to dataframe
stopifnot(length(npr$titles) == length(npr$authors))
npr_books = data.frame(list(npr$titles, npr$authors))
names(npr_books) = c('title', 'author')
npr_books = npr_books[!duplicated(paste(npr$titles, npr$authors)), ]

# check if these books are included in the nyt list already
load('intermediary/nyt_books.rda')
nyt_authors_titles = rm_space(gsub('[^a-z]', ' ', tolower(paste(nyt_books$author, nyt_books$title))))
npr_authors_titles = rm_space(gsub('[^a-z]', ' ', tolower(paste(npr_books$author, npr_books$title))))
dups = grep(paste(nyt_authors_titles, collapse = '|'), npr_authors_titles)

npr_books = npr_books[-dups, ]
save(npr_books, file = 'intermediary/npr_books.rda')

# retrieve titles, authors, genres, descriptions, ratings, image, and amazon links from goodreads
extract_gr_npr = function(title, author) {
  # first find the link to the book based on the title and author pair
  link = find_books_gr(title, author)
  
  # then retrieve relevant information
  if (!is.null(link)) {
    return(extract_gr(link))    
  }
}

gr = list()
for (i in 1:nrow(npr_books)) {
  cat(i, 'of', nrow(npr_books), '\n')
  gr[i] = list(extract_gr_npr(npr_books$title[i], npr_books$author[i]))
}

# filter out unfound or unhelpful books
gr_filtered = gr[sapply(gr, function(x) !is.null(x))]
gr_filtered = gr_filtered[sapply(gr_filtered, function(x) x$genres != '' & x$desc != '')]
gr_filtered = lapply(gr_filtered, lapply, function(x) ifelse(length(x) == 0, '', x))
gr_filtered = ldply(gr_filtered, data.frame)

# analyze distribution of ratings
summary(gr_filtered$rating) # 2.76 - 4.76

# keep only books with ratings >= median rating
npr_books = filter(gr_filtered, rating >= median(gr_filtered$rating))
save(npr_books, file = 'intermediary/npr_books.rda')

# combine nyt books
nyt_npr = rbind(nyt_books, npr_books)
nyt_npr = nyt_npr[!duplicated(paste(nyt_npr$title, nyt_npr$author)), ]
nyt_npr = nyt_npr[c('title', 'author', 'genres', 'desc', 'rating', 'img', 'amazon')]
save(nyt_npr, file = 'intermediary/nyt_npr.rda')
