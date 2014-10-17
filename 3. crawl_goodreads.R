library(rvest)
library(plyr)
library(stringr)
options(stringsAsFactors = F)
source('app/helper_functions.R')

base_url = 'https://www.goodreads.com/list/show/264.Books_That_Everyone_Should_Read_At_Least_Once'
base_ps = html(base_url)

#find the total number of pages
pgs = as.integer(html_text(html_node(base_ps, 'a:nth-child(13)')))

#loop over all the pages
gr_df = data.frame(matrix(nrow = 0, ncol = 5))
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
  
  gr_df = data.frame(rbind(gr_df, cbind(title, author, url, rating, vote)))
}

gr_df$rank = 1:nrow(gr_df)
save(gr_df, file = 'gr_df.RData')

gr_df_s = subset(gr_df, rating >= 4) #only keep books with a rating above 4
gr_df_s = subset(gr_df_s, rank <= quantile(rank, .5)) #only keep the top 50 percentile
save(gr_df_s, file = 'gr_df_s.RData')

#check if these books are included in the nyt and npr list already
load('nyt_npr.RData')
title_author_nyt_npr = paste(nyt_npr$title, nyt_npr$author)
title_author_nyt_npr = gsub('[^[:alpha:]]', '', tolower(title_author_nyt_npr))

title_author_gr = paste(gr_df_s$title, gr_df_s$author)
title_author_gr = gsub('[^[:alpha:]]', '', tolower(title_author_gr))

incl = which(title_author_gr %in% title_author_nyt_npr)
gr_df_s = gr_df_s[-incl, ]
save(gr_df_s, file = 'gr_df_s.RData')

#get genres and descriptions for the remaining books
gr = data.frame(matrix(nrow = 0, ncol = 6))
for (i in 1:nrow(gr_df_s)) {
  cat(i, 'of', nrow(gr_df_s), '\n')
  book_url = paste0('https://www.goodreads.com', gr_df_s$url[i])
  book_ps = tryCatch(html(book_url),
                     error = function(cond) return(NA))
  
  #in case of error, try 2 more times
  t = 0
  while (is.na(book_ps) & t < 2) {
    Sys.sleep(10)
    book_ps = tryCatch(html(book_url),
                       error = function(cond) return(NA))
    t = t + 1
  }
  
  if(!is.na(book_ps)) {
    #check language
    lang = html_text(html_nodes(book_ps, '#bookDataBox :nth-child(3) .infoBoxRowItem'))
    if('English' %in% lang) {
      #get genres
      genres = extract_genre(book_ps, 'css')
      
      #get descriptions
      desc = extract_desc(book_ps, 'css')
      
      #get image
      img = html_attr(html_nodes(book_ps, '#coverImage'), 'src')
      
      #get amazon link
      amazon = html_attr(html_nodes(book_ps, '.firstBuyButton .buttonBar'), 'href')
      amazon = paste0('https://www.goodreads.com', amazon)
      
      gr = data.frame(rbind(gr, c(gr_df_s$title[i], gr_df_s$author[i], genres, desc, img, amazon))) 
    }
  }
}
names(gr) = c('title', 'author', 'genre_gr', 'desc', 'img', 'amazon')
gr = subset(gr, genre_gr != '')
save(gr, file = 'gr.RData')

#combine with nyt and npr
nyt_npr_gr = data.frame(rbind.fill(nyt_npr, gr))
save(nyt_npr_gr, file = 'nyt_npr_gr.RData')

