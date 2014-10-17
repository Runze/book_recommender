library(rvest)
library(plyr)
options(stringsAsFactors = F)
source('app/helper_functions.R')

npr = list()
for (year in 2012:2014) {
  for (week in 1:52) {
    cat(year, week, '\n')
    link = sprintf('http://www.npr.org/books/bestsellers/%s/week%s', year, week)
    
    ps = tryCatch(html(link), error = function(cond) return(NA))
    if(!is.na(ps)) {
      titles = tryCatch(html_text(html_nodes(ps, '.title')), 
                        error = function(cond) return(NA))
      titles = titles[titles != 'Title']
      authors = tryCatch(html_text(html_nodes(ps, '.author')), 
                         error = function(cond) return(NA))
      authors = authors[authors != 'Author']
      
      npr$titles = c(npr$titles, titles)
      npr$authors = c(npr$authors, authors) 
    }
  }
}

#convert to dataframe
stopifnot(length(npr$titles) == length(npr$authors))
npr_df = data.frame(cbind(npr$titles, npr$authors))
names(npr_df) = c('title', 'author')

#remove duplicates
title_author = paste(npr_df$title, npr_df$author)
npr_df$title_author = gsub('[^[:alpha:]]', '', tolower(rm_space(title_author)))
npr_uni = npr_df[!duplicated(npr_df$title_author), ]

#check if these books are included in the nyt list already
load('bs_ex_uni.RData')
title_author_nyt = paste(bs_ex_uni$title, bs_ex_uni$author)
title_author_nyt = gsub('[^[:alpha:]]', '', tolower(rm_space(title_author_nyt)))

incl = which(npr_uni$title_author %in% title_author_nyt)
npr_uni = npr_uni[-incl, ]
npr_uni = data.frame(sapply(npr_uni,  as.character))
save(npr_uni, file = 'npr_uni.RData')

#search for them on goodreads
npr_gr = data.frame(matrix(nrow = 0, ncol = 6))
for(n in 186:nrow(npr_uni)) {
  cat(n, '\n')
  
  link = gsub(' ', '%20', paste0('https://www.goodreads.com/search?&query=', paste(npr_uni$title[n], npr_uni$author[n])))
  ps = html(link)
  
  #check if book is found
  found = html_text(html_nodes(ps, '.searchSubNavContainer'))
  if(length(found) > 0 & length(grep('0 of 0 results', found)) == 0) {
    #analyze returned results
    titles = html_text(html_nodes(ps, '.bookTitle span'))
    authors = html_text(html_nodes(ps, '.authorName:nth-child(1) span')) #only selecting 1st authors
    
    #find the first result that match the title and author pair
    for (i in 1:length(titles)) {
      if(length(grep(npr_uni$title[n], titles[i], ignore.case = T)) > 0 &
           length(grep(npr_uni$author[n], authors[i], ignore.case = T)) > 0) {
        break
      }
    }
    
    #find the link to the chosen book
    link_found = html_attr(html_nodes(ps, '.bookTitle'), 'href')[i]
    link_found = paste0('https://www.goodreads.com', link_found)
    
    #extract genre and description
    ps_found = tryCatch(html(link_found),
                       error = function(cond) return(NA))
    
    #in case of error, try 2 more times
    t = 0
    while (is.na(ps_found) & t < 2) {
      Sys.sleep(10)
      ps_found = tryCatch(html(link_found),
                          error = function(cond) return(NA))
      t = t + 1
    }
    
    if(!is.na(ps_found)) {
      #get genres
      genres = extract_genre(ps_found, 'css')
      
      #get descriptions
      desc = extract_desc(ps_found, 'css')
      
      #get image
      img = html_attr(html_nodes(ps_found, '#coverImage'), 'src')
      
      #get amazon link
      amazon = html_attr(html_nodes(ps_found, '.firstBuyButton .buttonBar'), 'href')
      amazon = paste0('https://www.goodreads.com', amazon)
      
      npr_gr = data.frame(rbind(npr_gr, c(npr_uni$title[n], npr_uni$author[n], genres, desc, img, amazon)))  
    }
  }
}
names(npr_gr) = c('title', 'author', 'genre_gr', 'desc', 'img', 'amazon')
npr_gr = subset(npr_gr, genre_gr != '')
save(npr_gr, file = 'npr_gr.RData')

#combine nyt with npr
load('bs_ex_uni.RData')
nyt_npr = data.frame(rbind.fill(bs_ex_uni, npr_gr))
save(nyt_npr, file = 'nyt_npr.RData')

