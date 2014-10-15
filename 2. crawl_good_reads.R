library(plyr)
library(rvest)
options(stringsAsFactors = F)

load('bs_ex.RData')
bs_ex = data.frame(sapply(bs_ex, as.character))

rm_space = function(x) {
  x = gsub('^ +', '', x)
  x = gsub(' +$', '', x)
  x = gsub(' +', ' ', x)  
}

title_author = tolower(rm_space(paste(bs_ex$title, bs_ex$author)))
bs_ex_uni = bs_ex[!duplicated(title_author), ]

extract_desc = function(x) {
  link = paste0('https://www.goodreads.com/search?&query=', x)
  ps = html(link)
  y = list()
  
  #description
  descs = tryCatch(html_text(html_nodes(ps, '#description span')), 
                   error = function(cond) return(NA))
  desc = descs[which.max(sapply(descs, nchar))]
  desc = gsub('-', '', desc)
  desc = gsub('[^[:alpha:]]', ' ', desc)
  y$desc = rm_space(desc)
  
  #genre
  genres = tryCatch(html_text(html_nodes(ps, '.elementList ')),
                    error = function(cond) return(NA))
  genre = paste(genres, collapse = '')
  genre = gsub('-', '', genre)
  genre = gsub('[^[:alpha:]]|users|Add |a |comment |comments ', ' ', genre)
  y$genre = rm_space(genre)
  
  return(y)
}

gr = lapply(bs_ex_uni$isbn1, extract_desc)
bs_ex_uni$desc_gr = sapply(gr, function(x) x$desc)
bs_ex_uni$genre_gr = sapply(gr, function(x) x$genre)

#remove entries without a genre
bs_ex_uni = subset(bs_ex_uni, genre_gr != '')
bs_ex_uni$genre_gr = gsub('Non Fiction', 'Nonfiction', bs_ex_uni$genre_gr)
save(bs_ex_uni, file = 'bs_ex_uni.RData')
