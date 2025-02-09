library(renv)
library(dplyr)
library(jsonlite)
# library(devtools)
# devtools::install_github('ewenme/discogger')
library(discogger)

renv::snapshot()
renv::status()

# https://github.com/ewenme/discogger

# DISCOGS_API_TOKEN <- "foo"

#my.token <- ""

mycol <- discogger::discogs_user_collection(user_name    = "bendertj", 
                                            access_token = my.token, 
                                            #simplify_df  = F, 
                                            simplify_df  = T)

# mycol.df <- NULL
# for(i in 1:length(mycol$content)){
#   mycol.df <- rbind(mycol.df, 
#                     data.frame(album_title = mycol$content[[i]]$basic_information$title, 
#                                master_id   = mycol$content[[i]]$basic_information$master_id, 
#                                album_id    = mycol$content[[i]]$id))
# }

mycol.df <- mycol$content$basic_information.title
mycol$content$basic_information.master_id
mycol$content$id
mycol$content$basic_information.artists[[70]]$name

mycol.df$value    <- NA
mycol.df$currency <- NA

for(i in 1:nrow(mycol.df)){
  #Sys.sleep(3)
  try(temp.val <- discogger::discogs_mkt_price_suggest(release_id = mycol.df$album_id[i], 
                                                   access_token = my.token)) 
  
  try(mycol.df$value[i]    <- temp.val[["content"]][["Very Good Plus (VG+)"]][["value"]])
  try(mycol.df$currency[i] <- temp.val[["content"]][["Very Good Plus (VG+)"]][["currency"]])
  rm(temp.val)
}

as_tibble(mycol.df)

View(temp.val)


sample(c(0:9, letters, LETTERS, ",", "!"), 
       size = 16, 
       # prob = c(rep(10/(10+26+26+2),10),
       #          rep(26/(10+26+26+2), 26), 
       #          rep(26/(10+26+26+2),26), 
       #          rep(2/(10+26+26+2),2)), 
       prob = c(rep(1/10, 10), 
                rep(1/26, 26), 
                rep(1/26, 26), 
                rep(1/2, 2)), 
       replace = T) %>% paste(., sep = "", collapse = "")
