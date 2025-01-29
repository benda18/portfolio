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


mycol <- discogger::discogs_user_collection(user_name = "bendertj", 
                                    access_token = "foo")

mycol.df <- NULL
for(i in 1:length(mycol$content)){
  mycol.df <- rbind(mycol.df, 
                    data.frame(album_title = mycol$content[[i]]$basic_information$title, 
                               master_id   = mycol$content[[i]]$basic_information$master_id))
}

mycol.df

