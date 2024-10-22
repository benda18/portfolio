library(dplyr)
library(renv)
library(lexicon)

snapshot()
status()

rm(list=ls());cat('\f')

# vars----
outer.ltrs <- "glarmh"
inner.ltr  <- "o"
nyt        <- lexicon::grady_augmented

# cleanup----
 
outer.ltrs  <- outer.ltrs %>% strsplit(., "") %>% unlist()
not.letters <- letters[!letters %in% outer.ltrs]
not.letters <- not.letters[!not.letters %in% inner.ltr]

# analysis---
# only words with given letters

panogram.nyt <- nyt[grepl(pattern = paste(outer.ltrs, sep = "|", collapse = "|"), x = nyt) |
      grepl(pattern = paste(inner.ltr, sep = "|", collapse = "|"), x = nyt) ] %>%
  .[!grepl(paste(not.letters, sep = "|", collapse = "|"), x = .)] %>%
  .[grepl(inner.ltr, x = .)] %>%
  .[nchar(.) >= 4]

panogram.nyt[grepl(inner.ltr, panogram.nyt) & 
  grepl(outer.ltrs[1], panogram.nyt)& 
  grepl(outer.ltrs[2], panogram.nyt)& 
  grepl(outer.ltrs[3], panogram.nyt)& 
  grepl(outer.ltrs[4], panogram.nyt)&
  grepl(outer.ltrs[5], panogram.nyt)&
  grepl(outer.ltrs[6], panogram.nyt)]

nyt <- nyt[grepl(pattern = paste(outer.ltrs, sep = "|", collapse = "|"), x = nyt) |
      grepl(pattern = paste(inner.ltr, sep = "|", collapse = "|"), x = nyt) ] %>%
  .[!grepl(paste(not.letters, sep = "|", collapse = "|"), x = .)] %>%
  .[grepl(inner.ltr, x = .)] %>%
  .[nchar(.) >= 4] 


nyt[nchar(nyt) == max(nchar(nyt))-0]
nyt[nchar(nyt) == max(nchar(nyt))-1]
nyt[nchar(nyt) == max(nchar(nyt))-2]

# all.letters <- nyt[grepl(outer.ltrs[1], nyt) & 
#       grepl(outer.ltrs[2], nyt) & 
#       grepl(outer.ltrs[3], nyt) & 
#       grepl(outer.ltrs[4], nyt) & 
#       grepl(outer.ltrs[5], nyt) & 
#       grepl(outer.ltrs[6], nyt) & 
#       grepl(innter.ltr, nyt) ]
# 
# only.letters <- NA