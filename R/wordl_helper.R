library(renv)
library(dplyr)
library(readr)
library(curl)
library(lexicon)
library(glue)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f')


# build dataset----
nyt    <- lexicon::grady_augmented 

# 5 letter words only
nyt <- nyt[nchar(nyt) == 5]

# # no duplicate letters
# nyt <- nyt[(strsplit(nyt, "") %>%
#               lapply(., unique) %>%
#               lapply(., length) %>%
#               unlist()) == 5]

# explore----
solution <- "sa"
guess    <- sample(nyt, size = 1) #"fully" #"gulpy"  #"bulky" # "match"  # "resin"
c("fleas", "terga", "soupy", "siker", "sedan", "serac", "saber")

not.ltrs <- c("f", "l", "t", "g", "o", "u", "i", "k", 
              "d", "n", "c")

l1 <- NA
l2 <- "a"
l3 <- NA
l4 <- NA
l5 <- NA

# compare guess to solution (letter & placement)

gue.lp <- unlist(strsplit(x = guess, split = ""))[unlist(strsplit(x = guess, split = "")) == 
                     unlist(strsplit(x = solution, split = ""))]
gue.lp <- ifelse(length(gue.lp) == 0, NA, gue.lp)

# compare guess to solution (letter only)

gue.lo <- unlist(strsplit(x = guess, split = ""))[unlist(strsplit(x = guess, split = "")) %in% 
                     unlist(strsplit(x = solution, split = ""))]
gue.lo <- ifelse(length(gue.lo) == 0, NA, gue.lo)

#gue.lo <- not.ltrs

# get guessed letters that aren't in solution
gue.not <- unlist(strsplit(x = guess, split = ""))[!unlist(strsplit(x = guess, split = "")) %in% gue.lo]



# new list----

nyt <- nyt %>%
  .[!grepl(pattern = paste(not.ltrs, # gue.not,
                           sep = "|", collapse = "|"), 
           x = .)] %>% # remove words with not-permitted letters
  .[grepl(pattern = paste(gue.lo, sep = "|", collapse = "|"),
         x = .)] %>%
  .[grepl(pattern = glue("[{ifelse(is.na(l1), paste(letters, sep = \"\", collapse = \"\"), l1)}][{ifelse(is.na(l2), paste(letters, sep = \"\", collapse = \"\"), l2)}][{ifelse(is.na(l3), paste(letters, sep = \"\", collapse = \"\"), l3)}][{ifelse(is.na(l4), paste(letters, sep = \"\", collapse = \"\"), l4)}][{ifelse(is.na(l5), paste(letters, sep = \"\", collapse = \"\"), l5)}]"), 
          x = .)]

nyt

