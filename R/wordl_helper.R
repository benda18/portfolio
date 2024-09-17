library(renv)
library(dplyr)
library(readr)
library(curl)
library(lexicon)
library(glue)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f')
gc()

# build dataset----
nyt    <- lexicon::grady_augmented 

# 5 letter words only
nyt <- nyt[nchar(nyt) == 5]
# no duplicate letters
nyt <- nyt[(strsplit(nyt, "") %>%
              lapply(., unique) %>%
              lapply(., length) %>%
              unlist()) == 5]

# play----
solution <- "leach"
guess    <- "beach"

sample(nyt, size = 1)


nyt <- nyt[(nyt %>% 
               strsplit(., "") %>%
               lapply(., nth, n = 1) %>%
               unlist()) == ""]
nyt <- nyt[(nyt %>% 
              strsplit(., "") %>%
              lapply(., nth, n = 2) %>%
              unlist()) == "e"]
nyt <- nyt[(nyt %>% 
              strsplit(., "") %>%
              lapply(., nth, n = 3) %>%
              unlist()) == "a"]
nyt <- nyt[(nyt %>% 
              strsplit(., "") %>%
              lapply(., nth, n = 4) %>%
              unlist()) == "c"]
nyt <- nyt[(nyt %>% 
              strsplit(., "") %>%
              lapply(., nth, n = 5) %>%
              unlist()) == "h"]


guess.output <- NULL
for(i in 1:5){
  guess.output[i] <- (strsplit(guess, "") %>%
     lapply(X = ., 
            FUN = nth, 
            n = i) %>%
     unlist()) == (strsplit(solution, "") %>%
                     lapply(X = ., 
                            FUN = nth, 
                            n = i) %>%
                     unlist())
}
rightLtr_rightPlc <- unlist(strsplit(guess, ""))[guess.output]
names(rightLtr_rightPlc) <- as.character(which(guess.output))

rightLtr_rightPlc %>% as.data.frame() %>% t() %>% as_tibble()

# 
# # older----
# l.nyt  <- strsplit(nyt, "")
# df.nyt <- data.frame(word = nyt,
#                      l1 = unlist(lapply(X = l.nyt,
#                                         FUN = nth,
#                                         n   = 1)),
#                      l2 = unlist(lapply(X = l.nyt,
#                                         FUN = nth,
#                                         n   = 2)),
#                      l3 = unlist(lapply(X = l.nyt,
#                                         FUN = nth,
#                                         n   = 3)),
#                      l4 = unlist(lapply(X = l.nyt,
#                                         FUN = nth,
#                                         n   = 4)),
#                      l5 = unlist(lapply(X = l.nyt,
#                                         FUN = nth,
#                                         n   = 5))) %>% as_tibble()
# rm(l.nyt, nyt)
# 
# # solution word ----
# solution <- "argue"
# sample(x = df.nyt$word, size = 1)
# 
# # guesses ----
# sample(x = df.nyt$word, size = 1)
# 
# g1 <- "paeon"
# g2 <- "ictus" # sample(df.word$word,1)
# 
# # exact 
# unlist(strsplit(solution, "")) == 
#   unlist(strsplit(g1, ""))
# 
# p1.imp <- letters[!letters %in% unlist(strsplit(solution, ""))[unlist(strsplit(solution, "")) %in% 
#   unlist(strsplit(g1, ""))]]
# 
# 
# unlist(strsplit(solution, "")) == 
#   unlist(strsplit(g2, ""))
# 
# 
# #correct_letters <- c(NA)
# 
# l1 <- list(correct = NA, 
#            impossible = c(p1.imp))
# l2 <- list(correct = NA, 
#            impossible = c(p1.imp))
# l3 <- list(correct = NA, 
#            impossible = c(p1.imp))
# l4 <- list(correct = "u", 
#            impossible = c(p1.imp))
# l5 <- list(correct = NA, 
#            impossible = c(p1.imp))
# 
# 
# 
# df.word <- df.nyt
# 
# df.word <- df.word[df.word$l1 %in% unlist(ifelse(!is.na(l1$correct), 
#                                                  yes = l1$correct, 
#                                                  no = list(letters[!letters %in% l1$impossible]))) &
#                      df.word$l2 %in% unlist(ifelse(!is.na(l2$correct), 
#                                                    yes = l2$correct, 
#                                                    no = list(letters[!letters %in% l2$impossible]))) & 
#                      df.word$l3 %in% unlist(ifelse(!is.na(l3$correct), 
#                                                    yes = l3$correct, 
#                                                    no = list(letters[!letters %in% l3$impossible]))) & 
#                      df.word$l4 %in% unlist(ifelse(!is.na(l4$correct), 
#                                                    yes = l4$correct, 
#                                                    no = list(letters[!letters %in% l4$impossible]))) & 
#                      df.word$l5 %in% unlist(ifelse(!is.na(l5$correct), 
#                                                    yes = l5$correct, 
#                                                    no = list(letters[!letters %in% l5$impossible]))),]
# 
# df.word
# 
