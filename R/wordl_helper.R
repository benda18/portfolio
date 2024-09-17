library(renv)
library(dplyr)
library(readr)
library(curl)
library(lexicon)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f')
gc()

# explore----
#nyt    <- readr::read_lines("https://raw.githubusercontent.com/gwstaten/wordl/main/wordlists/nytimes2")

nyt    <- lexicon::grady_augmented 

# 5 letter words only
nyt <- nyt[nchar(nyt) == 5]
# no duplicate letters
nyt <- nyt[(strsplit(nyt, "") %>%
              lapply(., unique) %>%
              lapply(., length) %>%
              unlist()) == 5]

l.nyt  <- strsplit(nyt, "")
df.nyt <- data.frame(word = nyt,
                     l1 = unlist(lapply(X = l.nyt,
                                        FUN = nth,
                                        n   = 1)),
                     l2 = unlist(lapply(X = l.nyt,
                                        FUN = nth,
                                        n   = 2)),
                     l3 = unlist(lapply(X = l.nyt,
                                        FUN = nth,
                                        n   = 3)),
                     l4 = unlist(lapply(X = l.nyt,
                                        FUN = nth,
                                        n   = 4)),
                     l5 = unlist(lapply(X = l.nyt,
                                        FUN = nth,
                                        n   = 5))) %>% as_tibble()
# 
# rm(l.nyt, nyt)

# game----
correct_letters <- c("e", "t", "u", "b")

l1 <- list(correct = NA, 
           impossible = c("r", "q", "p", "d"))
l2 <- list(correct = "e", 
           impossible = c("i", "u", "o"))
l3 <- list(correct = NA, 
           impossible = c("p", "e", "k"))
l4 <- list(correct = "u", 
           impossible = c("e", "s"))
l5 <- list(correct = "t", 
           impossible = c("n", "r"))



df.word <- df.nyt

df.word[df.word$l1 %in% unlist(ifelse(!is.na(l1$correct), 
                                      yes = l1$correct, 
                                      no = list(letters[!letters %in% l1$impossible]))) &
          df.word$l2 %in% unlist(ifelse(!is.na(l2$correct), 
                                        yes = l2$correct, 
                                        no = list(letters[!letters %in% l2$impossible]))) & 
          df.word$l3 %in% unlist(ifelse(!is.na(l3$correct), 
                                        yes = l3$correct, 
                                        no = list(letters[!letters %in% l3$impossible]))) & 
          df.word$l4 %in% unlist(ifelse(!is.na(l4$correct), 
                                        yes = l4$correct, 
                                        no = list(letters[!letters %in% l4$impossible]))) & 
          df.word$l5 %in% unlist(ifelse(!is.na(l5$correct), 
                                        yes = l5$correct, 
                                        no = list(letters[!letters %in% l5$impossible]))),]

# letters[letters %in% l1$correct & 
#           !letters %in% l1$impossible]
# letters[letters %in% l2$correct & 
#           !letters %in% l2$impossible]
# 
# 
# # letter 1
# df.word[df.word$l1 %in% l1$correct & 
#           df.word$l2 %in% "e" & 
#           #df.word$l3 %in% letters & 
#           df.word$l4 %in% "u" & 
#           df.word$l5 %in% "t",]
# 
# 
# # # df.nyt[grepl(l1$correct, x = df.nyt$l1) & 
# # #          !grepl(paste(l1$impossible,sep = "|", collapse = "|"), x = df.nyt$l1),]
# # # df.nyt[grepl(l2$correct, x = df.nyt$l2),]
# # # df.nyt[grepl(l3$correct, x = df.nyt$l3),]
# # # df.nyt[grepl(l4$correct, x = df.nyt$l4),]
# # # df.nyt[grepl(l5$correct, x = df.nyt$l5) & 
# # #          !grepl(paste(l5$impossible,sep = "|", collapse = "|"), x = df.nyt$l5),]
# # # 
# # # df.nyt[!grepl(paste(l1$impossible,sep = "|", collapse = "|"), x = df.nyt$l1),]
# # # df.nyt[!grepl(l2$impossible, x = df.nyt$l2),]
# # # df.nyt[!grepl(l3$impossible, x = df.nyt$l3),]
# # # df.nyt[!grepl(l4$impossible, x = df.nyt$l4),]
# # # df.nyt[!grepl(l5$impossible, x = df.nyt$l5),]
# # 
# # 
# # # # reduce list down to words containing all correct letters
# # dfpos <- df.nyt
# # # dfpos <- df.nyt[grepl(paste(correct_letters, sep = "|", collapse = "|"), 
# # #              x = df.nyt$word),]
# # 
# # 
# # # for solved positions include words that match 'possible' 
# # l1$correct # NA
# # l2$correct # e
# # 
# # eval.tfna <- (dfpos$l1 == l1$correct & 
# #   dfpos$l2 == l2$correct & 
# #   dfpos$l3 == l3$correct & 
# #   dfpos$l4 == l4$correct & 
# #   dfpos$l5 == l5$correct)  
# # 
# # eval.tfna[is.na(eval.tfna)] <- T
# # 
# # 
# # dfpos[eval.tfna,]
# # 
# # if(!is.na(l1$correct)){
# #   dfpos <- dfpos[dfpos$l1 == l1$correct,]
# # }
# # if(!is.na(l2$correct)){
# #   dfpos <- dfpos[dfpos$l2 == l2$correct,]
# # }
# # if(!is.na(l3$correct)){
# #   dfpos <- dfpos[dfpos$l3 == l3$correct,]
# # }
# # if(!is.na(l4$correct)){
# #   dfpos <- dfpos[dfpos$l4 == l4$correct,]
# # }
# # if(!is.na(l5$correct)){
# #   dfpos <- dfpos[dfpos$l5 == l5$correct,]
# # }
# # 
# # # for all positions include words that match any 
# # 
# # dfpos <- dfpos[grepl(pattern = paste(letters[!letters %in% l1$impossible], sep = "|", collapse = "|"), 
# #                      x       = dfpos$l1) &
# #                  grepl(pattern = paste(letters[!letters %in% l2$impossible], sep = "|", collapse = "|"), 
# #                        x       = dfpos$l2) &
# #                  grepl(pattern = paste(letters[!letters %in% l3$impossible], sep = "|", collapse = "|"), 
# #                        x       = dfpos$l3) &
# #                  grepl(pattern = paste(letters[!letters %in% l4$impossible], sep = "|", collapse = "|"), 
# #                        x       = dfpos$l4) &
# #                  grepl(pattern = paste(letters[!letters %in% l5$impossible], sep = "|", collapse = "|"), 
# #                        x       = dfpos$l5),]
# # 
# # # must include all correct letters
# # allcor.list <- list()
# # for(i in 1:length(correct_letters)){
# #   allcor.list[[i]] <- grepl(correct_letters[i], dfpos$word)
# # }
# # 
# # keep.which <- NULL
# # for(i in 1:nrow(dfpos)){
# #   keep.which <- c(keep.which, 
# #                   all(unlist(lapply(X = allcor.list, 
# #          FUN = nth, n = i))))
# # }
# # 
# # dfpos <- dfpos[keep.which,]
# # 
# # # eliminate words with duplicate letters
# # 
# # dfpos <- dfpos[unlist(lapply(lapply(strsplit(dfpos$word, ""), unique), length)) == 5,]
# # 
# # # solution----
# # 
# # dfpos$word
