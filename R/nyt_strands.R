
rm(list=ls());cat('\f')
nyt <- lexicon::grady_augmented

ltrs.yes <- "foifce"


ltrs.yes <- unique(unlist(strsplit(ltrs.yes,"")))
ltrs.no  <- letters[!letters %in% ltrs.yes]


nyt[!grepl(paste(ltrs.no, sep = "|", collapse = "|"), 
      x = nyt)]


