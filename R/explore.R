library(renv)
library(dplyr)

rm(list=ls());cat('\f');gc()
# snapshot()

df_tr2023 <- data.frame(tr = c(.1,.12,.22,
                               .24,.32,.35,
                               .37), 
                        lower = c(0,11001,44726,95376,
                                  182101,231251,578126),
                        upper = c(11000,44725,95375,
                                  182100,231250,578125,
                                  999999999))

df_tr2023
