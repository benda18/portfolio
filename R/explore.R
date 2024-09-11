library(renv)
library(dplyr)

rm(list=ls());cat('\f');gc()
# snapshot()

df_tr2023 <- data.frame(lvl = LETTERS[1:7], 
                        tr    = c(.1,.12,.22,
                                  .24,.32,.35,
                                  .37), 
                        lower = c(0,11001,44726,95376,
                                  182101,231251,578126),
                        upper = c(11000,44725,95375,
                                  182100,231250,578125,
                                  999999999))

df_tr2023

taxbrax <- function(inc = 1234567){
  out <- df_tr2023
  out$taxed <- 0
  out[out$upper <= inc,]$taxed <- (out$upper[out$upper <= inc] - 
                                     out$lower[out$upper <= inc]) * 
    out$tr[out$upper <= inc]
  
  out[min(which(!out$upper <= inc)),]$taxed <- (inc - 
                                                  out[min(which(!out$upper <= inc)),]$lower) *
    out[min(which(!out$upper <= inc)),]$tr
  return(out)
}

sum(taxbrax(150000+39000)$taxed)*0.26
