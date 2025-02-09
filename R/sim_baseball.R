# baseball game simulation


library(dplyr)
library(renv)
library(igraph)

snapshot()
status()

rm(list=ls());cat('\f')

out.count   <- 0
team.id     <- -1
pitch.count <- c("B" = 0,"S" = 0)
inning.num  <- 0.5


game.over <- F; n <- 0
while(!game.over){
  # check safe
  n <- n+1
  stopifnot(n<10000)
  
  # play ball----
  # END OF INNING
  if(out.count == 3){
    team.id   <- team.id * -1
    out.count <- 0
    pitch.count <- c("B" = 0,"S" = 0)
    inning.num  <- inning.num + 0.5
  }
  
  # PITCH
  
  # strikeout
  
  # walk
  
  the.pitch <- c("strike", "ball", "hit")
  
  if(the.pitch == "strike"){
    pitch.count["S"] <- pitch.count["S"] + 1
  }
  
  
  
  
}


