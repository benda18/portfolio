# baseball game simulation


library(dplyr)
library(renv)
library(igraph)

snapshot()
status()

rm(list=ls());cat('\f')

# https://batflipsandnerds.com/2018/07/23/the-anatomy-of-an-at-bat/

# functions----
the.count <- function(){
  
}

# THE GAME----

# THE INNING----

# AT BAT----
pitch_outcome <- function(){
  choices <- list(choices = c("B", "S", "HBP", "foul", "1B", "2B", "3B", "HR"), 
                  probs   = c(39.4,31.8, 0.24,   11.3, 2.7, 3.8, 4.7, 6.1))
  
  
  
  n.lo <- 100-sum(choices$probs)
  
  probs <- c(b1 = .877, b2 = 1.232, 
             b3 = 1.552, hr = 1.98)
  
  probs2 <- (probs / sum(probs)) 
  
  (round(probs2*n.lo, digits = 1)) %>% sum
  
  probs3 <- round(probs2 * n.lo,1)
  
  outcome <- sample(x = choices$choices, size = 1, prob = choices$probs)
  
}




the.inning_loop <- function(){
  
}  



# older----


# out.count   <- 0
# team.id     <- -1
# pitch.count <- c("B" = 0,"S" = 0)
# inning.num  <- 0.5
# 
# 
# game.over <- F; n <- 0
# while(!game.over){
#   # check safe
#   n <- n+1
#   stopifnot(n<10000)
#   
#   # play ball----
#   # END OF INNING
#   if(out.count == 3){
#     team.id   <- team.id * -1
#     out.count <- 0
#     pitch.count <- c("B" = 0,"S" = 0)
#     inning.num  <- inning.num + 0.5
#   }
#   
#   # PITCH
#   
#   # strikeout
#   
#   # walk
#   
#   the.pitch <- c("strike", "ball", "hit")
#   
#   if(the.pitch == "strike"){
#     pitch.count["S"] <- pitch.count["S"] + 1
#   }
#   
#   
#   
#   
# }


