# baseball game simulation


library(dplyr)
library(renv)
library(igraph)

snapshot()
status()

rm(list=ls());cat('\f')

# https://batflipsandnerds.com/2018/07/23/the-anatomy-of-an-at-bat/

# functions----
at.bat_outcome <- function(){
  choices <- list(choices = c("B", "S", "HBP", "foul", "1B", "2B", "3B", "HR", "out"), 
                  probs   = c(39.4,31.8, 0.24,   11.3, 2.7, 3.8, 4.7, 6.1, 20))
  
  outcome <- sample(x = choices$choices, size = 1, prob = choices$probs)
  
  return(outcome)
  
}
the.count <- function(atb, pB = 0, pS = 0, pO = 0, 
                      reset.count = F, print.input = F){
  if(print.input){
    print(atb)
  }
  # if HBP
  if(atb == "HBP"){
    # reset balls and strikes
    pB <- 0; pS <- 0
  }
  # if foul 0,1,2
  if(atb == "foul" & pS %in% c(0,1)){
    pS <- pS + 1
  }
  # if foul 3+
  if(atb == "foul" & pS >= 2){
    pS <- pS
  }
  # if 1b, 2b, 3b, 4b
  if(atb %in% c("1B", "2B", "3B", "HR", "out")){
    # reset balls and strikes
    pB <- 0; pS <- 0
  }
  # if S 0,1,2
  if(atb == "S" & pS %in% c(0,1,2)){
    pS <- pS + 1
  }
  # if S 3
  if(atb == "S" & pS >2){
    pS <- 3
  }
  # if B 0,1,2,3
  if(atb == "B"){
    pB <- pB + 1
  }
  # # if B 4
  if(pB == 4){
    # pB <- 0
    # pS <- 0
    pO <- pO
  }
  
  # if out
  if(pS == 3 | atb == "out"){
    pO <- pO + 1
    # pS <- 0
    # pB <- 0
  }
  
  # reset count
  if(reset.count){
    pS = 0; pB = 0
  }
  # # reset inning
  # if(reset.inning){
  #   
  # }
  out <- c(pB = pB,
           pS = pS, 
           pO = pO)
  return(out)
}


# THE GAME----
# setup
pb  <- 0
ps  <- 0
po  <- 0
pi  <- 1
pit <- T
var_reset.count <- F

# THE INNING----
n.error <- 0

while(po < 3 ){
  # error handling
  n.error <- n.error + 1
  stopifnot(n.error < 1000)
  
  # AT BAT----
  abo <- at.bat_outcome()
  temp.count <- the.count(atb = abo, 
                          pB  = pb, 
                          pS  = ps, 
                          pO  = po, 
                          reset.count = F)
  
  # was there just an out???
  if(po != temp.count[["pO"]]){
    po <- po + 1
    
    print.out <- T
    
    # reset count
    pb <- 0
    ps <- 0
     var_reset.count <- T
  }else{
    print.out <- F
  # don't reset count
    var_reset.count <- F
  }
  
  if(! var_reset.count){
    pb <- temp.count[["pB"]]
    ps <- temp.count[["pS"]]
    po <- temp.count[["pO"]]
  }else{
    pb <- 0
    ps <- 0
  }
  
  # print count
  print(unname(temp.count))
  
  # print stuff
  if(print.out & temp.count[["pS"]] < 3){
    print("-------out----------")
  } 
  
  if(print.out & temp.count[["pS"]] == 3){
    print("----strikeout-------")
  }
  
  # was it the third out?
  if(po == 3){
    # reset count
    pb <- 0
    ps <- 0
    
    print("------INNING OVER-----")
  }
  # when the batter advances (1b,2b,3b,hr,hbp,b&pb==4)
  if(abo %in% c("1B", "2B", "3B", "HR", "HBP") | 
     (abo == "B" & pb == 4 )){
    
    print.bat <- T
    
    # reset count
    pb <- 0
    ps <- 0
    
  }else{
    print.bat <- F
  }
  if(print.bat){
    print('batter advances')
  }
  
  
}


# at


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


