library(renv)
library(dplyr)
library(lubridate)
library(ggplot2)

"https://github.com/rstudio/cheatsheets/blob/main/lubridate.pdf"


rm(list=ls());cat('\f')


tim.concerts <- tibble(date  = ymd(c("20100715")), 
                       venue = c("Southgate House"), 
                       city  = c("Newport, KY"), 
                       url   = c("https://www.concertarchives.org/concerts/carolina-chocolate-drops-a30a3afb-8945-40dd-b299-45d6873a397f"))

# urls----
"https://minstrelbanjo.ning.com/events/black-banjo-gathering"

"https://www.biscuitsandbanjos.com/about"

# Periods (specific clock times, or the distance between them ignoring
# irregularities like DST)----
ex.period <- ymd_hms("2018-01-01 01:30:00", 
                     tz = "America/New_York")

class(ex.period)
# Durations (tracks the physical passage of time including irregularities like
# DST)----

# Intervals (modeling the boundary created between two periods)----