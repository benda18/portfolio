# generate resume

library(renv)
library(dplyr)
library(lubridate)

# https://www.reddit.com/r/jobs/comments/7y8k6p/im_an_exrecruiter_for_some_of_the_top_companies/

rm(list=ls());cat('\f')


fun_job <- function(j_name, 
                    j_title,
                    start_date, 
                    end_date = NA){
  require(lubridate)
  # correct end_date of NA to present
  end_date <- ifelse(is.na(end_date), "Present", end_date)
  
  # format dates as Month -- Year
  
}

fun_job(j_name, 
        j_title,
        start_date, 
        end_date = NA)

# vars----
var_name       <- "Tim Bender"
var_email      <- "bendertj@gmail.com"
var_phone      <- "513.477.8924"
var_portfolio  <- NULL
var_address    <- "Durham, NC"  # city/state is good enough
var_filename   <- "Timothy_Bender_Resume"
var_fileformat <- "pdf"

var_interests  <- c("Eclipses", "US History - Civil Rights Era", "Bicycling", 
                    "Listening to / discovering jazz albums", 
                    "Data Exploration and Visualization")

var_sec01      <- "Work Experience"
var_sec02      <- "Education"
var_sec03      <- "Skills & Interests"
