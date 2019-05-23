# Sentiment analysis using Twitter data of US airlines
setwd("~/Github/ieseDataSciTwitterProject")

## install rtweet package if it's not already
if (!requireNamespace("rtweet", quietly = TRUE)) {
  install.packages("rtweet")
}

# Load libraries
library("rtweet")
library("readr")

# Defining Twitter access tokens
app_name <- "AirlineCustomerSatisfaction"
consumer_key <- " "
consumer_secret <-" "

# Generating access token
#twitter_token <- create_token(
#  app = app_name,
#  consumer_key,
#  consumer_secret)

# Defining airlines
airlines <- c("united", "alaskaair", "allegiant", "americanair", 
              "delta", "flyfrontier", "hawaiianair", "jetblue", "southwestair", "spiritairlines")

# Getting tweet for airline
getAirlineTweets <- function(i){
  tweets <- search_tweets(paste("@",i," OR ", "#", i, sep = ""), n=1000000,
                          lang = "en", geocode = lookup_coords("USA"),include_rts = TRUE,
                          retryonratelimit = TRUE, since = as.character(Sys.Date()-1, until = as.character(Sys.Date())))
  return(tweets)
}

airlineTweets <- lapply(airlines, getAirlineTweets)
names(airlineTweets) <- airlines
#saveRDS(airlineTweets, file = "airlineTweets.rds")
