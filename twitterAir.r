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
consumer_key <- "BujsomF1UhhKKWvITZOwF5Zsc"
consumer_secret <-"fTvj08eVKFFYXwxnWBms2RvZWtN5UISFr5utcDky2Ykux89aoR"

# Generating access token
twitter_token <- create_token(
  app = app_name,
  consumer_key,
  consumer_secret)

# Defining airlines
airlines <- c("united", "alaskaair", "allegiant", "americanair", 
              "delta", "flyfrontier", "hawaiianair", "jetblue", "southwestair", "spiritairlines")

# Getting tweet for airline
getAirlineTweets <- function(i){
  tweets <- search_tweets(paste("@",i," OR ", "#", i, sep = ""), n=10000,
                          lang = "en", geocode = lookup_coords("USA"),include_rts = FALSE,
                          retryonratelimit = TRUE)
  return(tweets)
}

airlineTweets <- lapply(airlines, getAirlineTweets)
names(airlineTweets) <- airlines
saveRDS(airlineTweets, file = "airlineTweets.rds")
