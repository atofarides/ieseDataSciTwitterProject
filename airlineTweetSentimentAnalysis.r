# Performing analysis on airline tweets dataset

# Loading necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)

# Reading tweets from csv
path <- file.path("C:", "Users", "anton", "Dropbox", "Twitter Data", "airlines.csv", fsep = "/")
tweetsText <- read_csv(path, col_names = TRUE)

# Perform unnest tokens and identify positive/negative sentiments
tweetsWords <- unnest_tokens(tweetsText, word, text)
tweetsSentiments <- tweetsWords %>%
                    inner_join(get_sentiments("bing"))

# Get sentiments by airline 
sentimentsByAirline <- count(tweetsSentiments, airline, sentiment) %>%
                        group_by(airline) %>%
                        mutate(percent = n/sum(n)) %>%
                        filter(sentiment == "positive") %>%
                        arrange(desc(percent)) %>%
                        #arrange(desc(sentiment),desc(percent)) %>%
                        ungroup() 
ggplot(sentimentsByAirline, aes(x=reorder(airline,-percent), y=percent)) +
  geom_col() 


