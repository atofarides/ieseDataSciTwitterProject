# Attemtping the replicate the ACSI index using sentiment analysis on Twitter data
# Using the Nrc lexicon

# Loading necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)

# Reading tweets from csv. Also optionally elimintaing retweets by choosing distinct text entries. 
# Removing all retweets would have also removed tweets whose original tweet has not 
# been captured

path <- file.path("C:", "Users", "anton", "Dropbox", "Twitter Data", "airlines.csv", fsep = "/")
tweetsText <- read_csv(path, col_names = TRUE) 

# Comment out the following line if you want to include retweets in analysis
tweetsText <- distinct(tweetsText,text,.keep_all = TRUE) # To be used in order to exclude retweets

# Removing tweets from airline accounts as they do not reflect consumer sentiment 
tweetsText$screen_name <- tolower(tweetsText$screen_name)
tweetsText <- tweetsText[!(tweetsText$screen_name %in% c("united", "alaskaair", "allegiant", "americanair", 
                                                         "delta", "flyfrontier", "hawaiianair", "jetblue", "southwestair", "spiritairlines")),]

# Perform unnest tokens, adding the tweet ID on a separate column to keep track for further grouping 
# Sentiment analysis using nrc lexicon

tweetsNrc <- tweetsText %>%
  mutate(.,tweetNumber = row_number()) %>%
  unnest_tokens(., word, text) %>%
  filter(!word %in% stop_words$word)%>%
  group_by(airline) %>%
  mutate(.,total_words = n()) %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(.,total_sentiments = n()) %>%
  ungroup() 

# Creating a chart to identify if any of the highest contributing words are out of context
tweetsNrc %>%
  count(airline,word,sentiment,total_words) %>%
  group_by(airline,word) %>%
  mutate(.,percent = n/total_words) %>%
  ungroup() %>%
  filter(sentiment %in% c("positive","negative")) %>%
  group_by(airline, sentiment == "positive") %>%
  top_n(n=12,wt=percent) %>%
  ungroup() %>%
  mutate(word = reorder(word,percent)) %>%
  ggplot(aes(x=word,y=percent, fill = sentiment)) + 
  geom_col() + 
  coord_flip() +
  facet_wrap(~airline, scales = "free") 

# Removing words out of context in this case such as "united", "terminal", "spirit" and "flying"

tweetsNrc <- tweetsNrc[!(tweetsNrc$word %in% c("united","spirit","terminal","flying")),]

# Splitting the sentiment column into two: emotion and sentiment 
tweetsNrcSentiments <- tweetsNrc %>%
  filter(sentiment %in% c("positive","negative")) %>%
  select(c("tweetNumber","word","sentiment")) %>%
  distinct()
tweetsNrcEmotions <- tweetsNrc %>%
  filter(!(sentiment %in% c("positive","negative"))) %>%
  rename(emotion = sentiment)

tweetsNrc <- inner_join(tweetsNrcEmotions,tweetsNrcSentiments, by = c("tweetNumber","word"))

# Create a emotion and sentiment chart
tweetsNrc %>%
  count(airline,sentiment,emotion,total_sentiments) %>%
  mutate(percent=n/total_sentiments) %>%
  ggplot(.,aes(x=emotion,y=percent,fill=sentiment))+
  geom_col() +
  facet_wrap(~airline, scales = "free") +
  ggsave(file.path("~", "Github","ieseDataSciTwitterProject", "airlineEmotionNRC.pdf", fsep = "/"))
  

# Create polarity matrix (WIP)

pol <- tweetsNrc %>%
  select(c("airline","word","tweetNumber","sentiment","emotion","total_sentiments")) 
pol$sentiment <- ifelse(pol$sentiment == "positive",1,-1)
pol <- pol %>%
  group_by(airline,tweetNumber,word,emotion,total_sentiments) %>%
  summarise(sentiment = sum(sentiment)) %>%
  ungroup() %>%
  group_by(airline,emotion,total_sentiments) %>%
  summarise(sentiment = sum(sentiment)) %>%
  spread(key=emotion,value=sentiment)

