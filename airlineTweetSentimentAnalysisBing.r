# Attemtping the replicate the ACSI index using sentiment analysis on Twitter data
# Using the Bing lexicon

# Loading necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)

# Reading tweets from tweets csv. Also elimintaing retweets by choosing distinct text entries. 
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
# Sentiment analysis using bing lexicon

tweetsSentimentsBing <- tweetsText %>%
  mutate(.,tweetNumber = row_number()) %>%
  unnest_tokens(., word, text) %>%
  filter(!word %in% stop_words$word)%>%
  group_by(airline) %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(.,total_words = n()) %>%
  ungroup()
                       
# Creating a chart to identify if any of the highest contributing words are out of context
tweetsSentimentsBing %>%
  count(airline,word,sentiment,total_words) %>%
  mutate(.,percent = n/total_words) %>%
  group_by(airline, sentiment == "positive") %>%
  top_n(n=10,wt=percent) %>%
  ungroup() %>%
  mutate(word = reorder(word,percent)) %>%
  ggplot(aes(x=word,y=percent,fill = sentiment)) + 
  geom_col() + 
  coord_flip() +
  facet_wrap(~airline, scales = "free") 

# Removing words falsely identified as sentiment word such as the word "trump"

tweetsSentimentsBing <- tweetsSentimentsBing[!(tweetsSentimentsBing$word %in% c("trump")),]


# Create polarity matrix
positiveSentiments <- tweetsSentimentsBing %>%
                      filter(sentiment == "positive") %>%
                      count(.,airline, tweetNumber) 
                      
negativeSentiments <- tweetsSentimentsBing %>%
                      filter(sentiment == "negative") %>%
                      count(.,airline, tweetNumber)

pol <- full_join(positiveSentiments, negativeSentiments, by=c("tweetNumber","airline"), suffix = c(".positive",".negative"))
pol[is.na(pol)] <- 0
pol <- mutate(pol, polarity = n.positive - n.negative)

ggplot(pol, aes(x=polarity)) + geom_histogram(binwidth = 1, color="black",fill="white") + 
  facet_wrap(facets = "airline", scales = "free") + 
  ggsave(file.path("~", "Github","ieseDataSciTwitterProject", "airlinePolarityBing.pdf", fsep = "/"))

# As we can see the airlines with the most positive comments, normalised to volume, are 
# Positive: AlaskaAir, Delta, JetBlue, SouthWest, Allegiant
# Negative: Spirit, Fly Frontier

# Average polarities to rank airlines 

airlinePolarity <- pol %>%
                    group_by(airline) %>%
                    summarise(avgPol = mean(polarity)) %>%
                    mutate(rankPol = rank(desc(avgPol))) %>%
                    ungroup()

# A more extensive method of ranking polarities using thresholds but really not needed
#airlinePolarity <- pol %>%
#                    group_by(airline) %>%
#                    summarise(threshold1 = sum(polarity>=1)/sum(polarity<=-1),
#                              threshold2 = sum(polarity>=2)/sum(polarity<=-2),
#                              threshold3 = sum(polarity>=3)/sum(polarity<=-3)) %>%
#                    mutate(rankThr1 = rank(desc(threshold1)),
#                           rankThr2 = rank(desc(threshold2)),
#                           rankThr3 = rank(desc(threshold3)))

#airlinePolarity$avgRank <- 0
#for(i in 1:nrow(airlinePolarity)){
#  airlinePolarity$avgRank[i] <- round(mean(c(as.numeric(airlinePolarity[i,"rankThr1"]),
#         as.numeric(airlinePolarity[i,"rankThr2"]),
#         as.numeric(airlinePolarity[i,"rankThr3"]))))
#}

# compare against ACSI
acsiRank <- c(1,6,5,4,8,3,2,9,7)
airlinePolarity <- mutate(airlinePolarity, acsi_rank = acsiRank, rank_diff = abs(rankPol-acsi_rank)) %>%
  mutate(airline = reorder(airline,acsi_rank))

ggplot(airlinePolarity,aes(x=airline, group=1)) + 
  geom_line(aes(y=desc(rankPol),color="blue")) +
  geom_line(aes(y=desc(acsi_rank),color="red")) +
  scale_color_discrete(name = "Rank", labels = c("OurRank", "ACSI")) +
  ggsave(file.path("~", "Github","ieseDataSciTwitterProject", "airlinePolarityRankBing.pdf", fsep = "/"))

mean(airlinePolarity$rank_diff) # 0.88 mean rank diff
sd(airlinePolarity$rank_diff) # 1.16 standard deviation

# write to csv
write_csv(airlinePolarity, file.path("~", "Github","ieseDataSciTwitterProject", "airlinePolarityBing.csv", fsep = "/"))



