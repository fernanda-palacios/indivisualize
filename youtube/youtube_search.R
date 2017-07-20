# TEXT MINING
# Assuming data frame of the form date, text message
library(rvest)
library(readr)
library(RCurl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(tidytext)
library(lubridate)

youtube = read_html("youtube/search-history.html")
youtube_table = youtube %>% html_nodes("table") %>% .[[1]] %>% html_table()
colnames(youtube_table) = c("text", "date")

youtube_table$date = as.Date(youtube_table$date)


# Remove duplicate searches
youtube_table = unique(youtube_table)


# Sort by date
youtube_table = arrange(youtube_table, .bygroup = date)


# Create year buckets to track development over time

year = year(youtube_table$date)

youtube_table = cbind(youtube_table, year)



# Split each message into words.
# word is names of col
youtube.tidy <- youtube_table %>% unnest_tokens(word, text)


# text is dominated by unimportant words
youtube.tidy %>% count(word, sort = TRUE)

# REMOVE STOP WORDS 
stop_words

youtube.tidy <- youtube.tidy %>% anti_join(stop_words)



# VISUALIZATION
# Most common words in general


youtube.count = youtube.tidy %>%
  group_by(year) %>% 
  count(word, sort = TRUE) %>% 
  top_n(3) %>% 
  filter(is.na(as.numeric(word))) %>% 
  filter(!n == 0) %>% 
  ungroup() 



youtube.count = arrange(youtube.count, year, n)


# From 2012 & 2013 & 2014
youtube.1 = youtube.count[which(youtube.count$year == 2012 | 
                                  youtube.count$year == 2013 |
                                  youtube.count$year == 2014 ), ]

ggplot(youtube.1, aes(x = word, y = n)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~year, scales = "free_x")

# From 2015 & 2016 & 2017 

youtube.2 = youtube.count[which(youtube.count$year == 2015 | 
                                  youtube.count$year == 2016 |
                                youtube.count$year == 2017), ]

ggplot(youtube.2, aes(x = word, y = n)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~year, scales = "free_x")


















