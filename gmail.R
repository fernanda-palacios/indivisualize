# TEXT MINING
# Assuming data frame of the form date, text message

library(readr)
library(RCurl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(tidytext)
library(lubridate)

# load data
name <- "Aida-Ylanan"
# data = read_csv("data/gmail.csv")

data = readRDS(paste0("data/", name, "-gmail-wide.RData"))
# youtube_table = youtube %>% html_nodes("table") %>% .[[1]] %>% html_table()

colnames(data) = c("date", "text")
# youtube_table$date = as.Date(youtube_table$date)

# Remove duplicate entries
data = unique(data)

# Adjust date formatting

data$date = substr(data$date, 5, 16)
data$date= gsub(" ", "", data$date)
data$date = as.Date(data$date,format='%d%b%Y')


# Sort by date
data = arrange(data, .bygroup = date)

# Create year buckets to track development over time

data <- data %>% mutate(year = year(date))
data$text <- as.character(data$text)

# Split each message into words.
# word is names of col
data.tidy <- data %>% unnest_tokens(word, text)

new_rows = data.frame(
  word = c("aida", "ylanan", "ucla.edu", "avelina", "a1daylanan", "zack", "college.harvard.edu", "chauvin", "zchauvin"),
  lexicon = c("fer")
)

stop_words_2 <- rbind(stop_words, new_rows)

data.tidy <- data.tidy %>% anti_join(stop_words_2) %>% 
  select(-date)

saveRDS(data.tidy, paste0("data/", name, "-gmail.RData"))

# VISUALIZATION

# Most common words by year
data.count = data.tidy %>%
  group_by(year) %>% 
  count(word, sort = TRUE) %>% 
  top_n(3) %>% 
  filter(is.na(as.numeric(word))) %>% 
  filter(!n == 0) %>% 
  ungroup() 

# Sort by year and frequency
data.count = arrange(data.count, year, n)


ggplot(data.count, aes(x = word, y = n)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~year, scales = "free")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))
