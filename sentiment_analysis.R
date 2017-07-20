library(dplyr)
library(tidytext)
library(rvest)
library(lubridate)
library(ggplot2)

# Reads the youtube html file
youtube = read_html("~/Documents/iXperience/karamazov/youtube/search-history.html")
youtube_table = youtube %>% html_nodes("table") %>% .[[1]] %>% html_table()

# Depending on the platform, df is assigned to the platform's dataframe (date,text)
df_table = youtube_table
df_table = data.tidy # data is tidy from gmail.R, can directly run sentiment analysis

# Remove duplicate entries
df_table = unique(df_table)

# Extracts year and gets added to the data frame
year = year(df_table[[1]]) # change depending on platform
df_table = df_table %>% mutate(Year = year)
df_table = df_table %>% select(-`Time of Search`) # argument must be changed for different platform

# Makes the data frame tidy
df_tidy = df_table %>% unnest_tokens(word, `YouTube Search Query`) # argument must be changes for different platform
df_tidy = df_table %>% unnest_tokens(word, `YouTube Search Query`)

# Remove stop words
df_tidy %>% count(word, sort = TRUE)
df_tidy = df_tidy %>% anti_join(stop_words)

# Sentiment analysis
sentiment_bing = df_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(index = Year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Plotting the findings by year
ggplot(sentiment_bing, aes(x = index, y = sentiment)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Sentiment", title = "Change in Sentiment Per Year") +
  scale_x_discrete(limits = year) + 
  ylim(-10, 10) # limits can be changed
