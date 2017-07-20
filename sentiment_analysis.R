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

# Extracts year and gets added to the data frame
year = year(df_table[[2]]) # change depending on platform ([[1]] for gmail)
df_table = df_table %>% mutate(Year = year)
df_table = df_table %>% select(-`Time of Search`) # argument must be changed for different platform

# Makes the data frame tidy
df_tidy = df_table %>% unnest_tokens(word, `YouTube Search Query`) # argument must be changes for different platform

# Remove stop words and duplicates
df_tidy %>% count(word, sort = TRUE)
df_tidy = df_tidy %>% anti_join(stop_words)

df_tidy = unique(df_tidy)

# Sentiment analysis (sentiment value over the years)
sentiment_afinn = df_tidy %>% inner_join(get_sentiments("afinn")) %>% 
  group_by(Year) %>%
  summarise(total = sum(score))

# Sentiment analysis (most common emotions over years) ERROR
sentiment_nrc = df_tidy %>% inner_join(get_sentiments("nrc")) %>%
  group_by(Year) %>%
  summarise(count = count(sentiment, sort = TRUE)) # not working as doesnt work well with character

# Plotting the findings by year (sentiment value over the years)
ggplot(sentiment_afinn, aes(x = Year, y = total)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Sentiment", title = "Change in Sentiment Per Year") +
  scale_x_discrete(limits = year) + 
  ylim(-10, 10) # limits can be changed


