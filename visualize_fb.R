library(tidytext)
library(lubridate)

feed <- readRDS("data/Aida-Ylanan.RData")

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

new_row = data.frame(
  word = c(sapply(1:10, as.character)),
  lexicon = "chauvin"
)
stop_words <- rbind(stop_words, new_row)

feed.tidy <- feed %>%
  mutate(message = nullToNA(message)) %>% 
  filter(!is.na(message)) %>% 
  transmute(message = unlist(message), post_year = year(unlist(created_time))) %>% 
  unnest_tokens(word, message) %>% 
  anti_join(stop_words)

feed.tidy %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  #
  # Change the order of factor levels.
  #
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Count", title = "Your Most Common Words")

# Sentiment analysis
feed.tidy %>% inner_join(get_sentiments("bing")) %>%
  count(post_year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative, valence = sentiment > 0) %>% 
  ggplot(aes(x = factor(post_year), y = sentiment, color = valence)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Sentiment", title = "Your Sentiment Through the Years") +
  theme(legend.position= "none")
