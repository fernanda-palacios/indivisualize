library(tidytext)
library(lubridate)

name <- "fernanda_palacios"
feed <- readRDS(paste0("data/", name, ".RData"))

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
  transmute(message = unlist(message), year = year(unlist(created_time))) %>% 
  unnest_tokens(word, message) %>% 
  anti_join(stop_words)

feed.counts <- feed.tidy %>% count(year, word, sort = TRUE)
feed.year_counts <- feed.counts %>% group_by(year) %>% summarise(total = sum(n)) %>% ungroup()
feed.total <- left_join(feed.counts, feed.year_counts)

# Add in TF, IDF and TFIDF.
#
feed.total <- feed.total %>% bind_tf_idf(word, year, n)

feed.total %>% arrange(desc(tf_idf)) %>% 
  group_by(year) %>% 
  top_n(4) %>% 
  ungroup %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(x = word, y = tf_idf, fill = factor(year))) +
  geom_col() +
  facet_wrap(~factor(year), scales = "free") +
  coord_flip() + 
  labs(title = "Your Vocabulary Through the Years") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

saveRDS(feed.tidy, paste0("data/", name, "-facebook.RData"))

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
  count(year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative, valence = sentiment > 0) %>% 
  ggplot(aes(x = factor(year), y = sentiment, color = valence)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Sentiment", title = "Your Sentiment Through the Years") +
  theme(legend.position = "none")
