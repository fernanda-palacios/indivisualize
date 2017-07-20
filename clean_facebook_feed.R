feed <- readRDS("data/zack_chauvin.RData")
library(tidytext)

View(feed)

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

feed.tidy <- feed %>%
  mutate(message = nullToNA(message)) %>% 
  filter(!is.na(message)) %>% 
  transmute(message = unlist(message), date = unlist(created_time)) %>% 
  unnest_tokens(word, message) %>% 
  anti_join(stop_words)

feed.tidy %>% count(word, sort = TRUE)

feed.tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  #
  # Change the order of factor levels.
  #
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
