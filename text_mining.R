# TEXT MINING

library(RCurl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(tidytext)


# Assuming data frame of the form date, text message

# Sort by date

# Create year buckets to track development over time




# Split each message into words.
# word is names of col
gmail.tidy <- gmail.indices %>% unnest_tokens(word, text))


# text is dominated by unimportant words
gmail.tidy %>% count(word, sort = TRUE)

# REMOVE STOP WORDS 
stop_words

gmail.tidy <- gmail.tidy %>% anti_join(stop_words)



# VISUALIZATION
# Most common words in general
gmail.tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 130) %>%
  #
  # Change the order of factor levels.
  #
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# How does this change over time?
# Most common words per bucket

ggplot(sentiment, aes(x = bucket, y = sentiment)) +
  geom_bar(stat = "identity") 





  







