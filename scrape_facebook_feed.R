library(Rfacebook)
library(jsonlite)
library(dplyr)

# Make sure you set your env variables once within your R session with:
# Sys.setenv(FACEBOOK_APP_ID = <app_id>, FACEBOOK_APP_SECRET = <app_secret>)

FACEBOOK_APP_ID = Sys.getenv("FACEBOOK_APP_ID")
FACEBOOK_APP_SECRET = Sys.getenv("FACEBOOK_APP_SECRET")

fb_oauth <- fbOAuth(FACEBOOK_APP_ID, FACEBOOK_APP_SECRET, extended_permissions = TRUE, scope = "user_posts")

# Find your fbid here: https://findmyfbid.in/
fbid = 733359913
user = getUsers(fbid, fb_oauth)

get_feed_page_df <- function(data) {
  sapply(data, function(post) {
    list(
      message = post$message,
      story = post$story,
      created_time = post$created_time,
      id = post$id
    )
  }) %>%
    t() %>% 
    data.frame()
} 

scrape_feed <- function() {
  url <- "https://graph.facebook.com/v2.10/me/feed"
  urls <- character()
  feed <- data.frame(
    message = character(),
    story = character(),
    created_time = character(),
    id = character()
  )
  i <- 1
  while(TRUE) {
    feed.page <- callAPI(url, fb_oauth)
    data <- get_feed_page_df(feed.page$data)
    if (length(data) == 0)
      break
    feed <- rbind(feed, data)
    url <- feed.page$paging$`next`
    urls[i] <- url
    i <- i + 1
  }
  feed
}

feed <- scrape_feed()
# puts final data frame in file with your name in data folder 
saveRDS(feed, paste0("data/", tolower(gsub(" ", "_", user$name)), ".RData"))
