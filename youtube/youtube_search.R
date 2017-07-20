youtube = read_html("~/Downloads/Takeout/YouTube/history/search-history.html")
youtube_table = youtube %>% html_nodes("table") %>% .[[1]] %>% html_table()
