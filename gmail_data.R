# install.packages("gmailr")
library(gmailr)
# library(help = "gmailr")

# Sys.setenv(CLIENT_ID = <id>, CLIENT_SECRET = <secret>)

## Set up credentials 
client.id = Sys.getenv("CLIENT_ID")
client.secret = Sys.getenv("CLIENT_SECRET")

gmail_auth(scope = "full", id = client.id, secret = client.secret)


## Retrieve message id's of sent messages 
# pg.tokens = c("","14367107179700456670", "13496120671825930671", "15512031075180144861", "10750456174552507187", "17264870195802435667")
# all.sent = sapply(pg.tokens, function(x) messages(label_ids = "SENT", page_token = as.character(x)) %>% 
#                     id(what = "message_id")) 

# set the number of emails you want to retrieve
num_sent_emails = 3300
messages.sent <- messages(num_results = num_sent_emails, label_ids = "SENT") 

messages.sent.id <- messages.sent %>% 
  id(what = "message_id")

## Useful gmail_message functionality
# to()
# from()
# cc() 
# bcc()
# subject()
# body()

messages.sent.clean = sapply(messages.sent.id, function(id) {
  msg = message(id = id)
  c(
    date = date(msg),
    body = ifelse(length(body(msg)) == 0, NA, unlist(body(msg)))
  )
}) %>% t() %>% as.data.frame()

# set your name
name <- "zack_chauvin"
saveRDS(messages.sent.clean, paste0("data/", name, "-gmail-wide.RData"))
