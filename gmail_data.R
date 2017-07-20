# install.packages("gmailr")
library(gmailr)
# library(help = "gmailr")

## Set up credentials 
client.id = Sys.getenv("CLIENT_ID")
client.secret = Sys.getenv("CLIENT_SECRET")

gmail_auth(scope = "full", id = client.id, secret = client.secret)


## Retrieve message id's of sent messages 
pg.tokens = c("","14367107179700456670", "13496120671825930671", "15512031075180144861", "10750456174552507187", "17264870195802435667")

all.sent = sapply(pg.tokens, function(x) messages(label_ids = "SENT", page_token = as.character(x)) %>% 
                    id(what = "message_id")) 


## to(); from(); cc(); bcc(); subject(); body()


## Retrieve body of text from message id's 
sent.text = sapply(unlist(all.sent), function(x) message(id = as.character(x)) %>% body() %>% as.character())
text.vector = sub("^character(0)$", "no text", sent.text)


## Retrieve dates 
sent.dates = sapply(unlist(all.sent), function(x) message(id = as.character(x)) %>% date() %>% as.character())


## Create data frame 
date.text = data.frame(Dates = sent.dates, Message = text.vector)









all.sent.fixed = sapply(1:8, function(x) threads(label_ids = "SENT", page_token = as.character(x)) %>% 
                    id(what = "thread_id"))
hmm = all.sent.fixed %>% unlist() 


all.sent = sapply(1:100, function(x) messages(label_ids = "SENT", page_token = as.character(x)) %>% 
                    id(what = "message_id"))
big = all.sent %>% unlist() 

# OLDEST MESSAGE/THREAD ID: 1487d1bb52a6af40
# NEWEST MESSAGE ID: 15d575bfd23b88ec
