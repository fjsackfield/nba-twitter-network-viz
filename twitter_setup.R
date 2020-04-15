Sys.getenv()
Sys.setenv(TWITTER_APP='Network_Viz_App')
Sys.setenv(TWITTER_CONSUMER_KEY='gHvYsYgHeB9HylcWZ7LNDO91n')
Sys.setenv(TWITTER_CONSUMER_SECRET='iklKrD5R37eawVBrWmQB5OC8uz3LYQBgv8VzCiJDjxXSMxMH1V')
install.packages("rtweet")
library(rtweet)
create_token(app=TWITTER_APP,consumer_key=TWITTER_CONSUMER_KEY,consumer_secret=TWITTER_CONSUMER_SECRET) -> twitter_token
create_token(app=Sys.getenv("TWITTER_APP"),consumer_key=Sys.getenv("TWITTER_CONSUMER_KEY"),consumer_secret=Sys.getenv("TWITTER_CONSUMER_SECRET")) -> twitter_token
saveRDS(twitter_token, "~/.rtweet.rds")
Sys.setenv(TWITTER_PAT=~/.rtweet.rds)
Sys.setenv(TWITTER_PAT='~/.rtweet.rds')

##########################################
library(httr)
library(base64enc)
library(jsonlite)
library(stringr)
suppressMessages(library(tidyverse))
library(tidytext)
library(knitr)
library(XML)
suppressMessages(library(RCurl))
library(methods)
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
library(topicmodels)

#############################################

api <- read.table("Twitter_API_Key.txt", header = TRUE, stringsAsFactors = FALSE)
names(api)
dim(api)
App_Name <- api$app_name
Consumer_Key <- api$key
Consumer_Secret <- api$secret_Key
Access_Token <- api$access_token
Access_Secret <- api$access_token_secret

