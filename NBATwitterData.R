### File: NBATwitterData.R
### Author: Fred Sackfield
### Description: This file contains R code that uses the rtweet library to collect twitter data 
###              from NBA player accounts.



########### Historical Tweets ################

#The goal of obtaining historical (30day) tweet history is to create edges based on total interactions, rather than just following

source("twitter_30day_setup.R")
mytoken <- create_btoken()

players_reduced <- read.csv('players_reduced.csv',header=T)
players_reduced$handle <- as.character(players_reduced$handle)
selected_handles <- players_reduced$handle

sbody = "{\"query\": \"from:"
ebody = "\",\"maxResults\": 100,\"fromDate\":\"202003100000\",\"toDate\":\"202004100000\" }"

cols <- c('results.created_at',
'results.id',
'results.in_reply_to_user_id',
'results.in_reply_to_screen_name',
'results.is_quote_status',
'results.user.id',
'results.user.name',
'results.user.screen_name',
'results.quoted_status.user.id',
'results.quoted_status.user.name',
'results.quoted_status.user.screen_name',
'results.retweeted_status.user.id',
'results.retweeted_status.user.name',
'results.retweeted_status.user.screen_name')

tweets_df <- data.frame(matrix(ncol=14,nrow=0))
colnames(tweets_df) <- cols


for (s in 1:length(selected_handles)){
  
  full_body = paste(sbody,selected_handles[s],ebody,sep="")
  
  resTweets <- POST(url = "https://api.twitter.com/1.1/tweets/search/30day/devnetworkapp2.json",
                    add_headers("authorization" = mytoken, "content-Type" = "application/json"),
                    body = full_body)
  
  js <- fromJSON(content(resTweets, "text"),flatten = TRUE)
  
  if (length(js$`results`)==0){
    next
  }
  
  res_df <- js %>% data.frame()
  
  allCols = TRUE
  for (c in 1:length(cols)){
    if (!cols[c] %in% colnames(res_df)){
      allCols = FALSE
    }
  }
  
  if (allCols == FALSE){
    next
  }
  
  tweets_df <- rbind(tweets_df, res_df[cols])
  
  # pause if divisible by 15
  if (s %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }

}


write.csv(tweets_df, "historical_tweets.csv", row.names = FALSE)
