### File: NBATwitterData.R
### Author: Fred Sackfield
### Description: This file contains R code that uses the rtweet library to collect twitter data 
###              from NBA player accounts.


### Import/Setup Dependencies
library(dplyr)
library(rtweet)
library(jsonlite)

source("twitter_30day_setup.R")
mytoken <- create_btoken()

########### Historical Tweets ################

#The goal of obtaining historical (30day) tweet history is to create edges based on total interactions, rather than just following

##because of rate limits, we need to work with a reduced list from the original 380 players
##we can look at account info for each player to see the last date they tweeted, and exclude players whose last tweet was >1 month ago

players_reduced <- read.csv('players_reduced.csv',header=T,encoding="UTF-8")
players_reduced$handle <- as.character(players_reduced$handle)
handles <- players_reduced$handle


user_info <- lookup_users(handles)

user_info$created_at <- as.Date(user_info$created_at)

active_users <- subset(user_info, created_at > as.Date("2020-03-11"))
selected_handles <- active_users$screen_name


#now we have a list of 194 players to collect historical tweets from

sbody = "{\"query\": \"from:"
ebody = "\",\"maxResults\": 100,\"fromDate\":\"202003110000\",\"toDate\":\"202004100000\" }"

#we need to create separate dataframes to store the tweets data depending on what columns are returned

#cols are the column names for results that contain replies, quotes, and retweets
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

#repquote_cols are the column names for results that contain replies and quotes, but no retweets
repquote_cols <- c('results.created_at',
          'results.id',
          'results.in_reply_to_user_id',
          'results.in_reply_to_screen_name',
          'results.is_quote_status',
          'results.user.id',
          'results.user.name',
          'results.user.screen_name',
          'results.quoted_status.user.id',
          'results.quoted_status.user.name',
          'results.quoted_status.user.screen_name')

#repretweet_cols are the column names for results that contain replies and retweets, but no quotes
repretweet_cols <- c('results.created_at',
          'results.id',
          'results.in_reply_to_user_id',
          'results.in_reply_to_screen_name',
          'results.is_quote_status',
          'results.user.id',
          'results.user.name',
          'results.user.screen_name',
          'results.retweeted_status.user.id',
          'results.retweeted_status.user.name',
          'results.retweeted_status.user.screen_name')

#quoteretweet_cols are the column names for results that contain quotes and retweets, but no replies
quoteretweet_cols <- c('results.created_at',
                     'results.id',
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


####START: 238 requests available, 194 handles to search


####ATTEMPT 1: 60 requests made, 38 successful

#After 1: 178 requests available, 156 handles to search

#tweets_df <- data.frame(matrix(ncol=14,nrow=0))
#colnames(tweets_df) <- cols


####ATTEMPT 2: 156 requests made, 137 successful

selected_handles <- selected_handles[-c(which(selected_handles %in% tweets_df$results.user.screen_name))]

repquote_df <- data.frame(matrix(ncol=11,nrow=0))
repretweet_df <- data.frame(matrix(ncol=11,nrow=0))
quoteretweet_df <- data.frame(matrix(ncol=12,nrow=0))

colnames(repquote_df) <- repquote_cols
colnames(repretweet_df) <- repretweet_cols
colnames(quoteretweet_df) <- quoteretweet_cols


#loop through handles and collect historical data into one of the four available df's, depending on which columns are returned
for (s in 1:length(selected_handles)){
  
  # pause if divisible by 15
  if (s %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
  
  #create the query to pass into the request url
  full_body = paste(sbody,selected_handles[s],ebody,sep="")
  
  #get tweet payload, passing my unique token and the query
  resTweets <- POST(url = "https://api.twitter.com/1.1/tweets/search/30day/devnetworkapp2.json",
                    add_headers("authorization" = mytoken, "content-Type" = "application/json"),
                    body = full_body)
  
  #convert to js
  js <- fromJSON(content(resTweets, "text"),flatten = TRUE)
  
  #skip if no results are returned
  if (length(js$`results`)==0){
    next
  }
  
  #convert to df
  res_df <- js %>% data.frame()
  
  #compare returned columns to each of the 4 options
  missing1 <- length(c(which(!cols %in% colnames(res_df))))
  missing2 <- length(c(which(!repquote_cols %in% colnames(res_df))))
  missing3 <- length(c(which(!repretweet_cols %in% colnames(res_df))))
  missing4 <- length(c(which(!quoteretweet_cols %in% colnames(res_df))))
  
  #insert data into correct df based on which columns are missing
  if (missing1==0){
    tweets_df <- rbind(tweets_df, res_df[cols])
  } else if (missing2==0) {
    repquote_df <- rbind(repquote_df, res_df[repquote_cols])
  } else if (missing3==0) {
    repretweet_df <- rbind(repretweet_df, res_df[repretweet_cols])
  } else if (missing4==0) {
    quoteretweet_df <- rbind(quoteretweet_df, res_df[quoteretweet_cols])
  } else next
  
}

#fill in missing columns so that the dfs can be combined
repquote_df$results.retweeted_status.user.id <- NA
repquote_df$results.retweeted_status.user.name <- NA
repquote_df$results.retweeted_status.user.screen_name <- NA

repretweet_df$results.quoted_status.user.id <- NA
repretweet_df$results.quoted_status.user.name <- NA
repretweet_df$results.quoted_status.user.screen_name <- NA
repretweet_df <- repretweet_df[, c('results.created_at','results.id','results.in_reply_to_user_id','results.in_reply_to_screen_name',
                                   'results.is_quote_status','results.user.id','results.user.name','results.user.screen_name',
                                   'results.quoted_status.user.id','results.quoted_status.user.name','results.quoted_status.user.screen_name',
                                   'results.retweeted_status.user.id','results.retweeted_status.user.name','results.retweeted_status.user.screen_name')]


tweets_df_final <- rbind(tweets_df,repquote_df,repretweet_df)

write.csv(tweets_df_final, "30day_tweets.csv", row.names = FALSE)


######GET FAVORITES

