

library(jsonlite)
source("twitter_30day_setup.R")
mytoken <- create_btoken()


testsbody = "{\"query\": \"from:"
testhandle = "jaytatum0"
testebody = "\",\"maxResults\": 100,\"fromDate\":\"202003100000\",\"toDate\":\"202004100000\" }"


testbodypaste = paste(testsbody,testhandle,testebody,sep="")

test_tweets <- POST(url = "https://api.twitter.com/1.1/tweets/search/30day/devnetworkapp2.json",
                          add_headers("authorization" = mytoken, "content-Type" = "application/json"),
                          body = testbodypaste)

test_tweets_df <- fromJSON(content(test_tweets, "text"),flatten = TRUE) %>% data.frame()

test_tweets_df <- test_tweets_df[,c(1,2,10,12,19,30,32,33,97,99,100,190,192,193)]
