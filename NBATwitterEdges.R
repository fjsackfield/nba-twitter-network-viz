### File: NBATwitterEdges.R
### Author: Fred Sackfield
### Description: This file contains R code that uses the rtweet library to collect twitter data 
###              from NBA player accounts.


### Import/Setup Dependencies
library(dplyr)
library(rtweet)


### Read in player data (players represent the graph vertices)
players <- read.csv('players.csv',header=T,encoding="UTF-8")
players$handle <- as.character(players$handle)

players[players$id=="barneha02",]$handle <- "hbarnes"
players[players$id=="willijo04",]$handle <- "Iam_J3"
players[players$id=="smithis01",]$handle <- "IshSmith"
players[players$id=="hezonma01",]$handle <- "mariohezonja"
players[players$id=="conlemi01",]$handle <- "MCONLEY10"
players[players$id=="cartemi01",]$handle <- "mcarterwilliams"
players[players$id=="spellom01",]$handle <- "Omarispellman"
players[players$id=="ojelese01",]$handle <- "semi"
players[players$id=="warretj01",]$handle <- "TonyWarrenJr"

handles <- players$handle

#need to obtain user_id from twitter account info and add it as a column (table with new column = players_merge)
lookup_info <- lookup_users(handles)
colnames(lookup_info)[4] <- "handle"
players_merge <- merge(x=players,y=lookup_info[,c(1,4)],by="handle")

#store the updated players data in a file for reference
write.csv(players_merge,"players_merge.csv",row.names=FALSE)

############## Get Edges ###################

#The goal of obtaining "following" list for each player is to define edges in our initial graph

handles <- players_merge$handle

edges <- list()

# start loop
for (h in 1:length(handles)){
  edges[[h]] <- get_friends(handles[h])
  
  # pause if divisible by 15
  if (h %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
edges_df <- bind_rows(edges)

write.csv(edges_df, "edges.csv", row.names = FALSE)



###Cleanup - Missing Users
users_original <- unique(players_merge$handle)
users_returned <- unique(edges_df$user)
missing_users <- handles[!(handles %in% users_returned)]

edges_2 <- list()


for (m in 1:length(missing_users)){
  edges_2[[m]] <- get_friends(missing_users[m])
  
  # pause if divisible by 15
  if (m %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}

# Combine data tables in list
edges_2_df <- bind_rows(edges_2)

#combine original edges df with 2nd cleanup wave
edges_final <- rbind(edges_df, edges_2_df)

##store the final edges file
write.csv(edges_final, "edges_final.csv", row.names = FALSE)
