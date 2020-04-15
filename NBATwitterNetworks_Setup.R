### File: NBATwitterNetworks.R
### Author: Fred Sackfield
### Description: This file contains R code that creates graphs from the raw vertices and edges files.


create_n1_data <- function() {
  
  library(dplyr)
  library(reshape2)
  
  #read in player vertices
  all_players <- read.csv('players_merge.csv',header=T)
  team_colors <- read.csv('team_colors.csv',header=T)
  
  #read in the raw edges data
  e1 <- read.csv('edges_final.csv',header=T)
  v1 <- subset(all_players, handle %in% e1$user)
  v1 <- merge(v1,team_colors,by="team")
  v1 <- v1[,c(8,2,3,4,1,5,6,7,9,10)]
  
  colnames(e1) <- c("handle","following_id")
  e1 <- merge(e1,v1[,c(1,2)],by="handle")
  e1 <- e1[,c(3,2)]
  
  #filter out all non-NBA-to-NBA edges
  e1 <- subset(e1, following_id %in% v1$user_id)
  
  return(list(e1,v1))
  
}

create_n2_data <- function() {
  
  library(dplyr)
  library(reshape2)
  
  #read in player vertices
  all_players <- read.csv('players_merge.csv',header=T)
  team_colors <- read.csv('team_colors.csv',header=T)
  
  #read in the raw edges data
  historical_tweets <- read.csv('30day_tweets.csv')
  tweet_counts <- historical_tweets %>% count(results.user.id)
  colnames(tweet_counts) <- c("user_id","count")
  v2 <- subset(all_players, user_id %in% historical_tweets$results.user.id)
  v2 <- merge(v2,team_colors,by="team")
  v2 <- merge(v2,tweet_counts,by="user_id")
  v2 <- v2[,c(1,3,4,5,2,6,7,8,9,10,11)]
  
  interactions <- historical_tweets[,c(6,3,9,12)]
  colnames(interactions) <- c("user_id","reply_to_user_id","quoted_user_id","rt_user_id")
  
  #filter out all non-NBA-to-NBA edges
  interactions <- subset(interactions, (reply_to_user_id %in% v2$user_id |
                                          quoted_user_id %in% v2$user_id |
                                          rt_user_id %in% v2$user_id)
  )
  interactions_long <- melt(data = interactions, id.vars = "user_id", measure.vars = c("reply_to_user_id", 
                                                                                       "quoted_user_id",
                                                                                       "rt_user_id"))
  
  e2 <- subset(interactions_long, value %in% v2$user_id)
  e2 <- e2[,c(1,3)]
  colnames(e2) <- c("user_id","interaction_id")
  e2.agg <- e2 %>% count(user_id,interaction_id)
  
  return(list(e2,e2.agg,v2))
  
}

##### NETWORK 1a - Network of Followings, aggregated by Team

create_graph_team_following <- function() {
  
  library(igraph)
  library(dplyr)
  
  e1 <- create_n1_data()[[1]]
  v1 <- create_n1_data()[[2]]
  
  teams <- distinct(v1,team,color,border)
  team_edges <- merge(e1,v1[,c(1,5)],by="user_id")
  colnames(team_edges) <- c("user_id","following_id","team_id")
  team_edges <- merge(team_edges,v1[,c(1,5)],by.x="following_id",by.y="user_id")
  colnames(team_edges) <- c("following_id","user_id","team_id","following_team_id")
  team_edges <- team_edges[,c(3,4)]
  team_edges <- team_edges %>% count(team_id,following_team_id)
  
  team_edges <- subset(team_edges, team_id != following_team_id)
  
  
  g.teams <- graph_from_data_frame(team_edges,directed=T,vertices=teams)
  
  #add edge attributes: weight
  #set_edge_attr(g.teams, "weight", index = E(g.teams), team_edges$n)
  E(g.teams)$weight <- team_edges$n
  
  #add node attributes: name and colors
  set_vertex_attr(g.teams, "team", index = V(g.teams), teams$team)
  set_vertex_attr(g.teams, "color", index = V(g.teams), teams$color)
  set_vertex_attr(g.teams, "border", index = V(g.teams), teams$border)
  
  return(g.teams)
  
}


##### NETWORK 1 - Network of Followings (Directed Network; player a follows player b)

create_graph_following <- function() {
  
  library(igraph)
  
  e1 <- create_n1_data()[[1]]
  v1 <- create_n1_data()[[2]]
  
  g.following <- graph_from_data_frame(e1,directed=T,vertices=v1)
  
  #add vertex attributes: columns from v1
  set_vertex_attr(g.following, "fullname", index = V(g.following), v1$fullname)
  set_vertex_attr(g.following, "team", index = V(g.following), v1$team)
  set_vertex_attr(g.following, "age", index = V(g.following), v1$age)
  set_vertex_attr(g.following, "exp", index = V(g.following), v1$exp)
  set_vertex_attr(g.following, "college", index = V(g.following), v1$college)
  set_vertex_attr(g.following, "color", index = V(g.following), v1$color)
  set_vertex_attr(g.following, "border", index = V(g.following), v1$border)
  
  return(g.following)
}




##### NETWORK 2 - Network of Interactions (Undirected Network of retweets, quotes, replies, favorites)

create_graph_interactions <- function() {
  
  library(igraph)
  
  e2 <- create_n2_data()[[1]]
  e2.agg <- create_n2_data()[[2]]
  e2.red <- subset(e2,user_id != interaction_id)
  v2 <- create_n2_data()[[3]]
  
  g.interactions.unwt <- graph_from_data_frame(e2,directed=T,vertices=v2)
  
  #add vertex attributes: columns from v2
  set_vertex_attr(g.interactions.unwt, "fullname", index = V(g.interactions.unwt), v2$fullname)
  set_vertex_attr(g.interactions.unwt, "team", index = V(g.interactions.unwt), v2$team)
  set_vertex_attr(g.interactions.unwt, "age", index = V(g.interactions.unwt), v2$age)
  set_vertex_attr(g.interactions.unwt, "exp", index = V(g.interactions.unwt), v2$exp)
  set_vertex_attr(g.interactions.unwt, "college", index = V(g.interactions.unwt), v2$college)
  set_vertex_attr(g.interactions.unwt, "color", index = V(g.interactions.unwt), v2$color)
  set_vertex_attr(g.interactions.unwt, "border", index = V(g.interactions.unwt), v2$border)
  set_vertex_attr(g.interactions.unwt, "count", index = V(g.interactions.unwt), v2$count)
  
  g.interactions.wt <- graph_from_data_frame(e2.agg,directed=T,vertices=v2)
  
  #add edge attributes: weight
  set_edge_attr(g.interactions.wt, "weight", index = E(g.interactions.wt), e2.agg$n)
  
  #add vertex attributes: columns from v2
  set_vertex_attr(g.interactions.wt, "fullname", index = V(g.interactions.wt), v2$fullname)
  set_vertex_attr(g.interactions.wt, "team", index = V(g.interactions.wt), v2$team)
  set_vertex_attr(g.interactions.wt, "age", index = V(g.interactions.wt), v2$age)
  set_vertex_attr(g.interactions.wt, "exp", index = V(g.interactions.wt), v2$exp)
  set_vertex_attr(g.interactions.wt, "college", index = V(g.interactions.wt), v2$college)
  set_vertex_attr(g.interactions.wt, "color", index = V(g.interactions.wt), v2$color)
  set_vertex_attr(g.interactions.wt, "border", index = V(g.interactions.wt), v2$border)
  set_vertex_attr(g.interactions.wt, "count", index = V(g.interactions.wt), v2$count)
  
  g.interactions.noref <- graph_from_data_frame(e2.red,directed=T,vertices=v2)
  set_vertex_attr(g.interactions.noref, "fullname", index = V(g.interactions.noref), v2$fullname)
  
  return(list(g.interactions.unwt,g.interactions.wt,g.interactions.noref))
  
  
}









