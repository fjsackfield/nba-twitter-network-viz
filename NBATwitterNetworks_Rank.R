### File: NBATwitterNetworks_Rank.R
### Author: Fred Sackfield
### Description: This file contains R code that implements the PageRank algorithm on NBA Twitter Networks to find the most popular players. 


library(igraph)
source("../R Code and Files/LinkAnalysisFunctions.R")
source("NBATwitterNetworks_Setup.R")


#### NETWORK 1

g.following <- create_graph_following()

#construct the adjacency matrix
adj1 <- get.adjacency(g.following, sparse = F)

#set/adjust s parameter for dampening
#s <- seq(from=0.7,to=0.8,by=0.01)
s <- 1
n <- c(10,100,500,1000)
scores <- c()

for (i in 1:length(n)){
  pr <- PageRank(adj1, s=s, num_iteration=n[i])
  scores <- rbind(scores,round(pr*100,6))
  
}

v1 <- create_n1_data()[[2]]

scores_df_1 <- as.data.frame(scores)
colnames(scores_df_1) <- v1$fullname

rnk1 <- as.data.frame(t(scores_df_1))
rnk1$fullname <- rownames(rnk1)
rnk1 <- rnk1[,c(5,4)]
colnames(rnk1)[2] <- "score"

rnk1 <- rnk1[order(-rnk1$score),]


### NETWORK 2

g.interactions <- create_graph_interactions()[[1]]
g.interactions.noloop <- create_graph_interactions()[[3]]

#construct the adjacency matrix
adj <- get.adjacency(g.interactions, sparse = F)
adj.noloop <- get.adjacency(g.interactions.noloop, sparse = F)

#set/adjust s parameter for dampening
#s <- seq(from=0.7,to=0.8,by=0.01)
s <- 0.85
n <- c(500,1000,5000)


scores <- c()
scores.noloop <- c()

for (i in 1:length(n)){
  pr <- PageRank(adj, s=s, num_iteration=n[i])
  scores <- rbind(scores,round(pr*100,6))
  
}

for (i in 1:length(n)){
  pr <- PageRank(adj.noloop, s=s, num_iteration=n[i])
  scores.noloop <- rbind(scores.noloop,round(pr*100,6))
  
}


#v2 <- create_n2_data()[[2]]

scores_df_2 <- as.data.frame(scores)
colnames(scores_df_2) <- V(g.interactions)$fullname
#colnames(scores_df_2) <- v2$fullname

rnk2 <- as.data.frame(t(scores_df_2))
rnk2$fullname <- rownames(rnk2)
rnk2 <- rnk2[,c(4,3)]
colnames(rnk2)[2] <- "score"

rnk2 <- rnk2[order(-rnk2$score),]



scores_df_noloop <- as.data.frame(scores.noloop)
colnames(scores_df_noloop) <- V(g.interactions.noloop)$fullname

rnk.noloop <- as.data.frame(t(scores_df_noloop))
rnk.noloop$fullname <- rownames(rnk.noloop)
rnk.noloop <- rnk.noloop[,c(4,3)]
colnames(rnk.noloop)[2] <- "score"

rnk.noloop <- rnk.noloop[order(-rnk.noloop$score),]

