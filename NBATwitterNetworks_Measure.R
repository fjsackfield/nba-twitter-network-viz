### File: NBATwitterNetworks_Measure.R
### Author: Fred Sackfield
### Description: This file contains R code that measures the graphs created in NBATwitterNetworks_Setup.

library(igraph)
source("NBATwitterNetworks_Setup.R")


################## NETWORK 1 

g.following <- create_graph_following()

## Network Measurement

# 1) average path length
average.path.length(g.following)

# 2) degree distribution
hist(degree(g.following), col="lightblue")

# 3) degree centrality
degree(g.following) / (vcount(g.following) - 1)

# 4) closeness centrality
closeness(g.following, normalized=T)

# 5) betweenness centrality
betweenness(g.following, normalized=T)

# 6) cliques
maximal.cliques(g.following, min=3)

# 7) the size of the largest clique
clique.number(g.following)

# 8) average clustering coeff
mean(transitivity(g.following, "local", vids=c(1:5)))

# 9) overall clustering coeff
transitivity(g.following)




################## NETWORK 2

g.interactions <- create_graph_interactions()[[1]]

## Network Measurement

# 1) average path length
average.path.length(g.interactions)

# 2) degree distribution
hist(degree(g.interactions), col="lightblue")

# 3) degree centrality
degree(g.interactions) / (vcount(g.interactions) - 1)

# 4) closeness centrality
closeness(g.interactions, normalized=T)

# 5) betweenness centrality
betweenness(g.interactions, normalized=T)

###****IDEA: Rank players by total # of tweets (from historical_tweets) as well as betweenness centrality, look at differences

# 6) cliques
maximal.cliques(g.interactions, min=3)

# 7) the size of the largest clique
clique.number(g.interactions)

# 8) average clustering coeff
mean(transitivity(g.interactions, "local", vids=c(1:5)))

# 9) overall clustering coeff
transitivity(g.interactions)




