### File: NBATwitterNetworks_Vis.R
### Author: Fred Sackfield
### Description: This file contains R code that visualizes the graphs created in NBATwitterNetworks_Setup.


########TEST EXAMPLE#########

library(igraphdata)
data(karate)
karate_groups <- cluster_optimal(karate)
coords <- layout_in_circle(karate, order =
                             order(membership(karate_groups)))
V(karate)$label <- sub("Actor ", "", V(karate)$name)
V(karate)$label.color <- membership(karate_groups)
V(karate)$shape <- "none"
plot(karate, layout = coords)

#######################


library(igraph)
source("NBATwitterNetworks_Setup.R")


##### NETWORK 1 

g.following <- create_graph_following()
g.teams <- create_graph_team_following()


##Team-based network
plot(simplify(g.teams), 
     layout=layout.circle, 
     vertex.label=V(g.teams)$team, 
     #vertex.label=NA,
     vertex.size=10,
     vertex.label.cex = 1,
     vertex.label.dist=1.2,
     edge.arrow.size=0,
     #edge.width=(get.edge.attribute(g.teams)[[1]]^2)/100,
     edge.width=(E(g.teams)$weight^2)/100,
     vertex.label.color=V(g.teams)$color,
     vertex.frame.color=V(g.teams)$border
     #mark.groups=v1_teams,
)

tkplot(g.teams, 
     layout=layout.circle, 
     vertex.label=V(g.teams)$team, 
     #vertex.label=NA,
     vertex.size=10,
     vertex.label.cex = 1,
     vertex.label.dist=1.2,
     edge.width=get.edge.attribute(g.teams)[[1]],
     vertex.frame.color=V(g.teams)$border
     #mark.groups=v1_teams,
)

##full network
par(mfrow=c(1,1))
plot(g.following, 
     layout=layout.circle, 
     vertex.label=V(g.following)$fullname, 
     #vertex.label=NA,
     vertex.size=10,
     vertex.label.cex = 1,
     vertex.label.dist=1.2,
     edge.width=0.001
     #vertex.frame.color=V(g.interactions)$border,
     #mark.groups=v1_teams,
     )

tkplot(g.following, 
       layout=layout.circle, 
       vertex.label=V(g.following)$fullname, 
       #vertex.label=NA,
       vertex.size=10,
       vertex.label.cex = 1,
       vertex.label.dist=1.2,
       edge.width=0.001
       #vertex.frame.color=V(g.interactions)$border,
       #mark.groups=v1_teams,)
)




##### NETWORK 2 

g.interactions <- create_graph_interactions()[[2]]

View(as.data.frame(betweenness(g.interactions, normalized=T)))
View(as.data.frame(degree(g.interactions)))

plot(g.interactions, 
     layout=layout.fruchterman.reingold(g.interactions)*10, 
     vertex.label=V(g.interactions)$fullname, 
     vertex.size=5+(degree(g.interactions)/5),
     vertex.frame.color=V(g.interactions)$border,
     edge.width=get.edge.attribute(g.interactions)[[1]]/2,
     vertex.label.cex = ifelse(betweenness(g.interactions, normalized=T)>0.05,2,0.75),
     vertex.label.dist=1.2,
     vertex.label.font=1)


tkplot(g.interactions, 
       layout=layout.fruchterman.reingold(g.interactions), 
       vertex.label=V(g.interactions)$fullname, 
       vertex.size=8+(degree(g.interactions)/2),
       vertex.frame.color=V(g.interactions)$border,
       vertex.label.color=V(g.interactions)$color,
       edge.width=get.edge.attribute(g.interactions)[[1]],
       vertex.label.cex = ifelse(betweenness(g.interactions, normalized=T)>0.04,2.4,1),
       vertex.label.dist=1.1,
       vertex.label.font=1)


subg <- subgraph(g.interactions,as.vector(which(betweenness(g.interactions, normalized=T)>0)))
plot(simplify(subg), 
     layout=layout.kamada.kawai,
     vertex.label=V(subg)$fullname, 
     vertex.size=5+(degree(subg)/5),
     vertex.frame.color=V(subg)$border,
     edge.width=get.edge.attribute(subg)[[1]],
     #vertex.label.cex = ifelse(betweenness(subg, normalized=T)>0.1,2,0.75),
     #vertex.label.cex = 0.5+(degree(subg)/10),
     vertex.label.cex = 1,
     vertex.label.dist=1.2,
     vertex.label.font=1)

tkplot(subg, 
       layout=layout.fruchterman.reingold,
       vertex.label=V(subg)$fullname, 
       vertex.size=7+(degree(subg)),
       vertex.frame.color=V(subg)$border,
       edge.width=get.edge.attribute(subg)[[1]],
       edge.arrow.size=.2,
       vertex.label.color=V(subg)$color,
       vertex.label.cex = 1+(degree(subg) / (vcount(subg) - 1)),
       vertex.label.dist=1.05,
       vertex.label.font=1,
       curved=list(TRUE))

