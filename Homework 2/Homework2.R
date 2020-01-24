### Homework 2 of Konstantin Boss ###

library("igraph")
library("igraphdata")
library("qgraph")
library("Cairo")


# Deleting stuff from the workspace environment and setting random seed
rm(list = ls())
set.seed(1)

# Loading graph dataset for the Karate Club and turn into igraph object
data("karate")
g <- karate

# Implement Greedy Agglomeration Algorithm
fc <- cluster_fast_greedy(karate, modularity = TRUE)
merges <- fc$merges
modul <- fc$modularity

# Plotting the modularity score as a function of the number of merges and the dendogram
par(mar = c(2, 2, 2, 2))
plot_dendrogram(fc,use.modularity = FALSE)

plot.new()
plot(modul, type ="l", xlab = "Merges", ylab = "Modularity", main = "Karate Network")


# Plotting the network communities colored according to modularity 
greedymembers=lapply(unique(fc$membership), function(x) V(karate)[fc$membership==x])


## qgrpah requires generating the adjacency matrix (or alternatively the edgelist) from igraph object in order to plot 
Matrix=get.adjacency(karate, sparse=T)


## plot karate with the communities given by the algorithm above using qgraph
plot.new()
qgraph(Matrix, diag=F, directed=F, groups=greedymembers, 
       vsize=c(4,4), palette='pastel', rainbowStart=100, legend=F, label.cex = 1.25)
