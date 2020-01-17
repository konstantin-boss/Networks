library("igraph")
library("netrankr")
library("magrittr")
library("ggplot2")
library("CINNA")
library("matrixStats")
library("xtable")

# Deleting stuff from the workspace environment and setting random seed
rm(list = ls())
set.seed(1)

# In case you want to read files rather than using preinstalled dataframes
#setwd("C:/Users/Tino/Desktop/Uni/barcelona/Networks/PS1/Medici network")

############################################################################
#####################         Exercise 2      ##############################
############################################################################

# Read in the Medici dataframe from library 'netrankr'
data("florentine_m")

# Assigning the object to a new List
q1 <-florentine_m

# Extracting the relevant attributes from the list
names <-vertex_attr(q1, "name", index = V(q1))

# Computing scores
betweenness <- betweenness(florentine_m)
harmonic<-harmonic_centrality(q1, vids = V(q1))
centrality <-centr_degree(q1, normalized = TRUE)$res

# Creating a table with names and betweenness measure
betw <- data.frame(betweenness, names)
betw1 <- data.frame(betw[order(round(betw[,1], digits=3),decreasing = TRUE),])

# Creating a table with names and harmonic measure
harm <- data.frame(harmonic, names)
harm1 <- data.frame(harm[order(round(harm[,1], digits=3),decreasing = TRUE),])

# Creating a table with names and centrality measure
centr <- data.frame(centrality, names)
centr1 <- data.frame(centr[order(round(centr[,1], digits=3),decreasing = TRUE),])

# Creating a table with all measures combined
ranking <- data.frame(betw1, centr1, harm1)

# Turning the table into a LaTex object
#tab<-xtable(ranking, caption= "Centrality statistics for Florentine network", 
#            align=c("|c","|c","|c","|c","|c","|c","|c|"))

#print(tab,file="medicitab.tex",append=T,table.placement = "h",
#      caption.placement="bottom", hline.after=seq(from=-1,to=nrow(tab),by=1))


# Repeating the degree sequence
deg_seq <- centrality

# Checking if sum of deq_seq is even as we have an undirected graph
if((sum(deg_seq) %% 2) == 0) {
  print(paste("The sum of degrees in the sequence is Even"))
}

# Simulating 1000 random networks given the sequence of nodes from the original graph
d = array(dim = c(1000,vcount(q1)))
c = array(dim = c(1000,vcount(q1)))
for (i in 1:1000) {
  random_network=sample_degseq(deg_seq)
  random_network=simplify(random_network,remove.multiple = FALSE, remove.loops = FALSE,)
  #d[i,]=degree(random_network)
  c[i,]=harmonic-round(harmonic_centrality(random_network, vids = V(random_network)),digits = 3)
}

# Compute the average centrality over the 1000 iterations and the difference to the original
harm_means <- colMeans(c)


# Obtain quantiles for the empirical distribution
probs <- c(0.25, 0.75)
quantiles <- colQuantiles(c, probs = probs)

# Plotting the differences (first ugly, if I have time nicer)
par(mfrow=c(1,1))
ylim = range(quantiles)
plot(harm_means,main="Medici Harmonic Centrality", xlab = "Nodes (Medici=9) ", ylab = "diffs", t="l", col="black",lty=1, ylim = range(quantiles))
lines(seq(length(harm_means)), col="blue",quantiles[,1], lty=2) 
lines(seq(length(harm_means)), col="red", quantiles[,2],lty=2)  
abline(h =0,lty="dotted")
legend("topleft", legend=c("Difference", "25 quantile", "75 quantile"), cex = 0.5,lwd=c(1,2,1), col=c("black","blue","red"), lty=c(2,1,2))

############################################################################
#####################         Exercise 3      ##############################
############################################################################

# Change the working directory
setwd("C:/Users/Tino/Desktop/Uni/barcelona/Networks/PS1/Data/Villages/CSVs")

# Read in the CSVs
visitcome <- lapply(Sys.glob("adj_visitcome_vilno_*.csv"), read.csv, header = FALSE)
visitgo <- lapply(Sys.glob("adj_visitgo_vilno_*.csv"), read.csv,header = FALSE)
borrowmoney <- lapply(Sys.glob("adj_borrowmoney_vilno_*.csv"), read.csv, header = FALSE)
lendmoney <- lapply(Sys.glob("adj_lendmoney_vilno_*.csv"), read.csv, header = FALSE)

# Turn into matrices (not sure if this is double work) 
visitcome1 <-list()
visitgo1 <-list()
borrow1 <-list()
lend1 <-list()
visits <- list()
money <- list()

for (i in 1:9){
  visitcome1[[i]]<-data.matrix(visitcome[[i]])
  visitgo1[[i]]<-data.matrix(visitgo[[i]])
  borrow1[[i]]<-data.matrix(borrowmoney[[i]])
  lend1[[i]]<-data.matrix(lendmoney[[i]])
  
  # Creating the intersections where entry=1 iff both are 1 (will leave entries as 0,1,2 which I correct later)
  visits[[i]] <- visitcome1[[i]]+visitgo1[[i]]
  money[[i]] <- borrow1[[i]] + lend1[[i]]
}

# Now correcting the matrix back to ones and zeros,luckily dimensions for visits and money coincide
for (j in 1:9){
  for (i in 1:nrow(visits[[j]])){
    for (k in 1:ncol(visits[[j]])){
      if (visits[[j]][i,k]>=1){
        visits[[j]][i,k] = visits[[j]][i,k] - 1
      }
      if (money[[j]][i,k]>=1){
        money[[j]][i,k] = money[[j]][i,k] - 1
      }
    }
  }
}

# Turn the adjacency matrices into igraph objects and create Fruchterman-Reingold layouts
visit_graphs <- list()
money_graphs <- list()
visit_layouts <- list()
money_layouts <- list()

for (i in 1:9){
  visit_graphs[[i]] <- graph_from_adjacency_matrix(visits[[i]], mode = "undirected")
  money_graphs[[i]] <- graph_from_adjacency_matrix(money[[i]], mode = "undirected")
  visit_layouts[[i]] <- layout_with_fr(visit_graphs[[i]])
  money_layouts[[i]] <- layout_with_fr(money_graphs[[i]])
}

# Plotting the two 3x3 grids, first for the visits data
par(mfrow=c(3,3))
par(mar = c(2, 2, 2, 2))
for (i in 1:9) { 

  # Plot networks
  plot(visit_graphs[[i]], layout=visit_layouts[[i]], vertex.size=5, edge.arrow.size=0.2,
       rescale=FALSE,
       vertex.label=NA,xlim=range(visit_layouts[[i]][,1]), ylim=range(visit_layouts[[i]][,2]),
       vertex.label.color="red",main = paste("Visits village", i))
}

# Now for the money data
par(mfrow=c(3,3))
par(mar = c(2, 2, 2, 2))

for (i in 1:9) { 
  
  # Plot networks
  plot(money_graphs[[i]], layout=money_layouts[[i]], vertex.size=5, edge.arrow.size=0.2,
       rescale=FALSE,
       xlim=range(money_layouts[[i]][,1]), ylim=range(money_layouts[[i]][,2]),vertex.label=NA,
       vertex.label.color="red",main = paste("Money transfers village", i))
}

# Computing descriptive network statistics, first for visits
for (i in 1:9){
  Degree = degree_distribution(visit_graphs[[i]])
  IndCluster = component_distribution(visit_graphs[[i]])
  Betweenness = betweenness(visit_graphs[[i]])
  EigenvalCent = eigen_centrality(visit_graphs[[i]])
  GlobalClust = transitivity(visit_graphs[[i]], type = "global")
  Diameter = diameter(visit_graphs[[i]], directed = FALSE, unconnected = TRUE, weights = NULL)
  assign(paste0("Descr_stat_visit_", i), list(DegreeDistribution = Degree, IndCluster=IndCluster, Betweenness=Betweenness, EigenvalCent=EigenvalCent,GlobalClust=GlobalClust,Diameter=Diameter))
}

# Now for money
for (i in 1:9){
  Degree = degree_distribution(money_graphs[[i]])
  IndCluster = component_distribution(money_graphs[[i]])
  Betweenness = betweenness(money_graphs[[i]])
  EigenvalCent = eigen_centrality(money_graphs[[i]])
  GlobalClust = transitivity(money_graphs[[i]], type = "global")
  Diameter = diameter(money_graphs[[i]], directed = FALSE, unconnected = TRUE, weights = NULL)
  assign(paste0("Descr_stat_money_", i), list(DegreeDistribution = Degree, IndCluster=IndCluster, Betweenness=Betweenness, EigenvalCent=EigenvalCent,GlobalClust=GlobalClust,Diameter=Diameter))
}


# The Bonacich fails due to singularity of the adjacency matrix,
# except for the case of the 8th village in the visits networks so I report this by hand
Bonacich_visit8 = power_centrality(visit_graphs[[8]],tol = 1e-7, nodes = V(visit_graphs[[8]]))











