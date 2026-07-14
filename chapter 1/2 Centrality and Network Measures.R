rm(list=ls())
setwd("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange")
set.seed(54321)

library(tidyverse)
library(network)
library(countrycode)
library(networkdata)
library(networkDynamic)
library(pbapply)
library(igraph)
library(ITNr)
library(readxl)

############
# ANALYSIS #
############
rm(list=ls())

#loading everything
dipGraphs <- read_rds("dipGraphs.rds")
#dip456Graph <- read_rds("dip456Graph.rds")
#dip3456Graph <- read_rds("dip3456Graph.rds")
#dipBinGraph <- read_rds("dipBinGraph.rds")
#dipWeightGraph <- read_rds("dipWeightGraph.rds")

#tradeList <- read_rds("tradeList.rds")
#tradeGraph <- read_rds("tradeGraph.rds")
#tradeCutGraph <- read_rds("tradeCutGraph.rds")
tradeGraphs <- read_rds("tradeGraphs.RDS")
tradeGraphs <- tradeGraphs[names(tradeGraphs) !="tradeCutGraph5.0"] #5% threshold is an empty list

#aidList <- read_rds("aidList.rds")
#aidGraph <- read_rds("aidGraph.rds")
#aidCutGraph <- read_rds("aidCutGraph.rds")
aidGraphs <- read_rds("aidGraphs.RDS")

#CENTRALITY#
#calculate centrality for each network
dipExEigFn <- function(dipExGraph) {
  dipExEig <- list()
  for (i in 1:length(dipExGraph)) {
    print(names(dipExGraph)[i])
    dipExEig[[i]] <- igraph::eigen_centrality(dipExGraph[[i]], directed=FALSE, weights=NULL, scale=FALSE)$vector #uses euclidean norm so it's ndoe centrality
    print(paste(names(dipExGraph)[i], "done"))
  }
  names(dipExEig) <- names(dipExGraph)
  return(dipExEig)
}

dipEig <- lapply(dipGraphs, dipExEigFn)
#dipBinEig <- dipExEigFn(dipBinGraph)
#dip456Eig <- dipExEigFn(dip456Graph)
#dipWeightEig <- dipExEigFn(dipWeightGraph)


dipExKatzFn <- function(dipExGraph) {  
  dipExKatz <- list()
  for (i in 1:length(dipExGraph)) {
    print(names(dipExGraph)[i])
    dipExEig <- igraph::eigen_centrality(dipExGraph[[i]], directed=TRUE, weights=NULL, scale=FALSE)
    alpha <- 1/(dipExEig$value+1)
    #alpha <- 2
    #dipExKatz[[i]] <- alpha_centrality(dipExGraph[[i]], alpha=alpha)/max(alpha_centrality(dipExGraph[[i]]))
    dipExKatz[[i]] <- alpha_centrality(dipExGraph[[i]], alpha=alpha)
    print(paste(names(dipExGraph)[i], "done"))
  }
  names(dipExKatz) <- names(dipExGraph)
  return(dipExKatz)
}


dipKatz <- lapply(dipGraphs, dipExKatzFn)
#dipKatz[["dipBinGraph"]] <- dipExKatzFn(dipBinGraph)
#dip456Katz <- dipExKatzFn(dip456Graph) #singularity issue
#dipWeightKatz <- dipExKatzWeightFn(dipWeightGraph)

dipExPowerFn <- function(dipExGraph) {
  dipExPow<- list()
  for (i in 1:length(dipExGraph)) {
    beta <- 1/(igraph::eigen_centrality(dipExGraph[[i]], directed=TRUE, weights=NULL, scale=FALSE)$value+1) #uses euclidean norm of eigenvector
    print(names(dipExGraph)[i])
    dipExPow[[i]] <- igraph::power_centrality(dipExGraph[[i]], exponent=beta, rescale=FALSE)
    print(paste(names(dipExGraph)[i], "done"))
  }
  names(dipExPow) <- names(dipExGraph)
  return(dipExPow)
}


dipPower <- lapply(dipGraphs, dipExPowerFn)
#dipBinPower <- dipExPowerFn(dipBinGraph)
#dip456Power <- dipExPowerFn(dip456Graph) 
#dipWeightPower <- dipExPowerWeightFn(dipWeightGraph)


dipExClosenessFn <- function(dipExGraph, mode) {
  dipExCloseness<- list()
  for (i in 1:length(dipExGraph)) {
    print(names(dipExGraph)[i])
    dipExCloseness[[i]] <- igraph::harmonic_centrality(dipExGraph[[i]], 
                                                       mode=mode, 
                                                       weights=NA, 
                                                       normalized=TRUE)
    print(paste(names(dipExGraph)[i], "done"))
  }
  names(dipExCloseness) <- names(dipExGraph)
  return(dipExCloseness)
}

dipExClosenessWeightFn <- function(dipExGraph, mode) {
  dipExCloseness<- list()
  for (i in 1:length(dipExGraph)) {
    print(names(dipExGraph)[i])
    dipExCloseness[[i]] <- igraph::harmonic_centrality(dipExGraph[[i]], 
                                                       mode=mode, 
                                                       weights=1/E(dipExGraph[[i]])$Weight,
                                                       normalized=TRUE) #normalized by THEORETICAL maximum. so this IS node centrality
    print(paste(names(dipExGraph)[i], "done"))
  }
  names(dipExCloseness) <- names(dipExGraph)
  return(dipExCloseness)
}

dipClosenessIn <- lapply(dipGraphs, dipExClosenessFn, mode="in")
dipClosenessIn[["dipWeightGraph"]] <- dipExClosenessWeightFn(dipGraphs[["dipGraphWeight"]],
                                                             mode="in")

dipClosenessOut <- lapply(dipGraphs, dipExClosenessFn, mode="out")
dipClosenessOut[["dipWeightGraph"]] <- dipExClosenessWeightFn(dipGraphs[["dipGraphWeight"]],
                                                             mode="out")

dipClosenessAll <- lapply(dipGraphs, dipExClosenessFn, mode="all")
dipClosenessAll[["dipWeightGraph"]] <- dipExClosenessWeightFn(dipGraphs[["dipGraphWeight"]],
                                                             mode="all")

#dipBinClosenessIn <- dipExClosenessFn(dipBinGraph, mode="in")
#dipBinClosenessOut <- dipExClosenessFn(dipBinGraph, mode="out")
#dipBinClosenessAll <- dipExClosenessFn(dipBinGraph, mode="all")
#dip456ClosenessIn <- dipExClosenessFn(dip456Graph, mode="in")
#dip456ClosenessOut <- dipExClosenessFn(dip456Graph, mode="out")
#dip456ClosenessAll <- dipExClosenessFn(dip456Graph, mode="all")
#dipWeightClosenessIn <- dipExClosenessWeightFn(dipWeightGraph, mode="in")
#dipWeightClosenessOut<- dipExClosenessWeightFn(dipWeightGraph, mode="out")
#dipWeightClosenessAll<- dipExClosenessWeightFn(dipWeightGraph, mode="all")

## TRADE ##

tradeBetFn <- function(tradeGraph, directed) {
tradeBet <- list()
#calculating betweenness centrality for trade
for (i in 1:length(tradeGraph)) {
  print(names(tradeGraph)[i])
  tradeBet[[i]] <- betweenness(tradeGraph[[i]], 
                               directed=directed,
                               weights=1/(E(tradeGraph[[i]])$TradeValue),
                               normalized=TRUE) #normalized by max poss value -- node centrality
  print(paste(names(tradeGraph)[i], "done"))
  }
names(tradeBet) <- names(tradeGraph)
return(tradeBet)
  }

#tradeBetDir <- tradeBetFn(tradeGraph, directed=TRUE)``
#tradeCutBetDir <- tradeBetFn(tradeCutGraph, directed=TRUE)
#tradeBet <- tradeBetFn(tradeGraph, directed=FALSE)
#tradeCutBet <- tradeBetFn(tradeCutGraph, directed=FALSE)

tradeBetDir <- lapply(tradeGraphs, tradeBetFn, directed=TRUE)
tradeBetUndir <- lapply(tradeGraphs, tradeBetFn, directed=FALSE)

aidDegFn <- function(aidGraph, mode) {
aidDeg <- list() #i think i'll use degree centrality (weighted) for aid because the more money you send out, the more powerful you are
for (i in 1:length(aidGraph)) {
  print(names(aidGraph)[i])
  aidDeg[[i]] <- igraph::strength(aidGraph[[i]], weights=E(aidGraph[[i]])$Value, mode=mode)/sum(E(aidGraph[[i]])$Weight) #normalize by total aid flows of the year
  print(paste(names(aidGraph)[i], "done"))
}
names(aidDeg) <- names(aidGraph)
return(aidDeg)
}

#aidInDeg <- aidDegFn(aidGraph, mode="in")
#aidOutDeg <- aidDegFn(aidGraph, mode="out")
#aidCutInDeg <- aidDegFn(aidCutGraph, mode="in")
#aidCutOutDeg <- aidDegFn(aidCutGraph, mode="out")

aidInDeg <- lapply(aidGraphs, aidDegFn, mode="in")
aidOutDeg <- lapply(aidGraphs, aidDegFn, mode="out")


aidKatzFn <- function(aidGraph) {
  aidKatz <- list()
  for (i in 1:length(aidGraph)) {
    print(names(aidGraph)[i])
    alpha <- 1/(igraph::eigen_centrality(aidGraph[[i]], directed=TRUE, weights=E(aidGraph[[i]])$Value, scale=FALSE)$value+1) #uses euclidean norm of eigenvector
    #alpha <- 2
    aidKatz[[i]] <- alpha_centrality(aidGraph[[i]], alpha=alpha, weights=E(aidGraph[[i]])$Value)
    print(paste(names(aidGraph)[i], "done"))
  }
  names(aidKatz) <- names(aidGraph)
  return(aidKatz)
}

#aidKatz <- aidKatzFn(aidGraph)
aidKatz <- lapply(aidGraphs, aidKatzFn)

centMeasures <- ls()[!(endsWith(ls(), "Graphs") | endsWith(ls(), "Fn"))]

#save(dipBinEig, dipWeightEig, dip456Eig,
#     dipBinKatz, dipWeightKatz,
#     dipBinPower, dipWeightPower,
#     dipBinClosenessIn, dipBinClosenessOut, dipBinClosenessAll,
#     dip456ClosenessIn, dip456ClosenessOut, dip456ClosenessAll,
#     dipWeightClosenessIn, dipWeightClosenessOut, dipWeightClosenessAll,
#     tradeBet, tradeCutBet,
#     aidInDeg, aidOutDeg,
#     aidCutInDeg, aidCutOutDeg,
#     aidKatz,
#     file="C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/centralityMeasures.rdata")

save(list=centMeasures, file="C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/centMeasures.RDS")

#creating tables
cent <- "aidInDeg0.s."
centTab %>% select(year, cc, cent) %>%
  
  
tabFn <- function(centList) {
  centTab <- matrix(nrow=15,ncol=length(centList))
  for (i in 1:length(centList)) {
    centTab[,i] <- names(sort(centList[[i]], decreasing=TRUE)[1:15]) 
  }
  colnames(centTab) <- names(centList)
  return(centTab)
}

tabValFn <- function(centList) {
  centValTab <- matrix(nrow=15,ncol=2*length(centList))
  colnames(centValTab) <- rep(names(centList), each=2)
  for (i in 1:length(centList)) {
    centValTab[,2*i-1] <- names(sort(centList[[i]], decreasing=TRUE)[1:15]) 
    centValTab[,2*i] <- sort(centList[[i]], decreasing=TRUE)[1:15]
  }
  return(centValTab)
}

tabFn(dipBinKatz)
tabValFn(dipBinKatz)


write.csv(dipExEigTab, "dipExEigTab.csv")
write.csv(dipExKatzTab, "dipExKatzTab.csv")
write.csv(dipExPowTab, "dipExPowTab.csv")
write.csv(dipExKatzTab456, "dipExKatz456Tab.csv")
write.csv(tradeBetTab, "tradeBetTab.csv")
write.csv(aidOutDegTab, "aidOutDegTab.csv")
write.csv(aidInDegTab, "aidInDegTab.csv")

library(tsna)
#tsna apparently lets you calculate the same thing and returns a column per year
tSnaStats(dipExDyn, snafun="evcent", )
tSnaStats(tradeDyn, snafun="betweenness", rescale=TRUE,ignore.eval=FALSE)
#doesn't work because ther eare countries and drop in and out over the years

####################
# CENTRALIZATION ###
####################

centTrade <- list()
#calculating betweenness centraliZATION for trade
for (i in 1:length(tradeCutGraph)) {
  print(names(tradeCutGraph)[i])
  centTrade[[i]] <- centr_betw(tradeCutGraph[[i]])$centralization
  print(paste(names(tradeGraph)[i], "done"))
}
names(centTrade) <- names(tradeGraph)

#####################
#COMMUNITY DETECTION#
#####################
library(blockmodeling)
library(concorR)

#try this for cut trade graph
cluster_walktrap(tradeGraph[[50]])

#for trade:
tradeWalk <- lapply(tradeCutGraph, cluster_walktrap)
plot(tradeCutGraph[[59]], mark.groups=tradeWalk)
membership(tradeWalk)

tradeEB <- cluster_edge_betweenness(tradeCutGraph[[59]])
plot(tradeCutGraph[[59]], mark.groups=tradeEB)


tradeInfo <- lapply(tradeCutGraph, cluster_infomap)
plot(tradeCutGraph[[59]], mark.groups=tradeInfo)
membership(tradeInfo)

concor <- concor(list(as.matrix(get.adjacency(tradeCutGraph[[59]]))), nsplit = 2)
modularity(tradeCutGraph[[59]], membership=concor$block)
#modularity for concor is terrible for year 59

#looks like for trade, walktrap is the way to go
tradeClust <- lapply(tradeWalk,membership)

cluster_walktrap(dipExGraph[[1]])
cluster_edge_betweenness(dipExGraph[[1]])

cluster_infomap(dipExGraph[[50]])
#walktrap best for dipex
dipExWalk <- lapply(dipExGraph, cluster_walktrap)
dipExClust <- lapply(dipExWalk,membership)

cluster_walktrap(aidCutGraph[[59]])
cluster_infomap(aidCutGraph[[59]])
cluster_edge_betweenness(aidCutGraph[[59]])
#walktrap also best for aid
#actually, as of 11/3/2022, spinglass has best modularity
aidWalk <- lapply(aidCutGraph, edge.betweenness)
aidClust <- lapply(aidWalk,membership)


#saving walktrap list objects

saveRDS(tradeClust, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/tradeClust.rds")
saveRDS(dipExClust, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/dipExClust.rds")
saveRDS(aidClust, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/aidClust.rds")


##### IDENTIFYING TRADE DEPENDENCY ######

#do incoming nodes vs outgoing trade matter in terms of who's influential?


#see if the person you're in a cluster with matters for your TFR
#countries to test for aid: UK, US, Japan, France, Germany, Netherlands, Saudie, UAE, China(?only 2000-2017)
#US

#Japan

#France

#Netherlands

#UAE

#Saudie

#China?
