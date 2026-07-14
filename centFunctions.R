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

aidDegFn <- function(aidGraph, mode) {
  aidDeg <- list() #i think i'll use degree centrality (weighted) for aid because the more money you send out, the more powerful you are
  for (i in 1:length(aidGraph)) {
    print(names(aidGraph)[i])
    aidDeg[[i]] <- igraph::strength(aidGraph[[i]], weights=E(aidGraph[[i]])$Value, mode=mode)/sum(E(aidGraph[[i]])$Value) #normalize by total aid flows of the year
    print(paste(names(aidGraph)[i], "done"))
  }
  names(aidDeg) <- names(aidGraph)
  return(aidDeg)
}


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

tabValFn <- function(centList) {
  centValTab <- matrix(nrow=10 ,ncol=2*length(centList))
  colnames(centValTab) <- rep(names(centList), each=2)
  for (i in 1:length(centList)) {
    centValTab[,2*i-1] <- names(sort(centList[[i]], decreasing=TRUE)[1:15]) 
    centValTab[,2*i] <- sort(centList[[i]], decreasing=TRUE)[1:15]
  }
  return(centValTab)
}