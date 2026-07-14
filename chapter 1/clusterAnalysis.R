rm(list=ls())
set.seed(12345)

library(tidyverse)
library(network)
library(countrycode)
library(networkdata)
library(networkDynamic)
library(pbapply)
library(igraph)
library(ITNr)
library(readxl)

setwd("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange")
source("C:/Users/joanm/OneDrive/Documents/R/self-made functions/Mode.R")

cCOW <- read.csv("COW country codes.csv")
cComtrade <- read_csv("../../Data/Comtrade Country Code and ISO list.csv")
#really weird discrepancy where antigua and barbuda are named the same as antartica (ATA). it should be atg
cComtrade[cComtrade$CountryName=="Antigua and Barbuda", 6] <- "ATG"

#load up all networks
dipExNet <- read_rds("dipExNet.rds")
dipExGraph <- read_rds("dipExGraph.rds")

tradeList <- read_rds("tradeList.rds")
tradeNet <- read_rds("tradeNet.rds")
tradeGraph <- read_rds("tradeGraph.rds")
tradeCutGraph <- read_rds("tradeCutGraph.rds")

aidList <- read_rds("aidList.rds")
aidNet <- read_rds("aidNet.rds")
aidGraph <- read_rds("aidGraph.rds")
aidCutGraph <- read_rds("aidCutGraph.rds")



## Assigning Clusters ##
#COMMUNITY DETECTION
library(blockmodeling)
library(concorR)

#using the Palla et al method of combining graphs across two years

# helper function
as.data.frame.igraph= function(g) {
  # prepare data frame
  res= cbind(as.data.frame(get.edgelist(g)),
             asDF(g)$edges)[ , c(-3, -4)]
  # unfactorize
  res$V1= as.character(res$V1)
  res$V2= as.character(res$V2)
  # return df
  res
}

for (i in 1:length(aidGraph)) {}
df.g1 <- as.data.frame(aidGraph[[i]])
df.g2 <- as.data.frame(aidGraph[[i+1]])
df <- bind_rows(df.g1, df.g2) %>%
  group_by(V1,V2) %>%
  summarise(weight=sum(Value))

jointGraph <- simplify(graph.data.frame(df, directed=T))
E(jointGraph)$weight <- df$weight
E(jointGraph)$label <- E(jointGraph)$weight

jointMem <- membership(cluster_walktrap(jointGraph))
tMem <- membership(cluster_walktrap(aidGraph[[i]]))
t1Mem <- membership(cluster_walktrap(aidGraph[[i+1]]))

#detecing which nodes in Di and Ej are in Vk
#split the membership into groups
split(jointMem)


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
#modularity for concor is terribly for year 59

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
aidWalk <- lapply(aidCutGraph, cluster_walktrap)
aidClust <- lapply(aidWalk,membership)


#Including NA clusters
#AID
aidTabWalk <- list()
for (i in 1:length(aidWalk)) {
  tab <- data.frame(country = aidWalk[[i]]$names,
                    year = names(aidWalk)[i],
                    cluster = aidWalk[[i]]$membership
  )
  
  aidTabWalk[[names(aidWalk)[i]]] <- tab
}

aidTabDeg <- list()
for (i in 1:length(aidDeg)) {
  tab <- data.frame(country = names(aidDeg[[i]]),
                    year = names(aidDeg)[i],
                    cent = as.numeric(aidDeg[[i]])
  )
  
  aidTabDeg[[names(aidDeg)[i]]] <- tab
}

aidTab <- list()
#merging cluster and centrality
for (i in 1:length(aidTabWalk)) {
  aidTab[[names(aidTabWalk)[i]]]  <- left_join(aidTabDeg[[i]], aidTabWalk[[i]],
                                               by = c("country", "year"))
  
}

#assign countries with NA for cluster to mode cluster of top 5 aid donors
for (i in 1:length(aidTab)) {
  yearTemp <- names(aidTab)[i]
  aidNATemp <- aidTab[[i]]$country[is.na(aidTab[[i]][,4])]
  tab <- aidList[[yearTemp]]
  for (j in 1:length(aidNATemp)) {
    countryTemp <- aidNATemp[j]
    tab1 <- tab %>% filter(Sender==countryTemp | Receiver==countryTemp) %>%
      mutate(partner=case_when(Sender!= countryTemp ~ Sender,
                               Receiver != countryTemp ~ Receiver)) %>%
      select(partner, Value) %>%
      group_by(partner) %>% summarize(valueTot=sum(Value))
    tab1 <- left_join(tab1, select(aidTab[[yearTemp]], country, cluster), by=c("partner" = "country"))
    tab2 <- tab1 %>% summarize(cluster=weighted_mode(cluster, valueTot, na.rm=T))
    
    #if multimodal, choose the country with more influence
    if (nrow(tab2)!=1) {
      tab2 <- left_join(tab1, select(aidTab[[yearTemp]], country, cent, cluster), by=c("partner"="country", "cluster")) %>%
        summarize(cluster=weighted_mode(cluster, cent, na.rm=T))
      
    }
    
    if (nrow(tab2)!=1) {
      aidTab[[yearTemp]]$cluster[aidTab[[yearTemp]]$country==countryTemp] <- NA
    } else {
      aidTab[[yearTemp]]$cluster[aidTab[[yearTemp]]$country==countryTemp] <- as.numeric(tab2)
    }
    
  }
}

#do process again for any countries that might have been missed (coz partner was also NA)
for (i in 1:length(aidTab)) {
  yearTemp <- names(aidTab)[i]
  aidNATemp <- aidTab[[i]]$country[is.na(aidTab[[i]][,4])]
  tab <- aidList[[yearTemp]]
  if (length(aidNATemp)>0) {
    for(j in 1:length(aidNATemp)) {
      countryTemp <- aidNATemp[j]
      tab3 <- tab %>% filter(Sender==countryTemp | Receiver==countryTemp) %>%
        mutate(partner=case_when(Sender!= countryTemp ~ Sender,
                                 Receiver != countryTemp ~ Receiver)) %>%
        select(partner, Value) %>%
        group_by(partner) %>% summarize(valueTot=sum(Value))
      tab3 <- left_join(tab3, aidTab[[yearTemp]], by=c("partner"="country")) %>%
        summarize(cluster=weighted_mode(cluster,valueTot, na.rm=T))
      
      aidTab[[yearTemp]]$cluster[aidTab[[yearTemp]]$country==countryTemp] <- as.numeric(tab3)
    }
  }
}

#anymore NA cluster countries?
sum(unlist(lapply(lapply(aidTab, is.na), sum)))
#nope

aidCL <- bind_rows(aidTab) %>% rename(cc=country)
aidCL <- aidCL %>% mutate(country= cComtrade$CountryName[match(aidCL$cc, cComtrade$ISO3)])
# write.csv(aidCL, "aidCL.csv")

#FOR TRADE
tradeTabWalk <- list()
for (i in 1:length(tradeWalk)) {
  tab <- data.frame(country = tradeWalk[[i]]$names,
                    year = names(tradeWalk)[i],
                    cluster = tradeWalk[[i]]$membership
  )
  
  tradeTabWalk[[names(tradeWalk)[i]]] <- tab
}

tradeTabBet <- list()
for (i in 1:length(tradeBet)) {
  tab <- data.frame(country = names(tradeBet[[i]]),
                    year = names(tradeBet)[i],
                    cent = as.numeric(tradeBet[[i]])
  )
  
  tradeTabBet[[names(tradeBet)[i]]] <- tab
}

tradeTab <- list()
#merging cluster and centrality
for (i in 1:length(tradeTabWalk)) {
  tradeTab[[names(tradeTabWalk)[i]]]  <- left_join(tradeTabBet[[i]], tradeTabWalk[[i]],
                                                   by = c("country", "year"))
  
}

#assign countries with NA for cluster to mode cluster of top 5 trade donors
for (i in 1:length(tradeTab)) {
  yearTemp <- names(tradeTab)[i]
  tradeNATemp <- tradeTab[[i]]$country[is.na(tradeTab[[i]][,4])]
  tab <- tradeList[[yearTemp]]
  for (j in 1:length(tradeNATemp)) {
    countryTemp <- tradeNATemp[j]
    tab1 <- tab %>% filter(Sender==countryTemp | Receiver==countryTemp) %>%
      mutate(partner=case_when(Sender!= countryTemp ~ Sender,
                               Receiver != countryTemp ~ Receiver)) %>%
      select(partner, TradeValue) %>%
      group_by(partner) %>% summarize(valueTot=sum(TradeValue))
    tab1 <- left_join(tab1, select(tradeTab[[yearTemp]], country, cluster), by=c("partner" = "country"))
    tab2 <- tab1 %>% summarize(cluster=weighted_mode(cluster, valueTot, na.rm=T))
    
    #if multimodal, choose the country with more influence
    if (nrow(tab2)!=1) {
      tab2 <- left_join(tab1, select(tradeTab[[yearTemp]], country, cent, cluster), by=c("partner"="country", "cluster")) %>%
        summarize(cluster=weighted_mode(cluster, cent, na.rm=T))
      
    }
    
    if (nrow(tab2)!=1) {
      tradeTab[[yearTemp]]$cluster[tradeTab[[yearTemp]]$country==countryTemp] <- NA
    } else {
      tradeTab[[yearTemp]]$cluster[tradeTab[[yearTemp]]$country==countryTemp] <- as.numeric(tab2)
    }
    
  }
}

#do process again for any countries that might have been missed (coz partner was also NA)
while (sum(unlist(lapply(lapply(tradeTab, is.na), sum)))>0) {
  for (i in 1:length(tradeTab)) {
    yearTemp <- names(tradeTab)[i]
    tradeNATemp <- tradeTab[[i]]$country[is.na(tradeTab[[i]][,4])]
    tab <- tradeList[[yearTemp]]
    if (length(tradeNATemp)>0) {
      for(j in 1:length(tradeNATemp)) {
        countryTemp <- tradeNATemp[j]
        tab3 <- tab %>% filter(Sender==countryTemp | Receiver==countryTemp) %>%
          mutate(partner=case_when(Sender!= countryTemp ~ Sender,
                                   Receiver != countryTemp ~ Receiver)) %>%
          select(partner, TradeValue) %>%
          group_by(partner) %>% summarize(valueTot=sum(TradeValue))
        tab3 <- left_join(tab3, tradeTab[[yearTemp]], by=c("partner"="country")) %>%
          summarize(cluster=weighted_mode(cluster, valueTot, na.rm=T))
        
        tradeTab[[yearTemp]]$cluster[tradeTab[[yearTemp]]$country==countryTemp] <- as.numeric(tab3)
      }
    }
  }}

#anymore NA cluster countries?
sum(unlist(lapply(lapply(tradeTab, is.na), sum)))
#nope

tradeCL <- bind_rows(tradeTab) %>% rename(cc=country) 
tradeCL <- tradeCL %>% mutate(country= cComtrade$CountryName[match(tradeCL$cc, cComtrade$ISO3)])
# write.csv(tradeCL, "tradeCL.csv")

### SIMPLIFYING CLUSTERS ###
# if cap max no of clusters at 4
# identify 5 and up  gorups in terms of size (decreasing)
# for every country, loop to find country with modal ties
# do by trying countries in cluster 1 first then cluster 2 etc.


maxClust <- 3
#the smallest number of clusters in a year is 3

#aid
#for each year, take all countries that are in clusters greater than or equal to maxClust

for (i in 1:length(aidTab)) {
  yearTemp <- names(aidTab)[i]
  temp <- aidTab[[i]]
  temp$cluster[temp$cluster>maxClust] <- NA
  aidTab[[i]] <- temp
}

#replace NAs using process used when generating clusters in the first place  
while (sum(unlist(lapply(lapply(aidTab, is.na), sum)))>0) {
  for (i in 1:length(aidTab)) {
    yearTemp <- names(aidTab)[i]
    aidNATemp <- aidTab[[i]]$country[is.na(aidTab[[i]][,4])]
    tab <- aidList[[yearTemp]]
    if (length(aidNATemp)>0) {
      for(j in 1:length(aidNATemp)) {
        countryTemp <- aidNATemp[j]
        tab3 <- tab %>% filter(Sender==countryTemp | Receiver==countryTemp) %>%
          mutate(partner=case_when(Sender!= countryTemp ~ Sender,
                                   Receiver != countryTemp ~ Receiver)) %>%
          select(partner, Value) %>%
          group_by(partner) %>% summarize(valueTot=sum(Value))
        tab3 <- left_join(tab3, aidTab[[yearTemp]], by=c("partner"="country")) %>%
          summarize(cluster=weighted_mode(cluster,valueTot, na.rm=T))
        
        aidTab[[yearTemp]]$cluster[aidTab[[yearTemp]]$country==countryTemp] <- as.numeric(tab3)
      }
    }
    print(yearTemp) }
  
}

#anymore NA cluster countries?
sum(unlist(lapply(lapply(aidTab, is.na), sum)))
#in 1962, portugal is its own little cluster with a bunch of recipient countries
#so let's let that be an exceptino to the max no. of clusters adn give it its own cluster ID
aidTab[["1962"]]$cluster[is.na(aidTab[["1962"]]$cluster)] <- 4
#now no more NAs

aidCL3 <- bind_rows(aidTab) %>% rename(cc=country)
aidCL3 <- aidCL3 %>% mutate(country= cComtrade$CountryName[match(aidCL2$cc, cComtrade$ISO3)])
write.csv(aidCL3, "aidCL3.csv")

#REDO FOR TRADE
maxTradeClust <- tradeCL %>% group_by(year) %>% summarize(max=max(cluster))
maxClust <- 3
#the smallest number of clusters in a year is 2 which is for many years but earlier on i would say 3

#aid
#for each year, take all countries that are in clusters greater than or equal to maxClust

for (i in 1:length(tradeTab)) {
  yearTemp <- names(tradeTab)[i]
  temp <- tradeTab[[i]]
  temp$cluster[temp$cluster>maxClust] <- NA
  tradeTab[[i]] <- temp
}

#replace NAs using process used when generating clusters in the first place  
while (sum(unlist(lapply(lapply(tradeTab, is.na), sum)))>0) {
  for (i in 1:length(tradeTab)) {
    yearTemp <- names(tradeTab)[i]
    tradeNATemp <- tradeTab[[i]]$country[is.na(tradeTab[[i]][,4])]
    tab <- tradeList[[yearTemp]]
    if (length(tradeNATemp)>0) {
      for(j in 1:length(tradeNATemp)) {
        countryTemp <- tradeNATemp[j]
        tab3 <- tab %>% filter(Sender==countryTemp | Receiver==countryTemp) %>%
          mutate(partner=case_when(Sender!= countryTemp ~ Sender,
                                   Receiver != countryTemp ~ Receiver)) %>%
          select(partner, TradeValue) %>%
          group_by(partner) %>% summarize(valueTot=sum(TradeValue))
        tab3 <- left_join(tab3, tradeTab[[yearTemp]], by=c("partner"="country")) %>%
          summarize(cluster=weighted_mode(cluster,valueTot, na.rm=T))
        
        tradeTab[[yearTemp]]$cluster[tradeTab[[yearTemp]]$country==countryTemp] <- as.numeric(tab3)
      }
    }
    print(yearTemp) }
  
}

#anymore NA cluster countries?
sum(unlist(lapply(lapply(aidTab, is.na), sum)))

tradeCL3 <- bind_rows(tradeTab) %>% rename(cc=country) 
tradeCL3 <- tradeCL3 %>% mutate(country= cComtrade$CountryName[match(tradeCL2$cc, cComtrade$ISO3)])
write.csv(tradeCL3, "tradeCL3.csv")


### HARMONIZING CLUSTERS ###
aidTopClust3 <- aidCL3 %>% group_by(year, cluster) %>% slice_max(order_by=cent, n=2)
write.csv(aidTopClust3, "aidTopClust3.csv")

aidCL3 <- read.csv("aidCL3.csv")

# PLOTTING WITH SIMPLIFIED CLUSTERS ##
#aidCL3 (with 3 max clusters)
aidCL3List <- split(aidCL3, aidCL3$year)
aidCutGraph3 <- aidCutGraph
for (i in 1:length(aidCutGraph)) {
  
  aidCutGraph3[[i]] <- aidCutGraph3[[i]] %>%
    set_vertex_attr(., 
                    name = 'cluster3', 
                    index = V(aidCutGraph3[[i]]), 
                    value = sapply(V(aidCutGraph3[[i]])$name, function(x){
                      aidCL3List[[i]] %>%
                        filter(cc == x) %>%
                        .$cluster
                    }))
}


par(mfrow=c(3,4), mar=c(0,0,2,0))
yearsAid <- c(seq(1965,2015, by=5), 2017) #China aid data only goes from 2000 - 2017
for (i in 1:length(yearsAid)) {
  tempGraph <- aidCutGraph3[[as.character(yearsAid[i])]]
  plot.igraph(tempGraph, 
              
              #marks clusters
              #mark.groups=make_clusters(tempGraph, V(tempGraph)$cluster3),
              
              vertex.label=ifelse(names(igraph::strength(tempGraph, weights=E(tempGraph)$Value, mode="out")) %in% 
                                    names(sort(igraph::strength(tempGraph, weights=E(tempGraph)$Value, mode="out"), 
                                               decreasing=TRUE)[1:15])
                                  &igraph::strength(tempGraph, weights=E(tempGraph)$Value, mode="out")!=0,
                                  names(igraph::strength(tempGraph, weights=E(tempGraph)$Value, mode="out")), NA),
              vertex.size=19*rescale(igraph::strength(tempGraph, weights=E(tempGraph)$Value, mode="out")),
              #vertex.label.cex=.85,
              vertex.label.cex=2*scales::rescale(igraph::strength(tempGraph, weights=E(tempGraph)$Value, mode="out")),
              edge.arrow.size=.1,
              edge.width=E(tempGraph)$Value/10^12,
              #layout=layout_with_fr,
              main=as.character(yearsAid[i]),
              margin=c(-0.1,-0.1,-0.1,-0.1))
}
title("Aid Flows For Amounts Above 0.1% of Global Aid Amount, 1965-2017", line=-0.75, outer=TRUE)
