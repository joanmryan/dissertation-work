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

#loading mode function
source("C:/Users/joanm/OneDrive/Documents/R/self-made functions/Mode.R")

setwd("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange")

cCOW <- read.csv("COW country codes.csv")
cComtrade <- read_csv("../../Data/Comtrade Country Code and ISO list.csv")
#really weird discrepancy where antigua and barbuda are named the same as antartica (ATA). it should be atg
cComtrade[cComtrade$CountryName=="Antigua and Barbuda", 6] <- "ATG"


#retrieve countrycode conversion table
ccode <- countrycode::codelist


############################
# DIPLOMATIC EXCHANGE DATA #
############################

#redoing dipEx data by making it have different levels now--just make it binary based on level 4, 5 or 6 (ambassador or minister)
dipEx <- read.csv("../../Data/Diplometrics/dipExAll.csv") %>% 
  rename(Guest=Sending.Country,
         Host=Destination) %>%
  mutate(Guest=case_when(Guest=="Micronesia" ~ "Federated States of Micronesia",
                         TRUE ~ Guest),
         Host=case_when(Host=="Micronesia" ~ "Federated States of Micronesia",
                        Host=="Tanganyika" ~ "Tanzania",
                        TRUE ~ Host))

dipEx <- dipEx %>% mutate(Receiver=countrycode(dipEx$Host, "country.name", "iso3c", nomatch=NULL),
         Sender=countrycode(dipEx$Guest, "country.name", "iso3c", nomatch=NULL)) 

table(dipEx$Embassy) #make 4, 5, 6 = 1 and the rest 0. indicating whether it's a strong diplomatic tie or not
dipEx <- dipEx %>% mutate(Ex=(case_when(Embassy >= 4 ~ 1,
                             TRUE ~ 0)))

#three countries not listed in iso3c
dipEx$Receiver[which(dipEx$Receiver=="German Democratic Republic")] <- "DDR" #according to comtrade
dipEx$Sender[which(dipEx$Sender=="German Democratic Republic")] <- "DDR"

dipEx$Receiver[which(dipEx$Receiver=="Yemen People's Republic")] <- "YMD" #according to comtrade
dipEx$Sender[which(dipEx$Sender=="Yemen People's Republic")] <- "YMD"
dipEx$Receiver[which(dipEx$Receiver=="Yemen, People's Democratic Republic of")] <- "YMD"
dipEx$Sender[which(dipEx$Sender=="Yemen, People's Democratic Republic of")] <- "YMD"
dipEx$Receiver[which(dipEx$Receiver=="Kosovo")] <- "KOS"
dipEx$Sender[which(dipEx$Sender=="Kosovo")] <- "KOS"
dipEx$Receiver[which(dipEx$Receiver=="Czechoslovakia")] <- "CSK"
dipEx$Sender[which(dipEx$Sender=="Czechoslovakia")] <- "CSK"
dipEx$Receiver[which(dipEx$Receiver=="Serbia and Montenegro")] <- "YUG"
dipEx$Sender[which(dipEx$Sender=="Serbia and Montenegro")] <- "YUG"
dipEx$Receiver[which(dipEx$Receiver=="Yemen Arab Republic")] <- "YEM"
dipEx$Sender[which(dipEx$Sender=="Yemen Arab Republic")] <- "YEM"
dipEx$Receiver[which(dipEx$Receiver=="Yugoslavia")] <- "YUG"
dipEx$Sender[which(dipEx$Sender=="Yugoslavia")] <- "YUG"

dipEx <- dipEx %>% select(Sender, Receiver, Ex, Year)

dipExEdgeList <- dipEx %>% filter(Ex!=0)
dipExEdgeList  <- split(dipEx, dipEx$Year)

dipExList <- split(dipEx, dipEx$Year)
dipExList <- lapply(dipExList, function(x)x[,-4])

which <- c()
for (i in 1:length(dipExList)) {
which <- which(dipExList[[i]][,3]>1) }

dipExGraph456 <- list()
for (i in 1:length(dipExList)) {
  dipExGraph456[[i]] <- graph.data.frame(d=dipExEdgeList[[i]], 
                   vertices=unique(unlist(dipExList[[i]][,1:2])))
  print(paste(names(dipExList)[i], "done"))
}

names(dipExGraph456) <- names(dipExList)

saveRDS(dipExGraph456, "dipExGraph456.rds")


########################################

dipExOrig <- read.csv("../../Data/Diplometrics/Diplometrics_Diplomatic_Exchange.csv") #dipExBinary1960-2005.csv
dipEx <- dipExOrig %>% mutate(Guest=case_when(Guest=="Micronesia" ~ "Federated States of Micronesia",
                                 Guest=="Tanganyika" ~ "Tanzania",
                                 TRUE ~ Guest),
                 Host=case_when(Host=="Micronesia" ~ "Federated States of Micronesia",
                                Host=="Tanganyika" ~ "Tanzania",
                                TRUE ~ Host))
#changing country names to country codes according to iso3c
dipEx <- dipEx %>% mutate(Receiver=countrycode(dipEx$Host, "country.name", "iso3c", nomatch=NULL),
                          Sender=countrycode(dipEx$Guest, "country.name", "iso3c", nomatch=NULL)) 

#three countries not listed in iso3c
dipEx$Receiver[which(dipEx$Receiver=="German Democratic Republic")] <- "DDR" #according to comtrade
dipEx$Sender[which(dipEx$Sender=="German Democratic Republic")] <- "DDR"

dipEx$Receiver[which(dipEx$Receiver=="Yemen People's Republic")] <- "YMD" #according to comtrade
dipEx$Sender[which(dipEx$Sender=="Yemen People's Republic")] <- "YMD"
dipEx$Receiver[which(dipEx$Receiver=="Yemen, People's Democratic Republic of")] <- "YMD"
dipEx$Sender[which(dipEx$Sender=="Yemen, People's Democratic Republic of")] <- "YMD"

dipEx$Receiver[which(dipEx$Receiver=="Kosovo")] <- "KOS"
dipEx$Sender[which(dipEx$Sender=="Kosovo")] <- "KOS"

dipEx <- dipEx %>% select(Sender, Receiver, Year)

dipExList <- split(dipEx, dipEx$Year)

dipExList <- lapply(dipExList, function(x)x[,-3])

dipExMat <- lapply(dipExList, as.matrix)
dipExNet <- list()

for (i in 1:length(dipExMat)) {
dipExNet[[i]] <- as.network(dipExMat[[i]], directed=TRUE)
print(paste(names(dipExMat)[i], "done"))
}

dipExNet <- lapply(dipExMat, as.network)
#saveRDS(dipExNet, "dipExNet.rds")

#to check which year is problematic
#for (i in 1:length(dipExList)) {
  #print(names(dipExList)[i])
  #dipExNet_i <- as.network(dipExList[[i]], directed=TRUE)
  #saveRDS(dipExNet_i, paste0("dipExNet_year/",i,"dipExNet.rds"))
  #print(paste(names(dipExList)[i], "done"))
#}



list.vertex.attributes(dipExNet[[1]])

plot(dipExNet[[1]], label=network.vertex.names(dipExNet[[1]]))

#as igraph object
dipExGraph <- lapply(dipExMat, igraph::graph_from_edgelist)
#saveRDS(dipExGraph, "dipExGraph.rds")

#as dynamic network
#library(ndtv)
dipExDyn <- networkDynamic(network.list=dipExNet, vertex.pid="vertex.names")
#render.d3movie(dipExDyn, filename="dipExDyn.html")
#saveRDS(dipExDyn, "dipExDyn.rds")



##############
# TRADE DATA #
##############
#cleaning weird issue with countries reporting different flows between each other
tradeorg <- read_tsv("../../Data/comtrade1962-2019.txt") %>% mutate(id=row_number())
trade <- tradeorg %>% select(Year, Flow, Reporter, Partner, TradeValue, id) %>% filter(Reporter!=0 & Partner!=0) %>% 
  mutate(Sender=case_when(Flow==1 ~ cComtrade$ISO3[match(Partner, cComtrade$CountryCode)],
                          Flow==2 ~ cComtrade$ISO3[match(Reporter, cComtrade$CountryCode)]),
         Receiver=case_when(Flow==1 ~ cComtrade$ISO3[match(Reporter, cComtrade$CountryCode)],
                            Flow==2 ~ cComtrade$ISO3[match(Partner, cComtrade$CountryCode)])) %>%
  select(Year, Sender, Receiver, TradeValue, id) %>% filter(Sender!="N/A", Receiver!="N/A", TradeValue>1000) %>% arrange(Year, Sender, Receiver)


trade2 <- trade %>% mutate(Sender2=countrycode(Sender, "iso3c","iso3c", nomatch=NULL),
                           Receiver2=countrycode(Receiver, "iso3c", "iso3c", nomatch=NULL))

#find pairs that have different reported amounts for incoming/outgoing and take average
dup <- trade[duplicated(trade[,c(1,2,3)])|duplicated(trade[,c(1,2,3)], fromLast=TRUE),] %>% arrange(Year, Sender, Receiver)
dup2 <- dup %>% group_by(Year, Sender, Receiver) %>% summarize(TradeValue=round(mean(TradeValue)), id=min(id)) %>% arrange(Year, Sender, Receiver)

idDel <- dup %>% group_by(Year, Sender, Receiver) %>% summarize(TradeValue=round(mean(TradeValue)), id=max(id)) %>% ungroup() %>% select(id)
idKeep <- dup2$id

#replacingn all rows with id in idKeep with dup2
trade[trade$id %in% idKeep,] <- dup2
#delete duplicate rows in idDel
trade <-trade %>% filter(!(id %in% idDel$id), Sender!=Receiver)

# Only pairs of two countries that duplicate -- after taking care of weird ATG discrepancy in cComtrade 
#see how many items there are in eaach 'duplicate'. there should be only 2
#test2 <- test[which(test$num==3),] %>% group_by(Year, Sender, Receiver) #3 came up so i'm seeing how many countires are in groups of 3

#THRESHOLDING: use trade networks that are at least 1% of total global trade value of that year
tradeCut <- trade %>% group_by(Year) %>% mutate(yearTot=sum(TradeValue)) %>% ungroup() %>%
  filter(TradeValue>0.0005*yearTot)


#turing all years into network object
tradeList <- split(trade, trade$Year)
tradeCutList <- split(tradeCut, tradeCut$Year)
#tradeList <- lapply(tradeList, as.matrix)
tradeList <- lapply(tradeList, function(x)x[,c(-1,-5)])
tradeCutList <- lapply(tradeCutList, function(x)x[,c(-1,-5,-6)])
saveRDS(tradeList, "tradeList.rds")
saveRDS(tradeCutList, "tradeCutList.rds")

tradeNet <- pblapply(tradeList, as.network) 
tradeCutNet <- pblapply(tradeCutList, as.network)

#don't really need to use this loop anymore becuase it's very fast now to do lapply as.network for some reason
for (i in 1:length(tradeList)) {
  print(names(tradeList)[i])
  tradeNet_i <- as.network(tradeList[[i]], directed=TRUE)
  saveRDS(tradeNet_i, paste0("tradeNet_year/",i,"tradeNet.rds"))
  print(paste(names(tradeList)[i], "done"))
}

library(sna)
tradeNet <- pblapply(tradeNet, function(x)network::set.vertex.attribute(x, attrname="c_bet", value=sna::betweenness(x, rescale=TRUE)))
get.vertex.attribute(tradeNet[[1]], "vertex.names")

#saveRDS(tradeNet, "tradeNet.rds")
#saveRDS(tradeCutNet, "tradeCutNet.rds")

#using igraph
tradeGraph <- lapply(tradeList, graph_from_data_frame)
tradeCutGraph <- lapply(tradeCutList, graph_from_data_frame)
#saveRDS(tradeGraph, "tradeGraph.rds")
#saveRDS(tradeCutGraph, "tradeCutGraph.rds")

#as dynamic network
tradeDyn <- networkDynamic(network.list=tradeNet, vertex.pid="vertex.names")
saveRDS(tradeDyn, "tradeDyn.rds")
#dynamic network with cutnet
tradeCutDyn <- networkDynamic(network.list=tradeCutNet, vertex.pid="vertex.names")


####################
# FOREIGN AID DATA #
####################
aidorig <- read_csv("../../Data/OECD/OECDaid.csv") %>% filter(AIDTYPE==966)
aid <- aidorig %>% filter(`Amount type`=="Current Prices") %>% select(RECIPIENT, Recipient, DONOR, Donor, Year, Value) %>% mutate(Value=Value*1000000) #all values are in millions

cOECD <- read_csv("../../Data/OECD/OECDdonorRecipientCode.csv")

aid <- aid %>% mutate(Receiver=countrycode(Recipient, "country.name", "iso3c" ),
                      Sender=countrycode(Donor, "country.name", "iso3c"))

#no matching with Kosov, Micronesia (Federated States of Micronesia), Netherlands Antilles for Receiver
#no problem with Receiver except for EU Institutions, Other donor countries
aid$Receiver[which(aid$Recipient=="Kosovo")] <- "KOS"
aid$Receiver[which(aid$Recipient=="Micronesia")] <- "FSM"
aid$Receiver[which(aid$Recipient=="Netherlands Antilles")] <- "ANT"

#which rows have na?
aid.na <- aid %>% filter(is.na(Sender) | is.na(Receiver))

aid <- aid %>% select(Year, Sender, Receiver, Value) %>% filter(!is.na(Sender), !is.na(Receiver))


### NEW: Add AidData's info on China into the matrix
aidChina <- read_excel("../../Data/AidData/AidDatasGlobalChineseDevelopmentFinanceDataset_v2.0.xlsx",
           "Global_CDF2.0")

aidChina <- aidChina %>% select("Financier Country", "Recipient", "Commitment Year", "Flow Class", "Amount (Nominal)") %>%
  filter(`Flow Class`=="ODA-like") %>%
  mutate(Receiver=countrycode(Recipient, "country.name", "iso3c" ),
         Sender=countrycode(`Financier Country`, "country.name", "iso3c")) %>%
  
  rename("Year" = "Commitment Year",
         "Value" = "Amount (Nominal)") %>%
  filter(!is.na(Receiver), !is.na(Value)) %>%
  select(Year, Sender, Receiver, Value)

#China has duplicate values for receiving countries per year. so let's get rid of that
#all projects (rows) are unique so can sum across receiver-year
aidChinaUnique <- aidChina %>% group_by(Year, Sender, Receiver) %>%
  summarize(Value2=sum(Value)) %>%
  rename("Value"="Value2")

#Merging China data into aid data
aidWithChina <- bind_rows(aid, aidChinaUnique)


#let's put a threshold as well
aidCut <- aidWithChina %>% group_by(Year) %>% mutate(yearTot=sum(Value)) %>% ungroup() %>%
  filter(Value>0.001*yearTot)



aidList <- split(aidWithChina, aidWithChina$Year)
aidList <- lapply(aidList, function(x)x[,c(-1)])


aidCutList <- split(aidCut, aidCut$Year)
aidCutList <- lapply(aidCutList, function(x)x[,c(-1,-5)])
saveRDS(aidList, "aidList.rds")
saveRDS(aidCutList, "aidCutList.rds")

aidNet <- pblapply(aidList, as.network)
#saveRDS(aidNet, "aidNet.rds")

aidCutNet <- pblapply(aidCutList, as.network)
#saveRDS(aidCutNet, "aidCutNet.rds")

#as igraph object
aidGraph <- lapply(aidList, graph_from_data_frame)
#saveRDS(aidGraph, "aidGraph.rds")

aidCutGraph <- lapply(aidCutList, graph_from_data_frame)
#saveRDS(aidCutGraph, "aidCutGraph.rds")

#as dynamic network
#aidDyn <- networkDynamic(network.list=aidNet, vertex.pid="vertex.names")
#saveRDS(aidDyn, "aidDyn.rds")

##IMPORTANT NOTE: China aid data only goes to 2017

############
# ANALYSIS #
############
rm(list=ls())

#loading everything
dipExNet <- read_rds("dipExNet.rds")
dipExGraph <- read_rds("dipExGraph.rds")
dipExGraph456 <- read_rds("dipExGraph456.rds")

tradeList <- read_rds("tradeList.rds")
tradeNet <- read_rds("tradeNet.rds")
tradeGraph <- read_rds("tradeGraph.rds")
tradeCutGraph <- read_rds("tradeCutGraph.rds")

aidList <- read_rds("aidList.rds")
aidNet <- read_rds("aidNet.rds")
aidGraph <- read_rds("aidGraph.rds")
aidCutGraph <- read_rds("aidCutGraph.rds")

#CENTRALITY#
#calculate centrality for each network
dipExEig <- list()
for (i in 1:length(dipExGraph)) {
  print(names(dipExGraph)[i])
  dipExEig[[i]] <- scales::rescale(igraph::eigen_centrality(dipExGraph[[i]], directed=TRUE, weights=NA)$vector, to=c(0,100))
    print(paste(names(dipExGraph)[i], "done"))
}
names(dipExEig) <- names(dipExGraph)

dipExKatz<- list()
for (i in 1:length(dipExGraph)) {
  print(names(dipExGraph)[i])
  
  dipExEig <- igraph::eigen_centrality(dipExGraph[[i]], directed=TRUE, weights=NA)
  alpha <- 1/(dipExEig$value+1)
  dipExKatz[[i]] <- alpha_centrality(dipExGraph[[i]], alpha=alpha)/max(alpha_centrality(dipExGraph[[i]]))
  print(paste(names(dipExGraph)[i], "done"))
}
names(dipExKatz) <- names(dipExGraph)

dipExPow<- list()
for (i in 1:length(dipExGraph)) {
  print(names(dipExGraph)[i])
  dipExPow[[i]] <- scales::rescale(igraph::power_centrality(dipExGraph[[i]]), to=c(0,100))
  print(paste(names(dipExGraph)[i], "done"))
}
names(dipExPow) <- names(dipExGraph)

dipExKatz456<- list()
for (i in 1:length(dipExGraph456)) {
  print(names(dipExGraph456)[i])
  
  dipExEig <- igraph::eigen_centrality(dipExGraph456[[i]], directed=TRUE, weights=NA)
  alpha <- 1/(dipExEig$value+1)
  dipExKatz456[[i]] <- alpha_centrality(dipExGraph456[[i]], alpha=alpha)/max(alpha_centrality(dipExGraph456[[i]]))
  print(paste(names(dipExGraph456)[i], "done"))
}
names(dipExKatz456) <- names(dipExGraph456)


tradeBet <- list()
#calculating betweenness centrality for trade
for (i in 1:length(tradeGraph)) {
  print(names(tradeGraph)[i])
  tradeBet[[i]] <- betweenness(tradeGraph[[i]], weights=1/(E(tradeGraph[[i]])$TradeValue))/max(betweenness(tradeGraph[[i]], weights=1/(E(tradeGraph[[i]])$TradeValue)))
  print(paste(names(tradeGraph)[i], "done"))
}
names(tradeBet) <- names(tradeGraph)

aidOutDeg <- list() #i think i'll use degree centrality (weighted) for aid because the more money you send out, the more powerful you are
for (i in 1:length(aidCutGraph)) {
  print(names(aidCutGraph)[i])
  aidOutDeg[[i]] <- igraph::strength(aidCutGraph[[i]], weights=E(aidCutGraph[[i]])$Value, mode="out")/max(igraph::strength(aidCutGraph[[i]], weights=E(aidCutGraph[[i]])$Value, mode="out"))
  print(paste(names(aidCutGraph)[i], "done"))
}
names(aidOutDeg) <- names(aidCutGraph)

aidInDeg <- list()  #in-degree
for (i in 1:length(aidCutGraph)) {
  print(names(aidCutGraph)[i])
  aidInDeg[[i]] <- igraph::strength(aidCutGraph[[i]], weights=E(aidCutGraph[[i]])$Value, mode="in")/max(igraph::strength(aidCutGraph[[i]], weights=E(aidCutGraph[[i]])$Value, mode="in"))
  print(paste(names(aidCutGraph)[i], "done"))
}
names(aidInDeg) <- names(aidCutGraph)

saveRDS(dipExKatz, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/dipExKatz.rds")
saveRDS(dipExKatz456, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/dipExKatz456.rds")
saveRDS(tradeBet, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/tradeBet.rds")
saveRDS(aidOutDeg, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/aidOutDeg.rds")
saveRDS(aidInDeg, "C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/aidInDeg.rds")

dipExEigTab <- matrix(nrow=15,ncol=length(dipExEig))
for (i in 1:length(dipExEig)) {
dipExEigTab[,i] <- names(sort(dipExEig[[i]], decreasing=TRUE)[1:15]) 
}
colnames(dipExEigTab) <- names(dipExEig)

dipExKatzTab <- matrix(nrow=15,ncol=2*length(dipExKatz))
colnames(dipExKatzTab) <- rep(names(dipExKatz), each=2)
for (i in 1:length(dipExKatz)) {
  dipExKatzTab[,2*i-1] <- names(sort(dipExKatz[[i]], decreasing=TRUE)[1:15]) 
  dipExKatzTab[,2*i] <- sort(dipExKatz[[i]], decreasing=TRUE)[1:15]
}

dipExKatzTab456 <- matrix(nrow=15,ncol=2*length(dipExKatz456))
colnames(dipExKatzTab456) <- rep(names(dipExKatz456), each=2)
for (i in 1:length(dipExKatz456)) {
  dipExKatzTab456[,2*i-1] <- names(sort(dipExKatz456[[i]], decreasing=TRUE)[1:15]) 
  dipExKatzTab456[,2*i] <- sort(dipExKatz456[[i]], decreasing=TRUE)[1:15]
}

dipExPowTab <- matrix(nrow=15,ncol=length(dipExPow))
for (i in 1:length(dipExPow)) {
  dipExPowTab[,i] <- names(sort(dipExPow[[i]], decreasing=TRUE)[1:15]) 
}
colnames(dipExPowTab) <- names(dipExPow)

tradeBetTab <- matrix(nrow=15,ncol=2*length(tradeBet))
for (i in 1:length(tradeBet)) {
  tradeBetTab[,2*i-1] <- names(sort(tradeBet[[i]], decreasing=TRUE)[1:15]) 
  tradeBetTab[,2*i] <- sort(tradeBet[[i]], decreasing=TRUE)[1:15]
}
colnames(tradeBetTab) <- rep(names(tradeBet), each=2)

aidOutDegTab <- matrix(nrow=15,ncol=length(aidOutDeg))
for (i in 1:length(aidOutDeg)) {
  aidOutDegTab[,i] <- names(sort(aidOutDeg[[i]], decreasing=TRUE)[1:15]) 
}
colnames(aidOutDegTab) <- names(aidOutDeg)

aidInDegTab <- matrix(nrow=15,ncol=2*length(aidInDeg))
for (i in 1:length(aidInDeg)) {
  aidInDegTab[,2*i-1] <- names(sort(aidInDeg[[i]], decreasing=TRUE)[1:15])
  aidInDegTab[,2*i] <- sort(aidInDeg[[i]], decreasing=TRUE)[1:15]
}
colnames(aidInDegTab) <- rep(names(aidInDeg), each=2)

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