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
#source("C:/Users/joanm/OneDrive/Documents/R/self-made functions/Mode.R")

setwd("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange")

#cCOW <- read_csv("COW country codes.csv")
cComtrade <- read_csv("../../Data/Comtrade Country Code and ISO list.csv")
#really weird discrepancy where antigua and barbuda are named the same as antartica (ATA). it should be atg
cComtrade[cComtrade$CountryName=="Antigua and Barbuda", 6] <- "ATG"


#retrieve countrycode conversion table
ccode <- countrycode::codelist


############################
# DIPLOMATIC EXCHANGE DATA #
############################
dipExAll <- read.csv("../../Data/Diplometrics/dipExAll.csv") %>% na.omit() %>%
  rename(Guest=Sending.Country,
         Host=Destination)

#matching diplometrics names to names in countrycode using iso3c
dipExAll <- dipExAll %>% mutate(Sender=countrycode(dipExAll$Guest, "country.name", "iso3c", nomatch=NULL),
         Receiver=countrycode(dipExAll$Host, "country.name", "iso3c", nomatch=NULL)) 

#three countries not listed in iso3c
dipExAll[dipExAll=="German Democratic Republic"] <- "DDR" #according to comtrade
dipExAll[dipExAll == "Yemen People's Republic" | dipExAll== "Yemen, People's Democratic Republic of"] <- "YMD" #according to comtrade
dipExAll[dipExAll=="Kosovo"] <- "KOS"
dipExAll[dipExAll=="Czechoslovakia"] <- "CSK"
dipExAll[dipExAll=="Serbia and Montenegro"] <- "YUG"
dipExAll[dipExAll=="Yemen Arab Republic"] <- "YEM"
dipExAll[dipExAll=="Yugoslavia"] <- "YUG"

#write.csv(dipExAll[,c("Year", "Sender", "Receiver", "LOR")], "C:/Users/joanm/OneDrive/Documents/Dissertation/data/allDipEx1960-2020.csv")

dipGraphs <- list()
### 1. DIP EX 456
#redoing dipEx data by making it have different levels now--just make it binary based on level 4, 5 or 6 (ambassador or minister)
table(dipExAll$Embassy) #make 4, 5, 6 = 1 and the rest 0. indicating whether it's a strong diplomatic tie or not
dip456 <- dipExAll %>% mutate(Ex=(case_when(Embassy >= 4 ~ 1,
                             TRUE ~ 0)))
dip456 <- dip456 %>% select(Sender, Receiver, Ex, Year)

#as igraph object
dip456Edge <- dip456 %>% filter(Ex!=0) %>% select(-Ex)
dip456Edge  <- split(dip456Edge, dip456Edge$Year)
dip456Edge <- lapply(dip456Edge,function(x)x[,c("Sender", "Receiver")])
dipGraphs[["dipGraph456"]] <- lapply(dip456Edge, graph_from_data_frame)


#saveRDS(dip456Graph, "dip456Graph.rds")

### 1b. DIP EX 3456
#redoing dipEx data by making it have different levels now--just make it binary based on level 4, 5 or 6 (ambassador or minister)
table(dipExAll$Embassy) #make 4, 5, 6 = 1 and the rest 0. indicating whether it's a strong diplomatic tie or not
dip3456 <- dipExAll %>% mutate(Ex=(case_when(Embassy >= 5 ~ 1,
                                            TRUE ~ 0)))
dip3456 <- dip3456 %>% select(Sender, Receiver, Ex, Year)

#as igraph object
dip3456Edge <- dip3456 %>% filter(Ex!=0) %>% select(-Ex)
dip3456Edge  <- split(dip3456Edge, dip3456Edge$Year)
dip3456Edge <- lapply(dip3456Edge, function(x)x[,c("Sender", "Receiver")])
dipGraphs[["dipGraph3456"]] <- lapply(dip3456Edge, graph_from_data_frame)


#saveRDS(dip3456Graph, "dip3456Graph.rds")

########################################

## 2. DIPEX BINARY
dipBin <- dipExAll %>% select(Sender, Receiver, Year)

dipBinList <- split(dipBin, dipBin$Year)

dipBinList <- lapply(dipBinList, function(x)x[,c("Sender", "Receiver")])

dipGraphs[["dipGraphBin"]] <- lapply(dipBinList, graph_from_data_frame)

#saveRDS(dipBinGraph, "dipBinGraph.rds")

##########################


## 3. DIPEXWEIGHTED - look at level of representation
dipWeight <- dipExAll %>% mutate(Ex=LOR) %>% select(Sender, Receiver, Ex, Year) %>%
  mutate(Weight=case_when(Ex==1 ~ 7,
                          Ex==0.75 ~ 6,
                          Ex==0.5 ~ 5,
                          Ex==0.375 ~ 4,
                          Ex==0.125 ~ 3,
                          Ex==0.1 ~ 2,
                          Ex==0 ~ 1))

#as network with preserved levels 456
dipWeightEdge <- split(dipWeight, dipWeight$Year)
dipWeightEdge <- lapply(dipWeightEdge, function(x)x[,c("Sender", "Receiver", "Weight")])
dipGraphs[["dipGraphWeight"]] <- lapply(dipWeightEdge, graph_from_data_frame)

#saveRDS(dipWeightGraph, "dipWeightGraph.rds")

saveRDS(dipGraphs, "dipGraphs.rds")

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
  select(Year, Sender, Receiver, TradeValue, id) %>% filter(Sender!="N/A", Receiver!="N/A", TradeValue>1000) %>% arrange(Year, Sender, Receiver) %>%
  rename(Weight = TradeValue)


#find pairs that have different reported amounts for incoming/outgoing and take average
dup <- trade[duplicated(trade[,c(1,2,3)])|duplicated(trade[,c(1,2,3)], fromLast=TRUE),] %>% arrange(Year, Sender, Receiver)
dup2 <- dup %>% group_by(Year, Sender, Receiver) %>% summarize(Weight=round(mean(Weight)), id=min(id)) %>% arrange(Year, Sender, Receiver)

idDel <- dup %>% group_by(Year, Sender, Receiver) %>% summarize(Weight=round(mean(Weight)), id=max(id)) %>% ungroup() %>% select(id)
idKeep <- dup2$id

#replacingn all rows with id in idKeep with dup2
trade[trade$id %in% idKeep,] <- dup2
#delete duplicate rows in idDel
trade <-trade %>% filter(!(id %in% idDel$id), Sender!=Receiver)
#write.csv(trade[,c("Year", "Sender", "Receiver", "Weight")], "C:/Users/joanm/OneDrive/Documents/Dissertation/data/allTrade1962-2020.csv")

# Only pairs of two countries that duplicate -- after taking care of weird ATG discrepancy in cComtrade 
#see how many items there are in eaach 'duplicate'. there should be only 2
#test2 <- test[which(test$num==3),] %>% group_by(Year, Sender, Receiver) #3 came up so i'm seeing how many countires are in groups of 3

#THRESHOLDING: use trade networks that are at least 1% of total global trade value of that year
thresholdFn <- function(tab, threshold) {
  tabCut <- tab %>% group_by(Year) %>% mutate(yearTot=sum(Weight)) %>%
    filter(Weight>threshold*yearTot) %>% ungroup()
  
  tabCutList <- split(tabCut, tabCut$Year)
  tabCutList <- lapply(tabCutList, function(x)x[,c("Sender", "Receiver", "Weight")])
  
  tabGraph <- lapply(tabCutList, graph_from_data_frame)
  
  return(tabGraph)
}

tradeGraphs <- list()
tradeGraphs[["tradeCutGraph0.05"]] <- thresholdFn(trade, 0.0005)
tradeGraphs[["tradeCutGraph1.0"]] <- thresholdFn (trade, 0.01)
tradeGraphs[["tradeCutGraph5.0"]] <- thresholdFn (trade, 0.05)
tradeGraphs[["tradeCutGraph0.01"]] <- thresholdFn(trade,0.0001)
tradeGraphs[["tradeCutGraph0.5"]] <- thresholdFn(trade, 0.005)
tradeGraphs[["tradeCutGraph0.1"]] <- thresholdFn(trade,0.001)
tradeGraphs[["tradeGraph0"]] <- thresholdFn(trade, 0)

saveRDS(tradeGraphs, "tradeGraphs.RDS")

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

#don't really need to use this loop anymore because it's very fast now to do lapply as.network for some reason
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
aid <- aidorig %>% filter(`Amount type`=="Current Prices") %>% 
  select(RECIPIENT, Recipient, DONOR, Donor, Year, Value) %>% mutate(Weight=Value*1000000) #all values are in millions

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

aid <- aid %>% select(Year, Sender, Receiver, Weight) %>% filter(!is.na(Sender), !is.na(Receiver))


### NEW: Add AidData's info on China into the matrix
aidChina <- read_excel("../../Data/AidData/AidDatasGlobalChineseDevelopmentFinanceDataset_v2.0.xlsx",
           "Global_CDF2.0")

aidChina <- aidChina %>% select("Financier Country", "Recipient", "Commitment Year", "Flow Class", "Amount (Nominal)") %>%
  filter(`Flow Class`=="ODA-like") %>%
  mutate(Receiver=countrycode(Recipient, "country.name", "iso3c" ),
         Sender=countrycode(`Financier Country`, "country.name", "iso3c")) %>%
  
  rename("Year" = "Commitment Year",
         "Weight" = "Amount (Nominal)") %>%
  filter(!is.na(Receiver), !is.na(Weight)) %>%
  select(Year, Sender, Receiver, Weight)

#China has duplicate values for receiving countries per year. so let's get rid of that
#all projects (rows) are unique so can sum across receiver-year
aidChinaUnique <- aidChina %>% group_by(Year, Sender, Receiver) %>%
  summarize(Weight2=sum(Weight)) %>%
  rename("Weight"="Weight2")

#Merging China data into aid data
aidWithChina <- bind_rows(aid, aidChinaUnique)
#write.csv(aidWithChina, "C:/Users/joanm/OneDrive/Documents/Dissertation/data/allAid1960-2019.csv")

#let's put a threshold as well
aidGraphs <- list()
aidGraphs[["aidCutGraph0.05"]] <- thresholdFn(aidWithChina, 0.0005)
aidGraphs[["aidCutGraph1.0"]] <- thresholdFn (aidWithChina, 0.01)
aidGraphs[["aidCutGraph5.0"]] <- thresholdFn (aidWithChina, 0.05)
aidGraphs[["aidCutGraph0.01"]] <- thresholdFn(aidWithChina,0.0001)
aidGraphs[["aidCutGraph0.5"]] <- thresholdFn(aidWithChina, 0.005)
aidGraphs[["aidCutGraph0.1"]] <- thresholdFn(aidWithChina,0.001)
aidGraphs[["aidGraph0"]] <- thresholdFn(aidWithChina, 0)

saveRDS(aidGraphs,
     file="aidGraphs.RDS")


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
