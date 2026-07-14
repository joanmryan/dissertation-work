rm(list=ls())
setwd("C:/Users/joanm/OneDrive/Documents/Dissertation/Chap2/code")

library(tidyverse)
library(countrycode)
library(readxl)
library(haven)
library(igraph)


###################
## PREPPING DATA ##
###################
region <- countrycode::codelist %>% 
  dplyr::select(country.name.en, un.name.en, iso3c, un.regionsub.name)
SSAcc <- region %>% filter(un.regionsub.name=="Sub-Saharan Africa")
Vregion <- region %>% dplyr::select(iso3c, un.regionsub.name) %>% filter(!is.na(iso3c))

#bring in trade/aid/dip data
trade <- read_csv("C:/Users/joanm/OneDrive/Documents/Dissertation/data/allTrade1962-2020.csv")[,-1] %>% 
  filter(Sender %in% c(SSAcc$iso3c, "CHN") | Receiver %in% c(SSAcc$iso3c, "CHN"))
aid <- read_csv("C:/Users/joanm/OneDrive/Documents/Dissertation/data/allAid1960-2019.csv")[,-1] %>% 
  filter(Sender %in% c(SSAcc$iso3c, "CHN") | Receiver %in% c(SSAcc$iso3c, "CHN"),
         Year %in% 2000:2017)
dip <- read_csv("C:/Users/joanm/OneDrive/Documents/Dissertation/data/allDipEx1960-2020.csv")[,-1] %>% rename(Weight=LOR) %>% 
  filter(Sender %in% c(SSAcc$iso3c, "CHN") | Receiver %in% c(SSAcc$iso3c, "CHN"))

thresholdFn <- function(tab, threshold) {
  tabCut <- tab %>% group_by(Year) %>% mutate(yearTot=sum(Weight)) %>%
    filter(Weight>threshold*yearTot) %>% ungroup()
  
  tabCutList <- split(tabCut, tabCut$Year)
  tabCutList <- lapply(tabCutList, function(x)x[,c("Sender", "Receiver", "Weight")])
  
  tabGraph <- lapply(tabCutList, graph_from_data_frame, vertices=Vregion)
  
  return(tabGraph)
}

dipGraphs <- thresholdFn(dip, 0)
tradeGraphs <- thresholdFn(trade, 0)
aidGraphs <- thresholdFn(aid, 0)

g <- aidGraphs[[50]]
CHN <- V(g)[names(V(g))=="CHN"]
make_ego_graph(g, order=1, nodes="CHN")

par(mfrow=c(4,5),
    oma=c(5,0,3,0),
    mai=c(0.2,0,0.2,0))

yearsAid <- 2000:2017 #China aid data only goes from 2000 - 2017
for (i in 1:length(yearsAid)) {
  g <- aidGraphs[[as.character(yearsAid[i])]]
  ego <- make_ego_graph(g, order=1, nodes=V(g)[names(V(g))=="USA"], mode="out")[[1]]
  plot.igraph(ego,
              
              vertex.size=ifelse(V(ego)$name=="CHN", 30,
                                 30*igraph::strength(ego, weights=E(ego)$Weight, mode="in")/max(igraph::strength(ego, weights=E(ego)$Weight, mode="in"))),
              vertex.label=V(ego)$name,
              
              vertex.label.cex=ifelse(V(ego)$name=="CHN", 2,
                                      2*igraph::strength(ego, weights=E(ego)$Weight, mode="in")/max(igraph::strength(ego, weights=E(ego)$Weight, mode="in"))),
                                     
    
              vertex.color=ifelse(V(ego)$un.regionsub.name=="Sub-Saharan Africa", "orange",
                                  ifelse(V(ego)$name=="CHN", "grey",
                                  "light blue")),
                                         
              edge.arrow.size=.1,
              edge.width=2*E(ego)$Weight/max((E(ego)$Weight)),
              #layout=layout_with_fr,
              #main=as.character(yearsAid[i]),
              margin=c(-0.1,-0.5,-0.1,-0.5))
  title(main=as.character(yearsAid[i]), 
        cex.main=2, font.main=1)
}
mtext("Ego Network for Aid Flows From Japan Above 0.1% of Global Aid Amount, 1965-2017", line=1, outer=TRUE, 
      cex=2, font=1.5)
mtext("      The central node represents Japan; orange (light-blue) nodes represent donor (recipient) countries for which size is proportionate to donor (recipient) centrality within the first-order ego network. Grey nodes represent 
      countries that both received and donated international aid. The top 15 most central countries are labelled on each network. Grey arrows represent bilateral flows of aid amounts (in equivalent 2020 USD amount) sent by 
      one country to another, with arrow thickness reflecting the volume of each aid flow. Aid flows below 0.1% of the global aid amount for each year were not considered in the generation of centrality scores and network graphs.
      Data was drawn from the OECD and AidData databases for years 1965 to 2017.", 
      side=1, outer=T, line=3.5, adj=0,
      cex=1)
