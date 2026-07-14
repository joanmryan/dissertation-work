rm(list=ls())

library(statnet)				
library(rgl)
library(sna)
library(tsna)
library(ndtv)
library(tidyverse)
library(countrycode)
library(igraph)

#what I want to get is actor degree centrality for each country in each year
#and whether there's much centralization in the network at all

#i also want to find if there are identifiable clusters

#this is a DIRECTED binary network


setwd("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange")
dipExBin <- read.csv("dipExBinary1960-2005.csv") #dipExBinary1960-2005.csv
ccodeCOW <- read.csv("COW country codes.csv")

#creating edgelist with only ties present (DE==1)
#dipExBin <- dipExBin[,c(2,4,6,5)] %>% filter(DE==1)

#instead of above, try creating adjacency matrices with zero ties still preserved
#one adjacency matrix for each year
dipExBin <- dipExBin[,c(2,4,6,5)]
colnames(dipExBin)[3] <- "weight"
dipEx0 <- dipExBin %>% filter(weight==1)

#create list of matrices for each year, also removing the year column
dipExYear_List <- split(dipExBin[,-4], dipExBin[,4])
dipEx0Year_List <- split(dipEx0[,-4], dipEx0[,4])

#as a network
dipEx_Net<-lapply(dipExYear_List,network,matrix.type='edgelist',
                  directed=TRUE)

#as an igraph object
dipEx_iNet <- lapply(dipExYear_List,graph.data.frame, directed=TRUE)
#making igraph object for no zeros network
dipEx0_iNet <- lapply(dipEx0Year_List,graph.data.frame, directed=TRUE)

#to turn into a dynamic network object. takes SUPER long
dipEx_Dynamic <- networkDynamic(network.list=dipEx_Net, vertex.pid="vertex.names")

#degree centrality for all countries
#what is the freeman degree?

#tSnaStats(dipEx_Dynamic,snafun='degree')

################################
## FINDING COMPONENT ANALYSIS ##
################################
dipWalktrap <- lapply(dipEx_iNet, walktrap.community)
dipEB <- lapply(dipEx0_iNet, edge.betweenness.community, weights=NULL, modularity=FALSE)

# DONT RUN IT CRASHES dipCliques <- lapply(dipEx_iNet, cliques)
# cliques(dipEx_iNet[[1]])

#check if name exists in vertex.names then add to another year centrality, otherwise create new name and add that year's centrality
years <- seq(1950, 2005, 5)


cDat_list <- list()

for (i in 1:length(dipEx_Net)){
tab <- data.frame(degree = degree(dipEx_Net[[i]],gmode="graph"),
                  bpow = bonpow(dipEx_Net[[i]], gmode="graph"),
                  country = get.vertex.attribute(dipEx_Net[[i]], "vertex.names"),
                  eigen = evcent(dipEx_Net[[i]], gmode="digraph"),
                  year = years[i]
)
cDat_list[[paste(years[i])]] <- tab

}  

cDat <- bind_rows(cDat_list)
cDat <- cDat %>% group_by(year) %>%
  mutate(g = length(country),
         degreeNorm = degree/(g-1),
         bpow1 = bpow-1,
         eigenISH = scale(eigen))


cDat %>% group_by(year) %>% slice_max(degreeNorm)
cDat %>% group_by(year) %>% summarize(length(country))
cDat %>% group_by(year) %>% slice_min(eigenISH)
#adding COW country code
cDat <- left_join(cDat, ccodeCOW, by=c("country" = "StateNme")) %>%
                    rename(ccodeCOW = CCode,
                           abbCOW = StateAbb)

#adding ISO and UN naming conventions by MERGING instead of mutating since it's panel
#the country-year panel is in countrycode::codelist_panel
ccodeALL <- countrycode::codelist_panel %>%
  select(year, cowc, cown, iso3c, iso2c, iso3n, un, region, un.region.code)
#write.csv(ccodeALL,"ccodeALL.csv")

cDat <- left_join(cDat, ccodeALL, by=c("ccodeCOW"="cown", "year"))
cDat <- cDat %>% mutate(region=relevel(as.factor(region),"Europe & Central Asia"))

#WPP TFR data
wpp <-  read.csv("WPP2019_Period_Indicators_Medium.csv")                       
wpp <- wpp %>% select(LocID, Location, MidPeriod, TFR, CBR) %>%
  mutate(year=MidPeriod-3)

cDat <- left_join(cDat, wpp, by=c("un"="LocID", "year"))
#write.csv(cDat, "cDat.csv")

#bring in GDP data
pwtgdp <- read.csv("pwtgdp.csv")
pwtgdp <- pwtgdp %>% select(rgdpna, countrycode, year)
cDat <- left_join(cDat, pwtgdp, by=c("iso3c"="countrycode", "year"))
#i'm not going to care about harmonizing the weird countries for now

#by tonight
#is centrality score comparable across years with different network sizes? - no must normalize
# merge each df into table with vars: country, year, centrality
#decided on using in=degree

#by tonight
#identifying clusters --secondary?
#import fertility data -- done
#harmonize country code -- fuck it done
#plots for every year

#by thursday night
#some reg tables or correlations or visualisations or whatever
#must decide by TONIGHT
#use mds to see clusters...?

#regression with TFR, regression with onset of fertility transition
#controls: GDP? country-level fixed effects?

library(plm)
library(tseries)
library(nlme)
library(lmtest)
library(sandwich)
library(dynlm)

coplot(TFR~degreeNorm|country, type="b", data=cDat)

cDat <- distinct(cDat)
cDat <- pdata.frame(cDat, index=c("country", "year"))

mfixed <- plm(TFR~degreeNorm, data=cDat, index=c("country", "year"), model="within", effect="individual")
summary(mfixed)
mtime <- plm(TFR~degreeNorm, data=cDat, index=c("country", "year"), model="between", effect="time")
mtimefixed <- plm(TFR~degreeNorm, data=cDat, index=c("country", "year"), model="within", effect="twoways")
summary(mtimefixed)
myear <- plm(TFR~degreeNorm + factor(year), data=cDat, index=c("country", "year"), model="within")
mrandom <- plm(TFR~degreeNorm, data=cDat, index=c("country", "year"), model="random", effect="twoways")
summary(mrandom)
mpool <- plm(TFR~degreeNorm, data=cDat, model="pooling")

phtest(mfixed,mrandom) #used fixed

adf.test(plm.data(cDat, index=c("country", "year"))$y, k=2) #no unit roots present
pFtest(mtimefixed, mfixed) #need to use time-fixed effects
plmtest(mfixed, c("time"), type=("bp")) #yup need to use time-fixed effects
plmtest(mpool, type=c("bp"))
pbgtest(mtimefixed) #there is serial correlation
pcdtest(mtimefixed, test=c("cd")) #there is cross-sectional dependence/correlational correlation

mdegree <- plm(TFR~degree, data=cDat, index=c("country", "year"), model="within", effect="twoways")
mdegreeNorm <- plm(TFR~degreeNorm, data=cDat, index=c("country", "year"), model="within", effect="twoways")
coeftest(mdegree, vcovHC)
coeftest(mdegreeNorm, vcovHC)

##YEAR AS FACTOR
mdegreeNorm <- plm(TFR ~ degreeNorm + factor(year), data=cDat, index=c("country", "year"), model="within")
mdegreeNormRegion <- plm(TFR ~ degreeNorm*region + factor(year) + degreeNorm:factor(year) , data=cDat, index=c("year", "country"), model="within")
stargazer(mdegreeNormRegion,coeftest(mdegreeNormRegion, vcovHC), type="html", out="ARGH.html")
coeftest(mdegreeNorm, vcovHC)
coeftest(mdegreeNormRegion, vcovHC)

mdegreeNormGDP <- plm(TFR ~ degreeNorm*rgdpna + factor(year), data=cDat, index=c("country","year"), model="within")
coeftest(mdegreeNormGDP, vcovHC)
summary(mdegreeNorm)


mdegreeNormRegionGDP <- plm(TFR ~ degreeNorm*region + degreeNorm*rgdpna + factor(year), index=c("year", "country"), model="within",data=cDat)
coeftest(mdegreeNormRegionGDP, vcovHC)

stargazer(coeftest(mdegreeNormGDP, vcovHC), mdegreeNormGDP, mdegreeNormRegion, type="html", out="tab.html")

stargazer(mdegreeNorm, mdegreeNormGDP, mdegreeNormRegion, type="html", out="ARRRGH.html")
stargazer(mdegreeNorm, type="html", out="tab1.html")
stargazer(mdegreeNormGDP, type="html", out="tab2.html")
stargazer(mdegreeNormRegion, type="html",out="tab3.html")
stargazer(mdegreeNormGDP, mdegreeNormRegion, type="html", out="tab4.html")

#eigenvector centrality
meigenISH <- plm(TFR ~ eigenISH + factor(year), data=cDat, index=c("country", "year"), model="within")
meigenISHRegion <- plm(TFR ~ eigenISH*region + factor(year) , data=cDat, index=c("year", "country"), model="within")
stargazer(meigenISHRegion,coeftest(meigenISHRegion, vcovHC), type="html", out="ARGH.html")
coeftest(meigenISH, vcovHC)

meigenISHGDP <- plm(TFR ~ eigenISH*rgdpna + factor(year), data=cDat, index=c("country","year"), model="within")
coeftest(meigenISHGDP, vcovHC)
summary(meigenISH)


meigenISHRegionGDP <- plm(TFR ~ eigenISH*region + eigenISH*rgdpna + factor(year), index=c("year", "country"), model="within",data=cDat)
coeftest(meigenISHRegionGDP, vcovHC)

stargazer(coeftest(meigenISHGDP, vcovHC), meigenISHGDP, meigenISHRegion, type="html", out="tab.html")

stargazer(meigenISH, meigenISHGDP, meigenISHRegion, meigenISHRegionGDP, type="html", out="ARRRGHa.html")
stargazer(meigenISH, type="html", out="tab1a.html")
stargazer(meigenISHGDP, type="html", out="tab2a.html")
stargazer(meigenISHRegion, type="html",out="tab3a.html")
