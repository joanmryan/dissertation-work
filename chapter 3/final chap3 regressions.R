rm(list=ls())
set.seed(12345)

library(tidyverse)
library(countrycode)
library(lme4)
library(broom)
library(cowplot)
library(sjPlot)
library(stringi)
library(lmerTest)
library(multilevel)
library(igraph)

setwd("C:/Users/joanm/OneDrive/Documents/Dissertation/Chap3/code")
#setwd("D:/RDProfiles/joanryan/Documents/Dissertation/Chap3/code")
cDat <- readRDS("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange/cDat.rds")


#plot(cDat %>% group_by(year) %>% summarize(meanDipKatz = mean(dipWeightKatz, na.rm=T)))

## SCALING ##
#i have to scale it by year so group by year
cDat <- cDat %>% filter(year>1959) %>% group_by(year) %>% mutate(across(starts_with(c("dip", "trade", "aid")),
                                                  ~scales::rescale(.x, to=c(0,100)),
                                                  .names="{col}.s"))


#mean centering (as of 3/8) and creating lag for TFR and GNI
cDat <- cDat %>% 
  group_by(cc) %>% 
  mutate(GNIlag1=dplyr::lag(GNI),
         TFRlag1 = dplyr::lag(TFR),
         time=year-1960,
         logGNI=log(GNI)) %>%
  mutate(across(!c(devCat,Country), 
                ~.x-mean(.x,na.rm=T), 
                .names="{col}.um")) %>%
  
  ungroup() %>%
  mutate(across(!c(cc,devCat,Country)&!(ends_with("um")), 
                ~.x-mean(.x,na.rm=T), 
                .names="{col}.m"))


#POOLED MODEL
#base
base11.lmer <- lmer(TFR~time + (0+time|cc) + (1|cc), data=cDat)
base11b.lmer <- lmer(TFR~time + logGNI + (0+time|cc) + (1|cc), data=cDat)trade10base.lmer <- lmer(TFR~ time + logGNI +  tradeBetUndir0.s.um  +  (1|cc), data=cDat)

#trade
trade10.lmer <- lmer(TFR~ time +  logGNI + tradeBetUndir0.s.um  + logGNI:tradeBetUndir0.s.um +   (1|cc), data=cDat)
trade11base.lmer <- lmer(TFR~ time + logGNI +  tradeBetUndir0.s.um  + (0+time|cc) +(1|cc), data=cDat)
trade11.lmer <- lmer(TFR~ year + logGNI +tradeBetUndir0.s.um  +  logGNI:tradeBetUndir0.s.um + (0+year|cc) +(1|cc), 
                     control=lmerControl(optCtrl=list(maxfun=2000000000000000)),
                     data=cDat)

anova(trade10.lmer, trade11.lmer)


#dipEx
dip10base.lmer <- lmer(TFR~ time + logGNI + dipKatz456.s.um + (1|cc), data=cDat)
dip10.lmer <- lmer(TFR~ time + logGNI +dipKatz456.s.um  + logGNI:dipKatz456.s.um + (1|cc), data=cDat)
dip11base.lmer <- lmer(TFR~ time + logGNI + dipKatz456.s.um + (0+time|cc) +(1|cc), data=cDat)
dip11.lmer <- lmer(TFR~ time + logGNI +dipKatz456.s.um  +  logGNI:dipKatz456.s.um + (0+time|cc) +(1|cc), 
                   control=lmerControl(optCtrl=list(maxfun=20000000000)),
                   data=cDat)
anova(dip10.lmer, dip11.lmer)


#aid
aidOut10base.lmer <- lmer(TFR~ time + logGNI +  aidOutDeg0.05.s.um + (1|cc), data=cDat)
aidOut10.lmer <- lmer(TFR~ time +logGNI + aidOutDeg0.05.s.um +  logGNI:aidOutDeg0.05.s.um+ (1|cc), data=cDat)
aidOut11base.lmer <- lmer(TFR~ time + logGNI + aidOutDeg0.s.um +  (0+time|cc) +(1|cc), data=cDat)
aidOut11.lmer <- lmer(TFR~ time + logGNI +aidOutDeg0.s.um + logGNI:aidOutDeg0.s.um + (0+time|cc) +(1|cc), data=cDat)
anova(aidOut10.lmer, aidOut11.lmer)

aidIn10base.lmer <- lmer(TFR~ time + logGNI + aidInDeg0.s.um + (1|cc), data=cDat)
aidIn10.lmer <- lmer(TFR~ time + logGNI +aidInDeg0.s.um +  logGNI:aidInDeg0.s.um + (1|cc), data=cDat)
aidIn11base.lmer <- lmer(TFR~ time + logGNI + aidInDeg0.s.um + (0+time|cc) +(1|cc),
                         control=lmerControl(optCtrl=list(maxfun=20000000)),
                         data=cDat)
aidIn11.lmer <- lmer(TFR~ time + logGNI +aidInDeg0.s.um  + logGNI:aidInDeg0.s.um + (0+time|cc) +(1|cc), data=cDat)
anova(aidIn10.lmer, aidIn11.lmer)

tab_model(base11.lmer, base11b.lmer, dip11base.lmer, dip11.lmer, trade11base.lmer, trade11.lmer, aidOut11base.lmer, aidOut11.lmer, aidIn11base.lmer, aidIn11.lmer,
          digits=3,
          show.ci=FALSE,
          p.style="stars")

tab_model(dip10base.lmer, dip10.lmer, trade10base.lmer, trade10.lmer, aidOut10base.lmer, aidOut10.lmer, aidIn10base.lmer, aidIn10.lmer,
          digits=3,
          show.ci=FALSE,
          p.style="stars")
#end up using nlme spec (see below)

######################################################
#doing separate regressions for the devCat categories#
######################################################

#plot TFR over time for each group
ggplot(cDat[!is.na(cDat$devCat),], aes(x = tradeBetDir0.s.um[!is.na(cDat$devCat)], y = TFR, group = devCat[!is.na(cDat$devCat)], color = devCat[!is.na(cDat$devCat)])) + geom_point()
range(cDatH$TFR, na.rm=T)
range(cDatL$TFR, na.rm=T)
range(cDatUM$TFR, na.rm=T)
range(cDatLM$TFR, na.rm=T)
#shouldn't do a group mean since some H countries still have very high TFR and L countries have low TFR
#should just look at how a country (with reference to its baseline) changes over time
#maybe think about centering around the first year fertility level...? 
#update: this doesn't actually make a difference. it's already captured by the random cc intercept



#see if there are contries that were the same devCat the entire time--only about 10 in L and 2 in H
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

test <- cDat %>% dplyr::select(cc, year, tradeBetUndir0, tradeBetUndir0.s.um)


devN <- cDat %>% filter(year %in% 1962:2020) %>% group_by(cc) %>% summarize(length=length(unique(devCat)),
                                            devCat=getmode(devCat))
devN <- devN %>% filter(length==1)

cDatL <- cDat %>% filter(devCat=="L") %>%
  dplyr::select(!ends_with(c(".m", ".um"))) %>%
  group_by(cc) %>% 
  mutate(across(!c(devCat,Country), 
                ~.x-mean(.x,na.rm=T), 
                .names="{col}.um")) %>%
  ungroup() %>%
  mutate(across(!c(cc,devCat,Country)&!(ends_with("um")), 
                ~.x-mean(.x,na.rm=T), 
                .names="{col}.m"))

cDatLM <- cDat %>% filter(devCat=="LM")  %>% 
  dplyr::select(!ends_with(c(".m"))) %>%
  mutate(across(!c(cc,devCat,Country)&!(ends_with("um")), 
                ~.x-mean(.x,na.rm=T), 
                .names="{col}.m"))

cDatUM <- cDat %>% filter(devCat=="UM") %>%
  dplyr::select(!ends_with(c(".m"))) %>%
  mutate(across(!c(cc,devCat,Country)&!(ends_with("um")), 
                ~.x-mean(.x,na.rm=T), 
                .names="{col}.m"))

cDatH <- cDat %>% filter(devCat=="H")  %>% 
  dplyr::select(!ends_with(c(".m"))) %>%
  mutate(across(!c(cc,devCat,Country)&!(ends_with("um")), 
                ~.x-mean(.x,na.rm=T), 
                .names="{col}.m"))
#for trade
tradeH11 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um  +  (0+time|cc) + (1|cc), data=cDatH,
                 control=lmerControl(optCtrl=list(maxfun=20000000)))
tradeH10 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um +  logGNI:tradeBetUndir0.s.um +(1|cc), data=cDatH)
anova(tradeH10, tradeH11)

tradeUM11 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um +  logGNI:tradeBetUndir0.s.um +  (0+time|cc) +(1|cc), data=cDatUM)
tradeUM10 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um +  logGNI:tradeBetUndir0.s.um   +(1|cc), data=cDatUM)
anova(tradeUM10, tradeUM11)

tradeLM11 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um +  logGNI:tradeBetUndir0.s.um  +  (0+time|cc) +(1|cc), data=cDatLM)
tradeLM10 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um +  logGNI:tradeBetUndir0.s.um   +(1|cc), data=cDatLM)
anova(tradeLM10, tradeLM11)

tradeL11 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um +  logGNI:tradeBetUndir0.s.um  +  (0+time|cc) +(1|cc), data=cDatL)
tradeL10 <- lmer(TFR.um~ time + logGNI + tradeBetUndir0.s.um +  logGNI:tradeBetUndir0.s.um   +(1|cc), data=cDatL)
anova(tradeL10, tradeL11)
#all require random time slope

tradeH11m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um + logGNI.um:tradeBetUndir0.s.um  +  (0+time|cc) + (1|cc), data=cDatH,
                 control=lmerControl(optCtrl=list(maxfun=20000000)))
tradeH10m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um +  logGNI.um:tradeBetUndir0.s.um +(1|cc), data=cDatH)


tradeUM11m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um +  logGNI.um:tradeBetUndir0.s.um +  (0+time|cc) +(1|cc), data=cDatUM)
tradeUM10m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um +  logGNI.um:tradeBetUndir0.s.um   +(1|cc), data=cDatUM)


tradeLM11m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um +  logGNI.um:tradeBetUndir0.s.um  +  (0+time|cc) +(1|cc), data=cDatLM)
tradeLM10m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um +  logGNI.um:tradeBetUndir0.s.um   +(1|cc), data=cDatLM)


tradeL11m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um +  logGNI.um:tradeBetUndir0.s.um  +  (0+time|cc) +(1|cc), data=cDatL)
tradeL10m <- lmer(TFR.um~ time + logGNI.um + tradeBetUndir0.s.um +  logGNI.um:tradeBetUndir0.s.um   +(1|cc), data=cDatL)

tab_model(tradeH10m, tradeUM10m, tradeLM10m, tradeL10m,dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars",
          title="Mean-Centered Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")

tab_model(tradeH11m, tradeUM11m, tradeLM11m, tradeL11m,dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars",
          title="Mean-Centered Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")



tab_model(tradeH11, tradeUM11, tradeLM11, tradeL11,dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars",
          title="Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")

tab_model(tradeH10, tradeUM10, tradeLM10, tradeL10,dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars",
          title="Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")


#for aid
aidH11 <- lmer(TFR~ time + logGNI +aidOutDeg0.s.um + logGNI:aidOutDeg0.s.um + (0+time|cc) +(1|cc), data=cDatH)
aidUM11 <- lmer(TFR~ time + logGNI +aidOutDeg0.s.um + logGNI:aidOutDeg0.s.um + (0+time|cc) +(1|cc), data=cDatUM)
aidLM11 <- lmer(TFR~ time + logGNI +aidInDeg0.s.um  + logGNI:aidInDeg0.s.um + (0+time|cc) +(1|cc), data=cDatLM)
aidL11 <- lmer(TFR~ time + logGNI +aidInDeg0.s.um  + logGNI:aidInDeg0.s.um + (0+time|cc) +(1|cc), data=cDatL)
aidH10 <- lmer(TFR~ time + logGNI +aidOutDeg0.s.um + logGNI:aidOutDeg0.s.um +  (1|cc), data=cDatH)
aidUM10 <- lmer(TFR~ time + logGNI +aidOutDeg0.s.um + logGNI:aidOutDeg0.s.um +  (1|cc), data=cDatUM)
aidLM10 <- lmer(TFR~ time + logGNI +aidInDeg0.s.um  + logGNI:aidInDeg0.s.um +  (1|cc), data=cDatLM)
aidL10 <- lmer(TFR~ time + logGNI +aidInDeg0.s.um  + logGNI:aidInDeg0.s.um +  (1|cc), data=cDatL)

aidH10m <- lmer(TFR~ time + logGNI +aidOutDeg0.s.m + logGNI:aidOutDeg0.s.m +  (1|cc), data=cDatH)
aidUM10m <- lmer(TFR~ time + logGNI +aidOutDeg0.s.m + logGNI:aidOutDeg0.s.m +  (1|cc), data=cDatUM)
aidLM10m <- lmer(TFR~ time + logGNI +aidInDeg0.s.m  + logGNI:aidInDeg0.s.m +  (1|cc), data=cDatLM)
aidL10m <- lmer(TFR~ time + logGNI +aidInDeg0.s.m  + logGNI:aidInDeg0.s.m +  (1|cc), data=cDatL)

tab_model(aidH10m, aidUM10m, aidLM10m, aidL10m, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci=F, digits=3, p.style="stars",
          title="Unpooled Linear Mixed Model Regression of Aid Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2019")


tab_model(aidH10, aidUM10, aidLM10, aidL10, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci=F, digits=3, p.style="stars",
          title="Unpooled Linear Mixed Model Regression of Aid Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2019")

tab_model(aidH11, aidUM11, aidLM11, aidL11, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci=F, digits=3, p.style="stars",
          title="Unpooled Linear Mixed Model Regression of Aid Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2019")

#for dipEx
dipH11 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um + (0+time|cc) +(1|cc), 
               control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatH)
dipH10 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um  +(1|cc), 
             control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatH)
anova(dipH10, dipH11)

dipUM11 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um + (0+time|cc) +(1|cc), 
                control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatUM)
dipUM10 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um  +(1|cc), 
              control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatUM)
anova(dipUM10, dipUM11)

dipLM11 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um + (0+time|cc) +(1|cc), 
                control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatLM)
dipLM10 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um +(1|cc), 
              control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatLM)
anova(dipLM10, dipLM11)

dipL11 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um + (0+time|cc) +(1|cc), 
               control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatL)
dipL10 <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um  +(1|cc), 
             control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatL)
anova(dipL10, dipL11)

#doing for logGNI.m and 10

dipH10m <- lmer(TFR~ time + logGNI.m +dipKatzWeight.s.m  +  logGNI.m:dipKatzWeight.s.m  +(1|cc), 
                          control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatH)
dipUM10m <- lmer(TFR~ time + logGNI.m +dipKatzWeight.s.m  +  logGNI.m:dipKatzWeight.s.m  +(1|cc), 
                 control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatUM)
dipLM10m <- lmer(TFR~ time + logGNI.m +dipKatzWeight.s.m  +  logGNI.m:dipKatzWeight.s.m  +(1|cc), 
                control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatLM)
dipL10m <- lmer(TFR~ time + logGNI.m +dipKatzWeight.s.m  +  logGNI.m:dipKatzWeight.s.m  +(1|cc), 
                control=lmerControl(optCtrl=list(maxfun=20000000000)), data=cDatL)

tab_model(dipH10m, dipUM10m, dipLM10m, dipL10m,
          dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", 
          title="Unpooled Linear Mixed Model Regression of Diplomatic Representation Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2013")



tab_model(dipH11, dipUM11, dipLM11, dipL11, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", 
          title="Unpooled Linear Mixed Model Regression of Diplomatic Representation Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2013")

tab_model(dipH10, dipUM10, dipLM10, dipL10, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", 
          title="Unpooled Linear Mixed Model Regression of Diplomatic Representation Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2013")


#ALL WITHOUT GNI
#for trade
tradeH <- lmer(TFR ~ year + centTrade + (1|cc), data=cDatH)
tradeUM <- lmer(TFR ~ year + centTrade + (1|cc), data=cDatUM)
tradeLM <- lmer(TFR ~ year + centTrade + (1|cc), data=cDatLM)
tradeL <- lmer(TFR ~ year + centTrade + (1|cc), data=cDatL)

tab_model(tradeH, tradeUM, tradeLM, tradeL, show.ci=F,digits=3, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
          title="Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")

#for dipEx
dipExH <- lmer(TFR ~ year + centDipEx + (1|cc), data=cDatH)
dipExUM <- lmer(TFR ~ year + centDipEx + (1|cc), data=cDatUM)
dipExLM <- lmer(TFR ~ year + centDipEx + (1|cc), data=cDatLM)
dipExL <- lmer(TFR ~ year + centDipEx + (1|cc), data=cDatL)

tab_model(dipExH, dipExUM, dipExLM, dipExL, show.ci=F, digits=3, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
          title="Unpooled Linear Mixed Model Regression of Diplomatic Representation Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2013")


#for aid
aidH <- lmer(TFR ~ year + centAidOut + (1|cc), data=cDatH)
aidUM <- lmer(TFR ~ year + centAidOut + (1|cc), data=cDatUM)
aidLM <- lmer(TFR ~ year + centAidIn + (1|cc), data=cDatLM)
aidL <- lmer(TFR ~ year + centAidIn + (1|cc), data=cDatL)

tab_model(aidH, aidUM, aidLM, aidL, show.ci=F, digits=3, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
          title="Unpooled Linear Mixed Model Regression of Aid Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2019")


# MEAN CENTERED VERSIONS #
#for trade
meantradeH <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentTrade + meanCentTrade + (1|cc), data=cDatH)
meantradeUM <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentTrade + meanCentTrade + (1|cc), data=cDatUM)
meantradeLM <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentTrade + meanCentTrade + (1|cc), data=cDatLM)
meantradeL <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentTrade + meanCentTrade + (1|cc), data=cDatL)

tab_model(meantradeH, meantradeUM, meantradeLM, meantradeL, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
          title="Unpooled Mean-Centered Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")


#for aid
meanaidH <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentAidOut + meanCentAidOut + (1|cc), data=cDatH)
meanaidUM <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentAidOut + meanCentAidOut + (1|cc), data=cDatUM)
meanaidLM <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentAidIn + meanCentAidIn + (1|cc), data=cDatLM)
meanaidL <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentAidIn + meanCentAidIn + (1|cc), data=cDatL)

tab_model(meanaidH, meanaidUM, meanaidLM, meanaidL, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
          title="Unpooled Mean-Centered Linear Mixed Model Regression of Aid Centrality on Total Fertility Rate With Country-Level Grouping From 1960 - 2019")

#for dipEx
meandipExH <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentDipEx + meanCentDipEx + (1|cc), data=cDatH)
meandipExUM <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentDipEx + meanCentDipEx + (1|cc), data=cDatUM)
meandipExLM <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentDipEx + meanCentDipEx + (1|cc), data=cDatLM)
meandipExL <- lmer(TFR ~ year + meanGNI + meanGNI:meanCentDipEx + meanCentDipEx + (1|cc), data=cDatL)

tab_model(meandipExH, meandipExUM, meandipExLM, meandipExL, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
          title="Unpooled Linear Mean-Centered Mixed Model Regression of Diplomatic Representation Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2013")


#trying with nlme
tradeH11.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1+time|cc, data=cDatH,
                     na.action=na.omit)
tradeUM11.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1+time|cc, data=cDatUM,
                     na.action=na.omit)
tradeLM11.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1+time|cc, data=cDatLM,
                     na.action=na.omit)
tradeL11.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1+time|cc, data=cDatL,
                     na.action=na.omit)

tab_model(tradeH11.nlme, tradeUM11.nlme, tradeLM11.nlme, tradeL11.nlme,dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars",
          title="Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")

tradeH10.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1|cc, data=cDatH,
                     na.action=na.omit)
tradeUM10.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1|cc, data=cDatUM,
                      na.action=na.omit)
tradeLM10.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1|cc, data=cDatLM,
                      na.action=na.omit)
tradeL10.nlme <- lme(TFR ~ time + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1|cc, data=cDatL,
                     na.action=na.omit)

tab_model(tradeH10.nlme, tradeUM10.nlme, tradeLM10.nlme, tradeL10.nlme,dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
          show.ci = FALSE, digits=3, p.style="stars",
          title="Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")



#POOLED MODEL WITH NLME##

base11a.nlme <- lme(TFR ~ year, random=~1+year|cc, data=cDat,
                    na.action=na.omit)
base11.nlme <- lme(TFR ~ year + logGNI, random=~1+year|cc, data=cDat,
                    na.action=na.omit)

dip11base.nlme <- lme(TFR ~ year + logGNI + dipKatzWeight.s.um , random=~1+year|cc, data=cDat,
                  na.action=na.omit)
trade11base.nlme <- lme(TFR ~ year + logGNI + tradeBetUndir0.s.um , random=~1+year|cc, data=cDat,
                    na.action=na.omit)
aidOut11base.nlme <-lme(TFR ~ year + logGNI + aidOutDeg0.s.um , random=~1+year|cc, data=cDat,
                    na.action=na.omit)
aidIn11base.nlme <-lme(TFR ~ year + logGNI + aidInDeg0.s.um , random=~1+year|cc, data=cDat,
                   na.action=na.omit)

dip11.nlme <- lme(TFR ~ year + logGNI + dipKatzWeight.s.um + logGNI:dipKatzWeight.s.um, random=~1+year|cc, data=cDat,
                    na.action=na.omit)
trade11.nlme <- lme(TFR ~ year + logGNI + tradeBetUndir0.s.um + logGNI:tradeBetUndir0.s.um, random=~1+year|cc, data=cDat,
                    na.action=na.omit)
aidOut11.nlme <-lme(TFR ~ year + logGNI + aidOutDeg0.s.um + logGNI:aidOutDeg0.s.um, random=~1+year|cc, data=cDat,
                   na.action=na.omit)
aidIn11.nlme <-lme(TFR ~ year + logGNI + aidInDeg0.s.um + logGNI:aidInDeg0.s.um, random=~1+year|cc, data=cDat,
                   na.action=na.omit)

tab_model(base11a.nlme, base11.nlme, 
          dip11base.nlme, dip11.nlme, 
          trade11base.nlme,
          trade11.nlme, 
          aidOut11base.nlme,
          aidOut11.nlme, 
          aidIn11base.nlme,
          aidIn11.nlme,
          show.ci=FALSE, p.style="stars",
          digits=3) 
## Final specification used

dip11H.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1+time|cc, data=cDatH,
                   na.action=na.omit)
dip10H.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1|cc, data=cDatH,
                   na.action=na.omit)

dip11UM.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1+time|cc, data=cDatUM,
                    na.action=na.omit)
dip10UM.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1|cc, data=cDatUM,
                    na.action=na.omit)

dip11LM.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1+time|cc, data=cDatLM,
                     na.action=na.omit)
dip10LM.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1|cc, data=cDatLM,
                    na.action=na.omit)

dip11L.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1+time|cc, data=cDatL,
                   na.action=na.omit)
dip10L.nlme <- lme(TFR ~ time + logGNI + dipKatz456.s.um + logGNI:dipKatz456.s.um, random=~1|cc, data=cDatL,
                  na.action=na.omit)

tab_model(dip11H.nlme, dip11UM.nlme, dip11LM.nlme, dip11L.nlme,
          show.ci=FALSE, p.style="stars",
          digits=3) 

tab_model(dip10H.nlme, dip10UM.nlme, dip10LM.nlme, dip10L.nlme,
          show.ci=FALSE, p.style="stars",
          digits=3) 
