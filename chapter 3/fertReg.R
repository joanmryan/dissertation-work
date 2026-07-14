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
cDatPure <- cDat

#check which centrlaity measures have highest correlation to TFR
#corTab <- cDat %>% summarize(across(dipBinEig:aidKatz, ~cor(.x,TFR,use="pairwise.complete.obs")))
#dipWeightKatz has a decent correlation with TFR--let's see if it's comparable across years?



#plot(cDat %>% group_by(year) %>% summarize(meanDipKatz = mean(dipWeightKatz, na.rm=T)))

## SCALING ##
#i have to scale it by year so group by year
cDat <- cDat %>% group_by(year) %>% mutate(across(starts_with(c("dip", "trade", "aid")),
                               ~scales::rescale(.x, to=c(0,100)),
                               .names="{col}.s"))


#mean centering (as of 3/8) and creating lag for TFR and GNI
cDat <- cDat %>% as_tibble() %>% filter(year>1959) %>%
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

#so now each centrality measure has 6 versions: raw, raw+group(unit) mean, raw+(global) mean, scaled, scaled+unit mean, scaled+global mean
#write.csv(cDat, "cDatFin.csv")
###################
#   REGRESSIONS   #
###################
#to mean-center model:
cDat <- cDat %>% mutate(meanCentTrade=mean(centTrade, na.rm=T),
                        meanCentDipEx=mean(centDipEx, na.rm=T),
                        meanCentAidIn=mean(centAidIn, na.rm=T),
                        meanCentAidOut=mean(centAidOut, na.rm=T),
                        meanGNI=mean(GNI, na.rm=T))

tradeM1 <- lmer(TFR~log(GNI)+log(GNI):centTrade + year + (1+centTrade+log(GNI)+log(GNI)*centTrade|cc), 
                data=cDat,
                REML=FALSE)
tradeM2 <- lmer(TFR~log(GNI)+log(GNI):centTrade + centTrade + year + (1+centTrade|cc), 
                data=cDat,
                REML=FALSE)
tradeM3 <- lme4::lmer(TFR~ year + log(GNI)+ centTrade  + log(GNI):centTrade +  (1|cc), 
                      data=cDat,
                      REML=FALSE)
tradeM3main <- lme4::lmer(TFR~ year + log(GNI)+ centTrade  +  (1|cc), 
                          data=cDat,
                          REML=FALSE)


## trying to allow for random slope for year
tradeM31 <- lme4::lmer(TFR~ year + log(GNI)+ centTrade  + log(GNI):centTrade +  (1 + year|cc), 
                      data=cDat,
                      REML=FALSE)

tradeM32 <- lme4::lmer(TFR~  log(GNI)+ centTrade  + log(GNI):centTrade +  (1 |cc) + (0+year|cc), 
                      data=cDat,
                      REML=FALSE)


tradeM33 <- lme4::lmer(TFR~ log(GNI)+ centTrade  + log(GNI):centTrade +  (1|cc) + (1|year), 
                      data=cDat,
                      REML=FALSE)

tradeM34 <- lme4::lmer(TFR~ log(GNI)+ centTrade  + log(GNI):centTrade +  (1+ centTrade |cc), 
                       data=cDat,
                       REML=TRUE)


tradeM35 <- lme4::lmer(TFR~ log(GNI)+ centTrade  + log(GNI):centTrade +  (1+log(GNI)|cc), 
                       data=cDat,
                       REML=TRUE)


#note here meani is not a function. it's mean_i
plmesque <- lme4::lmer(TFR~ log(GNI)+ (centTrade-meani(centTrade))  + 
                         log(GNI):(centTrade-meani(centTrade)) + 
                         log(GNI):meani(centTrade)
                         meani(centTrade) + (1+(centTrade-meani(centTrade))|cc), 
                  data=cDat,
                  REML=TRUE)

##allowing for TFR~year to vary between country
#if it'is a fixed effect
tradeYearNotReallyFixed <- lme4::lmer(TFR~ year + log(GNI)+ centTrade  + log(GNI):centTrade  + (1+year|cc), 
                       data=cDat,
                       REML=FALSE)



linearMod <- lm(TFR~ year + log(GNI)+ centTrade  + log(GNI):centTrade + year:cc + cc, data=cDat)

#if if TFR~year varies randomly by country
tradeYearRandom <- lme4::lmer(TFR~ year + log(GNI)+ centTrade  + log(GNI):centTrade  + year:cc + cc+ (1|year) , 
                             data=cDat,
                             REML=FALSE)

tab_model(tradeM31, tradeM32, tradeM33)

#use tradeM3

aidInM3 <- lmer(TFR ~ year + log(GNI) + centAidIn +  log(GNI):centAidIn +  (1|cc), data=cDat)
aidInM3main <- lmer(TFR ~ year + log(GNI) + centAidIn +  (1|cc), data=cDat)
aidOutM3 <- lmer(TFR ~ year + log(GNI) + centAidOut +log(GNI):centAidOut +   (1|cc), data=cDat)
aidOutM3main <- lmer(TFR ~ year + log(GNI) + centAidOut  +   (1|cc), data=cDat)
dipExM3 <- lmer(TFR ~ year +  log(GNI) + centDipEx + log(GNI):centDipEx  +  (1|cc), data=cDat)
dipExM3main <- lmer(TFR ~ year +  log(GNI) + centDipEx +  (1|cc), data=cDat)

modelsummary(list(dipExM3, tradeM3, aidOutM3, aidInM3), stars=TRUE, statistic="p.value")
tab_model(dipExM3main, dipExM3, tradeM3main, tradeM3, aidOutM3main, aidOutM3, aidInM3main, aidInM3, show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE)

#########################
## MEAN-CENTERED MODEL ##
#########################
cDat <- cDat %>% mutate(meanCentTrade=centTrade-mean(centTrade, na.rm=T),
                        meanCentDipEx=centDipEx-mean(centDipEx, na.rm=T),
                        meanCentAidIn=centAidIn-mean(centAidIn, na.rm=T),
                        meanCentAidOut=centAidOut-mean(centAidOut, na.rm=T),
                        meanGNI=log(GNI)-log(mean(GNI, na.rm=T)))

#allowing for year to be random slope
tradeM3 <- lme4::lmer(TFR~ year + meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1+year|cc), data=cDat)
tradeM3main <- lme4::lmer(TFR~ year + meanGNI+ meanCentTrade  +  (1+year|cc), data=cDat)
aidInM3 <- lmer(TFR ~ year + meanGNI + meanCentAidIn +  meanGNI:meanCentAidIn +  (1+year|cc), data=cDat)
aidInM3main <- lmer(TFR ~ year + meanGNI + meanCentAidIn +  (1+year|cc), data=cDat)
aidOutM3 <- lmer(TFR ~ year + meanGNI + meanCentAidOut +meanGNI:meanCentAidOut +   (1+year|cc), data=cDat)
aidOutM3main <- lmer(TFR ~ year + meanGNI + meanCentAidOut  +   (1+year|cc), data=cDat)
dipExM3 <- lmer(TFR ~ year +  meanGNI + meanCentDipEx + meanGNI:meanCentDipEx  +  (1+year|cc), data=cDat)
dipExM3main <- lmer(TFR ~ year +  meanGNI + meanCentDipEx +  (1+year|cc), data=cDat)

base1 <- lmer(TFR~ year +  (1+year|cc), data=cDat)
base2 <- lmer(TFR~ year + meanGNI +  (1+year|cc), data=cDat)

tab_model(base1, base2, dipExM3main, dipExM3, tradeM3main, tradeM3, aidOutM3main, aidOutM3, aidInM3main, aidInM3, show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE)

## only random intercept for country
tradeM3 <- lme4::lmer(TFR~ year + meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1|cc), data=cDat)
tradeM3main <- lme4::lmer(TFR~ year + meanGNI+ meanCentTrade  +  (1|cc), data=cDat)
aidInM3 <- lmer(TFR ~ year + meanGNI + meanCentAidIn +  meanGNI:meanCentAidIn +  (1|cc), data=cDat)
aidInM3main <- lmer(TFR ~ year + meanGNI + meanCentAidIn +  (1|cc), data=cDat)
aidOutM3 <- lmer(TFR ~ year + meanGNI + meanCentAidOut +meanGNI:meanCentAidOut +   (1|cc), data=cDat)
aidOutM3main <- lmer(TFR ~ year + meanGNI + meanCentAidOut  +   (1|cc), data=cDat)
dipExM3 <- lmer(TFR ~ year +  meanGNI + meanCentDipEx + meanGNI:meanCentDipEx  +  (1|cc), data=cDat)
dipExM3main <- lmer(TFR ~ year +  meanGNI + meanCentDipEx +  (1|cc), data=cDat)

base1 <- lmer(TFR~ year +  (1|cc), data=cDat)
base2 <- lmer(TFR~ year + meanGNI +  (1|cc), data=cDat)

tab_model(base1, base2, dipExM3main, dipExM3, tradeM3main, tradeM3, aidOutM3main, aidOutM3, aidInM3main, aidInM3, show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE)


## trying to allow for random slope for year
tradeM3a <- lme4::lmer(TFR~ year + meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1*year|cc), 
                      data=cDat,
                      REML=FALSE)

tradeM3b <- lme4::lmer(TFR~ year + meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1|cc) + (0+year|cc), 
                      data=cDat,
                      REML=FALSE)

#testing for model fit
tradeM3a <- lme4::lmer(TFR~ year + meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1*year|cc), 
                       data=cDat,
                       REML=FALSE)

tradeM3b <- lme4::lmer(TFR~ year + meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1+year|cc), 
                       data=cDat,
                       REML=FALSE)

tradeM3c <- lme4::lmer(TFR~ year + meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1|cc) + (0 + year|cc), 
                       data=cDat,
                       REML=FALSE)

tradeM3d <- lme4::lmer(TFR~  meanGNI + meanCentTrade  + meanGNI:meanCentTrade +  (1|cc) + (year|cc), 
                       data=cDat,
                       REML=FALSE)

anova(tradeM3a, tradeM3b, tradeM3c)

tab_model(tradeM3a, tradeM3b, tradeM3c)

##(1+year|cc) maeks the most sense

#why is GNI insignificant?
trade10base.lmer <- lmer(TFR~ time + logGNI +  tradeBetDir0.01.s.um  +  (1|cc), data=cDat)
trade10.lmer <- lmer(TFR~ time +  logGNI + tradeBetDir0.01.s.um  + logGNI:tradeBetDir0.01.s.um +   (1|cc), data=cDat)
trade11base.lmer <- lmer(TFR~ time + logGNI +  tradeBetUndir0.s.um  + (0+time|cc) +(1|cc), data=cDat)
trade11.lmer <- lmer(TFR~ time + logGNI +tradeBetUndir0.s.um  +  logGNI:tradeBetUndir0.s.um + (0+time|cc) +(1|cc), 
                     control=lmerControl(optCtrl=list(maxfun=2000000000000000)),
                     data=cDat)

anova(trade10.lmer, trade11.lmer)
#tab_model(trade1, trade2, tradeM3main, tradeM3)

dip10base.lmer <- lmer(TFR~ time + logGNI + dipKatzWeight.s.um + (1|cc), data=cDat)
dip10.lmer <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  + logGNI:dipKatzWeight.s.um + (1|cc), data=cDat)
dip11base.lmer <- lmer(TFR~ time + logGNI + dipKatzWeight.s.um + (0+time|cc) +(1|cc), data=cDat)
dip11.lmer <- lmer(TFR~ time + logGNI +dipKatzWeight.s.um  +  logGNI:dipKatzWeight.s.um + (0+time|cc) +(1|cc), 
                   control=lmerControl(optCtrl=list(maxfun=20000000000)),
                   data=cDat)
anova(dip10.lmer, dip11.lmer)
#tab_model(dipEx1, dipEx2, dipExM3main, dipExM3)
base11.lmer <- lmer(TFR~time + (0+time|cc) + (1|cc), data=cDat)
base11b.lmer <- lmer(TFR~time + logGNI + (0+time|cc) + (1|cc), data=cDat)

aidOut10base.lmer <- lmer(TFR~ time + logGNI +  aidOutDeg0.05.s.um + (1|cc), data=cDat)
aidOut10.lmer <- lmer(TFR~ time +logGNI + aidOutDeg0.05.s.um +  logGNI:aidOutDeg0.05.s.um+ (1|cc), data=cDat)
aidOut11base.lmer <- lmer(TFR~ time + logGNI + aidOutDeg0.s.um +  (0+time|cc) +(1|cc), data=cDat)
aidOut11.lmer <- lmer(TFR~ time + logGNI +aidOutDeg0.s.um + logGNI:aidOutDeg0.s.um + (0+time|cc) +(1|cc), data=cDat)
anova(aidOut10.lmer, aidOut11.lmer)
#tab_model(aidOut1, aidOut2, aidOutM3main, aidOutM3)

aidIn10base.lmer <- lmer(TFR~ time + logGNI + aidInDeg0.05.s.um + (1|cc), data=cDat)
aidIn10.lmer <- lmer(TFR~ time + logGNI +aidInDeg0.05.s.um +  logGNI:aidInDeg0.05.s.um + (1|cc), data=cDat)
aidIn11base.lmer <- lmer(TFR~ time + logGNI + aidInDeg0.s.um + (0+time|cc) +(1|cc),
                         control=lmerControl(optCtrl=list(maxfun=20000000)),
                         data=cDat)
aidIn11.lmer <- lmer(TFR~ time + logGNI +aidInDeg0.s.um  + logGNI:aidInDeg0.s.um + (0+time|cc) +(1|cc), data=cDat)
anova(aidIn10.lmer, aidIn11.lmer)
#tab_model(aidIn1, aidIn2, aidInM3main, aidInM3)

tab_model(base11.lmer, base11b.lmer, dip11base.lmer, dip11.lmer, trade11base.lmer, trade11.lmer, aidOut11base.lmer, aidOut11.lmer, aidIn11base.lmer, aidIn11.lmer,
          digits=3,
          show.ci=FALSE,
          p.style="stars") #use this specification!!

tab_model(dip10base.lmer, dip10.lmer, trade10base.lmer, trade10.lmer, aidOut10base.lmer, aidOut10.lmer, aidIn10base.lmer, aidIn10.lmer,
          digits=3,
          show.ci=FALSE,
          p.style="stars")

######################################################
#doing separate regressions for the devCat categories#
######################################################
cDatL <- cDat %>% filter(devCat=="L")
cDatLM <- cDat %>% filter(devCat=="LM")
cDatUM <- cDat %>% filter(devCat=="UM")
cDatH <- cDat %>% filter(devCat=="H")

#for trade
tradeH <- lmer(TFR ~ year + log(GNI) + log(GNI):centTrade + centTrade + (1|cc), data=cDatH)
tradeUM <- lmer(TFR ~ year + log(GNI) + log(GNI):centTrade + centTrade + (1|cc), data=cDatUM)
tradeLM <- lmer(TFR ~ year + log(GNI) + log(GNI):centTrade + centTrade + (1|cc), data=cDatLM)
tradeL <- lmer(TFR ~ year + log(GNI) + log(GNI):centTrade + centTrade + (1|cc), data=cDatL)

tab_model(tradeH, tradeUM, tradeLM, tradeL, show.ci=F, digits=3, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low", 
                                                                             show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
                                                                             title="Unpooled Linear Mixed Model Regression of Trade Centrality On Total Fertility Rate With Country-Level Grouping From 1962 - 2020")
)

          #for aid
          aidH <- lmer(TFR ~ year + log(GNI) + log(GNI):centAidOut + centAidOut + (1|cc), data=cDatH)
          aidUM <- lmer(TFR ~ year + log(GNI) + log(GNI):centAidOut + centAidOut + (1|cc), data=cDatUM)
          aidLM <- lmer(TFR ~ year + log(GNI) + log(GNI):centAidIn + centAidIn + (1|cc), data=cDatLM)
          aidL <- lmer(TFR ~ year + log(GNI) + log(GNI):centAidIn + centAidIn + (1|cc), data=cDatL)
          
          tab_model(aidH, aidUM, aidLM, aidL, show.ci=F, digits=3, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
                    show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
                    title="Unpooled Linear Mixed Model Regression of Aid Centrality On Total Fertility Rate With Country-Level Grouping From 1960 - 2019")
          
          #for dipEx
          dipExH <- lmer(TFR ~ year + log(GNI) + log(GNI):centDipEx + centDipEx + (1|cc), data=cDatH)
          dipExUM <- lmer(TFR ~ year + log(GNI) + log(GNI):centDipEx + centDipEx + (1|cc), data=cDatUM)
          dipExLM <- lmer(TFR ~ year + log(GNI) + log(GNI):centDipEx + centDipEx + (1|cc), data=cDatLM)
          dipExL <- lmer(TFR ~ year + log(GNI) + log(GNI):centDipEx + centDipEx + (1|cc), data=cDatL)
          
          tab_model(dipExH, dipExUM, dipExLM, dipExL, show.ci=F, digits=3, dv.labels=c("High", "Upper Middle", "Lower Middle", "Low"),
                    show.ci = FALSE, digits=3, p.style="stars", show.p=FALSE,
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
          
          
          
          #plotting correlation of GNI and centrality by country
          ggplot(cDat, aes(centTrade, GNI)) + geom_line() + facet_wrap(~cc)
          
          #plotting to see group-level variation
          ggplot(cDat, aes(centTrade, TFR)) +
            geom_line() +
            facet_wrap(~cc)
          
          cDat %>% filter(!is.na(devCat)) %>% ggplot(., aes(centDipEx, TFR)) + geom_point() + facet_wrap(~devCat) #ignore dipEx?
          cDat %>% filter(!is.na(devCat)) %>% ggplot(., aes(centTrade, TFR)) + geom_point() + facet_wrap(~devCat) 
          cDat %>% filter(!is.na(devCat)) %>% ggplot(., aes(centAidOut, TFR)) + geom_point() + facet_wrap(~devCat)
          
          devCentDipEx <- cDat %>% filter(!is.na(devCat)) %>% ggplot(., aes(centDipEx)) + geom_histogram(binwidth=1) + facet_wrap(~devCat)
          devCentTrade <- cDat %>% filter(!is.na(devCat)) %>% ggplot(., aes(centTrade)) + geom_histogram(binwidth=1) + facet_wrap(~devCat) 
          devCentAidOut <- cDat %>% filter(!is.na(devCat)) %>% ggplot(., aes(centAidOut)) + geom_histogram(binwidth=1) + facet_wrap(~devCat)
          
          grid.arrange(devCentDipEx, devCentTrade, devCentAidOut)
          
          #what is the distribution of centrality like across time periods?
          ggplot(cDat, aes(centTrade)) + geom_histogram(binwidth=1) + facet_wrap(~year)
          
          #how much centrality changes over time
          cDat <- cDat %>% group_by(cc) %>% mutate(changeTrade=centTrade-lag(centTrade),
                                                   changeAidIn=centAidIn-lag(centAidIn),
                                                   changeAidOut=centAidOut-lag(centAidOut),
                                                   changeDipEx=centDipEx-lag(centDipEx)) %>%
            ungroup()
          
          #change for dipEx
          changeDipEx<- cDat %>% filter(changeDipEx!=0) %>%
            ggplot(., aes(changeDipEx)) + geom_histogram(binwidth=0.1)+
            xlim(-40,40)
          
          #change for Trade
          changeTrade <- cDat %>% filter(changeTrade!=0) %>%
            ggplot(., aes(changeTrade)) + geom_histogram(binwidth=0.1)+
            xlim(-40,40)
          
          #change for aidIn
          cDat %>% filter(changeAidIn!=0) %>%
            ggplot(., aes(changeAidIn)) + geom_histogram(binwidth=0.1) +
            xlim(-40,40)
          
          #change for aidOut
          changeAidOut <- cDat %>% filter(changeAidOut!=0) %>%
            ggplot(., aes(changeAidOut)) + geom_histogram(binwidth=0.1)+
            xlim(-40,40)
          
          grid.arrange(changeDipEx, changeTrade, changeAidOut)
          
          
          
          cDat %>% filter(!is.na(devCat)) %>% ggplot(aes(x = centAidOut, y = TFR)) + 
            geom_point() + 
            facet_wrap(~factor(devCat, levels = c("H", "UM", "LM", "L"))) +
            labs(title="Relationship between weighted outdegree aid centrality and TFR, by WB developmental status") +
            theme(plot.title = element_text(size = 25),
                  axis.title=element_text(size=20),
                  axis.text=element_text(size=15))
          
          cDat %>% filter(!is.na(devCat)) %>% ggplot(aes(x = centAidIn, y = TFR)) + 
            geom_point() + 
            facet_wrap(~factor(devCat, levels = c("H", "UM", "LM", "L"))) +
            labs(title="Relationship between weighted indegree aid centrality and TFR, by WB developmental status") +
            theme(plot.title = element_text(size = 25),
                  axis.title=element_text(size=20),
                  axis.text=element_text(size=15))
          
          cDat %>% filter(!is.na(devCat)) %>% ggplot(aes(x = centTrade, y = TFR)) + 
            geom_point() + 
            facet_wrap(~factor(devCat, levels = c("H", "UM", "LM", "L"))) +
            labs(title="Relationship between trade betweenness centrality and TFR, by WB developmental status") +
            theme(plot.title = element_text(size = 25),
                  axis.title=element_text(size=20),
                  axis.text=element_text(size=15))
          
          cDat %>% filter(!is.na(devCat)) %>% ggplot(aes(x = centDipEx, y = TFR)) + 
            geom_point() + 
            facet_wrap(~factor(devCat, levels = c("H", "UM", "LM", "L"))) +
            labs(title="Relationship between diplomatic exchange Katz centrality and TFR, by WB developmental status") +
            theme(plot.title = element_text(size = 25),
                  axis.title=element_text(size=20),
                  axis.text=element_text(size=15))
          
          
          
          
          library(plm)
          randomAid <- plm(TFR ~ centAidOut
                           + devCat 
                           # + devCat*centAid #cannot include because collinearity
                           , index=c("cc", "year"), model="random", data=cDat)
          
          fixedAid <- plm(TFR ~ centAidOut
                          + devCat + centAidOut*devCat
                          #+ devCat*region
                          # + factor(year)
                          , index=c("cc", "year"), model="within"
                          , data=cDat)
          
          fixedAidTime <- plm(TFR ~ centAidOut
                              + devCat + centAidOut*devCat
                              , index=c("cc", "year"), model="within", 
                              effect="time",
                              data=cDat)
          
          fixedGNI <- plm(TFR ~ centAidOut
                          + log(GNI) + centAidOut*log(GNI)
                          , index=c("cc", "year"), model="within", 
                          effect="time",
                          data=cDat)
          
          phtest(fixedAid, randomAid) #use fixed effects
          pbgtest(fixedAid) # there is serial correlation
          plmtest(fixedAid, c("time"), type=("bp")) # need to use time-fixed effects i.e. fixedAidTime
          summary(fixedAidTime)
          summary(randomAid)
          
          m2Aid <- plm(TFR ~ centAid 
                       + rgdpna
                       + rgdpna*centAid
                       + factor(region)*centAid 
                       + factor(year)
                       
                       , index=c("cc", "year"), 
                       effect="twoways",
                       data=cDat)
          
          m1Trade <- plm(TFR ~ centTrade
                         + devCat
                         + rgdpna
                         + factor(year)
                         , index=c("cc", "year"), model="within", data=cDat)
          
          m2Trade <- plm(TFR ~ centTrade 
                         #+ factor(clustTrade)
                         #+ centTrade*clustTrade 
                         + rgdpna
                         #+ rgdpna*centTrade
                         + factor(region)
                         + factor(region)*centTrade
                         , index=c("cc", "year"), model="within",
                         effect="twoways",
                         data=cDat)
          
          m3Trade <- plm(TFR ~ centTrade 
                         #+ factor(clustTrade)
                         #+ centTrade*clustTrade 
                         + rgdpna
                         , index=c("cc", "year"), model="within",
                         effect="twoways",
                         data=cDat)
          
          
          summary(m1Aid)
          
          summary(m1Aid)
          summary(m2Aid)
          
          summary(m1Trade)
          summary(m2Trade)
          
          #cross-sectional for 2019
          cDat2019 <- cDat %>% filter(year==2018)
          summary(lm(TFR ~ centTrade + factor(clustTrade) + rgdpna, data=cDat2019))
          summary(lm(TFR ~ centAid + factor(clustAid) + rgdpna, data=cDat2019))
          
          library(stargazer)
          stargazer(m1Aid, type="html", out="aid1.html")
          stargazer(m2Aid, type="html", out="aid2.html")
          stargazer(m2Trade, type="html", out="trade2.html")
          stargazer(m3Trade, type="html", out="trade3.html")
          
          
          #Generate a new variable for whether you're in a certain country's cluster or not:
          #see if the person you're in a cluster with matters for your TFR
          #countries to test for aid: UK, US, Japan, France, Germany, Netherlands, Saudie, UAE, China(?only 2000-2017)
          
          
          #US
          
          aidCLUSA <- aidCL %>% group_by(year) %>%
            mutate(USAc=clustAid[cc=="USA"]) %>%
            mutate(clustAidUSA=case_when(clustAid==USAc ~ "sameUSA",
                                         clustAid!=USAc ~ "diffUSA"))
          
          
          cDat <- left_join(wpp, tradeCL, by=c("cc", "year"="year")) %>%
            left_join(aidCLUSA, by=c("cc", "year")) %>%
            left_join(pwtgdp, by=c("cc"="countrycode", "year")) %>%
            filter(year %in% 1960:2020)
          
          cDat <- left_join(cDat, ccodeALL, by=c("cc"="cowc", "year")) %>%
            mutate(region=relevel(as.factor(region),"Europe & Central Asia"))
          
          row.names(cDat) <-NULL
          
          m1AidUSA <- plm(TFR ~ centAid 
                          #+ factor(clustAidUSA) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          m2AidUSA <- plm(TFR ~ centAid 
                          + factor(clustAidUSA) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          summary(m1AidUSA)
          summary(m2AidUSA)
          
          #Japan
          
          aidCLJPN <- aidCL %>% group_by(year) %>%
            mutate(JPNc=clustAid[cc=="JPN"]) %>%
            mutate(clustAidJPN=case_when(clustAid==JPNc ~ "sameJPN",
                                         clustAid!=JPNc ~ "diffJPN"))
          
          
          cDat <- left_join(wpp, tradeCL, by=c("cc", "year"="year")) %>%
            left_join(aidCLJPN, by=c("cc", "year")) %>%
            left_join(pwtgdp, by=c("cc"="countrycode", "year")) %>%
            filter(year %in% 1960:2020)
          
          cDat <- left_join(cDat, ccodeALL, by=c("cc"="cowc", "year")) %>%
            mutate(region=relevel(as.factor(region),"Europe & Central Asia"))
          
          row.names(cDat) <-NULL
          
          m1AidJPN <- plm(TFR ~ centAid 
                          #+ factor(clustAidJPN) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          m2AidJPN <- plm(TFR ~ centAid 
                          + factor(clustAidJPN) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          summary(m1AidJPN)
          summary(m2AidJPN)
          
          
          #France
          
          aidCLFRA <- aidCL %>% group_by(year) %>%
            mutate(FRAc=clustAid[cc=="FRA"]) %>%
            mutate(clustAidFRA=case_when(clustAid==FRAc ~ "sameFRA",
                                         clustAid!=FRAc ~ "diffFRA"))
          
          
          cDat <- left_join(wpp, tradeCL, by=c("cc", "year"="year")) %>%
            left_join(aidCLFRA, by=c("cc", "year")) %>%
            left_join(pwtgdp, by=c("cc"="countrycode", "year")) %>%
            filter(year %in% 1960:2020)
          
          cDat <- left_join(cDat, ccodeALL, by=c("cc"="cowc", "year")) %>%
            mutate(region=relevel(as.factor(region),"Europe & Central Asia"))
          
          row.names(cDat) <-NULL
          
          m1AidFRA <- plm(TFR ~ centAid 
                          #+ factor(clustAidFRA) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          m2AidFRA <- plm(TFR ~ centAid 
                          + factor(clustAidFRA) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          summary(m1AidFRA)
          summary(m2AidFRA)
          
          #Netherlands
          aidCLNLD <- aidCL %>% filter(year!=1966) %>% group_by(year) %>%
            mutate(NLDc=clustAid[cc=="NLD"]) %>%
            mutate(clustAidNLD=case_when(clustAid==NLDc ~ "sameNLD",
                                         clustAid!=NLDc ~ "diffNLD"))
          
          
          cDat <- left_join(wpp, tradeCL, by=c("cc", "year"="year")) %>%
            left_join(aidCLNLD, by=c("cc", "year")) %>%
            left_join(pwtgdp, by=c("cc"="countrycode", "year")) %>%
            filter(year %in% 1960:2020) 
          
          cDat <- left_join(cDat, ccodeALL, by=c("cc"="cowc", "year")) %>%
            mutate(region=relevel(as.factor(region),"Europe & Central Asia"))
          
          row.names(cDat) <-NULL
          
          m1AidNLD <- plm(TFR ~ centAid 
                          #+ factor(clustAidNLD) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          m2AidNLD <- plm(TFR ~ centAid 
                          + factor(clustAidNLD) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          summary(m1AidNLD)
          summary(m2AidNLD)
          
          #UAE
          aidCLARE <- aidCL %>% group_by(year) %>% filter(year!=c(1965)) %>%
            mutate(AREc=clustAid[cc=="ARE"]) %>%
            mutate(clustAidARE=case_when(clustAid==AREc ~ "sameARE",
                                         clustAid!=AREc ~ "diffARE"))
          
          
          cDat <- left_join(wpp, tradeCL, by=c("cc", "year"="year")) %>%
            left_join(aidCLARE, by=c("cc", "year")) %>%
            left_join(pwtgdp, by=c("cc"="countrycode", "year")) %>%
            filter(year %in% 1960:2020)
          
          cDat <- left_join(cDat, ccodeALL, by=c("cc"="cowc", "year")) %>%
            mutate(region=relevel(as.factor(region),"Europe & Central Asia"))
          
          row.names(cDat) <-NULL
          
          m1AidARE <- plm(TFR ~ centAid 
                          #+ factor(clustAidARE) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          m2AidARE <- plm(TFR ~ centAid 
                          + factor(clustAidARE) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          summary(m1AidARE)
          summary(m2AidARE)
          
          #UK
          aidCLGBR <- aidCL %>% group_by(year) %>%
            mutate(GBRc=clustAid[cc=="GBR"]) %>%
            mutate(clustAidGBR=case_when(clustAid==GBRc ~ "sameGBR",
                                         clustAid!=GBRc ~ "diffGBR"))
          
          
          cDat <- left_join(wpp, tradeCL, by=c("cc", "year"="year")) %>%
            left_join(aidCLGBR, by=c("cc", "year")) %>%
            left_join(pwtgdp, by=c("cc"="countrycode", "year")) %>%
            filter(year %in% 1960:2020)
          
          cDat <- left_join(cDat, ccodeALL, by=c("cc"="cowc", "year")) %>%
            mutate(region=relevel(as.factor(region),"Europe & Central Asia"))
          
          row.names(cDat) <-NULL
          
          m1AidGBR <- plm(TFR ~ centAid 
                          #+ factor(clustAidGBR) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          m2AidGBR <- plm(TFR ~ centAid 
                          + factor(clustAidGBR) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          summary(m1AidGBR)
          summary(m2AidGBR)
          
          #China?
          aidCLCHN <- aidCL %>% group_by(year) %>% filter(year %in% c(2000:2017)) %>%
            mutate(CHNc=clustAid[cc=="CHN"]) %>%
            mutate(clustAidCHN=case_when(clustAid==CHNc ~ "sameCHN",
                                         clustAid!=CHNc ~ "diffCHN"))
          
          
          cDat <- left_join(wpp, tradeCL, by=c("cc", "year"="year")) %>%
            left_join(aidCLCHN, by=c("cc", "year")) %>%
            left_join(pwtgdp, by=c("cc"="countrycode", "year")) %>%
            filter(year %in% 1960:2020)
          
          cDat <- left_join(cDat, ccodeALL, by=c("cc"="cowc", "year")) %>%
            mutate(region=relevel(as.factor(region),"Europe & Central Asia"))
          
          row.names(cDat) <-NULL
          
          m1AidCHN <- plm(TFR ~ centAid 
                          #+ factor(clustAidCHN) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          m2AidCHN <- plm(TFR ~ centAid 
                          + factor(clustAidCHN) 
                          + rgdpna
                          , index=c("cc", "year"), model="within", effect="twoways",
                          data=cDat)
          
          summary(m1AidCHN)
          summary(m2AidCHN)
          
          stargazer(m2AidUSA,
                    m2AidGBR,
                    m2AidJPN,
                    m2AidFRA,
                    m2AidNLD,
                    m2AidARE,
                    m2AidCHN,
                    type="html", out="aid2.html")
          
#######################################################################
### HIERARCHICAL GROWTH MODEL #########################################
#######################################################################

library(multilevel)

null <- lme(TFR ~ 1 , random =~1|cc, data=cDat, na.action=na.omit)
VarCorr(null)   
2.510869 /(2.510869 +1.416244) #gives the ICC
#what we just did is the same as doing:
ICC1(aov(TFR~as.factor(cc), data=cDat))
#does it do betetr thana model that doesn't group by country?
null.gls <- gls(TFR ~ 1, data=cDat, na.action=na.omit, control=list(opt="optim"))
anova(null.gls, null)

time <- lme(TFR~time, random=~1|cc, data=cDat)
time2 <- lme(TFR~time+I(time^2), random=~1|cc, data=cDat)
time3 <- lme(TFR~time+I(time^2)+I(time^3), random=~1|cc, data=cDat)
time4 <- lme(TFR~time+I(time^2)+I(time^3)+I(time^4), random=~1|cc, data=cDat)
time5<- lme(TFR~time+I(time^2)+I(time^3)+I(time^4)+I(time^5), random=~1|cc, data=cDat)

timeRan <- update(time, random=~ 1+ time|cc)
timeRan2 <- update(timeRan, ~. + I(time^2), 
                   random=~ 1 + time+I(time^2)|cc)
timeRan3 <- update(timeRan2, ~. + I(time^3), 
                   random=~ 1 + time+I(time^2)+I(time^3)|cc)
timeRan4 <- update(timeRan3, ~. + I(time^4),
                   random=~ 1 + time+I(time^2)+I(time^3)+I(time^4)|cc) #couldn't converge--whatever

anova(time, timeRan, time2, timeRan2, time3, timeRan3, time4)
anova(time, timeRan)
anova(time2, timeRan2)
anova(time3, timeRan3) #use random time up to time^3

#allowing for autocorrelation and lag
library(fUnitRoots)
library(dynamac)
library(tseries)
adfTest(cDat$TFR, lags=1) #stationery
adfTest(cDat$GNI, lags=1) #not stationery
adfTest(cDat$GNI, )

adfTFR <- cDat %>% group_by(cc) %>%
  summarise(p_val=ifelse(n() <=3, NA, adf.test(TFR)$p.value)) #most cannot reject null hypothesis


adfGNI <- cDat %>% group_by(cc) %>%
  drop_na(GNI) %>%
  summarise(p_val=ifelse(n() <=3, NA, adf.test(GNI)$p.value)) #most cannot reject null hypothesis

#creating lagged value for GNI and TFR
cDat <- cDat %>% group_by(cc) %>% mutate(GNIlag1=lag(GNI),
                                            TFRlag1 = lag(TFR))

timeRan3lag <- update(timeRan3, ~. + GNIlag1 + TFRlag1, na.action=na.omit) #TFRlag1 is insignificant
timeRan3lagGNI <- update(timeRan3, ~. + GNIlag1, na.action=na.omit)
timeRan4lagTFR <- update(timeRan3, ~. + TFRlag1, na.action=na.omit)
timeRan3lagTFR <- update(timeRan3, ~. + TFRlag1, na.action=na.omit)

timeRan3AR <- update(timeRan3, correlation=corAR1()) #doesn't converge
timeRan2AR <- update(timeRan2, correlation=corAR1()) #doesn't converge
timeRanAR <- update(timeRan, correlation=corAR1()) #doens't converge?
#apparently, the model sometimes cannot distinguish between AR and random slope so choose either

time3AR <- update(time3, correlation=corAR1())
time3ARCont <- update(time3,correlation=corCAR1())
anova(time3, time3AR, timeRan3, time3ARCont)
anova(timeRan2, timeRan2AR)
#timeRan3lagTFR has the best BIC

###### Heterogenous variance over time ###########
timeRan3lagTFR.b <- update(timeRan3lagTFR, weights=varExp(form=~time))
anova(timeRan3lagTFR, timeRan3lagTFR.b) #allowing for heterogenous variance is a better fit

#allow for spatial autocorrelation too
timeRan3lagTFR.c <- update(timeRan3lagTFR.b, correlation=corGaus(form=~1|cc), weights=varExp(form=~time)) #this one has best BIC
timeRan3lagTFR.d <- update(timeRan3lagTFR.b, correlation=corGaus(form=~1|cc), weights=varExp(form=~time))
## time to add in centrality! ##
trade <- lme(TFR ~ tradeBetS +
               time + I(time^2) + I(time^3), #+ TFRlag1,
             random=~1 + time + I(time^2) + I(time^3) | cc,
             na.action=na.omit,
             weights=varExp(form=~time),
             correlation=corGaus(form=~1|cc),
             data=cDat
             )

trade2 <- lme(TFR ~ tradeBetS +
               time + I(time^2) + I(time^3), #+ TFRlag1,
             random=~1 + time + I(time^2) + I(time^3) | cc,
             na.action=na.omit,
             weights=varExp(form=~time),
             #correlation=corGaus(form=~1|cc),
             data=cDat
)

tradeGNI <- lme(TFR ~ tradeBetS + GNI +
               time + I(time^2) + I(time^3), + TFRlag1,
             random=~1 + time + I(time^2) + I(time^3) | cc,
             na.action=na.omit,
             correlation=corGaus(form=~1|cc),
             weights=varExp(form=~time),
             data=cDat
)

tradeGNI2 <- lme(TFR ~ tradeBetS + GNI +
                  time + I(time^2) + I(time^3) + TFRlag1,
                random=~1 + time + I(time^2) + I(time^3) | cc,
                na.action=na.omit,
                #correlation=corGaus(form=~1|cc),
                weights=varExp(form=~time),
                data=cDat
)

tradeInt <- lme(TFR ~ tradeBetS + GNI + tradeBetS:GNI +
               time + I(time^2) + I(time^3) + TFRlag1,
             random=~1 + time + I(time^2) + I(time^3) | cc,
             na.action=na.omit,
             correlation=corGaus(form=~1|cc),
             weights=varExp(form=~time),
             data=cDat,
             control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)
)

tradeInt2 <- lme(TFR ~ tradeBetS + GNI + tradeBetS:GNI +
                time + I(time^2) + I(time^3) + TFRlag1,
                random=~1 + time + I(time^2) + I(time^3) | cc,
                na.action=na.omit,
                #correlation=corGaus(form=~1|cc),
                weights=varExp(form=~time),
                data=cDat
)

tradeInt2RanTime2 <- lme(TFR ~ tradeBetS + GNI + tradeBetS:GNI +
                   time + I(time^2) +# I(time^3) + 
                     TFRlag1,
                 random=~1 + time + I(time^2) #+ I(time^3) 
                 | cc,
                 na.action=na.omit,
                 #correlation=corGaus(form=~1|cc),
                 weights=varExp(form=~time),
                 data=cDat,
                 control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)
)

tradeInt2RanTime3 <- lme(TFR ~ tradeBetS + GNI + tradeBetS:GNI +
                           time + I(time^2) + I(time^3) + 
                           TFRlag1,
                         random=~1 + time + I(time^2) + I(time^3) 
                         | cc,
                         na.action=na.omit,
                         #correlation=corGaus(form=~1|cc),
                         weights=varExp(form=~time),
                         data=cDat,
                         control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)
)

anova(tradeInt2RanTime2, tradeInt2RanTime3)

tab_model(trade, trade2, tradeGNI, tradeGNI2, tradeInt, tradeInt2)

#it's called a growth model because you have TIME nested within individuals. typically, individuals are nested in other things, 

##trying for dipEx
dipExInt <- lme(TFR ~ dipWeightEig + GNI + tradeBetS:GNI +
                  time + I(time^2) + I(time^3), # + TFRlag1,
                random=~1 + time + I(time^2) + I(time^3) | cc,
                na.action=na.omit,
                correlation=corGaus(form=~1|cc),
                weights=varExp(form=~time),
                data=cDat,
                control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)
)

dipExInt2 <- lme(TFR ~ dipWeightEig + GNI + tradeBetS:GNI +
                   time + I(time^2) + I(time^3), # + TFRlag1,
                 random=~1 + time + I(time^2) + I(time^3) | cc,
                   na.action=na.omit,
                   #correlation=corGaus(form=~1|cc),
                   weights=varExp(form=~time),
                   data=cDat,
                 control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)
)

tab_model(dipExInt, dipExInt2)

aidInt <- lme(TFR ~ aidInDeg + GNI + tradeBetS:GNI +
                  time + I(time^2) + I(time^3), # + TFRlag1,
                random=~1 + time + I(time^2) + I(time^3) | cc,
                na.action=na.omit,
                correlation=corGaus(form=~1|cc),
                weights=varExp(form=~time),
                data=cDat,
              control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)
)

aidInt2 <- lme(TFR ~ aidInDeg + GNI + tradeBetS:GNI +
                 time + I(time^2) + I(time^3), # + TFRlag1,
                 random=~1 + time + I(time^2) + I(time^3) | cc,
                 na.action=na.omit,
                 #correlation=corGaus(form=~1|cc),
                 weights=varExp(form=~time),
                 data=cDat
)

tab_model(dipExInt, dipExInt2, tradeInt, tradeInt2, aidInt, aidInt2)

formulaFix <- "TFR~cent+logGNI+cent:logGNI"
formulaRan <- "~1"
group <- "cc"
DV <- "TFR"

lmeFn <- function(DV, cent, timeFixN, timeRanN, formulaFix, formulaRan, group) {
  
  timeFixVec <- c("+time")
  if (timeFixN>1) {
  for (i in 2:timeFixN) {
   timeFixVec <- paste0(timeFixVec,"+", "time^", as.character(i))
  }
  } else if (timeFixN==0) {
    timeFixVec <- c()
  }
  
  timeRanVec <- c("+time")
  if (timeRanN>1) {
  for (i in 2:timeRanN) {
    timeRanVec <- paste0(timeRanVec, "+", "time^", as.character(i))
  }
  } else if (timeRanN==0) {
    timeRanVec <- c()
  }
  
  formulaFixTemp <- stri_replace_all_regex(formulaFix,
                                           pattern=c("cent","DV"),
                                           replacement=c(cent, DV),
                                           vectorize_all=FALSE)
                         
  
  mod <- do.call("lme", list(
    fixed = as.formula(paste0( formulaFixTemp, timeFixVec)),
    random= as.formula(paste0(formulaRan, timeRanVec,  "|", group)),
    data=quote(cDat),
    na.action=na.omit,
    #correlation=corGaus(form=~1|cc),
    control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)
  ))
  
}

## PHACKING! ##
pHackFn <- function(typeCols, timeFixN, timeRanN) { 
  #assign(paste0(pHack, ".", sub("Cols", "", typeCols), "F", timeFixN, "R", timeRanN), data.frame())
  pHackTab <- data.frame()
  
  for (i in 1:length(typeCols)) {
    print(typeCols[i])
    regOut <- lmeFn("TFR", typeCols[i], timeFixN, timeRanN, formulaFix, formulaRan, group)
   pHackTab <- rbind(pHackTab,
                    cbind(type=rownames(summary(regOut)$tTable)[2],
                      coef= rownames(summary(regOut)$tTable), 
                      summary(regOut)$tTable[,c("Value","p-value")]))
    print(paste(deparse(substitute(typeCols)), i, "of", length(typeCols), "done"))
    }
  return(pHackTab)
}

dipCols <- colnames(cDat)[startsWith(colnames(cDat), "dip")]
pHack.dipF0R1 <- pHackFn(dipCols, 0,1)
pHack.dipF0R1$spec <- "dipF0R1"
pHack.dipF1R1 <- pHackFn(dipCols, 1,1)
pHack.dipF1R1$spec <- "dipF1R1"

tradeCols <- colnames(cDat)[startsWith(colnames(cDat), "trade")]
pHack.tradeF0R1 <- pHackFn(tradeCols, 0, 1)
pHack.tradeF0R1$spec <- "tradeF0R1"
pHack.tradeF1R1 <- pHackFn(tradeCols, 1, 1)
pHack.tradeF1R1$spec <- "tradeF1R1"

aidCols <- colnames(cDat)[startsWith(colnames(cDat), "aid")]
pHack.aidF0R1 <- pHackFn(aidCols, 0,1)
pHack.aidF0R1$spec <- "aidF0R1"
pHack.aidF1R1 <- pHackFn(aidCols, 1,1)
pHack.aidF1R1$spec <- "aidF1R1"

pHacks <- bind_rows(pHack.dipF0R1, pHack.dipF1R1,
                    pHack.tradeF0R1, pHack.tradeF1R1,
                    pHack.aidF0R1, pHack.aidF1R1)
pHacks$pval <- as.numeric(pHacks$'p-value')
pHacks$Value <- as.numeric(pHacks$Value)

saveRDS(pHacks, "pHacks.rds") #completed saving 5/4 18:30

#redo for log GDP.um...

formulaFix <- "TFR~cent+logGNI.um+cent:logGNI.um"
dipCols <- colnames(cDat)[startsWith(colnames(cDat), "dip")]
pHack.dipF0R1.log.um  <- pHackFn(dipCols, 0,1)
pHack.dipF0R1.log.um$spec <- "dipF0R1.log.um"
pHack.dipF1R1.log.um <- pHackFn(dipCols, 1,1)
pHack.dipF1R1.log.um$spec <- "dipF1R1.log.um"

tradeCols <- colnames(cDat)[startsWith(colnames(cDat), "trade")]
pHack.tradeF0R1.log.um <- pHackFn(tradeCols, 0, 1)
pHack.tradeF0R1.log$spec <- "tradeF0R1.log.um"
pHack.tradeF1R1.log.um <- pHackFn(tradeCols, 1, 1)
pHack.tradeF1R1.log.um$spec <- "tradeF1R1.log.um"

aidCols <- colnames(cDat)[startsWith(colnames(cDat), "aid")]
pHack.aidF0R1.log.um <- pHackFn(aidCols, 0,1)
pHack.aidF0R1.log.um$spec <- "aidF0R1.log.um"
pHack.aidF1R1.log.um <- pHackFn(aidCols, 1,1)
pHack.aidF1R1.log.um$spec <- "aidF1R1.log.um"

pHack.log.um<- bind_rows(pHack.dipF0R1.log.um, pHack.dipF1R1.log.um,
                         pHack.tradeF0R1.log.um, pHack.tradeF1R1.log.um,
                         pHack.aidF0R1.log.um, pHack.aidF1R1.log.um)
pHack.log.um$pval <- as.numeric(pHack.log.um$'p-value')
pHack.log.um$Value <- as.numeric(pHack.log.um$Value)

saveRDS(pHack.log.um, "pHackLogUM.rds") #saved

#do for logGDP.m...
formulaFix <- "TFR~cent+logGNI.um+cent:logGNI.m"
dipCols <- colnames(cDat)[startsWith(colnames(cDat), "dip")]
pHack.dipF0R1.log.m  <- pHackFn(dipCols, 0,1)
pHack.dipF0R1.log.m$spec <- "dipF0R1.log.m"
pHack.dipF1R1.log.m <- pHackFn(dipCols, 1,1)
pHack.dipF1R1.log.m$spec <- "dipF1R1.log.m"

tradeCols <- colnames(cDat)[startsWith(colnames(cDat), "trade")]
pHack.tradeF0R1.log.m <- pHackFn(tradeCols, 0, 1)
pHack.tradeF0R1.log.m$spec <- "tradeF0R1.log.m"
pHack.tradeF1R1.log.m <- pHackFn(tradeCols, 1, 1)
pHack.tradeF1R1.log.m$spec <- "tradeF1R1.log.m"

aidCols <- colnames(cDat)[startsWith(colnames(cDat), "aid") & str_detect(colnames(cDat), "Katz5", negate=TRUE)]
pHack.aidF0R1.log.m <- pHackFn(aidCols, 0,1)
pHack.aidF0R1.log.m$spec <- "aidF0R1.log.m" 
pHack.aidF1R1.log.m <- pHackFn(aidCols, 1,1) 
pHack.aidF1R1.log.m$spec <- "aidF1R1.log.m"


pHack.log.m<- bind_rows(mget(ls()[startsWith(ls(), "pHack.") & endsWith(ls(), "R1.log.m")]))
pHack.log.m$pval <- as.numeric(pHack.log.m$'p-value')
pHack.log.m$Value <- as.numeric(pHack.log.m$Value)

saveRDS(pHack.log.m, "pHackLogM.rds") #saved 5/6 11:33

#and without centering logGDP...
formulaFix <- "TFR~cent+logGNI.um+cent:logGNI"
dipCols <- colnames(cDat)[startsWith(colnames(cDat), "dip")]
pHack.dipF0R1.log.none  <- pHackFn(dipCols, 0,1)
pHack.dipF0R1.log.none$spec <- "dipF0R1.log.none"
pHack.dipF1R1.log.none <- pHackFn(dipCols, 1,1)
pHack.dipF1R1.log.none$spec <- "dipF1R1.log.none"

tradeCols <- colnames(cDat)[startsWith(colnames(cDat), "trade")]
pHack.tradeF0R1.log.none <- pHackFn(tradeCols, 0, 1)
pHack.tradeF0R1.log.none$spec <- "tradeF0R1.log.none"
pHack.tradeF1R1.log.none <- pHackFn(tradeCols, 1, 1)
pHack.tradeF1R1.log.none$spec <- "tradeF1R1.log.none"

aidCols <- colnames(cDat)[startsWith(colnames(cDat), "aid") & str_detect(colnames(cDat), "Katz5", negate=TRUE)]
pHack.aidF0R1.log.none <- pHackFn(aidCols, 0,1) #singular convergence error with aidKatz5.0.m:logGNI
pHack.aidF0R1.log.none$spec <- "aidF0R1.log.none"
pHack.aidF1R1.log.none <- pHackFn(aidCols, 1,1) #problem with aidKatz5.0.um:GNI
pHack.aidF1R1.log.none$spec <- "aidF1R1.log.none"

#haven't saved because of aid
pHack.log.none<- bind_rows(mget(ls()[startsWith(ls(), "pHack.") & endsWith(ls(), "R1.log.none")]))
pHack.log.none$pval <- as.numeric(pHack.log.none$'p-value')
pHack.log.none$Value <- as.numeric(pHack.log.none$Value)

saveRDS(pHack.log.none, "pHackLogNone.rds")

##insert bit for no time in random effect--use logGDP for all because seems to hvae better results across the board
##LOGGNI.UM
formulaFix <- "TFR~cent+logGNI.um+cent:logGNI.um"

pHack.dipF0R0.log.um <- pHackFn(dipCols, 0,0)
pHack.dipF0R0.log.um$spec <- "dipF0R0.log.um"
pHack.dipF1R0.log.um <- pHackFn(dipCols, 1,0)
pHack.dipF1R0.log.um$spec <- "dipF1R0.log.um"

pHack.tradeF0R0.log.um <- pHackFn(tradeCols, 0,0)
pHack.tradeF0R0.log.um$spec <- "tradeF0R0.log.um"
pHack.tradeF1R0.log.um <- pHackFn(tradeCols, 1,0)
pHack.tradeF1R0.log.um$spec <- "tradeF1R0.log.um"

pHack.aidF0R0.log.um <- pHackFn(aidCols, 0,0)
pHack.aidF0R0.log.um$spec <- "aidF0R0.log.um"
pHack.aidF1R0.log.um <- pHackFn(aidCols, 1,0)
pHack.aidF1R0.log.um$spec <- "aidF1R0.log.um"    

##LOGGNI.M
formulaFix <- "TFR~cent+logGNI.um+cent:logGNI.m"

pHack.dipF0R0.log.m <- pHackFn(dipCols, 0,0)
pHack.dipF0R0.log.m$spec <- "dipF0R0.log.m"
pHack.dipF1R0.log.m <- pHackFn(dipCols, 1,0)
pHack.dipF1R0.log.m$spec <- "dipF1R0.log.m" 

pHack.tradeF0R0.log.m <- pHackFn(tradeCols, 0,0)
pHack.tradeF0R0.log.m$spec <- "tradeF0R0.log.m"
pHack.tradeF1R0.log.m <- pHackFn(tradeCols, 1,0)
pHack.tradeF1R0.log.m$spec <- "tradeF1R0.log.m"

pHack.aidF0R0.log.m <- pHackFn(aidCols, 0,0)
pHack.aidF0R0.log.m$spec <- "aidF0R0.log.m"
pHack.aidF1R0.log.m <- pHackFn(aidCols, 1,0)
pHack.aidF1R0.log.m$spec <- "aidF1R0.log.m"   


##LOGGNI.NONE
formulaFix <- "TFR~cent+logGNI+cent:logGNI"

pHack.dipF0R0.log.none <- pHackFn(dipCols, 0,0)
pHack.dipF0R0.log.none$spec <- "dipF0R0.log.none"
pHack.dipF1R0.log.none <- pHackFn(dipCols, 1,0)
pHack.dipF1R0.log.none$spec <- "dipF1R0.log.none"

pHack.tradeF0R0.log.none <- pHackFn(tradeCols, 0,0)
pHack.tradeF0R0.log.none$spec <- "tradeF0R0.log.none"
pHack.tradeF1R0.log.none <- pHackFn(tradeCols, 1,0)
pHack.tradeF1R0.log.none$spec <- "tradeF1R0.log.none"

pHack.aidF0R0.log.none <- pHackFn(aidCols, 0,0)
pHack.aidF0R0.log.none$spec <- "aidF0R0.log.none"
pHack.aidF1R0.log.none <- pHackFn(aidCols, 1,0)
pHack.aidF1R0.log.none$spec <- "aidF1R0.log.none"

pHack.noR <- bind_rows(mget(ls()[str_detect(ls(),"R0")]))
pHack.noR$pval <- as.numeric(pHack.noR$'p-value')
pHack.noR$Value <- as.numeric(pHack.noR$Value)

saveRDS(pHack.noR, "pHackNoRand.rds") #saved



##### COMPILE ALL PHACKS #####

pHack.all <- data.table::rbindlist(lapply(list.files(pattern=".rds"), readRDS))
#pHack.all <- bind_rows(pHacks, #all F*R1 without logGDP
#                       pHack.log.um, 
#                       pHack.log.m, 
#                       pHack.log.none,
#                       pHack.noR)

sig <- pHack.all %>% filter(pval<0.05) %>% filter(abs(Value)>0.001) %>%
  mutate(absVal=abs(Value)) %>%
  dplyr::select(spec, type, coef, Value, absVal, pval)

gniCoef <- sig %>% filter(str_detect(coef, "GNI"))
sigScale <- sig %>% filter(str_detect(coef, "\\.s.")) %>%
  mutate(network=case_when(startsWith(type, "aid") ~ "aid",
                           startsWith(type, "trade") ~ "trade",
                           startsWith(type, "dip") ~ "dip"))
range(sig$Value, na.rm=T)
sort(table(sigScale$coef))
write.csv(sigScale, "sigScale.csv", row.names=FALSE)
write.csv(gniCoef, "gniCoef.csv", row.names=FALSE)

#best results: (all F0R1 slightly better) -- log gdp WITHOUT centering for log GDP
#dip: dipKatzWeight.s.um. for log.none F0R1 and F1R1
#dipKatzWeight.s.n
#trade: tradeBetDir0.01.s.um for F0R1 and F1R1
#aid: lognone--aidOutDeg0.05.s.um for F0R1 and F1R1; 
#what about s.m?

formulaFix <- "TFR~logGNI+cent+cent:logGNI"
aidIn10 <- lmeFn("TFR", "aidInDeg0.s.um", 1, 0, formulaFix, formulaRan, "cc")
aidIn01 <- lmeFn("TFR", "aidInDeg0.s.um", 0, 1, formulaFix, formulaRan, "cc")
aidIn11 <- lmeFn("TFR", "aidInDeg0.s.um", 1, 1, formulaFix, formulaRan, "cc")

aidOut10 <- lmeFn("TFR", "aidOutDeg0.s.um", 1, 0, formulaFix, formulaRan, "cc")
aidOut01 <- lmeFn("TFR", "aidOutDeg0.s.um", 0, 1, formulaFix, formulaRan, "cc")
aidOut11 <- lmeFn("TFR", "aidOutDeg0.s.um", 1, 1, formulaFix, formulaRan, "cc")

trade10 <- lmeFn("TFR", "tradeBetUndir0.s.um", 1, 0, formulaFix, formulaRan, "cc")
trade01 <- lmeFn("TFR", "tradeBetUndir0.s.um", 0, 1, formulaFix, formulaRan, "cc")
trade11 <- lmeFn("TFR", "tradeBetUndir0.s.um", 1, 1, formulaFix, formulaRan, "cc")

dip10 <- lmeFn("TFR", "dipKatzWeight.s.um", 1, 0, formulaFix, formulaRan, "cc")
dip01 <- lmeFn("TFR", "dipKatzWeight.s.um", 0, 1, formulaFix, formulaRan, "cc")
dip11 <- lmeFn("TFR", "dipKatzWeight.s.um", 1, 1, formulaFix, formulaRan, "cc")

test <- lme(TFR~time+logGNI+aidInDeg0.s.um+aidInDeg0.s.um:logGNI, random=list(cc=~1+time), data=cDat, na.action=na.omit)
test2 <- lmeFn("TFR", "aidInDeg0.s.um", 1, 1, formulaFix, formulaRan, "cc")

#save(list = ls(.GlobalEnv), file = "fertRegOutput.Rdata")

anova(aidIn10, aidIn11)
anova(aidOut10, aidOut11)
anova(trade10, trade11)
anova(dip10, dip11) #to check if time fixed effect relevant, see coefficient for time

#without interaction term
formulaFixBase <- "TFR~logGNI+cent"

aidIn10base <- lmeFn("TFR", "aidInDeg0.s.um", 1, 0, formulaFixBase, formulaRan, "cc")
aidIn01base <- lmeFn("TFR", "aidInDeg0.s.um", 0, 1, formulaFixBase, formulaRan, "cc")
aidIn11base <- lmeFn("TFR", "aidInDeg0.s.um", 1, 1, formulaFixBase, formulaRan, "cc")
aidOut10base <- lmeFn("TFR", "aidInDeg0.s.um", 1, 0, formulaFixBase, formulaRan, "cc")
aidOut01base <- lmeFn("TFR", "aidInDeg0.s.um", 0, 1, formulaFixBase, formulaRan, "cc")
aidOut11base <- lmeFn("TFR", "aidInDeg0.s.um", 1, 1, formulaFixBase, formulaRan, "cc")

trade10base <- lmeFn("TFR", "tradeBetUndir0.s.um", 1, 0, formulaFixBase, formulaRan, "cc")
trade01base <- lmeFn("TFR", "tradeBetUndir0.s.um", 0, 1, formulaFixBase, formulaRan, "cc")
trade11base <- lmeFn("TFR", "tradeBetUndir0.s.um", 1, 1, formulaFixBase, formulaRan, "cc")

dip10base <- lmeFn("TFR", "dipKatzWeight.s.um", 1, 0, formulaFixBase, formulaRan, "cc")
dip01base <- lmeFn("TFR", "dipKatzWeight.s.um", 0, 1, formulaFixBase, formulaRan, "cc")
dip11base <- lmeFn("TFR", "dipKatzWeight.s.um", 1, 1, formulaFixBase, formulaRan, "cc")

anova(aidIn10base, aidIn11base)
anova(aidOut10base, aidOut11base)
anova(trade10base, trade11base)
anova(dip10base, dip11base)

#with time polynomial
aidIn20 <- lmeFn("TFR", "aidInDeg0.05.s.um", 2, 0, formulaFix, formulaRan, "cc")
aidIn21 <- lmeFn("TFR", "aidInDeg0.05.s.um", 2, 1, formulaFix, formulaRan, "cc")
aidIn12 <- lmeFn("TFR", "aidInDeg0.05.s.um", 1, 2, formulaFix, formulaRan, "cc")


aidOut20 <- lmeFn("TFR", "aidOutDeg0.01.s.um", 2, 0, formulaFix, formulaRan, "cc")
aidOut21 <- lmeFn("TFR", "aidOutDeg0.01.s.um", 2, 1, formulaFix, formulaRan, "cc")
aidOut12 <- lmeFn("TFR", "aidOutDeg0.01.s.um", 1, 2, formulaFix, formulaRan, "cc")


trade20 <- lmeFn("TFR", "tradeBetDir0.01.s.um", 2, 0, formulaFix, formulaRan, "cc")
trade21 <- lmeFn("TFR", "tradeBetDir0.01.s.um", 2, 1, formulaFix, formulaRan, "cc")
trade12 <- lmeFn("TFR", "tradeBetDir0.01.s.um", 1, 2, formulaFix, formulaRan, "cc")


dip20 <- lmeFn("TFR", "dipKatzWeight.s.um", 2, 0, formulaFix, formulaRan, "cc")
dip21 <- lmeFn("TFR", "dipKatzWeight.s.um", 2, 1, formulaFix, formulaRan, "cc")
dip12 <- lmeFn("TFR", "dipKatzWeight.s.um", 1, 2, formulaFix, formulaRan, "cc")


anova(aidIn20, aidIn21)
anova(aidOut20, aidOut21)
anova(trade20, trade21)
anova(dip20, dip21)

anova(aidIn11, aidIn12)
  
tab_model(dip10base, dip10, trade10base, trade10, aidOut10base, aidOut10, aidIn10base, aidIn10,
          show.p=TRUE, p.style="stars",
          show.ci=FALSE,
          digits=3
          )

tab_model(dip11base, dip11, trade11base, trade11, aidOut11base, aidOut11, aidIn11base, aidIn11, 
          show.p=TRUE, p.style="stars",
          show.ci=FALSE,
          digits=3
          )

tab_model(dip11, trade11, aidOut11, aidIn11,
          show.p=TRUE, p.style="stars",
          show.ci=FALSE,
          digits=3)

tab_model(dip21, trade21, aidOut21, aidIn21,
          show.p=TRUE, p.style="stars",
          show.ci=FALSE,
          digits=3)

tab_model(dip01, trade01, aidOut01, aidIn01,
          show.p=TRUE, p.style="stars",
          show.ci=FALSE,
          digits=3)



#tradeF0R1 <- lmeFn("TFR", "tradeBetDir0.s.um", 0,1, formulaFix, formulaRan, group)
#tradeF0R0 <- lmeFn("TFR", "tradeBetDir0.s.um", 0,0, formulaFix, formulaRan, group)
#tradeF1R0 <- lmeFn("TFR", "tradeBetDir0.s.um", 1,0, formulaFix, formulaRan, group)
#tradeF1R1 <- lmeFn("TFR", "tradeBetDir0.s.um", 1, 1, formulaFix, formulaRan, group)
#tradeF2R0 <- lmeFn("TFR", "tradeBetDir0.s.um", 2,0, formulaFix, formulaRan, group)
#tradeF2R1 <- lmeFn("TFR", "tradeBetDir0.s.um", 2,1, formulaFix, formulaRan, group)
#tradeF2R2 <- lmeFn("TFR", "tradeBetDir0.s.um", 2,2, formulaFix, formulaRan, group)

#use tradecut? no significant results with everybody in the world


tab_model(tradeM, tradeF0R0, tradeF0R1, tradeF1R0, tradeF1R1, tradeF2R0, tradeF2R1, tradeF2R2, digits=3)
tab_model(tradeM, tradeF2R1, digits=3)

plmFn <- function(DV, cent, group) {
  

  formulaTemp <- stri_replace_all_regex(formulaFix,
                                        pattern=c("cent","DV"),
                                        replacement=c(cent, DV),
                                        vectorize_all=FALSE)
  mod <- do.call("plm", list(
    formula=as.formula(formulaTemp),
    data=quote(cDat),
    model="within",
    effect="individual",
    index=c(as.character(group), "year")
  )
  )
}

plm.Trade <- plmFn("TFR", "m.tradeBet", "cc")
summary(plm.Trade)
