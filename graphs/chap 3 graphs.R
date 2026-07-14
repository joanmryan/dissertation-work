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

##################
## Loading Data ##
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

####### GRAPHING ############


#overall TFR over time
cDat %>% 
  ggplot(aes(x=year, y=TFR)) +
  geom_line(aes(colour=as.factor(cc))) +
  geom_smooth(method = "lm", se = F, lty = "dashed") +
  theme(legend.position = "none") +
  xlab("Year")





##graphing devCat TFR over time
devCatLabels <- c(`H`="High Income",
                     `UM` = "Upper Middle Income",
                     `LM` = "Lower Middle Income",
                     `L` = "Low Income")
cDat %>% filter(!is.na(devCat)) %>% 
ggplot(aes(x=year, y=TFR)) +
  geom_line(aes(colour=as.factor(cc))) +
  geom_smooth(method = "lm", se = F, lty = "dashed") +
  facet_wrap(~devCat, ncol=2, labeller=as_labeller(devCatLabels)) +
  theme(legend.position = "none") +
  xlab("Year")
