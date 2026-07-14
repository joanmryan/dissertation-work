rm(list=ls())
set.seed(12345)

library(tidyverse)
library(countrycode)
library(lme4)
library(broom)
library(cowplot)
library(sjPlot)
library(plm)

setwd("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange")
cDat <- readRDS("cDat.rds")

is.pbalanced(cDat) #it's balanced

ggplot(data=cDat, aes(x=year, y=TFR)) +
  geom_line(aes(colour=as.factor(cc))) +
  geom_smooth(method = "lm", se = F, lty = "dashed") +
  theme(legend.position = "none")

feWithin <- plm(TFR ~ centTrade, data=cDat,
                  index = c("cc", "year"),
                  effect = "individual", model="within")

fePooled <- plm(TFR ~ centTrade, data=cDat,
                index = c("cc", "year"),
                effect = "individual", model="pooling")

pFtest(feWithin, fePooled) #shoudl include fixed effects for country

