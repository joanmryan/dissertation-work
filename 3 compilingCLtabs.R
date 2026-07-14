rm(list=ls())
library(tidyverse)
library(data.table)

setwd("C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/")

#loads and joins all centrality scores into one table
centList <- centNames<- load("C:/Users/joanm/OneDrive/Documents/Dissertation/Chap1/code/centMeasures.RDS")
centList <- lapply(centNames,get)
names(centList) <- centNames
#save(centList, file="centList.RDS")

centTib <- tibble(centList)
centTib$type <- names(centList)

centTab <- centTib %>% unnest_longer(col=centList, indices_to="graphs") %>%
  unnest_longer(col=centList, indices_to="year") %>%
  unnest_longer(col=centList, indices_to="country") %>%
  rename(centScore = centList) %>%
  mutate(col=paste0(type,sub(".*Graph", "", graphs))) %>%
  pivot_wider(names_from = col, values_from = centScore, id_cols=c(country, year))


#tablize <- function(i) {
#  label <- names(centList)[[i]]
#  bind_rows(centList[[i]], .id="year") %>%
#  pivot_longer(cols=-c(year),
#               values_to= as.character(label)
#               )
#}


#centList2 <- lapply(seq_along(centList), tablize)
#centTab <- centList2 %>% 
#  reduce(full_join, by=c("year", "name")) %>%
#  rename(cc=name) %>%
#  mutate(year=as.numeric(year))

centTab <- centTab %>% rename(cc=country) %>% mutate(year=as.numeric(year))


##adding on other country-level measures
#loading country codes
ccodeCOW <- read.csv("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange/COW country codes.csv")
ccodeALL <- countrycode::codelist_panel %>%
  dplyr::select(year, cowc, cown, iso3c, iso2c, iso3n, un, region, un.region.code)

#creating developmental status category
devStatus <- read_csv("C:/Users/joanm/OneDrive/Documents/Data/devStatus.csv") %>%
  rename(cc=ccode)
devStatus$devCat <- factor(devStatus$devCat, levels= c("H", "UM", "LM", "L"))

#WPP TFR data
wpp <-  read.csv("C:/Users/joanm/OneDrive/Documents/Data/WPP/TFR1960-2022.csv") %>%
  select(cc, timeLabel, value) %>%
  rename(year=timeLabel, TFR=value)

#bring in GDP data
pwtgdp <- read.csv("C:/Users/joanm/OneDrive/Documents/Projects/Diplomatic Exchange/pwtgdp.csv")
pwtgdp <- pwtgdp %>% dplyr::select(rgdpna, countrycode, year) %>% rename(cc=countrycode)

cDat <- list(centTab, wpp, pwtgdp, devStatus) %>% reduce(full_join, by=c("cc", "year"))

saveRDS(cDat, "cDat.rds")
write_csv(cDat, "cDat.csv")
