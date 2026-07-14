rm(list=ls())
library(tidyverse)
library(igraph)
library(readr)
library(countrycode)
library(readxl)

aidData <- read_csv("C:/Users/joanm/OneDrive/Documents/Data/OECD/OECD_ODA2024.csv")
chinaData <- read_xlsx("C:/Users/joanm/OneDrive/Documents/Data/AidData/AidDatasGlobalChineseDevelopmentFinanceDataset_v3.0.xlsx",
                      sheet="GCDF_3.0") %>% 
  select('Financier Country', 'Recipient', 'Recipient ISO-3', 'Implementation Start Year' ,'Flow Class', 'Status', 'Amount (Constant USD 2021)') %>%
  filter(`Flow Class`=="ODA-like", Status=="Implementation")


