rm(list=ls())
setwd("C:/Users/joanm/OneDrive/Documents/Dissertation/Chap2/code")

library(tidyverse)
library(countrycode)
library(readxl)
library(haven)
library(multilevel)
library(plm)
library(lme4)
library(stringi)
library(directlabels)


#country code and SSA ref list
ccode <- countrycode::codelist
cComtrade <- read_csv("C:/Users/joanm/OneDrive/Documents/Data/Comtrade Country Code and ISO list.csv")
#really weird discrepancy where antigua and barbuda are named the same as antartica (ATA). it should be atg
cComtrade[cComtrade$CountryName=="Antigua and Barbuda", 6] <- "ATG"
#retrieve countrycode conversion table
ccode <- countrycode::codelist

region <- ccode %>% dplyr::select(country.name.en, un.name.en, iso3c, un.regionsub.name)
SSAcc <- region %>% filter(un.regionsub.name=="Sub-Saharan Africa")
countries <- c("CHN", "FRA", "GBR", "USA")
###################
## PREPPING DATA ##
###################

#comtrade data just for mineral resources
minerals <- read_tsv("C:/Users/joanm/OneDrive/Documents/Data/Comtrade/mineralResourcesComtradeCFGU.txt") %>%
  mutate(Sender=countrycode(ReporterCode,"un","iso3c"),
         Receiver=countrycode(PartnerCode,"un","iso3c")) %>%
  left_join(., cComtrade[,c("CountryCode", "CountryName", "ISO3")], join_by(ReporterCode==CountryCode)) %>%
  mutate(Sender=case_when(is.na(Sender)  ~ ISO3, .default=Sender),
         Receiver=case_when(PartnerCode %in% c(841,842) ~ "USA",
                            PartnerCode==251 ~ "FRA",
                            PartnerCode==0 ~ "WLD",
                            .default=Receiver)) %>%
  filter(Sender %in% SSAcc$iso3c)

mins <- minerals %>% dplyr::select(Period, Sender, Receiver, CmdCode, PrimaryValue) %>%
  rename(Year=Period) %>%
  group_by(Year, Sender, Receiver) %>%
  mutate(Weight=sum(PrimaryValue))  %>% ungroup()  %>%#weight is expressed in millions
  dplyr::select(Year, Sender, Receiver, Weight) %>% distinct() %>% filter(Year<=2020)
#saveRDS(minerals, "minerals.rds")

#bring in trade/aid/dip data
trade <- read_csv("C:/Users/joanm/OneDrive/Documents/Dissertation/data/allTrade1962-2020.csv")[,-1]
aid <- read_csv("C:/Users/joanm/OneDrive/Documents/Dissertation/data/allAid1960-2019.csv")[,-1]
dip <- read_csv("C:/Users/joanm/OneDrive/Documents/Dissertation/data/allDipEx1960-2020.csv")[,-1] %>% rename(Weight=LOR)



#TRADE DATA FOR INTENSITY INDEX #
#################################
#tradeorg <- read_tsv("C:/Users/joanm/OneDrive/Documents/Data/comtrade1962-2019.txt")  %>% rename(Weight=TradeValue)

#trade <- tradeorg %>% dplyr::select(Year, Flow, Reporter, Partner, Weight) %>% filter(Partner!=0) %>%
#  mutate(Reporter2=cComtrade$ISO3[match(Reporter, cComtrade$CountryCode)],
#         Partner2=cComtrade$ISO3[match(Partner, cComtrade$CountryCode)],
#    Sender=case_when(Flow==2 ~ cComtrade$ISO3[match(Reporter, cComtrade$CountryCode)],
#                          Flow==1 ~ cComtrade$ISO3[match(Partner, cComtrade$CountryCode)]),
#         Receiver=case_when(Flow==2 ~ cComtrade$ISO3[match(Partner, cComtrade$CountryCode)],
#                            Flow==1 ~ cComtrade$ISO3[match(Reporter, cComtrade$CountryCode)]),
#         sName=cComtrade$CountryName[match(Sender, cComtrade$ISO3)],
#         rName=cComtrade$CountryName[match(Receiver, cComtrade$ISO3)]) %>%
#  filter(Reporter2 %in% SSAcc$iso3c & Partner2 %in% countries) %>% 
#  dplyr::select(Year, Sender, Receiver, Weight)

Xw <- trade %>% group_by(Year) %>% summarize(Xw=sum(Weight))

Mwi <- trade %>% group_by(Year, Receiver) %>% mutate(WLD=sum(Weight))  %>% ungroup() %>% 
  dplyr::select(Year, Receiver, WLD) %>%
  pivot_longer(WLD, names_to="Sender", values_to="Weight") %>% distinct()

Xiw <- trade %>% group_by(Year, Sender) %>% mutate(WLD=sum(Weight)) %>% ungroup() %>%
  dplyr::select(Year, Sender, WLD) %>%
  pivot_longer(WLD, names_to="Receiver", values_to="Weight") %>% distinct()

Mwj <- Mwi %>% filter(Receiver %in% countries) %>%
  pivot_wider(names_from=Receiver, values_from=Weight, names_prefix = "MwTo") %>%
  dplyr::select(-Sender)

trade <- bind_rows(trade, Mwi, Xiw) %>% left_join(., Xw, by="Year") %>% left_join(., Mwj, by="Year")



#getting trade intensity index for Africa-China and Africa-USA
tradeDep <- trade %>% filter(Receiver %in% c(countries, "WLD") & Sender %in% c(SSAcc$iso3c, "WLD")) %>%
  pivot_wider(names_from=Receiver, values_from=Weight) %>%
  group_by(Year) %>%
  mutate(Tic=(CHN/WLD)/(MwToCHN/Xw),
         Tig=(USA/WLD)/(MwToUSA/Xw),
         Tiu=(GBR/WLD)/(MwToGBR/Xw),
         Tif=(FRA/WLD)/(MwToFRA/Xw)) %>%
  mutate(depC=case_when(Tic > 1 ~ TRUE,
                        Tic < 1 ~ FALSE),
         depU=case_when(Tiu > 1 ~ TRUE,
                        Tiu < 1 ~ FALSE),
         depG=case_when(Tig > 1 ~ TRUE,
                        Tig < 1 ~ FALSE),
         depF=case_when(Tif > 1 ~ TRUE,
                        Tif < 1 ~ FALSE)) %>%
  rename(year=Year, cc=Sender)


# WGI #
#######
#bring in WorldwideGovernanceIndicators data
wgi <- read_dta("C:/Users/joanm/OneDrive/Documents/Data/WorldBank/wgidataset.dta") %>%
  dplyr::select(code, countryname, year, #only 1996 onwards
         gee, #government efficacy estimate --
         cce #corruption control estimate -- 
         )

wgi$cc <- countrycode(wgi$countryname, "country.name.en", "iso3c")
#Some values were not matched unambiguously: Kosovo, Netherlands Antilles (former), Türkiye
wgi$cc[wgi$countryname=="Kosovo"] <- "KOS"
wgi$cc[wgi$countryname=="Netherlands Antilles (former)"] <- "ANT"
wgi$cc[wgi$countryname=="Türkiye"] <- "TUR"
#add region
wgi$region <- ccode$un.regionsub.name[match(wgi$cc, ccode$iso3c)]
unique(wgi$countryname[is.na(wgi$region)]) #Netherlands Antilles (former);Kosovo;Taiwan, China don't have regions matched
wgi$region[wgi$cc=="ANT"] <- "Latin America and the Caribbean"
wgi$region[wgi$cc=="KOS"] <- "Southern Europe"
wgi$region[wgi$cc=="TWN"] <- "Eastern Asia"
wgi <- wgi %>% dplyr::select(-countryname)

#fragile states index
#setwd("C:/Users/joanm/OneDrive/Documents/Data/Fragile States")
#fsi <- list.files(pattern="*.xlsx") %>%                #compiled on server coz laptop insufficient ram
#  map_df(~read_xlsx(., col_types="text")) %>% 
#  dplyr::select(-tail(names(.),n=1))
fsi <- read_csv("C:/Users/joanm/OneDrive/Documents/Data/Fragile States/fsi.csv") %>%
  rename(countryname=Country, fsiTotal=Total)  %>% dplyr::select(-Rank) %>%
  mutate(cc=countrycode(countryname, "country.name.en", "iso3c")) #! Some values were not matched unambiguously: Israel and West Bank, Micronesia

fsi$cc[fsi$countryname=="Israel and West Bank"] <- "ISR"
fsi$cc[fsi$countryname=="Micronesia"] <- "FSM"

fsi <- fsi %>% dplyr::select(-countryname)

  
#bringing in GNI
gni <- read_csv("C:/Users/joanm/OneDrive/Documents/Data/WorldBank/GNI1962-2021.csv") %>%
  rename(cc='Country Code') %>% 
  dplyr::select(-c('Indicator Name', 'Indicator Code', 'Country Name')) %>%
  pivot_longer(cols=-cc, names_to="year", values_to = 'gni') %>%
  mutate(year=as.numeric(year))


#comparing China and USA
wgiChina <- wgi %>% filter(cc=="CHN")
wgiUSA <- wgi %>% filter(cc=="USA")
wgiGBR <- wgi %>% filter(cc=="GBR")
wgiFRA <- wgi %>% filter(cc=="FRA")

#compile into a table -- need wgi for all sub-saharan countries and filter trade to import/export w china
combine <- function (flow, partner) {
  colName <- paste0(flow, "Dir")
  dep <- get(flow) %>% mutate(colName=case_when(
                                  Sender==partner & Receiver %in% SSAcc$iso3c ~ paste0(flow,"From", partner),
                                  Sender %in% SSAcc$iso3c & Receiver==partner ~ paste0(flow,"To", partner)
          )) %>%
             filter(!is.na(colName)) %>%
                mutate(cc=case_when(
                                  Sender==partner ~ Receiver,
                                  Receiver==partner ~ Sender
          ))    %>%
                pivot_wider(names_from=colName, values_from=Weight,
                            id_cols=c("Year", "cc")) %>%
    rename(year=Year)
  
  return(dep)
}

tradeCHN <- combine("trade", "CHN")
dipCHN <- combine("dip", "CHN")
aidCHN <- combine("aid", "CHN")
tradeUSA <- combine("trade", "USA")
dipUSA <- combine("dip", "USA")
aidUSA <- combine("aid", "USA")
minsCHN <- combine("mins", "CHN")
minsUSA <- combine("mins", "USA")
tradeGBR <- combine("trade", "GBR")
dipGBR <- combine("dip", "GBR")
aidGBR <- combine("aid","GBR")
minsGBR <- combine("mins", "GBR")
tradeFRA <- combine("trade", "FRA")
dipFRA <- combine("dip", "FRA")
aidFRA <- combine("aid", "FRA")
minsFRA <- combine("mins", "FRA")

dep <- list(dipCHN, tradeCHN, aidCHN,
            dipUSA, tradeUSA, aidUSA,
            minsCHN, minsUSA, minsGBR, minsFRA,
            dipGBR, tradeGBR, aidGBR,
            dipFRA, tradeFRA, aidFRA,
            tradeDep, gni, wgi, fsi) %>% #list what tables to merge
  reduce(left_join, by=c("year", "cc")) %>% 
    mutate(countryname=cComtrade$CountryName[match(cc, cComtrade$ISO3)],
           region=ccode$un.regionsub.name[match(cc, ccode$iso3c)]) %>%
  #filter(year %in% wgi$year) %>% 
  dplyr::select(!c(code)) %>%
  #make no reported value 0 %>%
  #mutate(across(ends_with("CHN") | ends_with("USA") | ends_with("FRA") | ends_with("GBR"), ~replace_na(.x,0))) %>%
  #add binary variable for whether there was a relationship or not
  mutate(aidC=case_when(aidFromCHN>0 ~ TRUE, .default=FALSE),
         dipC=case_when(dipFromCHN>0 | dipToCHN <0 ~ TRUE, .default=FALSE),
         aidU=case_when(aidFromUSA>0 ~ TRUE, .default=FALSE),
         dipU=case_when(dipFromUSA>0 | dipToUSA>0 ~ TRUE, .default=FALSE),
         tradeCHN.nb=case_when(Tic<1 ~ 0,
                               Tic>1 ~ Tic))


depLong <- dep %>% pivot_longer(cols=!c(year, cc, countryname, region))

#create separate df for case studies
case <- dep %>% filter(countryname %in% c("Botswana", "South Africa", "Zimbabwe", "Zambia"))
caseLong <- depLong %>% filter(countryname %in% c("Botswana", "South Africa", "Zambia", "Zimbabwe"))


#saveRDS(depLong, "depLong.rds")
#saveRDS(dep, "dep.rds")

#################################
## ANALYSIS #####################
#################################
dep <- readRDS("dep.rds")
depLong <-readRDS("depLong.rds")

cor(dep$depC, dep$fsiTotal, use="complete.obs")

#do panel regression (predicting trade dependency) - predictor governance indicators, control by year and gni
#AND/OR do case study plots of flows vs governance indicators

#how much export can be predicted on how corrupt a government is
summary(lme(tradeToCHN~cce+gni+year+I(year^2), random=~1|cc, data=dep,
    na.action=na.omit))

summary(glm.nb(tradeCHN.nb~cc+cce+gni+year, data=dep, na.action=na.omit)) #cce is about 1 and significant
summary(glm.nb(minsToCHN~cce+gni+year, data=dep, na.action=na.omit,
               control=glm.control(maxit=500)))

summary(glmer(aidC~cce+gni+year+(1|cc), data=dep, na.action=na.omit))
summary(glm(aidC~cc+gee+gni+year, data=dep, na.action=na.omit))

regDat <- dep %>% dplyr::select(year, cc, cce, gee, gni, Tic, Tiu, Tig,
                                aidFromCHN, aidFromGBR, aidFromUSA) %>%
  mutate(Tic.hurd=case_when(Tic<1 ~ NA,
                           Tic>1 ~ Tic),
         Tic.bin=case_when(Tic<1 ~ 0,
                           Tic>=1 ~ 1),
         Tig.hurd=case_when(Tig<1 ~ NA,
                            Tig>1 ~ Tig),
         Tig.bin=case_when(Tig<1 ~ 0,
                           Tig>=1 ~ 1),
         Tiu.hurd=case_when(Tiu<1 ~ NA,
                            Tiu>1 ~ Tiu),
         Tiu.bin=case_when(Tiu<1 ~ 0,
                           Tiu>=1 ~ 1),
         time=year-min(year),
         aidC.bin=case_when(aidFromCHN=0 | is.na(aidFromCHN) ~ 0,
                            aidFromCHN>0 ~ 1),
         aidG.bin=case_when(aidFromGBR=0 | is.na(aidFromGBR) ~ 0,
                            aidFromGBR>0 ~ 1),
         aidU.bin=case_when(aidFromUSA=0 | is.na(aidFromUSA) ~ 0,
                            aidFromUSA>0 ~ 1),
         aidC.hurd=case_when(aidFromCHN>0~aidFromCHN,
                             aidFromCHN=0 | is.na(aidFromCHN) ~ NA),
         aidG.hurd=case_when(aidFromGBR>0~aidFromGBR,
                             aidFromGBR=0 | is.na(aidFromGBR) ~ NA),
         aidU.hurd=case_when(aidFromUSA>0~aidFromUSA,
                             aidFromUSA=0 | is.na(aidFromUSA) ~ NA)
         )

regDatT <- regDat %>% dplyr::select(year,cc,cce,gee,gni,time,starts_with("Ti", ignore.case=F)) %>%
  rename(Tic.Tij=Tic,
         Tig.Tij=Tig,
         Tiu.Tij=Tiu) %>%
  pivot_longer(cols=starts_with("Ti", ignore.case=FALSE), names_to=c("partner", "type"), names_sep = "\\.") %>%
  mutate(partner=case_when(grepl("ic", partner, ignore.case=F) ~ "CHN",
                           grepl("ig", partner, ignore.case=F) ~ "GBR",
                           grepl("iu", partner, ignore.case=F) ~ "USA")) %>%
  pivot_wider(names_from=type)

regDatAid <- regDat %>% dplyr::select(year,cc,cce,gee,gni,time,starts_with("aidFrom", ignore.case=F)) %>%
  pivot_longer(cols=starts_with("aid") , names_to="partner", values_to="aid", names_prefix = "aidFrom") %>%
  mutate(aid.bin=case_when(aid=0 | is.na(aid) ~ 0,
                           aid>0 ~ 1),
         aid.hurd=case_when(aid>0~aid,
                            aid=0 | is.na(aid) ~ NA))

regDatLong <- left_join(regDatT, regDatAid)


## REGRESSIONS ##
##CCE
#building a hurdle model - first binomial then continuous (what distribution?)
#first stage (threshold)
#for China
Tic.b <- glm(Tic.bin~cce+gni+time+cc, data=regDat, na.action=na.omit, family="binomial")
Tig.b <- glm(Tig.bin~cce+gni+time+cc, data=regDat, na.action=na.omit, family="binomial")
Tiu.b <- glm(Tiu.bin~cce+gni+time+cc, data=regDat, na.action=na.omit, family="binomial")

#library(sjPlot)
tab_model(Tic.b, Tig.b, Tiu.b,
          show.ci=FALSE, p.style="stars",
          digits=3)

#second stage (continuous)
hist(regDat$Tic.hurd[regDat$Tic.hurd])
hist(regDat$Tig.hurd[regDat$Tic.hurd])
hist(regDat$Tiu.hurd[regDat$Tic.hurd])
#i'll use the gamma distribution
Tic.h <- glm(Tic.hurd~cce+gni+time+cc, data=regDat, na.action=na.omit, family=Gamma(link=log))
Tig.h <- glm(Tig.hurd~cce+gni+time+cc, data=regDat, na.action=na.omit, family=Gamma(link=log))
Tiu.h <- glm(Tiu.hurd~cce+gni+time+cc, data=regDat, na.action=na.omit, family=Gamma(link=log))

tab_model(Tic.h, Tig.h, Tiu.h,
          show.ci=FALSE, p.style="stars",
          digits=3)
##GEE
summary(glm(Tic.bin~gee+gni+year, data=regDat, na.action=na.omit, family="binomial"))

#second stage (continuous)
#i'll use the gamma distribution
summary(glm(Tic.hurd~gee+gni+year, data=regDat[regDat$Tic.hurd>=1,], family="Gamma"))


#CCE AMD GEE
summary(glm(Tic.bin~gee+cce+gni+year, data=regDat, na.action=na.omit, family="binomial"))
summary(glm(Tic.hurd~gee+cce+gni+year, data=regDat[regDat$Tic.hurd>=1,], family="Gamma"))



#hurdle model for aid: remember aid can only be between 2000-2017
#first stage
regDatLongAid <-regDatLong %>% filter(year>=2000 & year<=2017)
aidC.b <- glm(aid.bin~cce+gni+time+cc, data=regDatLongAid[regDatLongAid$partner=="CHN",], na.action=na.omit, family="binomial")
aidG.b <- glm(aid.bin~cce+gni+time+cc, data=regDatLongAid[regDatLongAid$partner=="GBR",], na.action=na.omit, family="binomial")
aidU.b <- glm(aid.bin~cce+gni+time+cc, data=regDatLongAid[regDatLongAid$partner=="USA",], na.action=na.omit, family="binomial")

aidC.b2 <- glm(aidC.bin~cce+gni+time+cc, data=regDat[regDat$year %in% c(2000:2017),], na.action=na.omit, family="binomial")
aidG.b2 <- glm(aidG.bin~cce+gni+time+cc, data=regDat[regDat$year %in% c(2000:2017),], na.action=na.omit, family="binomial")
aidU.b2 <- glm(aidU.bin~cce+gni+time+cc, data=regDat[regDat$year %in% c(2000:2017),], na.action=na.omit, family="binomial")

tab_model(aidC.b, aidG.b, aidU.b,
          show.ci=FALSE, p.style="stars",
          digits=3)

tab_model(aidC.b2, aidG.b2, aidU.b2,
          show.ci=FALSE, p.style="stars",
          digits=3)

#hurdle stage (second stage)
hist(regDat$aidC.hurd[regDat$year %in% c(2000:2017)])

aidC.h <- glm(aidC.hurd~cce+gni+time, data=regDat[regDat$year %in% c(2000:2017),], family=Gamma(link=log))
aidG.h <- glm(aidG.hurd~cce+gni+time+cc, data=regDat[regDat$year %in% c(2000:2017),], family=Gamma(link=log))
aidU.h <- glm(aidU.hurd~cce+gni+time+cc, data=regDat[regDat$year %in% c(2000:2017),], family=Gamma(link=log))

tab_model(aidC.h, aidG.h, aidU.h,
          show.ci=FALSE, p.style="stars",
          digits=3)

## FULL MODEL ##
#binomial stage
Tif.b <- glm(bin~cce+gni+partner+partner*cce+time+cc, data=regDatLong, na.action=na.omit, family="binomial")

#hurdle model
hist(regDatLong$hurd)
Tif.h <- glm(hurd~cce+gni+partner+partner*cce+time+cc, data=regDatLong, family="Gamma")

tab_model(Tif.b, Tif.h,
          show.ci=FALSE, p.style="stars",
          digits=3)

#for aid
aidF.b <- glm(aid.bin~cce+gni+partner+partner*cce+time+cc, data=regDatLongAid, na.action=na.omit, family="binomial")
aidF.h <- glm(aid.hurd~cce+gni+partner+partner*cce+time+cc, data=regDatLongAid, family=Gamma(link=log))

tab_model(aidF.b, aidF.h,
          show.ci=FALSE, p.style="stars",
          digits=3)

tab_model(Tif.b, Tif.h, aidF.b, aidF.h,
          show.ci=FALSE, p.style="stars",
          digits=3)

#####################
### TABLES ##########
### Making TABLES ###
Tij.tab <- dep %>% dplyr::select(cc, year, Tic, Tig, Tiu)

