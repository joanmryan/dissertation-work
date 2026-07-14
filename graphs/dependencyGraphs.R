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
library(ggpattern)
library(gtable)

dep <- readRDS("dep.rds")
depLong <-readRDS("depLong.rds")

##################
## PLOTS #########
##################



#see how aid from China is related to cce or gee
dep %>% dplyr::select(year, cc, cce, aidFromCHN, aidFromGBR, aidFromUSA) %>%
  #filter(year >= 2000 & year <=2017) %>%
  pivot_longer(cols=starts_with("aidFrom")) %>%
  mutate(partner=substring(name, first=nchar(name)-2)) %>%
  filter(complete.cases(.)) %>% mutate(meanCCE=mean(cce)) %>%
  group_by(partner) %>% mutate(underC=sum(cce<0),
                               totalN=n(),
                               propC=underC/totalN) %>%
  ggplot(aes(x=cce, y=value)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(trans='pseudo_log',
                     labels=scales::label_number(scale_cut=scales::cut_short_scale()),
                     breaks=c(0.00005, 0.0005, 0.005, 0.05, 0.5, 5, 50, 500, 5000, 50000)*1000000) +
  #coord_trans(y='pseudo_log') +
  geom_vline(aes(xintercept=meanCCE, colour='red'), show.legend=FALSE) +
  geom_text(aes(label=paste0("Proportion of Partners More Corrupt Than \n The Average Recipient SSA Country: ", 
                             round(propC, digits=1)), x=meanCCE, y=2), 
            size=5)+
  facet_wrap(~partner) +
  labs(y="Aid Flows From China (equivalent 2020 USD Amount)", x="Control of Corruption Index") +
  theme_gray(base_size=18)

dep %>% dplyr::select(year, cc, cce, aidFromCHN) %>% filter(2017>=year & year>=2000) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=cce, y=aidFromCHN)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(trans='pseudo_log') +
  #coord_trans(y='pseudo_log') +
  geom_vline(xintercept=0, colour='red', show.legend=FALSE) +
  labs(y="Aid Flows From China", x="Control of Corruption Index") +
  theme_gray(base_size=16)

dep %>% filter(year>2000) %>%
  ggplot(aes(x=gee, y=aidFromCHN)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(trans='pseudo_log') +
  geom_vline(xintercept=0, colour='red', show.legend=FALSE)

#looking at how it is related to the FIRST point of aid contact for each AFrican country
dep %>% dplyr::select(year, cc, cce, aidFromCHN, aidFromGBR, aidFromUSA) %>%
  filter(year >= 2000 & year <=2017) %>%
  pivot_longer(cols=starts_with("aidFrom")) %>%
  mutate(partner=substring(name, first=nchar(name)-2)) %>%
  filter(complete.cases(.)) %>% mutate(meanCCE=mean(cce)) %>%
  group_by(year, cc, partner) %>% slice_min(year) %>%
  ggplot(aes(x=cce, y=aidFromCHN)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(trans='pseudo_log') +
  #coord_trans(y='pseudo_log') +
  geom_vline(xintercept=0, colour='red', show.legend=FALSE) +
  labs(y="Aid Flows From China", x="Control of Corruption Index") +
  theme_gray(base_size=16)


#find out how cce adn gee are calculated--is it a rankign system? is there a true absolute zero?
#panel of four plots with year on x axis and cce and TijC on y axes, each facet is a country
depLong %>% filter(name== "cce" | name=="tradeToCHN") %>% ggplot(data=depLong, aes(x=year, group=name)) +
  geom_line(aes(colour=as.factor(cc))) +
  geom_smooth(method = "lm", se = F, lty = "dashed") +
  theme(legend.position = "none")


ggplot(caseLong, aes(x=year)) +
  geom_line(aes(y=TijC, color="TijC")) +
  geom_line(aes(y=cce, color="cce"))

caseLong %>% filter(name %in% c("TijC", "cce")) %>%
  ggplot(aes(x=year, y=value, color=name)) + 
  geom_line() + geom_point() + 
  geom_hline(yintercept=1, color="#F8766D") + geom_hline(yintercept=0, color="#00BFC4") +
  facet_wrap(~countryname)

depLong %>% filter(name %in% c("TijC", "fsiTotal")) %>%
  ggplot(aes(x=year, y=value, color=name)) + 
  geom_line() + geom_point() + 
  geom_hline(yintercept=0, color="#F8766D") + geom_hline(yintercept=1, color="#00BFC4") +
  facet_wrap(~countryname)

#mineral exports to CHN vs USA over time - each country is a facet
depLong %>% filter(name %in% c("minsToCHN", "minsToUSA")) %>%
  ggplot(aes(x=year, y=value, color=name)) +
  geom_line() +
  facet_wrap(~countryname) + 
  scale_y_continuous(trans='pseudo_log')


#COL POWER COMPARISONS
#mineral exports to CHN vs USA over time - oen facet for one facet for each of 4 countries
depLong %>% filter(name %in% c("minsToCHN", "minsToUSA", "minsToGBR", "minsToFRA")) %>%
  ggplot(aes(x=year, y=value, color=cc)) +
  geom_smooth(se=FALSE, size=0.2) +
  facet_wrap(~name) + 
  scale_y_continuous(trans='pseudo_log')


#trade import vs export to CHN vs USA over time -- do two side-by-side panels -- 
tradeTo <- names(dep)[startsWith(names(dep), "tradeTo")]
tradeFrom <- names(dep)[startsWith(names(dep), "tradeFrom")]
depLong %>% filter(year>=1962) %>% #trade only starts in 1962!!
  filter(name %in% c(tradeTo, tradeFrom)) %>% 
  mutate(MorX=case_when(name %in% tradeTo ~ "Imports From SSA",
                        name %in% tradeFrom ~ "Exports To SSA")) %>%
  mutate(name=stri_sub(name,-3)) %>%
  ggplot(aes(x=year, y=value, group=interaction(MorX, cc), color=MorX)) +
  geom_line(size=0.2) +
  facet_wrap(~name) + 
  scale_y_continuous(trans='pseudo_log')

Tijs <- unique(depLong$name[startsWith(depLong$name, "T")])
depLong %>% filter(name %in% Tijs) %>%
  ggplot(aes(x=year, y=value, group=cc)) +
  geom_line(size=0.2) +
  geom_hline(yintercept=1, color="#F8766D") +
  facet_wrap(~name) +
  scale_y_continuous(trans='pseudo_log')

##########################
#PLOTS FOR CHN, GBR, USA # comparative
##########################
#COL POWER COMPARISONS

#stacked bar plot of trade partners and how many are dependent (with percentage)
depCount <- depLong %>% filter(name %in% c("depC", "depG", "depU")) %>% group_by(year, name) %>%
  mutate(tradePart=sum(!is.na(value)),
         tradeDep=sum(value==1, na.rm=T),
         tradeNotDep=sum(value==0, na.rm=T)) %>%
  mutate(partner=case_when(name=="depC" ~ "CHN",
                           name=="depG" ~ "GBR",
                           name=="depU" ~ "USA"),
         percDep=100*tradeDep/tradePart) %>% ungroup() %>%
  dplyr::select(year, partner, tradePart, tradeDep, tradeNotDep, percDep) %>% distinct() %>%
  pivot_longer(cols=starts_with("trade"), names_to = "trade", values_to = "count")

tradeBar <- ggplot(depCount %>% filter(trade %in% c("tradeDep", "tradeNotDep")), 
       aes(x=year, y=count, fill=trade)) +
  geom_col(position="stack", width=1) +
  labs(x="Year", y="Total SSA Trade Partners", fill="Nature of Trade Relationship") +
  scale_fill_hue(labels=c("Dependent", "Not Dependent")) +
  facet_wrap(~partner)+
  theme_gray(base_size=16) + 
  theme(legend.position="bottom",
        strip.text.x=element_text(size=16))
  

tradePer <- ggplot(depCount %>% filter(trade %in% c("tradeDep", "tradeNotDep")), 
                   aes(x=year, y=percDep)) +
  labs(x="Year", y="Percentage of Dependent SSA Trade Partners") +
  geom_line() +
  facet_wrap(~partner) +
  theme_gray(base_size=16) + 
  theme(strip.text.x=element_text(size=16))


g1 <- ggplotGrob(tradeBar)
g2 <- ggplotGrob(tradePer)

library(grid)
g1$widths <- unit.pmax(g1$widths, g2$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths)

tradePlot <- rbind(g2, g1, size="first")
grid.draw(tradePlot)


# mineral exports to CHN vs USA over time - oen facet for one facet for each of 4 countries
depLong %>% filter(name %in% c("minsToCHN", "minsToGBR", "minsToUSA")) %>%
  ggplot(aes(x=year, y=value, group=cc)) +
  geom_line(size=0.4) +
  geom_smooth(method = 'lm',group=1, se=FALSE) +
  facet_wrap(~name, labeller=labeller(name=setNames(c("CHN", "GBR", "USA"), c("minsToCHN", "minsToUSA", "minsToGBR")))) + 
  scale_y_continuous(trans='pseudo_log', labels=scales::label_number(scale_cut=scales::cut_short_scale()),
                     breaks=c(0.00005, 0.0005, 0.005, 0.05, 0.5, 5, 50, 500, 5000, 50000)*1000000)+ 
  labs(x="Year", y="Value of Minerals Exported From SSA Countries\n (equivalent 2020 USD amount)") +
  theme_gray(base_size=18)

#mineral exports out of total exports to CHN vs GBR vs USA over time - one facet for each country
minsTos <- c("minsToCHN", "minsToGBR", "minsToUSA")
tradeTos <- c("tradeToCHN", "tradeToGBR", "tradeToUSA")

#doing it as a bar plot (average) with all SSA countries added together
depLong %>% filter(name %in% c(minsTos, tradeTos)) %>%
  mutate(group=case_when(name %in% minsTos ~ "Natural Resources",
                         name %in% tradeTos ~ "Total Trade")) %>%
  mutate(partner=case_when(endsWith(name, "CHN") ~ "CHN",
                           endsWith(name, "GBR") ~ "GBR",
                           endsWith(name, "USA") ~ "USA")) %>%
  group_by(year, group, partner) %>%
  mutate(total=sum(value, na.rm=T)) %>% 
  dplyr::select(year, group, partner, total) %>% distinct() %>%
  ggplot(aes(x=year, y=total, fill=group, alpha=group)) +
  geom_col(size=0.4, position="identity") +
  scale_alpha_manual(values=c(0.8,0.3)) +
  #geom_smooth(method = 'lm',group=1, se=FALSE) +
  facet_wrap(~partner) + 
  # scale_y_continuous(trans='pseudo_log', labels=scales::label_number(scale_cut=scales::cut_short_scale()),
  #                    breaks=c(0.00005, 0.0005, 0.005, 0.05, 0.5, 5, 50, 500, 5000, 50000)*1000000)+ 
  # labs(x="Year", y="Value of Minerals Exported From SSA Countries\n (equivalent 2020 USD amount)") +
  theme_gray(base_size=18)


#one line representing percentage of minerals over total trade for each country
depLong %>% filter(name %in% c(minsTos, tradeTos)) %>%
  mutate(group=case_when(name %in% minsTos ~ "Natural Resources",
                         name %in% tradeTos ~ "Total Trade")) %>%
  mutate(name=case_when(endsWith(name, "CHN") ~ "CHN",
                           endsWith(name, "GBR") ~ "GBR",
                           endsWith(name, "USA") ~ "USA")) %>%
  pivot_wider(names_from=group, values_from=value) %>%
  mutate(percMin=`Natural Resources`/`Total Trade`) %>%
  ggplot(aes(x=year, y=percMin, group=cc)) +
  geom_line(size=0.4) +
  geom_smooth(method = 'lm',group=1, se=FALSE) +
  facet_wrap(~name) + 
  # scale_y_continuous(trans='pseudo_log', labels=scales::label_number(scale_cut=scales::cut_short_scale()),
  #                    breaks=c(0.00005, 0.0005, 0.005, 0.05, 0.5, 5, 50, 500, 5000, 50000)*1000000)+ 
  # labs(x="Year", y="Value of Minerals Exported From SSA Countries\n (equivalent 2020 USD amount)") +
  theme_gray(base_size=18)


#trade import vs export to CHN vs USA over time -- do two side-by-side panels -- 
tradeTo <- c("tradeToCHN", "tradeToUSA", "tradeToGBR")
tradeFrom <- c("tradeFromCHN", "tradeFromUSA", "tradeFromGBR")
depLong %>% filter(year>=1962) %>% #trade only starts in 1962!!
  filter(name %in% c(tradeTo, tradeFrom)) %>% 
  mutate(MorX=case_when(name %in% tradeTo ~ "Imports From SSA",
                        name %in% tradeFrom ~ "Exports To SSA")) %>%
  mutate(name=stri_sub(name,-3)) %>%
  ggplot(aes(x=year, y=value, group=interaction(MorX, cc))) +
  geom_line(size=0.2, aes(color=MorX)) +
  geom_smooth(method = 'lm', aes(color=MorX, group=MorX), se=TRUE, size=2) +
  facet_wrap(~name) + 
  scale_y_continuous(trans='pseudo_log', labels=scales::label_number(scale_cut=scales::cut_short_scale()),
                     breaks=c(0.00005, 0.0005, 0.005, 0.05, 0.5, 5, 50, 500, 5000, 50000)*1000000)+ 
  labs(color="Trade Flow Direction", x="Year", y="Trade Value (equivalent 2020 USD Amount) ") +
  theme_gray(base_size=18)


########################
#Tij
########################
#to add Tij trend line:
labelInfo <- depLong %>% filter(name %in% Tijs) %>%
  split(., .$name) %>%
  lapply(function(t) {
    data.frame(
      predAtMax = loess(value ~ year, span = 0.8, data = t) %>%
        predict(newdata = data.frame(year = max(t$year)))
      , max = max(t$year)
    )}) %>%
  bind_rows

library(ggrepel)

Tijs <- c("Tic", "Tig", "Tiu")

TijPlot <- depLong %>% filter(name %in% Tijs) %>%
  ggplot(aes(x=year, y=value)) +
  geom_hline(yintercept=1, color="red") + annotate("text", x=1960, y=1.15, label="Tij=1", color="red", size=6) +
  geom_line(size=0.1, aes(group=cc)) +
  geom_smooth(method = 'lm', se=FALSE) +
  facet_wrap(~name, 
             #labeller=labeller(name=setNames(c("j = CHN", "j = GBR", "j = USA"), Tijs))
  ) +
  #coord_trans(y="pseudo_log") +
  scale_y_continuous(trans='pseudo_log') +
  labs(x="Year", y="Trade Dependency \n (Dependent when Tij>1)") + 
  theme_gray(base_size=18)

smoothed <- ggplot_build(TijPlot)$data[[4]] %>% mutate(name=fct_recode(as.factor(PANEL),
                                                                       Tic="1",
                                                                       Tig="2", 
                                                                       Tiu="3"))

smoothedmin<- smoothed %>% group_by(PANEL) %>% mutate(grad=(max(y)-min(y))/(max(x)-min(x))) %>%
  ungroup()  %>% filter(x==max(x))


clabels <- Tij.tab %>% 
  filter(Tic==max(Tic, na.rm=T) | Tig==max(Tig, na.rm=T) | Tiu==max(Tiu, na.rm=T)) %>%
  pivot_longer(cols=Tic:Tiu, names_to="name", values_to = "y") %>% group_by(year) %>%
  slice_max(y, n=1) %>%
  mutate(label=paste0(cc, " ", year, ": ", round(y, 2)))

depLong %>% filter(name %in% Tijs) %>%
  ggplot(aes(x=year, y=value)) +
  geom_hline(yintercept=1, color="red") + annotate("text", x=1960, y=1.15, label="Tij=1", color="red", size=6) +
  geom_line(size=0.1, aes(group=cc)) +
  #geom_smooth(method = 'lm', se=FALSE) +
  geom_smooth(data=smoothed, aes(x=x,y=exp(y)-0.5, group=name)) +
  facet_wrap(~name, 
             labeller=labeller(name=setNames(c("j = CHN", "j = GBR", "j = USA"), Tijs))
  ) +
  coord_trans(y="pseudo_log") +
  #scale_y_continuous(trans='pseudo_log') +
  labs(x="Year", y="Trade Dependency \n (Dependent when Tij>1)") + 
  theme_gray(base_size=18) + 
  geom_text(data = smoothedmin, aes(x = x, y = c(1.1,0.54,0.55),     
                                    label = name),
            color = "#3366FF", hjust = 0, vjust = 0, size = 4, fontface = "bold",
            inherit.aes = FALSE) +
  geom_label_repel(data=clabels, aes(x=year, y =y, label=label),
                   nudge_y=1.5)

##ASKING MANUEL FOR HELP##
#manuel's code v1
depLong %>% filter(name %in% Tijs) %>%
  ggplot(aes(x=year, y=value)) +
  geom_hline(yintercept=1, color="red") + annotate("text", x=(1960), y=1.15, label="Tij=1", color="red", size=6) +
  geom_line(size=0.2, aes(group=cc)) +
  geom_smooth(method = 'lm', se=T) +
  facet_wrap(~name, labeller=labeller(name=setNames(c("j = CHN", "j = GBR", "j = USA"), Tijs))) +
  scale_y_continuous(trans='pseudo_log') +
  labs(x="Year", y="Trade Dependency \n (Dependent when Tij>1)") +
  theme_gray(base_size=18) +
  geom_dl(label=as.factor(depLong$name[depLong$name %in% Tijs]), 
          method="first.qp", inherit.aes=T, color="blue")

#geom_dl method
depLong %>% filter(name %in% Tijs) %>%
  ggplot(aes(x=year, y=value)) +
  geom_hline(yintercept=1, color="red") + annotate("text", x=(1960), y=1.15, label="Tij=1", color="red", size=6) +
  geom_line(size=0.2, aes(group=cc)) +
  geom_smooth(method = 'lm', se=T) +
  geom_dl(label=as.factor(depLong$name[depLong$name %in% Tijs]), 
          method="first.qp", inherit.aes=T, color="blue") +
  facet_wrap(~name, labeller=labeller(name=setNames(c("j = CHN", "j = GBR", "j = USA"), Tijs))) +
  scale_y_continuous(trans='pseudo_log') +
  labs(x="Year", y="Trade Dependency \n (Dependent when Tij>1)") +
  theme_gray(base_size=18)


#manuel's v2
ts <- depLong %>% filter(name %in% Tijs) %>%
  ggplot(aes(x=year, y=value, colour=name)) +
  #geom_hline(yintercept=1, color="red") + annotate("text", x=(1960), y=1.5, label="Tij=1", color="red", size=6) +
  geom_line(size=0.2, aes(group=cc)) +
  geom_smooth(method = 'lm', se=T) +
  facet_wrap(~name, labeller=labeller(name=setNames(c("j = CHN", "j = GBR", "j = USA"), Tijs))) +
  scale_y_continuous(trans='pseudo_log') +
  labs(x="Year", y="Trade Dependency (Tij>1)") +
  theme_gray(base_size=18) + theme(legend.position = "none")