library(readxl)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(readxl)
library(haven)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(grid)
library("gridExtra")
library(ipumsr)

## Samples 

# IPUMS-CPS, ASEC 2013	
# IPUMS-CPS, ASEC 2014	
# IPUMS-CPS, ASEC 2015	
# IPUMS-CPS, ASEC 2016	
# IPUMS-CPS, ASEC 2017	
# IPUMS-CPS, ASEC 2018	
# IPUMS-CPS, ASEC 2019	
# IPUMS-CPS, ASEC 2020	
# IPUMS-CPS, ASEC 2021	
# IPUMS-CPS, ASEC 2022	
# IPUMS-CPS, ASEC 2023	
# IPUMS-CPS, ASEC 2024

## Variables

# H	YEAR	Survey year
# H	SERIAL	Household serial number
# H	MONTH	Month
# H	CPSID	CPSID, household record
# H	ASECFLAG	Flag for ASEC
# H	HFLAG	Flag for the 3/8 file 2014
# H	ASECWTH	Annual Social and Economic Supplement Household weight
# H	REGION	Region and division
# H	STATEFIP	State (FIPS code)
# H	METRO	Metropolitan and central/principal city status
# H	HHINCOME	Total household income
# H	FOODSTMP	Food stamp recipiency
# H	STAMPVAL	Total value of food stamps
# H	FAMINC	Family income of householder
# H	NMOTHERS	Number of mothers in household
# H	NFATHERS	Number of fathers in household
# P	PERNUM	Person number in sample unit
# P	CPSIDP	CPSID, person record
# P	CPSIDV	Validated Longitudinal Identifier
# P	ASECWT	Annual Social and Economic Supplement Weight
# P	RELATE	Relationship to household head
# P	AGE	Age
# P	SEX	Sex
# P	RACE	Race
# P	MARST	Marital status
# P	ASIAN	Asian subgroup
# P	MOMLOC	Person number of first mother (from programming)
# P	MOMLOC2	Person number of second mother (from programming)
# P	POPLOC	Person number of first father (from programming)
# P	POPLOC2	Person number of second father (from programming)
# P	SPLOC	Person number of spouse (from programming)
# P	NCHILD	Number of own children in household
# P	NCHLT5	Number of own children under age 5 in hh
# P	ELDCH	Age of eldest own child in household
# P	YNGCH	Age of youngest own child in household
# P	ASPOUSE	Spouse line number (self-reported)
# P	PECOHAB	Cohabiting partner line number (self-reported)
# P	FTYPE	Family Type
# P	FAMKIND	Kind of family
# P	FAMREL	Relationship to family
# P	BPL	Birthplace
# P	YRIMMIG	Year of immigration
# P	CITIZEN	Citizenship status
# P	MBPL	Mother's birthplace
# P	FBPL	Father's birthplace
# P	NATIVITY	Foreign birthplace or parentage
# P	HISPAN	Hispanic origin
# P	EDUC	Educational attainment recode
# P	EDUC99	Educational attainment, 1990
# P	SCHLCOLL	School or college attendance

######################################################################
# Descriptive
######################################################################
ddi <- read_ipums_ddi("Data/CPS-ASEC/cps_00011.xml")
data <- read_ipums_micro(ddi) %>%
  mutate(EDUC=case_when(
    EDUC >=1 & EDUC < 73 ~ 1,
    EDUC ==73  ~ 2,
    EDUC >=80 & EDUC <= 92  ~ 3,
    EDUC >=111 & EDUC <= 125  ~ 4,
    TRUE ~ NA
  ))
pd <- position_dodge(0.1) # move them .05 to the left and right

mlink <- data %>% ## add mother or father's edu and race here
  dplyr::select(YEAR, SERIAL, MOMLOC=PERNUM, 
                MASPOUSE=ASPOUSE, MPECOHAB=PECOHAB,
                MEDU=EDUC,MCITI=CITIZEN,
                MMBPL=MBPL,MFBPL=FBPL,mrace=RACE,masian=ASIAN,mhisp=HISPAN,mage=AGE) %>% 
  mutate(mhisp=case_when(
    mhisp == 0 ~ 0,
    mhisp > 0 & mhisp <= 612 ~ 1,
    TRUE ~ NA
  )) %>%
  mutate(mrace = case_when(
    mhisp == 1 ~ "Hispanic", ## Hispanic
    mrace == 100 ~ "White", ## White
    mrace == 200 ~ "Black", ## Black
    mrace == 300 ~ "AIAN", ## AIAN
    mrace == 650 ~ "Asian", ## Asian
    mrace == 651 ~ "Asian", ## Asian
    mrace == 652 | mrace >= 700 ~ "Other/more than one race" ## Other two more
  )) %>%
  mutate(masian = case_when(
    mrace == "White" ~ "White",
    mrace == "Asian" & masian == 10 ~ "South Asian",
    mrace == "Asian" & masian == 20 ~ "East Asian",
    mrace == "Asian" & masian == 40 ~ "East Asian",
    mrace == "Asian" & masian == 50 ~ "East Asian",
    mrace == "Asian" & masian == 30 ~ "Southeast Asian",
    mrace == "Asian" & masian == 60 ~ "Southeast Asian",
    mrace == "Asian" & masian == 70 ~ "Other Asian"
  )) 

plink <- data %>%
  dplyr::select(YEAR, SERIAL, POPLOC=PERNUM, 
                PASPOUSE=ASPOUSE, PPECOHAB=PECOHAB, 
                FEDU=EDUC,FCITI=CITIZEN,
                FMBPL=MBPL,FFBPL=FBPL,frace=RACE,fasian=ASIAN,fhisp=HISPAN,fage=AGE) %>% 
  mutate(fhisp=case_when(
    fhisp == 0 ~ 0,
    fhisp > 0 & fhisp <= 612 ~ 1,
    TRUE ~ NA
  )) %>%
  mutate(frace = case_when(
    fhisp == 1 ~ "Hispanic", ## Hispanic
    frace == 100 ~ "White", ## White
    frace == 200 ~ "Black", ## Black
    frace == 300 ~ "AIAN", ## AIAN
    frace == 650 ~ "Asian", ## Asian
    frace == 651 ~ "Asian", ## Asian
    frace == 652 | frace >= 700 ~ "Other/more than one race" ## Other two more
  )) %>%
  mutate(fasian = case_when(
    frace == "White" ~ "White",
    frace == "Asian" & fasian == 10 ~ "South Asian",
    frace == "Asian" & fasian == 20 ~ "East Asian",
    frace == "Asian" & fasian == 40 ~ "East Asian",
    frace == "Asian" & fasian == 50 ~ "East Asian",
    frace == "Asian" & fasian == 30 ~ "Southeast Asian",
    frace == "Asian" & fasian == 60 ~ "Southeast Asian",
    frace == "Asian" & fasian == 70 ~ "Other Asian"
  )) 

df <- data %>%
  mutate(type = case_when(
    MOMLOC > 0 & POPLOC > 0 ~ 1,
    MOMLOC > 0 & POPLOC == 0 ~ 2,
    MOMLOC == 0 & POPLOC > 0 ~ 3,
    MOMLOC == 0 & POPLOC == 0 ~ NA
  )) %>%
  mutate(nadult = ifelse(AGE < 19,0,1)) %>%
  group_by(YEAR, SERIAL) %>%
  mutate(nadult = sum(nadult)) %>%
  left_join(mlink,by=c("YEAR","SERIAL","MOMLOC")) %>%
  left_join(plink,by=c("YEAR","SERIAL","POPLOC")) %>%
  filter(is.na(type)==FALSE) %>%
  ungroup()

dfx <- df %>%
  filter(YEAR >= 2013) %>% 
  mutate(typex = case_when(
    type == 1 & PASPOUSE > 0  ~ 1, ## married two parent
    type == 1 & MASPOUSE > 0  ~ 1, ## married two parent
    type == 1 & PASPOUSE == 0 & MASPOUSE == 0 ~ 2, ## Two unmarried parents
    type == 2 & MPECOHAB > 0 ~ 3, ## Mother with cohabiting partner
    type == 2 & MPECOHAB == 0 & nadult > 1 ~ 4, ## Mother with other adult
    type == 2 & MPECOHAB == 0 & nadult < 2 ~ 5, ## Mother only
    type == 3 & PPECOHAB > 0 ~ 6, ## Father with cohabiting partner
    type == 3 & PPECOHAB == 0 & nadult > 1 ~ 7, ## Father with other adult
    type == 3 & PPECOHAB == 0 & nadult < 2 ~ 8)) %>% ## Father only
  mutate(citi = case_when(
    FCITI == 5 | MCITI == 5  ~ 1,
    T ~ 0)) %>% 
  filter(AGE < 13) %>%
  dplyr::select(race=RACE,asian=ASIAN,hisp=HISPAN,
                medu=MEDU,fedu=FEDU,typex,mage,fage,
                id=SERIAL,mbpl=MBPL,fbpl=FBPL,region=REGION,metro=METRO,
                year=YEAR,citi,mrace,masian,frace,fasian) %>%  ## Variable selection
  mutate(edu = case_when(
    medu > fedu ~ medu,
    fedu > medu ~ fedu,
    medu == fedu ~ medu,
    is.na(medu)==T ~ fedu,
    is.na(fedu)==T ~ medu
  )) %>%
  mutate(edu = case_when(
    edu == 1 ~ "Less than HS",
    edu == 2 ~ "High school",
    edu == 3 ~ "Some college",
    edu == 4 ~ "BA+"
  )) %>%
  mutate(hisp=case_when(
    hisp == 0 ~ 0,
    hisp > 0 & hisp <= 612 ~ 1,
    TRUE ~ NA
  )) %>%
  mutate(racex=race) %>% 
  mutate(race = case_when(
    hisp == 1 ~ "Hispanic", ## Hispanic
    race == 100 ~ "White", ## White
    race == 200 ~ "Black", ## Black
    race == 300 ~ "AIAN", ## AIAN
    race == 650 ~ "Asian", ## Asian
    race == 651 ~ "Asian", ## Asian
    race == 652 | race >= 700 ~ "Other/more than one race" ## Other two more
  )) %>%
  mutate(famst = case_when(
    typex == 1 ~ "Two parents, married",
    typex == 2 | typex == 3 | typex == 6 ~ "Two parents, cohabiting",
    typex == 4 | typex == 5 | typex == 7 | typex == 8 ~ "Single parent"
  )) %>%
  mutate(asiand = case_when(
    race == "White" ~ "White",
    race == "Asian" & asian == 10 ~ "AI",
    race == "Asian" & asian == 20 ~ "EA",
    race == "Asian" & asian == 40 ~ "EA",
    race == "Asian" & asian == 50 ~ "EA",
    race == "Asian" & asian == 30 ~ "SEA",
    race == "Asian" & asian == 60 ~ "SEA",
    race == "Asian" & asian == 70 ~ "OA"
  )) %>% 
  mutate(asian = case_when(
    race == "White" ~ "White",
    race == "Asian" & asian == 10 ~ "South Asian",
    race == "Asian" & asian == 20 ~ "East Asian",
    race == "Asian" & asian == 40 ~ "East Asian",
    race == "Asian" & asian == 50 ~ "East Asian",
    race == "Asian" & asian == 30 ~ "Southeast Asian",
    race == "Asian" & asian == 60 ~ "Southeast Asian",
    race == "Asian" & asian == 70 ~ "Other Asian"
  )) %>% 
  mutate(multi = case_when(
    racex %in% c(801, 802, 803, 804, 806, 808, 809, 810, 811, 812, 813, 814, 816) ~ "Multi-racial White or Asian",
    TRUE ~ "Other"
  )) %>% 
  mutate(mimmg=case_when(
    mbpl == 9900 & fbpl == 9900 ~ "Mother native born",
    mbpl >= 10000 & mbpl <= 72000  ~ "Mother foreign born"
  )) %>% 
  mutate(immg=case_when(
    mbpl == 9900 & fbpl == 9900 ~ "Parents native born",
    mbpl >= 10000 & mbpl <= 72000  ~ "Parents foreign born",
    fbpl >= 10000 & fbpl <= 72000  ~ "Parents foreign born"
  )) %>% 
  mutate(immgx=case_when(
    immg == "Parents foreign born" & citi == 0 ~ "Parents foreign born, citizen",
    immg == "Parents foreign born" & citi == 1 ~ "Parents foreign born, non-citizen",
    T ~ "Parents native born"
  )) %>% 
  mutate(region=case_when(
    region >= 10 & region <= 19  ~ "Northeast",
    region >= 20 & region <= 29  ~ "Midwest",
    region >= 30 & region <= 39  ~ "South",
    region >= 40 & region <= 49  ~ "West"
  )) %>% 
  group_by(id) %>% 
  mutate(n=row_number()) %>% 
  filter(n == 1) %>% 
  filter(is.na(immg)==F) %>% 
  mutate(asianx=ifelse(asian=="White",NA,asian))

write_dta(dfx,"Data/CPS-ASEC/CPS_analysis.dta")

######################################################################
# Table construction
######################################################################
tab1 <- dfx %>% 
  filter(is.na(mimmg)==F) %>% 
  mutate(medu=case_when(
    medu == 1 ~ "LTHS",
    medu == 2 ~ "HS",
    medu == 3 ~ "SC",
    medu == 4 ~ "BA+",
  )) %>% 
  group_by(masian,medu,famst) %>%
  dplyr::select(masian,medu,famst) %>%
  mutate(n=1) %>%
  summarise_all((sum)) %>%
  ungroup() %>%
  group_by(masian,medu) %>%
  mutate(prop=n/sum(n)) %>%
  mutate(famst = factor(famst,levels=c("Two parents, married",
                                       "Two parents, cohabiting",
                                       "Single parent"))) %>%
  mutate(medu = factor(medu,levels=c("LTHS",
                                   "HS",
                                   "SC",
                                   "BA+"))) %>%
  mutate(masian = factor(masian,levels=c("White","South Asian",
                                       "East Asian","Southeast Asian",
                                       "Other Asian"))) %>%
  filter(is.na(masian)==FALSE) %>% 
  mutate(mimmg="Total")

tab2 <- dfx %>% 
  filter(is.na(mimmg)==F) %>% 
  mutate(medu=case_when(
    medu == 1 ~ "LTHS",
    medu == 2 ~ "HS",
    medu == 3 ~ "SC",
    medu == 4 ~ "BA+")) %>% 
  group_by(masian,medu,mimmg,famst) %>%
  dplyr::select(masian,medu,famst,mimmg) %>%
  mutate(n=1) %>%
  summarise_all((sum)) %>%
  ungroup() %>%
  group_by(masian,medu,mimmg) %>%
  mutate(prop=n/sum(n)) %>%
  mutate(famst = factor(famst,levels=c("Two parents, married",
                                       "Two parents, cohabiting",
                                       "Single parent"))) %>%
  mutate(medu = factor(medu,levels=c("LTHS",
                                   "HS",
                                   "SC",
                                   "BA+"))) %>%
  mutate(masian = factor(masian,levels=c("White","South Asian",
                                       "East Asian","Southeast Asian",
                                       "Other Asian"))) %>%
  filter(is.na(masian)==FALSE)

tab <- rbind(tab1,tab2) %>% 
  mutate(mimmg=factor(mimmg,levels=c("Total","Mother foreign born",
                                   "Mother native born")))

######################################################################
# Figure
######################################################################
ggplot(tab,aes(x = medu, y = n)) +
  geom_bar(
    aes(fill = famst),
    stat="identity",
    position=position_fill(vjust=0.5)) + theme_few() + 
  facet_grid(mimmg~masian) + 
  scale_fill_brewer(palette="Dark2",name = "Living arrangement",guide = guide_legend(nrow=3)) + 
  xlab("")+ 
  ylab("Distribution (percent)") +
  scale_y_continuous(labels = scales::number_format(scale = 100, accuracy = 1)) +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0),)+
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  #labs(caption = "Source: CPS-ASEC 2013-2024.")

ggsave(height=6,width=9,dpi=200, filename="Results/Figure3.pdf",family="Helvetica")
