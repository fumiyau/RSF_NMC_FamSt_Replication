#library(openxlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggthemes)
#library(tidylog)
library(readxl)
#library(ggokabeito)
library(haven)
library(ggplot2)
library(ggsci)
library(egg)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggrepel)

######################################################################
# Main results
######################################################################

df1 <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Model2_19Jun2026.dta") %>% 
  mutate(race = c(rep("White",4),rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),5),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu) %>% 
  mutate(group="Total")

df1a <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Model3_19Jun2026.dta") %>% 
  mutate(race = c(rep("White",4),rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),5),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu) %>% 
  mutate(group="Model 3")

df2 <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Foregin_Model1_19Jun2026.dta") %>% 
  mutate(race = c(rep("White",4),rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),5),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu)  %>% 
  mutate(group="Foreign-born")

df3 <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Native_Model1_19Jun2026.dta") %>% 
  mutate(race = c(rep("White",4),rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),5),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu)  %>% 
  mutate(group="Native-born") 


rbind(df1,df2,df3) %>% 
  mutate(group=factor(group,levels=c("Total","Foreign-born","Native-born"))) %>% 
  ggplot(aes(x=edu, y=margin, 
             color=race, 
             shape=race,
             group=race)) +
  xlab("")+ylab("")+ 
  facet_wrap(.~group) +
  geom_line(alpha = 0.8,
            linewidth=0.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.075) +
  geom_point(size=1.5,
             fill="white") + 
  theme_few() +
  ggsci::scale_color_jco() +
  scale_y_continuous(limits = c(0.0,1))+
  theme(legend.title=element_blank(),
        legend.position = "right",
        legend.text=element_text(size=9,family="Helvetica"),
        plot.caption = element_text(hjust = 0),
        legend.spacing.y = unit(-0.1, "cm"),
        axis.text.x=element_text(family="Helvetica"),
        axis.text.y=element_text(family="Helvetica")) 
  #labs(caption = "Source: NVSS 2015-2019.")
ggsave(height=5,width=11,dpi=200, filename="Results/Figure1.pdf",family="Helvetica")

######################################################################
# Intermarriage
######################################################################

df1 <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Native_Inter_Model2_19Jun2026.dta") %>% 
  mutate(race = c(rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),4),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu) %>% 
  mutate(native="Native-born",
         inter="Exogamy")

df2 <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Native_Endogamy_Model2_19Jun2026.dta") %>% 
  mutate(race = c(rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),4),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu) %>% 
  mutate(native="Native-born",
         inter="Endogamy")

df3 <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Foregin_Endogamy_Model2_19Jun2026.dta") %>% 
  mutate(race = c(rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),4),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu) %>% 
  mutate(native="Foregin-born",
         inter="Exogamy")

df4 <- read_dta("Results/NMC/margins_Logit_NMC_Asian_Foregin_Inter_Model2_19Jun2026.dta") %>% 
  mutate(race = c(rep("South Asian",4),
                  rep("East Asian",4),rep("Southeast Asian",4),
                  rep("Other Asian",4)),
         race = factor(race,levels=c("White","South Asian","East Asian","Southeast Asian","Other Asian")),
         edu = rep(c("Less than HS","HS","Some college","BA+"),4),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  dplyr::select(margin=6,lower=10,upper=11,race,edu) %>% 
  mutate(native="Foregin-born",
         inter="Endogamy")

df <- bind_rows(df1,df2,df3,df4)

ggplot(df,aes(x=edu, y=margin, 
           color=race, 
           shape=race,
           group=race)) +
  xlab("")+ylab("")+ 
  facet_grid(native~inter) +
  geom_line(alpha = 0.8,
            linewidth=0.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.075) +
  geom_point(size=1.5,
             fill="white") + 
  theme_few() +
  ggsci::scale_color_jco() +
  scale_y_continuous(limits = c(0.0,1))+
  theme(legend.title=element_blank(),
        #legend.position = c(0.8, 0.825),
        legend.text=element_text(size=9,family="Helvetica"),
        plot.caption = element_text(hjust = 0),
        legend.spacing.y = unit(-0.1, "cm"),
        axis.text.x=element_text(family="Helvetica"),
        axis.text.y=element_text(family="Helvetica")) 
  #labs(caption = "Source: NVSS 2015-2019.")
ggsave(height=5,width=7.5,dpi=200, filename="Results/Figure2.pdf",family="Helvetica")

