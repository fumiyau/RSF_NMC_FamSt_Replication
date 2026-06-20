######################################################################
# Prediction
######################################################################
df1 <- read_dta("Results/FamilyStructure/Prediction_Logit_Asian_1May2025.dta") %>% 
  dplyr::select(margin=6,lower=10,upper=11,asian=12,edu=13) %>% 
  mutate(group="Total")

df2 <- read_dta("Results/FamilyStructure/Prediction_Logit_Asian_Foreign_1May2025.dta") %>% 
  dplyr::select(margin=6,lower=10,upper=11,asian=12,edu=13) %>% 
  mutate(group="Foreign-born")

df3 <- read_dta("Results/FamilyStructure/Prediction_Logit_Asian_Native_1May2025.dta") %>% 
  dplyr::select(margin=6,lower=10,upper=11,asian=12,edu=13) %>% 
  mutate(group="Native-born")

df <- rbind(df1,df2,df3) %>% 
  mutate(asian = case_when(
    asian == 1 ~ "East Asian",
    asian == 2 ~ "Other Asian",
    asian == 3 ~ "South Asian",
    asian == 4 ~ "Southeast Asian",
    asian == 5 ~ "White"
  )) %>% 
  mutate(edu = case_when(
    edu == 4 ~ "BA+",
    edu == 2 ~ "HS",
    edu == 1 ~ "Less than HS",
    edu == 3 ~ "Some college",
  )) %>% 
  mutate(group = factor(group,levels=c("Total","Foreign-born","Native-born"))) %>% 
  mutate(asian = factor(asian,levels=c("White","South Asian","East Asian",
                                       "Southeast Asian","Other Asian","Multi-racial")),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  # remove cases less than 20
  mutate(margin=case_when(
    asian == "South Asian" & edu == "Less than HS" ~ NA,
    asian != "White" & edu == "Less than HS" & group == "Native-born" ~ NA,
    asian == "South Asian" & edu != "BA+" & group == "Native-born" ~ NA,
    #asian == "East Asian" & edu == "HS"  & group == "Native-born" ~ NA,
    T ~ margin
  )) %>%
  mutate(lower=ifelse(is.na(margin)==T,NA,lower),
         upper=ifelse(is.na(margin)==T,NA,upper)) %>% 
  filter(is.na(margin)==F)

df %>% 
  #filter(asian!="White") %>% 
  ggplot(aes(x=edu, y=margin, 
             color=asian, 
             group=asian)) +
  xlab("")+ylab("")+ 
  geom_line(alpha = 0.8,
            position = position_dodge(0.3),
            linewidth=1) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                position = position_dodge(0.3),
                width=.1) +
  geom_point(size=1.5,
             position = position_dodge(0.3),
             fill="white") + 
  theme_few() +
  facet_grid(.~group) + 
  ggsci::scale_color_jco() +
  scale_y_continuous(limits = c(0.0,1.1))+
  theme(legend.title=element_blank(),
        legend.position = "right",
        legend.text=element_text(size=11,family="Helvetica"),
        plot.caption = element_text(hjust = 0),
        axis.text.x=element_text(family="Helvetica"),
        axis.text.y=element_text(family="Helvetica"))
#labs(caption = "Source: CPS-ASEC 2013-2024.\nNote: Error bars indicate 95% confidence intervals. Predicted values based on less than 20 cases are dropped.")
ggsave(height=6,width=11,dpi=200, filename="Results/Figure4.pdf",family="Helvetica")
#ggsave(height=4,width=6,dpi=200, filename="Results/FamilyStructure/Figure4.png")

df4 <- read_dta("Results/FamilyStructure/Prediction_Logit_Asian_Endogamy_Total_1May2025.dta") %>% 
  dplyr::select(cat=3,margin=6,lower=10,upper=11,asian=12,edu=13) %>% 
  mutate(group="Total")

df5 <- read_dta("Results/FamilyStructure/Prediction_Logit_Asian_Endogamy_Foreign_1May2025.dta") %>% 
  dplyr::select(cat=3,margin=6,lower=10,upper=11,asian=12,edu=13) %>% 
  mutate(group="Foreign-born")

df6 <- read_dta("Results/FamilyStructure/Prediction_Logit_Asian_Endogamy_Native_1May2025.dta") %>% 
  dplyr::select(cat=3,margin=6,lower=10,upper=11,asian=12,edu=13) %>% 
  mutate(group="Native-born")

df <- rbind(df4,df5,df6) %>% 
  mutate(cat = case_when(
    cat == 1 ~ "Single parent or two parent, cohabiting", 
    cat == 2 ~ "Two parent, married & racial intermarriage",
    cat == 3 ~ "Two parent, married & racial endogamy"
  )) %>% 
  mutate(asian = case_when(
    asian == 1 ~ "East Asian",
    asian == 2 ~ "Other Asian",
    asian == 3 ~ "South Asian",
    asian == 4 ~ "Southeast Asian"
  )) %>% 
  mutate(edu = case_when(
    edu == 4 ~ "BA+",
    edu == 2 ~ "HS",
    edu == 1 ~ "Less than HS",
    edu == 3 ~ "Some college",
  )) %>% 
  mutate(group = factor(group,levels=c("Total","Foreign-born","Native-born"))) %>% 
  mutate(asian = factor(asian,levels=c("White","South Asian","East Asian",
                                       "Southeast Asian","Other Asian","Multi-racial")),
         edu = factor(edu,levels=c("Less than HS","HS","Some college","BA+"))) %>% 
  # remove cases less than 20
  mutate(margin=case_when(
    asian == "South Asian" & edu == "Less than HS" ~ NA,
    asian == "South Asian" & group == "Native-born" & edu != "BA+" ~ NA,
    edu == "Less than HS" & group == "Native-born" ~ NA,
    # asian != "Other Asian" & edu == "Less than HS" & group == "Racial endogamy" ~ NA,
    # asian == "South Asian" & edu != "BA+" & group == "Racial intermarriage" ~ NA,
    T ~ margin
  )) %>%
  mutate(lower=ifelse(is.na(margin)==T,NA,lower),
         upper=ifelse(is.na(margin)==T,NA,upper)) %>% 
  filter(is.na(margin)==F)

df %>% 
  #filter(asian!="White") %>% 
  ggplot(aes(x=edu, y=margin, 
             color=asian, 
             group=asian)) +
  xlab("")+ylab("")+ 
  # geom_errorbar(aes(ymin=lower, ymax=upper), 
  #               width=.1,
  #               col or="black",
  #               position=position_dodge(0.3)) +
  #scale_shape_manual(values = c(21:25))+
  geom_line(alpha = 0.8,
            position = position_dodge(0.3),
            linewidth=1) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                position = position_dodge(0.3),
                width=.1) +
  geom_point(size=1.5,
             position = position_dodge(0.3),
             fill="white") + 
  theme_few() +
  facet_grid(group~cat) + 
  ggsci::scale_color_jco() +
  scale_y_continuous(limits = c(-0.2,1.1))+
  theme(legend.title=element_blank(),
        legend.position = "right",
        legend.text=element_text(size=11,family="Helvetica"),
        plot.caption = element_text(hjust = 0),
        axis.text.x=element_text(family="Helvetica"),
        axis.text.y=element_text(family="Helvetica"))
#labs(caption = "Source: CPS-ASEC 2013-2024.\nNote: Error bars indicate 95% confidence intervals. Predicted values based on less than 20 cases are dropped.")
ggsave(height=8,width=12,dpi=200, filename="Results/Figure5.pdf",family="Helvetica")
