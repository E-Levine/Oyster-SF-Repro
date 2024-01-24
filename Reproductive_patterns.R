####Reproductive comparisons of oysters in SF estuaries
#Reproductive patterns
#
#
#Load packages, install as needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               rstatix, rcompanion, #Summary stats & multiple comparisons
               zoo, lubridate, #Dates and times
               readxl, writexl, #Reading excel files
               car, scales, #Basic analyses
               lmPerm, lme4, glmmTMB, DHARMa, #ordinal,
               emmeans, multcomp, ggpubr, #Arranging ggplots
               install = TRUE)
#
#
#
#
#
####Load files####
Repro_df <- read_excel("Output/Repro_data_2024 01_cleaned.xlsx", sheet = "Sheet1", #File name and sheet name
                       skip = 0, col_names = TRUE, 
                       na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                       .name_repair = "universal") %>% 
  mutate_at(c("Station", "Sex", "Site", "Estuary", "Final_Stage"), as.factor) %>% 
  subset(Site != "TB" & Site != "LW-R")

head(Repro_df)
summary(Repro_df)
#
#
##Proportion per stage
Repro_props <- read_excel("Output/Repro_proportions_2024 01.xlsx", sheet = "Sheet1", #File name and sheet name
                          skip = 0, col_names = TRUE, 
                          na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "universal") %>% 
  subset(Site != "TB" & Site != "LW-R")
#
summary(Repro_props)
#
#
#
####Formatting and helpers####
#
#Map color to Stage
Stages <- c("1" = "Developing", "2" = "Ripe/Spawning", "3" = "Spent/Recycling", "4" = "Indifferent", "8" = "Buceph", "M/F" = "Herm")
cbPalette <- c("#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#9966FF")
names(cbPalette) <- levels(Repro_df$Final_Stage)
StaFill <- scale_fill_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
#
#
##
###
####Overall annual pattern####
#
##Working with all_oysters
All_oysters_clean <- Repro_df %>% subset(Final_Stage != "M/F" & Final_Stage != "Buceph") %>% droplevels() %>%
  subset(Year != "2005" & Year != "2006") %>% droplevels() %>% #Comparing first "complete" year
  mutate(Year = factor(Year, levels = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")),
         Month = factor(Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")),
         ID = row_number())
#Group by Month to compare among months 
(Monthly_mean_counts_All <- All_oysters_clean %>% 
    group_by(Month, Final_Stage) %>% 
    count() %>%
    summarize(meanCount = mean(n),
              minCount = min(n),
              maxCount = max(n)) %>%
    pivot_wider(id_cols = Month,
                names_from = Final_Stage,
                values_from = c("meanCount", "minCount", "maxCount"), 
                names_glue = "{Final_Stage}_{.value}"))
#  
All_oysters_clean %>% group_by(Month) %>% 
  ggplot(aes(Month, fill = Final_Stage))+
  geom_bar(position = "fill")+
  scale_y_continuous("Percentage", labels = scales::percent_format(), expand = c(0,0))+
  StaFill
#
All_oysters_clean %>% group_by(Month) %>% 
  ggplot(aes(Month, fill = Final_Stage))+
  geom_bar(position = "fill")+
  lemon::facet_rep_grid(Year~.)+
  scale_y_continuous("Percentage", labels = scales::percent_format(), expand = c(0,0))+
  StaFill
#
#
#
####glm proportions - All: Year, Month####
#
(Overall_counts <- left_join(All_oysters_clean %>% group_by(Year, Month, Final_Stage) %>% summarise(Count= n()),
                             All_oysters_clean %>% group_by(Year, Month) %>% summarise(Total= n())) %>% 
   mutate(Prop = Count/Total) %>% ungroup() %>% 
   complete(Final_Stage, Year, Month, fill = list(Count = 0, Total = 0, Prop = 0)) %>% mutate(ID = row_number())) 
#
##View data
hist(Overall_counts$Prop, breaks = 20)
ggplot(Overall_counts, aes(Year, Prop, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Final_Stage)
#Rescale and view trends
Overall_counts <- Overall_counts %>% mutate(sProp = scales::rescale(Prop, to = c(0.00001, 0.99999))) #Remove 0s and 1s
ggplot(Overall_counts, aes(Year, sProp, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Final_Stage)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#
#Build model
Prop_mod1 <- glmmTMB(sProp ~ Year * Final_Stage, data = Overall_counts, family = "beta_family")
summary(Prop_mod1)
plot(simulateResiduals(Prop_mod1)) #No issues
testZeroInflation(Prop_mod1) #Not sig 
testDispersion(Prop_mod1) #Sig
testOutliers(Prop_mod1) #some
testQuantiles(Prop_mod1)
testCategorical(Prop_mod1, catPred = Overall_counts$Year)
testCategorical(Prop_mod1, catPred = Overall_counts$Final_Stage)
#
summary(Prop_mod1)
(Prop_yr_summ <- tidy(Anova(Prop_mod1)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m1_em <- emmeans(Prop_mod1, ~Year*Final_Stage, type = "response"))
(Prop_m1_pairs <- pairs(Prop_m1_em, simple = "Year", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null) %>%
  mutate(contrast = gsub("Year", "", contrast)))
#
(Prop_means <- Overall_counts %>% group_by(Year, Final_Stage) %>%
  summarise(meanProp = round(mean(Prop), 3),
            sdProp = round(sd(Prop), 3),
            minProp = round(min(Prop), 3),
            maxProp = round(max(Prop), 3)))
#
Overall_counts %>%
  ggplot(aes(Year, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Final_Stage~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
#
#MONTHS
#
ggplot(Overall_counts, aes(Month, sProp, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Final_Stage)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#
Prop_mod2 <- glmmTMB(sProp ~ Month * Final_Stage, data = Overall_counts, family = "beta_family")
summary(Prop_mod2)
plot(simulateResiduals(Prop_mod2)) 
testZeroInflation(Prop_mod1) #Not sig 
testDispersion(Prop_mod2) #Sig
testOutliers(Prop_mod2) #some
testQuantiles(Prop_mod2)
testCategorical(Prop_mod2, catPred = Overall_counts$Month)
testCategorical(Prop_mod2, catPred = Overall_counts$Final_Stage)
#
summary(Prop_mod2)
(Prop_mn_summ <- tidy(Anova(Prop_mod2)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m2_em <- emmeans(Prop_mod2, ~Month*Final_Stage, type = "response"))
(Prop_m2_pairs <- pairs(Prop_m2_em, simple = "Month", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null) %>%
    mutate(contrast = gsub("Month", "", contrast)))
#
(Prop_means2 <- Overall_counts %>% group_by(Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
Overall_counts %>%
  ggplot(aes(Month, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Final_Stage~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
#
####glm proportions - Estuaries: All, Year, Month####
#
#ESTUARY
(Site_counts <- left_join(All_oysters_clean %>% group_by(Year, Month, Estuary, Site, Final_Stage) %>% summarise(Count= n()),
                          All_oysters_clean %>% group_by(Year, Month, Estuary, Site) %>% summarise(Total= n())) %>% 
   mutate(Prop = Count/Total) %>% ungroup() %>% 
   complete(Final_Stage, Year, Month, Site, fill = list(Count = 0, Total = 0, Prop = 0)) %>% mutate(ID = row_number()) %>% 
   mutate(sProp = scales::rescale(Prop, to = c(0.00001, 0.99999)),
          Estuary = as.factor(ifelse(Site == "LW", "LW", 
                                     ifelse(Site == "CR-W" | Site == "CR-E", "CR",
                                            ifelse(Site == "LX-N" | Site == "LX-S", "LX", "SL")))),
          Coast = as.factor(ifelse(Estuary == "CR", "West", "East"))))
#
post2017 <- Site_counts %>% filter(Year == "2017" | Year == "2018" | Year == "2019" | Year == "2020" | Year == "2021" | Year == "2022" | Year == "2023") %>% droplevels()
pre2017 <- anti_join(Site_counts, post2017) %>% subset(Estuary != "CR") %>% droplevels()
#
ggplot(Site_counts, aes(Estuary, sProp, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Final_Stage)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#
Prop_mod3 <- glmmTMB(Prop ~ Estuary * Final_Stage * (1|Site), data = Site_counts, family = "ordbeta")
summary(Prop_mod3)
plot(simulateResiduals(Prop_mod3)) 
testZeroInflation(Prop_mod3) #Not sig 
testDispersion(Prop_mod3) #Sig
testOutliers(Prop_mod3) #some
testQuantiles(Prop_mod3)
testCategorical(Prop_mod3, catPred = Site_counts$Estuary)
testCategorical(Prop_mod3, catPred = Site_counts$Final_Stage)
#
summary(Prop_mod3)
(Prop_SiteYr_summ <- tidy(Anova(Prop_mod3)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m3_em <- emmeans(Prop_mod3, ~Estuary*Final_Stage, type = "response"))
(Prop_m3_pairs <- pairs(Prop_m3_em, simple = "Estuary", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null) %>%
    mutate(contrast = gsub("Month", "", contrast)))
#
(Prop_means3 <- Site_counts %>% group_by(Estuary, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
Site_counts %>%
  ggplot(aes(Estuary, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Final_Stage~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
#
ggplot(pre2017, aes(Estuary, Prop, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Final_Stage)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#
Prop_mod4 <- glmmTMB(Prop ~ Estuary * Final_Stage * (1|Site), data = pre2017, family = "ordbeta")
summary(Prop_mod4)
plot(simulateResiduals(Prop_mod4)) 
testZeroInflation(Prop_mod4) #Not sig 
testDispersion(Prop_mod4) #Sig
testOutliers(Prop_mod4) #some
testQuantiles(Prop_mod4)
testCategorical(Prop_mod4, catPred = pre2017$Estuary)
testCategorical(Prop_mod4, catPred = pre2017$Final_Stage)
#
Anova(Prop_mod4)
(Prop_2016Yr_summ <- tidy(Anova(Prop_mod4)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m4_em <- emmeans(Prop_mod4, ~Estuary*Final_Stage, type = "response"))
(Prop_m4_pairs <- pairs(Prop_m4_em, simple = "Estuary", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null))
#
(Prop_means4 <- pre2017 %>% group_by(Estuary, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
pre2017 %>%
  ggplot(aes(Estuary, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Final_Stage~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
ggplot(post2017, aes(Estuary, Prop, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Final_Stage)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#
Prop_mod5 <- glmmTMB(Prop ~ Estuary * Final_Stage * (1|Site), data = post2017, family = "ordbeta")
summary(Prop_mod5)
plot(simulateResiduals(Prop_mod5)) 
testZeroInflation(Prop_mod5) #Not sig 
testDispersion(Prop_mod5) #Sig
testOutliers(Prop_mod5) #none
testQuantiles(Prop_mod5)
testCategorical(Prop_mod5, catPred = post2017$Estuary)
testCategorical(Prop_mod5, catPred = post2017$Final_Stage)
#
Anova(Prop_mod5)
(Prop_2023Yr_summ <- tidy(Anova(Prop_mod5)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m5_em <- emmeans(Prop_mod5, ~Estuary*Final_Stage, type = "response"))
(Prop_m5_pairs <- pairs(Prop_m5_em, simple = "Estuary", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null))
#
(Prop_means5 <- post2017 %>% group_by(Estuary, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
post2017 %>%
  ggplot(aes(Estuary, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Final_Stage~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
#
#Estuary * YEAR
#
ggplot(pre2017, aes(Year, Prop, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Estuary)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#
Prop_mod6 <- glmmTMB(Prop ~ Estuary * Year * Final_Stage * (1|Site), data = pre2017, family = "ordbeta")
summary(Prop_mod6)
plot(simulateResiduals(Prop_mod6)) 
testZeroInflation(Prop_mod6) #Not sig 
testDispersion(Prop_mod6) #Sig
testOutliers(Prop_mod6) #some
testQuantiles(Prop_mod6)
testCategorical(Prop_mod6, catPred = pre2017$Estuary)
testCategorical(Prop_mod6, catPred = pre2017$Year)
testCategorical(Prop_mod6, catPred = pre2017$Final_Stage)
#
Anova(Prop_mod6)
(Prop_EstYr_summ <- tidy(Anova(Prop_mod6)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m6_em <- emmeans(Prop_mod6, ~Estuary*Year*Final_Stage, type = "response"))
(Prop_m6_pairs <- pairs(Prop_m6_em, simple = "Year", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null) %>%
    mutate(contrast = gsub("Year", "", contrast)) %>% arrange(Estuary, Final_Stage))
#
(Prop_means6 <- pre2017 %>% group_by(Estuary, Year, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
pre2017 %>%
  ggplot(aes(Year, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Estuary~Final_Stage)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
Prop_means6 %>%
  ggplot(aes(Year, meanProp, color = Final_Stage, group = Final_Stage))+
  geom_line(linewidth = 1)+
  lemon::facet_rep_grid(Estuary~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
#
ggplot(post2017, aes(Year, Prop, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Estuary)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#
Prop_mod7 <- glmmTMB(Prop ~ Estuary * Year * Final_Stage * (1|Site), data = post2017, family = "ordbeta")
summary(Prop_mod7)
plot(simulateResiduals(Prop_mod7)) 
testZeroInflation(Prop_mod7) #Not sig 
testDispersion(Prop_mod7) #Sig
testOutliers(Prop_mod7) #some
testQuantiles(Prop_mod7)
testCategorical(Prop_mod7, catPred = post2017$Estuary)
testCategorical(Prop_mod7, catPred = post2017$Year)
testCategorical(Prop_mod7, catPred = post2017$Final_Stage)
#
Anova(Prop_mod7)
Prop_mod7_2 <- glmmTMB(Prop ~ Estuary * Year * (1|Site), data = post2017, family = "ordbeta")
plot(simulateResiduals(Prop_mod7_2)) 
Anova(Prop_mod7_2)
(Prop_EstYr17_summ <- tidy(Anova(Prop_mod7_2)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m7_em <- emmeans(Prop_mod7_2, ~Estuary*Year, type = "response"))
(Prop_m7_pairs <- pairs(Prop_m7_em, simple = "Year", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null) %>%
    mutate(contrast = gsub("Year", "", contrast)) %>% arrange(Estuary))
#
(Prop_means7 <- post2017 %>% group_by(Estuary, Year) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
post2017 %>%
  ggplot(aes(Year, Prop, fill = Estuary))+
  geom_boxplot()+
  #lemon::facet_rep_grid(Estuary~Final_Stage)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
#
#
####glm proportions - By Estuary - changed by year, month, monyr####
#
(Worth <- left_join(All_oysters_clean %>% filter(Site == "LW" | Site == "LW-R") %>% group_by(Year, Month, Station, Final_Stage) %>% summarise(Count= n()),
         All_oysters_clean  %>% filter(Site == "LW" | Site == "LW-R") %>% group_by(Year, Month, Station) %>% summarise(Total= n())) %>% 
  mutate(Prop = Count/Total) %>% ungroup() %>% 
  complete(Final_Stage, nesting(Year, Month, Station), fill = list(Count = 0, Total = 0, Prop = 0)) %>% mutate(ID = row_number(),
                                                                                                      MonYr = as.yearmon(paste0(Year, "/", Month), format = "%Y/%m"))) 
#
##View data
hist(Worth$Prop, breaks = 20)
ggplot(Worth, aes(Year, Prop, color = Final_Stage))+ geom_point() + lemon::facet_rep_grid(.~Final_Stage)+
  geom_smooth(method = "glm", method.args = list(family = "beta_family"), formula = y~x)
#Build model
Worth_mod1 <- glmmTMB(Prop ~ Year * Final_Stage * (1|Station), data = Worth, family = "ordbeta")
summary(Worth_mod1)
plot(simulateResiduals(Worth_mod1)) #No issues
testZeroInflation(Worth_mod1) #Not sig 
testDispersion(Worth_mod1) #Sig
testOutliers(Worth_mod1) #some
testQuantiles(Worth_mod1)
testCategorical(Worth_mod1, catPred = Worth$Year)
testCategorical(Worth_mod1, catPred = Worth$Final_Stage)
#
Anova(Worth_mod1)
(Worth_yr_summ <- tidy(Anova(Worth_mod1)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Worth_mod1_em <- emmeans(Worth_mod1, ~Year*Final_Stage, type = "response"))
(Worth_mod1_pairs <- pairs(Worth_mod1_em, simple = "Year", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null) %>%
    mutate(contrast = gsub("Year", "", contrast)))
#
(Worth_yr_means <- Worth %>% group_by(Year, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
Worth %>%
  ggplot(aes(Year, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Final_Stage~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
#
#
#

