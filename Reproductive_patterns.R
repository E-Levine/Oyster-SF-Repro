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
####glm proportions####
#
##Add sample level factor (ID)
#
(Overall_counts <- left_join(All_oysters_clean %>% group_by(Year, Month, Final_Stage) %>% summarise(Count= n()),
                             All_oysters_clean %>% group_by(Year, Month) %>% summarise(Total= n())) %>% 
   mutate(#Year = as.numeric(paste(Year)),
          Prop = Count/Total) %>% ungroup() %>% 
   complete(Final_Stage, nesting(Year, Month), fill = list(Count = 0, Total = 0, Prop = 0)) %>% mutate(ID = row_number())) 
#
##VIew data
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
plot(simulateResiduals(Prop_mod1)) 
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
#
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
#
#