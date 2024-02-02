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
####glm - Counts - Year, Month v Stage####
#
(Overall_counts <- left_join(All_oysters_clean %>% group_by(Year, Month, Final_Stage) %>% summarise(Count= n()),
                            All_oysters_clean %>% group_by(Year, Month) %>% summarise(Total= n())) %>% 
  mutate(Year = as.numeric(paste(Year)),
         Prop = Count/Total) %>% ungroup() %>% complete(Final_Stage, nesting(Year, Month), fill = list(Count = 0, Total = 0, Prop = 0)))
#
##Check response
hist(Overall_counts$Count, breaks = 42)
#
##Fit model
Overall_model <- glmmTMB(Count ~ Year + Month + Final_Stage, contrasts = list(Month = "contr.sum"),
                         data = Overall_counts, family = poisson)
summary(Overall_model)
#Coefficients- last one = -sum(fixef(all other levels))
#
##Model checking - DHARMa
plot(simulateResiduals(Overall_model)) #Overdispersion - try negative binomial
#
Overall_model2 <- update(Overall_model, family = nbinom2)
summary(Overall_model2)
plot(simulateResiduals(Overall_model2)) #better
anova(Overall_model, Overall_model2)
testZeroInflation(Overall_model2) #Not sig 
testDispersion(Overall_model2) #Sig
testOutliers(Overall_model2)
testQuantiles(Overall_model2)
testCategorical(Overall_model2, catPred = Overall_counts$Month) #1-0.01; 4-0.03
testCategorical(Overall_model2, catPred = Overall_counts$Final_Stage) #1-<0.001
#
Overall_model3 <- glmmTMB(Count ~ as.numeric(Year) + Month + offset(log(as.numeric(Final_Stage))),
                          data = Overall_counts, family = nbinom2)
summary(Overall_model3)
plot(simulateResiduals(Overall_model3))
anova(Overall_model2, Overall_model3) #2 is better than 3
#
Overall_model4 <- update(Overall_model2, ziformula = ~1)
anova(Overall_model2, Overall_model4) #Better = 2
#
plot(simulateResiduals(Overall_model2), form = Overall_counts$Year)
plot(simulateResiduals(Overall_model2), form = Overall_counts$Month)
plot(simulateResiduals(Overall_model2), form = Overall_counts$Final_Stage)
#
Overall_model5 <- glmmTMB(Count ~ as.numeric(Year) + Month + (1|Final_Stage),
                          data = Overall_counts, family = nbinom2) 
summary(Overall_model5)
plot(simulateResiduals(Overall_model5)) 
anova(Overall_model2, Overall_model5)
#
Overall_model6 <- glmmTMB(Count ~ as.numeric(Year) + Final_Stage + (1|Month),
                          data = Overall_counts, family = nbinom2) 
summary(Overall_model6)
plot(simulateResiduals(Overall_model6)) 
anova(Overall_model2, Overall_model6)
#
Overall_model7 <- glmmTMB(Count ~ Year * Month * Final_Stage, contrasts = list(Month = "contr.sum"),
                          data = Overall_counts, family = poisson)
summary(Overall_model7)
anova(Overall_model2, Overall_model7)
#
Overall_model8 <- update(Overall_model2, ziformula = ~Final_Stage + Month)
summary(Overall_model8)
anova(Overall_model2, Overall_model8) ###BETTER
testZeroInflation(Overall_model8) #Not sig 
testDispersion(Overall_model8) #Sig
testOutliers(Overall_model8)
testQuantiles(Overall_model8)
testCategorical(Overall_model8, catPred = Overall_counts$Month) #1-0.004
testCategorical(Overall_model8, catPred = Overall_counts$Final_Stage) #2-0.04; 4-0.004
plot(simulateResiduals(Overall_model8)) 
#
Overall_model9 <- update(Overall_model2, dispformula = ~Final_Stage + Month)
anova(Overall_model2, Overall_model9) ###BETTER
plot(simulateResiduals(Overall_model9)) 
testZeroInflation(Overall_model9) #Not sig 
testDispersion(Overall_model9) #Sig
testOutliers(Overall_model9)
testQuantiles(Overall_model9)
testCategorical(Overall_model9, catPred = Overall_counts$Month)
testCategorical(Overall_model9, catPred = Overall_counts$Final_Stage)
#
#
#
####glm ESTUARY####
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
East_counts <- Site_counts %>% filter(Coast == "East") %>% droplevels()
Prop_mod4 <- glmmTMB(Prop ~ Estuary * Final_Stage * (1|Site), data = East_counts, family = "ordbeta")
summary(Prop_mod4)
plot(simulateResiduals(Prop_mod4)) 
testZeroInflation(Prop_mod4) #Not sig 
testDispersion(Prop_mod4) #Sig
testOutliers(Prop_mod4) #some
testQuantiles(Prop_mod4)
testCategorical(Prop_mod4, catPred = East_counts$Estuary)
testCategorical(Prop_mod4, catPred = East_counts$Final_Stage)
#
summary(Prop_mod4)
(Prop_East_summ <- tidy(Anova(Prop_mod4)) %>% rename("F" = statistic) %>% mutate_if(is.numeric,round, digits = 3))
(Prop_m4_em <- emmeans(Prop_mod4, ~Estuary, type = "response"))
(Prop_m4_pairs <- pairs(Prop_m4_em, simple = "Estuary", adjust = "tukey") %>% as.data.frame() %>% dplyr::select(-df, -null) )
#
(Prop_means4 <- East_counts %>% group_by(Estuary, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3),
              sdProp = round(sd(Prop), 3),
              minProp = round(min(Prop), 3),
              maxProp = round(max(Prop), 3)))
#
East_counts %>%
  ggplot(aes(Estuary, Prop, fill = Final_Stage))+
  geom_boxplot()+
  lemon::facet_rep_grid(Final_Stage~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
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
####WORKING####
##Ordinal logistic regression - ordinal
All_1 <- clm(Final_Stage ~ Year + Month, data = All_oysters_clean, Hess = TRUE)
summary(All_1)
anova(All_1)
#Post hoc
All_1_Year <- emmeans(All_1,"Year")
All_1_Month <- emmeans(All_1, "Month")
pairs(All_1_Year, adjust = "tukey")
pairs(All_1_Month, adjust = "tukey")
multcomp::cld(All_1_Year, Letters = letters)
multcomp::cld(All_1_Month, Letters = letters)
#Assumptions
nominal_test(All_1) #Effects
scale_test(All_1) #Effects
#
All_2 <- clm(Final_Stage ~ Year, data = All_oysters_clean, Hess = TRUE)
summary(All_2)
anova(All_1, All_2)
#All_1 better
#Assumptions
nominal_test(All_1) #Evidence of non-proportional odds
scale_test(All_1) #Evidence of scale effects
#
All_1nom <- clm(Final_Stage ~ Year, nominal = ~Month, data = All_oysters_clean)
anova(All_1, All_1nom) #Better
nominal_test(All_1nom) #Better?
scale_test(All_1nom)
All_1nom
#
All_1sca <- clm(Final_Stage ~ Year + Month, scale = ~Month, data = All_oysters_clean)
anova(All_1, All_1sca)
nominal_test(All_1sca)
scale_test(All_1sca)
summary(All_1sca)
#
#
#
library(emmeans)
All_results <- emmeans(All_1, list(pairwise ~ Year, pairwise ~ Month))
#
All_years <- tidy(All_results$`pairwise differences of Year`) %>% dplyr::select(-term, -df)
names(All_years) <- c("Comps", "Est.", "SE", "t", "p-value")
All_months<- tidy(All_results$`pairwise differences of Month`) %>% dplyr::select(-term, -df)
names(All_months) <- c("Comps", "Est.", "SE", "t", "p-value")
All_years; All_months
#
All_oysters_clean %>% 
  ggplot(aes(Month, fill = Final_Stage))+
  geom_bar(position = "fill")+
  StaFill
#
#
#
#multinomal logistic regression - nnet 
All_m1 <- multinom(Final_Stage ~ Year + Month, data = All_oysters_clean)
summary(All_m1)
fitted(All_m1)
#
pairs(emmeans(All_m1, "Year"), adjust = "tukey")

#
pred_All_m1 <- cbind(All_oysters_clean[c(2,3)], predict(All_m1, newdata = All_oysters_clean[c(2,3)], type = "probs", se = TRUE)) %>%
  distinct()
pred_All_m1 %>% gather(key = "Stage", value = "Prob", -Year, -Month) %>%
  ggplot(aes(Year, Prob, color = Month, group = Month))+
  geom_line()+
  lemon::facet_rep_grid(Stage~.)
#
##pearson chi-squared
(All_test <- chisq.test(All_oysters_clean$Final_Stage, All_oysters_clean$Month))
(All_test_pvalues <- chisq.posthoc.test::chisq.posthoc.test(xtabs(~Final_Stage + Month, All_oysters_clean), method = "bonferroni"))
#
##KW test
kruskal.test(as.numeric(Final_Stage) ~ Month, data = All_oysters_clean)
pairwise.wilcox.test(as.numeric(All_oysters_clean$Final_Stage), All_oysters_clean$Month, p.adjust.method = "BH")
#
#
##Site differences
(Site_mean_counts_All <- All_oysters_clean %>% 
    group_by(Site, Final_Stage) %>% 
    count() %>%
    summarize(meanCount = mean(n),
              minCount = min(n),
              maxCount = max(n)) %>%
    pivot_wider(id_cols = Site,
                names_from = Final_Stage,
                values_from = c("meanCount", "minCount", "maxCount"), 
                names_glue = "{Final_Stage}_{.value}"))
#
All_oysters_clean %>% group_by(Site) %>% 
  ggplot(aes(Site, fill = Final_Stage))+
  geom_bar(position = "fill")+
  scale_y_continuous("Percentage", labels = scales::percent_format(), expand = c(0,0))+
  StaFill
#
##KW test
kruskal.test(as.numeric(Final_Stage) ~ Site, data = All_oysters_clean)
pairwise.wilcox.test(as.numeric(All_oysters_clean$Final_Stage), All_oysters_clean$Site, p.adjust.method = "BH")
#
#
#
#Group by Month and Site to compare among months and sites
(Monthly_mean_counts_All_Sites <- All_oysters_clean %>%
    group_by(Month, Site, Final_Stage) %>% 
    count() %>% #ungroup() %>%
    summarize(meanCount = mean(n),
              minCount = min(n),
              maxCount = max(n)) %>%
    pivot_wider(id_cols = c("Month", "Site"),
                names_from = Final_Stage,
                values_from = c("meanCount", "minCount", "maxCount"), 
                names_glue = "{Final_Stage}_{.value}") %>%
    arrange(Site))
#  
All_oysters_clean %>% group_by(Month, Site) %>% 
  ggplot(aes(Month, fill = Final_Stage))+
  geom_bar(position = "fill")+
  lemon::facet_rep_wrap(.~Site)+
  scale_y_continuous("Percentage", labels = scales::percent_format(), expand = c(0,0))+
  StaFill
#
All_oysters_clean %>% group_by(Month) %>% 
  group_by(Month, Final_Stage) %>% 
  count() %>% #ungroup() %>%
  summarize(medianCount = median(n)) 
####Comparisons over time####
Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 2) %>%
  ggplot(aes(Month, meanProp, group = Final_Stage))+
  geom_line(linewidth = 1, color = "#E69F00")+
  geom_vline(data = First_Last0.35, aes(xintercept  = Month), color = "black", size = 2)+
  geom_vline(data = First_Last0.5, aes(xintercept =  Month), color = "red", size = 2)+
  lemon::facet_rep_grid(Year~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
(First_Last0.35 <- rbind(
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 2) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(1),
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 2) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(n())))
#
(First_Last0.5 <- Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 2) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.5) %>%
    slice(1))
#
#
#
#Development
Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 1) %>%
  ggplot(aes(Month, meanProp, group = Final_Stage))+
  geom_line(linewidth = 1, color = "#D55E00")+
  geom_vline(data = First_Last0.35_1, aes(xintercept  = Month), color = "black", size = 2)+
  geom_vline(data = First_Last0.5_1, aes(xintercept =  Month), color = "red", size = 2)+
  lemon::facet_rep_grid(Year~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
(First_Last0.35_1 <- rbind(
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 1) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(1),
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 1) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(n())))
#
(First_Last0.5_1 <- Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 1) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.5) %>%
    slice(1))
#
#
#Spent
Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 3) %>%
  ggplot(aes(Month, meanProp, group = Final_Stage))+
  geom_line(linewidth = 1, color = "#F0E442")+
  geom_vline(data = First_Last0.35_3, aes(xintercept  = Month), color = "black", size = 2)+
  geom_vline(data = First_Last0.5_3, aes(xintercept =  Month), color = "red", size = 2)+
  lemon::facet_rep_grid(Year~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
(First_Last0.35_3 <- rbind(
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 3) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(1),
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 3) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(n())))
#
(First_Last0.5_3 <- Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 3) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.5) %>%
    slice(1))
#
#
#
#Indifferent
Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
  summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 4) %>%
  ggplot(aes(Month, meanProp, group = Final_Stage))+
  geom_line(linewidth = 1, color = "#009E73")+
  geom_vline(data = First_Last0.35_4, aes(xintercept  = Month), color = "black", size = 2)+
  geom_vline(data = First_Last0.5_4, aes(xintercept =  Month), color = "red", size = 2)+
  lemon::facet_rep_grid(Year~.)+
  theme_classic()+
  scale_x_discrete(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0,0))
#
(First_Last0.35_4 <- rbind(
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 4) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(1),
  Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 4) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.35) %>%
    slice(n())))
#
(First_Last0.5_4 <- Overall_counts %>% group_by(Year, Month, Final_Stage) %>%
    summarise(meanProp = round(mean(Prop), 3)) %>% subset(Final_Stage == 4) %>% ungroup() %>%
    arrange(Year) %>% group_by(Year) %>%#Arrange and group by Year
    filter(meanProp > 0.5) %>%
    slice(1))
