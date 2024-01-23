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
               car, #Basic analyses
               lmPerm, lme4, glmmTMB, DHARMa, #ordinal,
               ggpubr, #Arranging ggplots
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
####glm - Year, Month v Stage####
#
(Overall_counts <- left_join(All_oysters_clean %>% group_by(Year, Month, Final_Stage) %>% summarise(Count= n()),
                            All_oysters_clean %>% group_by(Year, Month) %>% summarise(Total= n())) %>% 
  mutate(Year = as.numeric(Year),
         Prop = Count/Total) %>% ungroup() %>% complete(Final_Stage, nesting(Year, Month), fill = list(Count = 0, Total = 0, Prop = 0)))
#
##Check response
hist(Overall_counts$Count)
#
##Fit model
Overall_model <- glmmTMB(Count ~ Year + Month, contrasts = list(Month = "contr.sum"),
                         data = Overall_counts, family = poisson)
summary(Overall_model)
#Coefficients -sum=Month12
c(fixef(Overall_model)$cond,-sum(fixef(Overall_model)$cond))
#
##Model checking - DHARMa
plot(simulateResiduals(Overall_model)) #Overdispersion - try negative binomial
#
Overall_model2 <- update(Overall_model, family = nbinom2)
summary(Overall_model2)
plot(simulateResiduals(Overall_model2)) #better
anova(Overall_model, Overall_model2)
testZeroInflation(Overall_model2) #Not sig 
testDispersion(Overall_model2) #Barely Sig
testOutliers(Overall_model2)
#
Overall_model3 <- glmmTMB(Count ~ as.numeric(Year) + offset(log(as.numeric(Month))),
                          data = Overall_counts, family = nbinom2)
summary(Overall_model3)
plot(simulateResiduals(Overall_model3))
anova(Overall_model2, Overall_model3) #2 is better than 3
#
Overall_model4 <- update(Overall_model2, ziformula = ~1)
anova(Overall_model2, Overall_model4) #Better = 2
#5
plot(simulateResiduals(Overall_model2), form = Overall_counts$Year)
#
(Overall_Sites <- left_join(All_oysters_clean %>% group_by(Year, Month, Final_Stage, Site) %>% summarise(Count= n()),
                             All_oysters_clean %>% group_by(Year, Month, Site) %>% summarise(Total= n())) %>% 
    mutate(Year = as.numeric(Year),
           Prop = Count/Total) %>% ungroup() %>% complete(Final_Stage, nesting(Year, Month, Site), fill = list(Count = 0, Total = 0, Prop = 0)))
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