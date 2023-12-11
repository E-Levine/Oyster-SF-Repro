####Reproductive comparions of oysters in SF estuaries
#
#
#Load packages, install as needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               rstatix, rcompanion, #Summary stats & multiple comparisons
               zoo, lubridate, #Dates and times
               readxl, writexl, #Reading excel files
               car, #Basic analyses
               lmPerm, lme4, FSA, #FSA = lencat
               mgcv, ggpubr, #Plots, Arranging ggplots
               install = TRUE)
#
#
#
#
####Load cleaned data####
#
Repro_df <- read_excel("Output/Repro_data_2023 12 06_cleaned.xlsx", sheet = "Sheet 1", #File name and sheet name
                    skip = 0, col_names = TRUE, 
                    na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal") 
head(Repro_df)
summary(Repro_df)
#
#
##Proportion per stage
Repro_props <- read_excel("Output/Repro_proportions_2023 12 06.xlsx", sheet = "Sheet 1", #File name and sheet name
                           skip = 0, col_names = TRUE, 
                           na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                           .name_repair = "universal")
#
summary(Repro_props)
#
##Standardize to n/Year
(Repro_props_annual <- left_join(Repro_df %>% group_by(Year, Month, Estuary, Site, Final_Stage) %>% drop_na(Estuary, Final_Stage) %>%
                                   summarise(Count = n()),
                                 Repro_df %>% group_by(Year) %>% drop_na(Estuary, Final_Stage) %>%
                                   summarise(Total = n())) %>%
    mutate(Prop = Count/Total)) 
#
#
#
#
####Formatting and helpers####
#
#Map color to Stage
Stages <- c("0" = "Immature", "1" = "Developing", "2" = "Ripe/Spawning", "3" = "Spent/Recycling", "4" = "Indifferent", "8" = "Buceph", "M/F" = "Herm")
cbPalette <- c("#333333", "#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#9966FF")
names(cbPalette) <- levels(Repro_df$Final_Stage)
StaFill <- scale_fill_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
#
##
##
####Size at immature staging####
#
(Overall_pct_staging <- cbind(Repro_df %>% filter(Bad_Slide == "No") %>% group_by(Final_Stage) %>% summarise(Count = n()),
                             Repro_df %>% filter(Bad_Slide == "No") %>% summarise(Total = n())) %>%
  mutate(Pct = round(Count/Total*100,2)))
#
#Df of mature v immature, 5 mm bins
Maturity <- Repro_df %>%
  mutate(Mature = as.factor(ifelse(Final_Stage == 0, "I", "M")), #maturity class
         bins = lencat(Repro_df$SH, 0, w = 5), #SH bins by 5mm
         Sex = as.factor(ifelse(Male_Female == "Yes", "M/F", as.character(Sex)))) %>%  #Add M/F into sex
  subset(Bad_Slide == "No") #Remove bad slides
#
head(Maturity)
#
##Number per SL bin
Maturity %>%
  ggplot(aes(SH, fill = Sex))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5), alpha = 0.8)
#
Maturity %>%
  ggplot(aes(SH, fill = Mature))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5))+
  facet_wrap(.~Sex)
#