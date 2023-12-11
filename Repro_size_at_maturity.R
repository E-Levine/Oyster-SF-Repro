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
Repro_df <- read_excel("Output/Repro_data_2023 12_cleaned.xlsx", sheet = "Sheet1", #File name and sheet name
                    skip = 0, col_names = TRUE, 
                    na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal") %>% 
  mutate_at(c("Station", "Sex", "Site", "Estuary", "Final_Stage"), as.factor) 

head(Repro_df)
summary(Repro_df)
#
#
##Proportion per stage
Repro_props <- read_excel("Output/Repro_proportions_2023 12 06.xlsx", sheet = "Sheet1", #File name and sheet name
                           skip = 0, col_names = TRUE, 
                           na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                           .name_repair = "universal")
#
summary(Repro_props)
#
#
##Recruitment data
Rcrt_df <- read_excel("Output/Rcrt_data_2023 12_cleaned.xlsx", sheet = "Sheet1", #File name and sheet name
                          skip = 0, col_names = TRUE, 
                          na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                          .name_repair = "universal") %>%
head(Rcrt_df)
#
#
##Standardize to n/Year
#(Repro_props_annual <- left_join(Repro_df %>% group_by(Year, Month, Estuary, Site, Final_Stage) %>% drop_na(Estuary, Final_Stage) %>%
#                                   summarise(Count = n()),
#                                 Repro_df %>% group_by(Year) %>% drop_na(Estuary, Final_Stage) %>%
#                                   summarise(Total = n())) %>%
#    mutate(Prop = Count/Total)) 
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
####Repro collections per date####
#
##Data frame of number of samples per Month/Year
Repro_samples <- rbind(Repro_df %>% filter(Estuary != "CR" & Estuary != "TB") %>% droplevels() %>% complete(Year, Month, Site, Station) %>%
                          filter(!(Site == "LW" & Station == "4") & !(Site == "LX-N" & Station == "4") & !(Site == "LX-S" & Station == "4") & 
                                   !(Site == "SL-C" & Station == "4") & !(Site == "SL-N" & Station == "4") & !(Site == "SL-S" & Station == "4")), 
                        Repro_df %>% filter(Estuary == "CR") %>% droplevels()  %>% complete(Year, Month, Site, Station) %>% 
                         filter(!(Site == "CR-E" & Station == "3") & !(Site == "CR-E" & Station == "4") & !(Site == "CR-W" & Station == "1") &
                                  !(Site == "CR-W" & Station == "2"))) %>% 
  mutate(MonYr = as.Date(paste(Year, Month, "01", sep = "-")), format = "%Y-%m-%d") %>%
  mutate_at(c("Year", "Month"), as.integer) %>%
  group_by(Year, Month, MonYr, Site, Station) %>% summarise(Samples = sum(!is.na(SH))) 
#
LW_Repro <- Repro_samples %>% filter(Site == "LW" & MonYr > "2005-01-01")
#Identify eacj instance of 0 samples collected.
LW1_df <- LW_Repro %>% filter(Station == "1") #Subset to proper group of data to compare
LW1_rle <- rle(LW1_df$Samples == 0) #Identify each first instance of 0 or non-zero and how many rows until next change
LW1_first <- LW1_rle$values == 0 & LW1_rle$lengths > 1 #Select the first instance of each sequence of 0s
Lw1_index <- (cumsum(LW1_rle$lengths)+1)[LW1_first] #Get the index of the first instance 
LW1_zeros <- LW1_df[Lw1_index,] #Select the first instance of each sequence of 0s
#
#Identify next time samples were collected
LW1_rle_n <- rle(LW1_df$Samples > 0)
LW1_first_n <- LW1_rle_n$values > 0 & LW1_rle_n$lengths > 1 
LW1_last_index <- (cumsum(LW1_rle$lengths))[LW1_first_n]
LW1_counts <- LW1_df[LW1_last_index,]
#
#
Repro_samples %>% subset(Site == "LW" & MonYr  > "2005-01-01") %>% arrange(MonYr) %>% filter(Samples == 0)
                                                                