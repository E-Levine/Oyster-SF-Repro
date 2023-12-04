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
               lmPerm, lme4, 
               ggpubr, #Arranging ggplots
               install = TRUE)
#
#
#
#
####Load raw data, clean, create dataframes for analyes####
#
Repro <- read_excel("../../Data/Repro_staging.xlsx", sheet = "Raw data", #File name and sheet name
                    skip = 0, col_names = TRUE, 
                    col_types = c("date","text","text", "text", "text", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text"), 
                    na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal")
head(Repro)
#
Repro_df <- Repro %>% drop_na(Parasite) %>%
  mutate_at(c("Site", "Year", "Month", "Sex"), as.factor) %>%
  mutate(Estuary = as.factor(ifelse(grepl("SL", Site), "SL", 
                                    ifelse(grepl("LX", Site), "LX",
                                           ifelse(grepl("CR", Site), "CR",
                                                  ifelse(grepl("LW", Site), "LW", NA))))),
         Month = factor(Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")))%>%
  mutate(Comb_Stage = ifelse(!is.na(Stage), Stage, 
                             ifelse(Stage_Old == 0 | Stage_Old == 10, 1, 
                                    ifelse(Stage_Old > 0 & Stage_Old < 5, 2, 
                                           ifelse(Stage_Old > 4 & Stage_Old < 7, 3, 
                                                  ifelse(Stage_Old > 6 & Stage_Old < 10, 4, NA)))))) %>%
  mutate(Final_Stage = as.factor(ifelse(!is.na(SH) & Parasite == "Buceph" & is.na(Comb_Stage), 8, 
                              ifelse(Male_Female == "Yes", "M/F", 
                                     ifelse(Bad_Slide == "No" & is.na(Stage) & is.na(Stage_Old), "0", Comb_Stage)))))
#
summary(Repro_df)
#
Data_checks <- Repro_df %>% filter(is.na(Final_Stage) & Bad_Slide != "Yes") #Dont' want any rows of data
  #rbind(Repro_df %>% filter(Sex == "M" & is.na(Final_Stage)), #Anything Bad_Slide = Yes should be NA
  #      Repro_df %>% filter(Sex == "F" & is.na(Final_Stage)))
#
##Write output of cleaned data
write_xlsx(Repro_df, "Output/Repro_data_2023 12_cleaned.xlsx", format_headers = TRUE)
#
#Dataframes by Estuary
SL_oysters <- Repro_df %>% subset(Estuary == "SL")
LX_oysters <- Repro_df %>% subset(Estuary == "LX")
LW_oysters <- Repro_df %>% subset(Site == "LW")
CR_oysters <- Repro_df %>% subset(Estuary == "CR")
All_oysters <- rbind(SL_oysters, LX_oysters, LW_oysters, CR_oysters)
#
write_xlsx(All_oysters, "Output/Repro_data_2023 12_cleaned_final.xlsx", format_headers = TRUE)
#
#
Stages <- c("0" = "Immature", "1" = "Developing", "2" = "Ripe/Spawning", "3" = "Spent/Recycling", "4" = "Indifferent", "8" = "Buceph", "M/F" = "Herm")
cbPalette <- c("#333333", "#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#9966FF")
names(cbPalette) <- levels(Repro_df$Final_Stage)
StaFill <- scale_fill_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")

#
#
####Overall annual pattern####
#
##Working with all_oysters
#Group by Month to compare among months 
(Monthly_mean_counts_All <- All_oysters %>% filter(Final_Stage != "M/F" & Final_Stage != "8") %>%
  group_by(Month, Final_Stage) %>% 
  count() %>% #ungroup() %>%
  summarize(meanCount = mean(n),
            minCount = min(n),
            maxCount = max(n)) %>%
  pivot_wider(id_cols = Month,
              names_from = Final_Stage,
              values_from = c("meanCount", "minCount", "maxCount"), 
              names_glue = "{Final_Stage}_{.value}"))
  
All_oysters %>% group_by(Month) %>% filter(Final_Stage != "M/F" & Final_Stage != "8") %>%
  ggplot(aes(Month, fill = Final_Stage))+
  geom_bar(position = "fill")+
  scale_y_continuous("Percentage", labels = scales::percent_format(), expand = c(0,0))+
  StaFill
