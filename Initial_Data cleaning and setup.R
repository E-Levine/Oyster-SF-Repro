#
#Load packages, install as needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               rstatix, rcompanion, #Summary stats & multiple comparisons
               zoo, lubridate, #Dates and times
               readxl, writexl, #Reading excel files
               car, #Basic analyses
               ggpubr, #Arranging ggplots
               install = TRUE)


#
###Load data files and check data####
#
##Excel
Repro <- read_excel("../../Data/Repro_staging.xlsx", sheet = "Raw data", #File name and sheet name
                    skip = 0, col_names = TRUE, 
                    col_types = c("date","text","text", "text", "text", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text"), 
                    na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal")
head(Repro)
#
Repro_df <- Repro %>% drop_na(Parasite) %>%
  mutate_at(c("Site", "Year", "Month", "Sex"), as.factor) %>%
  mutate(Comb_Stage = ifelse(!is.na(Stage), Stage, 
                              ifelse(Stage_Old == 0 | Stage_Old == 10, 1, 
                                     ifelse(Stage_Old > 0 & Stage_Old < 5, 2, 
                                            ifelse(Stage_Old > 4 & Stage_Old < 7, 3, 
                                                   ifelse(Stage_Old > 6 & Stage_Old < 10, 4, NA)))))) %>%
  mutate(Final_Stage = ifelse(!is.na(SH) & Parasite == "Buceph" & is.na(Comb_Stage), 8, 
                              ifelse(Male_Female == "Yes", "M/F", 
                                     ifelse(Bad_Slide == "No" & is.na(Stage) & is.na(Stage_Old), "Z", Comb_Stage))))
#
#
#
summary(Repro_df)
#
Data_checks <- #Repro_df %>% filter(is.na(Final_Stage) & Bad_Slide != "Yes")
  rbind(Repro_df %>% filter(Sex == "M" & is.na(Final_Stage)),
                     Repro_df %>% filter(Sex == "F" & is.na(Final_Stage)))
#
#
##Data frame of years and site of interest
Repro_data <- Repro_df %>% 
  filter(Year == "2007" | Year == "2012" | Year == "2017" | Year == "2019" | Year == "2020" | Year == "2021" | Year == "2022") %>%
  filter(Site == "LW" | Site == "SL-C" | Site == "SL-N" | Site == "SL-S" | Site == "LX-N" | Site == "LX-S")
#
#
write_xlsx(Repro_data, "Repro_data_2023 11.xlsx", format_headers = TRUE)
#
#
#