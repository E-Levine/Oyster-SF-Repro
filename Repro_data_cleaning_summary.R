####Reproductive comparisons of oysters in SF estuaries
#Data setup and cleaning
#Data checks and summary info
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
####Repro-Load raw data, clean, create data frames for analyses####
#
Repro <- read_excel("../../Data/Repro_staging.xlsx", sheet = "Raw data", #File name and sheet name
                    skip = 0, col_names = TRUE, 
                    col_types = c("date","text","text", "text", "text", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text"), 
                    na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal") %>% rename(Site2 = Site)
head(Repro)
#
Repro_df <- Repro %>% drop_na(Parasite) %>%
  mutate_at(c("Year", "Month", "Sex"), as.factor)  %>%
  mutate(Site = as.factor(case_when(grepl("CR", Site2) & Station == "1" ~ "CR-E", 
                                    grepl("CR", Site2) & Station == "2" ~ "CR-E",
                                    grepl("CR", Site2) & Station == "3" ~ "CR-W",
                                    grepl("CR", Site2) & Station == "4" ~ "CR-W",
                                    TRUE ~ Site2)),
         Stage_new = Stage) %>%
  #Create Estuary designation and Month levels
  mutate(Estuary = as.factor(ifelse(grepl("SL", Site), "SL", 
                                    ifelse(grepl("LX", Site), "LX",
                                           ifelse(grepl("CR", Site), "CR",
                                                  ifelse(grepl("LW", Site), "LW", 
                                                         ifelse(grepl("TB", Site), "TB", NA)))))),
         Month = factor(Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))) %>%
  #Convert old to new staging
  mutate(Comb_Stage = ifelse(!is.na(Stage_new), Stage_new, 
                             ifelse(Stage_Old == 0, 0,
                                    ifelse(Stage_Old == 10, 4,
                                           ifelse(Stage_Old > 0 & Stage_Old < 5, 1, 
                                                  ifelse(Stage_Old > 4 & Stage_Old < 8, 2, 
                                                         ifelse(Stage_Old > 7 & Stage_Old < 10, 3, NA))))))) %>%
  #Get final stage, including Buceph and M/F designations
  mutate(Final_Stage = as.factor(ifelse(!is.na(SH) & Parasite == "Buceph" & is.na(Comb_Stage), "Buceph", 
                                        ifelse(Male_Female == "Yes", "M/F", 
                                               ifelse(Bad_Slide == "No" & is.na(Stage) & is.na(Stage_Old), "0", Comb_Stage)))),
         Buceph = as.factor(ifelse(Parasite == "Buceph", "Y", "N"))) %>%
  dplyr::select(-Site2, Stage_new)
#
#temp <- Repro %>% filter(Sex == "Z" & Stage == 4 & SH < 35) 
#temp <- Repro_df %>% mutate(Check = ifelse(Site == Site2, "Y", "N")) %>% filter(Check == "N")
#
summary(Repro_df)
#
Data_checks <- Repro_df %>% filter(is.na(Final_Stage) & Bad_Slide != "Yes") #Dont' want any rows of data
  #rbind(Repro_df %>% filter(Sex == "M" & is.na(Final_Stage)), #Anything Bad_Slide = Yes should be NA
  #      Repro_df %>% filter(Sex == "F" & is.na(Final_Stage)))
#
##Write output of cleaned data
#write_xlsx(Repro_df, "Output/Repro_data_2024 01_cleaned.xlsx", format_headers = TRUE)
#
#
##Calculate proportion per stage
(Repro_props <- left_join(Repro_df %>% group_by(Year, Month, Estuary, Site, Final_Stage) %>% drop_na(Estuary, Final_Stage) %>%
                            summarise(Count = n()),
                          Repro_df %>% group_by(Year, Month, Estuary, Site) %>% drop_na(Estuary, Final_Stage) %>%
                            summarise(Total = n())) %>%
    mutate(Prop = Count/Total)) 
#
##Write output of cleaned data (if changed)
#write_xlsx(Repro_props, "Output/Repro_proportions_2024 01.xlsx", format_headers = TRUE)
#
summary(Repro_props)
#
#
####Rcrt-load raw data, clean, create data frames####
#
Rcrt <- read_excel("../../Data/RCRT_Station_means_sd_2005_11_2023.xlsx", sheet = "Sheet1", #File name and sheet name
                   col_types = c("date", "numeric", "numeric", "text","text", "numeric", "numeric"),
                   na = c("", "Z", "."), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                   .name_repair = "universal")
#
head(Rcrt)
#
##Data frame of all possible years and months for each site - complete by station to make sure date ranges are correct
(Rcrt_df <- rbind(Rcrt %>% subset(Site == "CR-E") %>% complete(Year, Month, Site, Station),
                  Rcrt %>% subset(Site == "CR-W") %>% complete(Year, Month, Site, Station),
                  Rcrt %>% subset(Site == "LW") %>% complete(Year, Month, Site, Station),
                  Rcrt %>% subset(Site == "LX-N") %>% complete(Year, Month, Site, Station),
                  Rcrt %>% subset(Site == "LX-S") %>% complete(Year, Month, Site, Station),
                  Rcrt %>% subset(Site == "SL-C") %>% complete(Year, Month, Site, Station),
                  Rcrt %>% subset(Site == "SL-N") %>% complete(Year, Month, Site, Station),
                  Rcrt %>% subset(Site == "SL-S") %>% complete(Year, Month, Site, Station)) %>%
    mutate(MonYr = paste(Year, Month, "01", sep = "-")))
#
Rcrt_df %>%
  ggplot(aes(MonYr, Mean, color = Station))+
  geom_point()+
  facet_wrap(Site~.)+
  theme_classic()
#
##Write output of cleaned data
#write_xlsx(Rcrt_df, "Output/Rcrt_data_2024 01_cleaned.xlsx", format_headers = TRUE)
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
###
####Data checks####
#
#
#Comparing old stages to new staging
ggarrange(Repro_props %>%
            ggplot(aes(Year, Prop, fill = Final_Stage))+
            geom_col(position = "fill")+ StaFill+
            theme_classic(),
          left_join(Repro_df %>% group_by(Year, Month, Estuary, Site, Stage_Old) %>% drop_na(Estuary) %>%
                      summarise(Count = n()),
                    Repro_df %>% group_by(Year) %>% drop_na(Estuary) %>%
                      summarise(Total = n())) %>%
            mutate(Prop = Count/Total) %>%
            ggplot(aes(Year, Prop, fill = as.factor(Stage_Old)))+
            geom_col(position = "fill") + theme_classic())
#
#
#
#
#
####Data summary####
#
#
#
summary(Repro_df)
#
Repro_df %>% filter(Site == "CR-E", Bad_Slide == "No") %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>% spread(Month, Count)
Repro_df %>% filter(Site == "CR-W", Bad_Slide == "No") %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>% spread(Month, Count)
Repro_df %>% filter(Site == "LX-N", Bad_Slide == "No") %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>% spread(Month, Count)
Repro_df %>% filter(Site == "LX-S", Bad_Slide == "No") %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>% spread(Month, Count)
Repro_df %>% filter(Site == "SL-C", Bad_Slide == "No") %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>% spread(Month, Count)
Repro_df %>% filter(Site == "SL-N", Bad_Slide == "No") %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>% spread(Month, Count)
Repro_df %>% filter(Site == "SL-S", Bad_Slide == "No") %>%
  group_by(Year, Month) %>% summarise(Count = n()) %>% spread(Month, Count)
#
#
##Size
Repro_df %>% filter(Bad_Slide == "No" & Estuary != "LW" & Estuary != "TB") %>%
  #group_by(Site) %>%
  summarise(min = min(SH, na.rm = T),
            max = max(SH, na.rm = T),
            mean= mean(SH, na.rm = T),
            sd = sd(SH, na.rm = T))
#
#
Repro_df %>% filter(Bad_Slide == "No" & Estuary != "LW" & Estuary != "TB") %>%
  mutate(MonYr = as.yearmon(Date)) %>%
  group_by(Site, MonYr) %>% summarise(Count = n()) %>%
  ggplot(aes(MonYr, Count))+
  geom_bar(stat = "identity")+
  lemon::facet_rep_grid(Site~.)+
  scale_x_yearmon("Date", expand = c(0,0), breaks = (as.yearmon("2005-01") + 0:9/0.5))+
  scale_y_continuous(limits = c(0, 15), expand = c(0,0))+
  theme_classic()
#
#
#
#