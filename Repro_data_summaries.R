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
  mutate(Estuary = as.factor(ifelse(grepl("SL", Site), "SL", 
                                    ifelse(grepl("LX", Site), "LX",
                                           ifelse(grepl("CR", Site), "CR",
                                                  ifelse(grepl("LW", Site), "LW", 
                                                         ifelse(grepl("TB", Site), "TB", NA)))))),
         Month = factor(Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))) %>%
  mutate(Comb_Stage = ifelse(!is.na(Stage_new), Stage_new, 
                             ifelse(Stage_Old == 0, 0,
                                    ifelse(Stage_Old == 10, 4,
                                           ifelse(Stage_Old > 0 & Stage_Old < 5, 1, 
                                                  ifelse(Stage_Old > 4 & Stage_Old < 8, 2, 
                                                         ifelse(Stage_Old > 7 & Stage_Old < 10, 3, NA))))))) %>%
  mutate(Final_Stage = as.factor(ifelse(!is.na(SH) & Parasite == "Buceph" & is.na(Comb_Stage), 8, 
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
#write_xlsx(Repro_df, "Output/Repro_data_2023 12_cleaned.xlsx", format_headers = TRUE)
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
#write_xlsx(Repro_props, "Output/Repro_proportions_2023 12 06.xlsx", format_headers = TRUE)
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
#write_xlsx(Rcrt_df, "Output/Rcrt_data_2023 12_cleaned.xlsx", format_headers = TRUE)
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
####By estuary####
#
#Dataframes by Estuary - counts of each stage
SL_oysters <- Repro_df %>% subset(Estuary == "SL") 
LX_oysters <- Repro_df %>% subset(Estuary == "LX") 
LW_oysters <- Repro_df %>% subset(Site == "LW") 
CR_oysters <- Repro_df %>% subset(Estuary == "CR") %>% mutate(Site = as.factor(ifelse(Station == "1" | Station == "2", "CR-E", "CR-W"))) 
All_oysters <- rbind(SL_oysters, LX_oysters, LW_oysters, CR_oysters)
#
#write_xlsx(All_oysters, "Output/Repro_data_2023 12_cleaned_final.xlsx", format_headers = TRUE)
#
#
#
#
####Data checks####
#
#
#Comparing old stages
ggarrange(Repro_props_annual %>%
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
####Overall annual pattern####
#
##Working with all_oysters
All_oysters_clean <- All_oysters %>% subset(Final_Stage != "M/F" & Final_Stage != "8") %>% droplevels()
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