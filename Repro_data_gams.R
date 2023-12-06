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
               mgcv, ggpubr, #Plots, Arranging ggplots
               install = TRUE)
#
#
#
#
####Load raw data, clean, create data frames for analyses####
#
Repro <- read_excel("../../Data/Repro_staging.xlsx", sheet = "Raw data", #File name and sheet name
                    skip = 0, col_names = TRUE, 
                    col_types = c("date","text","text", "text", "text", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text"), 
                    na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal") %>% rename(Site2 = Site)
head(Repro)
#
Repro_df <- Repro %>% drop_na(Parasite) %>%
  mutate_at(c("Site2", "Year", "Month", "Sex"), as.factor)  %>%
  mutate(Site = as.factor(ifelse(grepl("CR", Site2) & Station == "1", "CR-E", 
                                 ifelse(grepl("CR", Site2) & Station == "2", "CR-E", 
                                        ifelse(grepl("CR", Site2) & Station == "3",  "CR-W",
                                               ifelse(grepl("CR", Site2) & Station == "4", "CR-W", Repro$Site2)))))) %>%
  mutate(Estuary = as.factor(ifelse(grepl("SL", Site), "SL", 
                                    ifelse(grepl("LX", Site), "LX",
                                           ifelse(grepl("CR", Site), "CR",
                                                  ifelse(grepl("LW", Site), "LW", NA))))),
         Month = factor(Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))) %>%
  mutate(Comb_Stage = ifelse(!is.na(Stage), Stage, 
                             ifelse(Stage_Old == 0 | Stage_Old == 10, 4, 
                                    ifelse(Stage_Old > 0 & Stage_Old < 5, 1, 
                                           ifelse(Stage_Old > 4 & Stage_Old < 8, 2, 
                                                  ifelse(Stage_Old > 7 & Stage_Old < 10, 3, NA)))))) %>%
  mutate(Final_Stage = as.factor(ifelse(!is.na(SH) & Parasite == "Buceph" & is.na(Comb_Stage), 8, 
                                        ifelse(Male_Female == "Yes", "M/F", 
                                               ifelse(Bad_Slide == "No" & is.na(Stage) & is.na(Stage_Old), "0", Comb_Stage)))),
         Buceph = as.factor(ifelse(Parasite == "Buceph", "Y", "N"))) %>%
  dplyr::select(-Site2)
#
summary(Repro_df)
#
Data_checks <- Repro_df %>% filter(is.na(Final_Stage) & Bad_Slide != "Yes") #Dont' want any rows of data
#rbind(Repro_df %>% filter(Sex == "M" & is.na(Final_Stage)), #Anything Bad_Slide = Yes should be NA
#      Repro_df %>% filter(Sex == "F" & is.na(Final_Stage)))
#
##Write output of cleaned data (if changed)
#write_xlsx(Repro_df, "Output/Repro_data_2023 12 06_cleaned.xlsx", format_headers = TRUE)
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
####Data exploration####
#
##Stage proportion by Year, Month, Estuary, Site
Repro_props %>% dplyr::select(-Count, -Total) %>%
  mutate(id = row_number()) %>% gather(Variable, Value, - Final_Stage, -id, -Prop) %>%
  ggplot(aes(Prop, Value, group = Final_Stage, color = Final_Stage))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Variable, scales = "free_y")+ theme_classic()
#
Repro_props %>% dplyr::select(-Count, -Total) %>%
  mutate(id = row_number()) %>% gather(Variable, Value, - Final_Stage, -id, -Prop) %>%
  ggplot(aes(Value, Prop, fill = Final_Stage))+
  geom_col(position = "fill")+
  #geom_smooth(method = "lm")+
  facet_wrap(~Variable, scales = "free_x")+ theme_classic()
#
ggsave(path = "Output/Figures/", filename = "Proportions_stages_overview.tiff", dpi = 1000, height = 5, width = 5, unit = "in")
#
#Weird increase from 2016 to 2017+. Standardize to total/year. 
(Repro_props_annual <- left_join(Repro_df %>% group_by(Year, Month, Estuary, Site, Final_Stage) %>% drop_na(Estuary, Final_Stage) %>%
                            summarise(Count = n()),
                          Repro_df %>% group_by(Year) %>% drop_na(Estuary, Final_Stage) %>%
                            summarise(Total = n())) %>%
    mutate(Prop = Count/Total)) 
#
#Raw vs standardized
ggarrange(
  Repro_props %>%
    ggplot(aes(Year, Prop, fill = Final_Stage))+
    geom_col(position = "fill")+
    theme_classic(),
  Repro_props_annual %>%
    ggplot(aes(Year, Prop, fill = Final_Stage))+
    geom_col(position = "fill")+
    theme_classic()
)
#
#Comparing old stages
ggarrange(Repro_props_annual %>%
            ggplot(aes(Year, Prop, fill = Final_Stage))+
            geom_col(position = "fill")+
            theme_classic(),
          left_join(Repro_df %>% group_by(Year, Month, Estuary, Site, Stage_Old) %>% drop_na(Estuary) %>%
                      summarise(Count = n()),
                    Repro_df %>% group_by(Year) %>% drop_na(Estuary) %>%
                      summarise(Total = n())) %>%
            mutate(Prop = Count/Total) %>%
            ggplot(aes(Year, Prop, fill = as.factor(Stage_Old)))+
            geom_col(position = "fill"))
##Weird break due to misclassifcation of Old 7 in New 3. Old 7 should be in New 2.
##Corrected and new data output. Using corrected data moving forward.
###Q1: Overall annual and month pattern for south Florida####
#
#Proportions vary naturally and stages have different trends so need to distinguish between stages
Props_annual <- Repro_props %>% dplyr::select(-Count, -Total, -Month, -Estuary, -Site) %>% group_by(Year, Final_Stage) %>%
  mutate(Year = as.numeric(Year)) %>%
  summarise(meanProp = mean(Prop, na.rm = T)) %>%
  spread(Final_Stage, meanProp)
Props_19 <- as.vector(as.matrix(Props_annual[,2:8]))
Stages <- c("0", "1", "2", "3", "4", "8", "M/F")
ID19 <- factor(rep(Stages, each = length(Props_annual$Year), levels = Stages))
Year19 <- rep(Props_annual$Year, 7)
Stage0 <- as.numeric(ID19 == "0")
Stage1 <- as.numeric(ID19 == "1")
Stage2 <- as.numeric(ID19 == "2")
Stage3 <- as.numeric(ID19 == "3")
Stage4 <- as.numeric(ID19 == "4")
Stage8 <- as.numeric(ID19 == "8")
StageMF <- as.numeric(ID19 == "M/F")
#
M0 <- gamm(Props_19 ~ ID19 + s(Year19, by = Stage0, bs = "tp") + s(Year19, by = Stage1, bs = "tp") +
             s(Year19, by = Stage2, bs = "tp") + s(Year19, by = Stage3, bs = "tp") + s(Year19, by = Stage4, bs = "tp") +
             s(Year19, by = Stage8, bs = "tp") + s(Year19, by = StageMF, bs = "tp"),
           method = "REML", weights = varIdent(form = ~1 | ID19))
#
#
#
##
##
Props_annual <- Repro_props %>% ungroup() %>% dplyr::select(-Count, -Total, -Month, -Estuary, -Site) %>%
  mutate(Year = as.integer(Year)) %>% complete(Year, Final_Stage)
#
gam1 <- gam(Prop ~ Year + Final_Stage, data = Props_annual, method = "REML", family = "quasibinomial")
summary(gam1) #R2=0.244, dev 23.7
par(mfrow = c(1,2))
plot(gam1, all.terms = TRUE)

gam2 <- gam(Prop ~ s(Year) + Final_Stage, data = Props_annual, method = "REML", family = "quasibinomial")
summary(gam2) #R2=0.008, dev 24
plot(gam2, all.terms = TRUE)

gam3 <- gam(Prop ~ s(Year, by = Final_Stage), data = Props_annual, method = "REML", family = "quasibinomial")
summary(gam3) #R2=0.124, dev 10
par(mfrow = c(3,3))
plot(gam3, all.terms = TRUE)
#