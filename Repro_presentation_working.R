####Size at maturity of oysters in SF estuaries
#Use of repro stages, recruitment, etc. 
#
#
#Load packages, install as needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               rstatix, rcompanion, #Summary stats & multiple comparisons
               zoo, lubridate, #Dates and times
               readxl, writexl, #Reading excel files
               janitor, lincar, scales, #Basic analyses
               FSA, #lencat
               emmeans, multcomp, ggpubr, #Arranging ggplots
               install = TRUE)
#
#
#
#
#
####Load files####
Repro_df <- read_excel("Output/Repro_data_2024 02_cleaned.xlsx", sheet = "Sheet1", #File name and sheet name
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
Repro_props <- read_excel("Output/Repro_proportions_2024 02.xlsx", sheet = "Sheet1", #File name and sheet name
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
Stages <- c("0" = "Undifferentiated", "1" = "Developing", "2" = "Ripe/Spawning", "3" = "Spent/Recycling", "4" = "Indifferent", "8" = "Buceph", "M/F" = "M/F")
cbPalette <- c("#666666", "#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#9966FF")
names(cbPalette) <- levels(Repro_df$Final_Stage)
StaFill <- scale_fill_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
#
#
Base <- theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, color = "black", family = "sans"),
        axis.text.x = element_text(size = 13, color = "black", 
                                   margin = unit(c(0.4, 0.5, 0, 0.5), "cm"), family = "sans"),
        axis.text.y = element_text(size = 13, color = "black", 
                                   margin = unit(c(0, 0.4, 0, 0), "cm"), family = "sans"),
        axis.ticks.length = unit(-0.15, "cm"), plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
#
##
###
####Data summary####
#
#Number of samples
Repro_c <- Repro_df %>% filter(!is.na(Final_Stage))
summary(Repro_c)
#
(Site_counts <- Repro_c %>% group_by(Estuary, Final_Stage) %>% summarise(Count = n()) %>% spread(Final_Stage, Count))
Site_counts %>% adorn_totals(c("row", "col"))
#
#
##Figure of overall stages
Repro_c %>% 
  ggplot(aes(Estuary, fill = Final_Stage))+
  geom_bar(position = "fill")+
  StaFill + Base +
  scale_x_discrete(expand = c(0.2, 0))+
  scale_y_continuous("", expand = c(0,0), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1)))
#          
#
##Very few 0s. For sake of practice, logistic curve for age at maturity
#
#####Size at maturity####
#
#
#Df of mature v immature, 5 mm bins
Maturity <- Repro_c %>%
  mutate(Mature = as.factor(ifelse(Final_Stage == 0, "I", "M")), #maturity class
         bins = lencat(Repro_c$SH, 0, w = 5), #SH bins by 5mm
         Sex = as.factor(ifelse(Male_Female == "Yes", "M/F", as.character(Sex)))) %>%  #Add M/F into sex
  subset(Bad_Slide == "No") #Remove bad slides
#
head(Maturity)
#
#
##Number per SL bin - show with and without free_y for comparison
Maturity %>%
  ggplot(aes(SH, fill = Sex))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5), alpha = 0.8)+
  lemon::facet_rep_grid(Sex~., scales = 'free_y')+
  Base + scale_x_continuous("Shell height (mm)", expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))+
  theme(legend.position = "none")
#
Maturity %>% subset(Mature == "I") %>%
  ggplot(aes(SH))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5))+
  Base + scale_x_continuous("Shell height (mm)", expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))
#
Maturity %>% subset(Mature == "I") %>% summarise(n())
#
##Already see a slight issue.
##Based on 46 samples staged as immature:
