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
               janitor, scales, #Basic analyses
               FSA, #lencat
               emmeans, multcomp, ggpubr, #Arranging ggplots
               install = TRUE)
#
#
#
#
#
####Load files####
#
Repro_df <- read_excel("Output/Repro_data_2024 02_cleaned.xlsx", sheet = "Sheet1", #File name and sheet name
                       skip = 0, col_names = TRUE, 
                       na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                       .name_repair = "universal") %>% 
  mutate_at(c("Station", "Sex", "Site", "Estuary", "Final_Stage"), as.factor) %>% 
  subset(Site != "TB" & Site != "LW-R")

head(Repro_df)
summary(Repro_df)
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
##Recruitment data
Rcrt_df <- read_excel("Rcrt_data_2023 12_cleaned.xlsx", sheet = "Sheet1", #File name and sheet name
                      skip = 0, col_names = TRUE, 
                      na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                      .name_repair = "universal") %>%
  subset(Site != "TB" & Site != "LW-R") 
head(Rcrt_df)
Rcrt_df <-  Rcrt_df %>%
  mutate(MonYr = as.Date(MonYr))
#
##Site start dates
Start_dates <- data.frame(Site = c("CR-E", "CR-W", "LW", "LX-N", "LX-S", "SL-C", "SL-S", "SL-N"),
                          Start_Date = c("2017-02-01", "2017-02-01", "2005-02-01", "2005-02-01", "2005-02-01", "2005-02-01", "2006-12-01", "2006-12-01"))
#
#
#
####Formatting and helpers####
#
#Map color to Stage
Stages <- c("0" = "Undifferentiated", "1" = "Developing", "2" = "Ripe/Spawning", "3" = "Spent/Recycling", "4" = "Indifferent", "8" = "Buceph", "M/F" = "M/F")
cbPalette <- c("#666666", "#D55E00", "#E69F00", "#009E73", "#56B4E9", "#9966FF", "#333333")
names(cbPalette) <- levels(Repro_df$Final_Stage)
StaFill <- scale_fill_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
StaColor <- scale_color_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
#
#
#Map color to Sampling - run after making ReproSpat df (line 406)
Sampling <- c("NRS&NS" = "No repro / 0 spat", "NRS&NSS" = "No repro / No spat", "NRS&S" = "No repro / Spat", "R&NS" = "Repro / 0 Spat", "R&NSS" = "Repro / No Spat", "R&S" = "Repro / Spat")
SaPalette <- c("#E69F00", "#FF0000", "#009E73", "#9966FF", "#56B4E9", "#666666")
names(SaPalette) <- levels(as.factor(ReproSpat$Type))
SampFill <- scale_fill_manual(name = "Sampling", labels = Sampling, values = SaPalette, na.value = "#999999")
SampColor <- scale_color_manual(name = "Sampling", labels = Sampling, values = SaPalette, na.value = "#999999")
Sample_order <- c("NRS&NSS", "NRS&NS", "NRS&S", "R&NSS", "R&NS", "R&S")
#
#
##Color to sex
Sex <- c("F" = "Female", "M" = "Male", "M/F" = "M/F", "Z" = "Undetermined")
color_og <- c("#009E73", "#E69F00", "#9966FF", "#666666")
#Map color to Sex
names(color_og) <- c("F","M","M/F","Z")
MFCol <- scale_color_manual(name = "", labels = Sex, values = color_og) 
MFFill <- scale_fill_manual(name = "", labels = Sex, values = color_og) 
#
#
Base <- theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 15, color = "black", family = "sans"),
        axis.text.x = element_text(size = 14, color = "black", 
                                   margin = unit(c(0.4, 0.5, 0, 0.5), "cm"), family = "sans"),
        axis.text.y = element_text(size = 14, color = "black", 
                                   margin = unit(c(0, 0.4, 0, 0), "cm"), family = "sans"),
        axis.ticks.length = unit(-0.15, "cm"), plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
#
theme_f <- theme(strip.text.y = element_text(color = "black", size = 13, family = "sans", face = "bold"),
                 strip.background = element_rect(fill = "#CCCCCC"),
                 panel.spacing = unit(0.75, "lines"),
                 strip.text.x = element_text(size = 10, face = "bold", family = "sans"))
#
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
  scale_y_continuous("", expand = c(0,0), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1)))+
  theme(legend.text = element_text(size = 11))
#          
#
##Very few 0s. For sake of practice, logistic curve for age at maturity
#
#####Size at maturity####
#
#Df of mature v immature, 5 mm bins
Maturity <- Repro_c %>%
  mutate(Mature = as.factor(ifelse(Final_Stage == 0, "I", "M")), #maturity class
         Mature2 = as.factor(ifelse(Final_Stage == 0 | Final_Stage == 4, "I", "M")),
         Mature3 = as.factor(ifelse(Final_Stage == 0 | Final_Stage == 4 | Final_Stage == 1, "I", "M")),
         bins = lencat(Repro_c$SH, 0, w = 5), #SH bins by 5mm
         Sex = as.factor(ifelse(Male_Female == "Yes", "M/F", as.character(Sex)))) %>%  #Add M/F into sex
  subset(Bad_Slide == "No") #Remove bad slides
#
head(Maturity)
#
#
##Number per SL bin 
Maturity %>%
  ggplot(aes(SH, fill = Sex))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5), alpha = 0.8)+
  lemon::facet_rep_grid(Sex~., scales = 'free_y')+
  Base + scale_x_continuous("Shell height (mm)", expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))+
  theme(legend.position = "none") +MFFill +theme_f
#
Maturity %>% subset(Mature == "I") %>%
  ggplot(aes(SH))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5))+
  Base + scale_x_continuous("Shell height (mm)", expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))
#
ggarrange(
  Maturity %>% subset(Mature == "I") %>%
    ggplot(aes(SH))+
    geom_histogram(aes(y = ..count.., fill = Final_Stage), breaks = seq(0,90, by = 5))+
    Base + scale_x_continuous(expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))+
    StaFill + theme(axis.title.x = element_blank(), legend.position = "none"),
  Maturity %>% subset(Mature2 == "I") %>%
    ggplot(aes(SH))+
    geom_histogram(aes(y = ..count.., fill = Final_Stage), breaks = seq(0,90, by = 5))+
    Base + scale_x_continuous(expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))+
    StaFill + theme(axis.title.x = element_blank(), legend.position = "none"),
  Maturity %>% subset(Mature3 == "I") %>%
    ggplot(aes(SH))+
    geom_histogram(aes(y = ..count.., fill = Final_Stage), breaks = seq(0,90, by = 5))+
    Base + scale_x_continuous("Shell height (mm)", expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))+
    StaFill + theme(legend.position = "bottom"),
  nrow = 3, ncol = 1, common.legend = FALSE)
#
Maturity %>% subset(Mature3 == "I") %>% summarise(n())
#
##Already see a slight issue.
##Based on 46 samples staged as immature:
#
##Working from scratch:
round5 <- function(x){5*ceiling(x/5)} #Rounds x up to nearest 5
#Limited to desrired columns and check for missing data
mat_all <- Maturity %>%  mutate(Mature = as.factor(Mature), Mature2 = as.factor(Mature2), Mature3 = as.factor(Mature3), Sex = as.factor(Sex)) %>% 
  dplyr::select(SH, bins, Sex, Mature, Mature2, Mature3) %>% drop_na()
Amelia::missmap(mat_all) #None missing
#
#Fit model
set.seed(54321)
lrSH <- glm(Mature ~ SH, family = binomial(link = "logit"), data = mat_all)
output <- data.frame(SH = seq(0, round5(max(mat_all$SH)), 5))
output$Mature <- predict(lrSH, newdata = output, type = "response")
#Get x where y = 0.5
(LD50 <- MASS::dose.p(lrSH, p = 0.5))
summary(lrSH)
#
mat_all %>%
  ggplot(aes(SH, as.numeric(Mature)-1))+
  geom_jitter(aes(color = Sex), size = 3, alpha = 0.6, width = 0.1, height = 0.05)+
  stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
              method.args = list(family = binomial), size = 1.25)+
  Base + MFCol +
  geom_vline(xintercept = LD50[[1]],linetype = "dashed", color = "black", size = 1)+
  scale_x_continuous(name = "Shell height (mm)", expand = c(0,0), limits = c(0, round5(max(mat_all$SH))), breaks = seq(0, round5(max(mat_all$SH)), by = 10))+
  scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))+
  theme(legend.text = element_text(size = 11))
#
#
#Clearly isn't informative. What next? Attempt to use "indifferent" as immature?
Maturity %>% filter(Mature == "M") %>% get_summary_stats(SH, type = "five_number")
Maturity %>% filter(Mature == "I") %>% get_summary_stats(SH, type = "five_number")
Maturity %>% filter(Mature2 == "I") %>% get_summary_stats(SH, type = "five_number")
Maturity %>% filter(Mature3 == "I") %>% get_summary_stats(SH, type = "five_number")
#
Maturity %>% filter(Final_Stage == "1" & SH < 47) %>%
  ggplot(aes(SH))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5))+
  Base + scale_x_continuous("Shell height (mm)", expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))
#
#
#Fit model 2
set.seed(54321)
lrSH2 <- glm(Mature2 ~ SH, family = binomial(link = "logit"), data = mat_all)
output2 <- data.frame(SH = seq(0, round5(max(mat_all$SH)), 5))
output2$Mature2 <- predict(lrSH2, newdata = output2, type = "response")
#Get x where y = 0.5
(LD502 <- MASS::dose.p(lrSH2, p = 0.5))
summary(lrSH2)
#
mat_all %>%
  ggplot(aes(SH, as.numeric(Mature2)-1))+
  geom_jitter(aes(color = Sex), size = 3, alpha = 0.6, width = 0.1, height = 0.05)+
  stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
              method.args = list(family = binomial), size = 1.25)+
  Base + MFCol +
  geom_vline(xintercept = LD502[[1]],linetype = "dashed", color = "black", size = 1)+
  scale_x_continuous(name = "Shell height (mm)", expand = c(0,0), limits = c(0, round5(max(mat_all$SH))), breaks = seq(0, round5(max(mat_all$SH)), by = 10))+
  scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))+
  theme(legend.text = element_text(size = 11))
#
#
#Fit model 3
set.seed(54321)
lrSH3 <- glm(Mature3 ~ SH, family = binomial(link = "logit"), data = mat_all)
output3 <- data.frame(SH = seq(0, round5(max(mat_all$SH)), 5))
output3$Mature3 <- predict(lrSH3, newdata = output3, type = "response")
#Get x where y = 0.5
(LD503 <- MASS::dose.p(lrSH3, p = 0.5))
summary(lrSH3)
#
mat_all %>%
  ggplot(aes(SH, as.numeric(Mature3)-1))+
  geom_jitter(aes(color = Sex), size = 3, alpha = 0.6, width = 0.1, height = 0.05)+
  stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
              method.args = list(family = binomial), size = 1.25)+
  Base + MFCol +
  geom_vline(xintercept = LD502[[1]],linetype = "dashed", color = "black", size = 1)+
  scale_x_continuous(name = "Shell height (mm)", expand = c(0,0), limits = c(0, round5(max(mat_all$SH))), breaks = seq(0, round5(max(mat_all$SH)), by = 10))+
  scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))+
  theme(legend.text = element_text(size = 11))
#
#
####Repro collections data####
#
####Repro collections per date######Data frame of full sampling time frame - remove extra stations from all time sites and extra stations created when filling out CR
Repro_full <- rbind(Repro_c %>% filter(Estuary != "CR" & Estuary != "TB") %>% droplevels() %>% complete(Year, Month, Site, Station) %>%
                      filter(!(Site == "LW" & Station == "4") & !(Site == "LX-N" & Station == "4") & !(Site == "LX-S" & Station == "4") & 
                               !(Site == "SL-C" & Station == "4") & !(Site == "SL-N" & Station == "4") & !(Site == "SL-S" & Station == "4")), 
                    Repro_c %>% filter(Estuary == "CR") %>% droplevels()  %>% complete(Year, Month, Site, Station) %>% 
                      filter(!(Site == "CR-E" & Station == "3") & !(Site == "CR-E" & Station == "4") & !(Site == "CR-W" & Station == "1") &
                               !(Site == "CR-W" & Station == "2"))) %>% 
  mutate(MonYr = as.Date(paste(Year, Month, "01", sep = "-")), format = "%Y-%m-%d") 
##Data frame of number of samples per Month/Year - includes Buceph
(Repro_samples <- Repro_full %>%
    mutate_at(c("Year", "Month"), as.integer) %>%
    group_by(Year, Month, MonYr, Site, Station) %>% summarise(Samples = sum(!is.na(SH))))
#Excludes Buceph
(Repro_samples_noB <- Repro_full %>% filter(Buceph == "N") %>%
    mutate_at(c("Year", "Month"), as.integer) %>%
    group_by(Year, Month, MonYr, Site, Station) %>% summarise(Samples = sum(!is.na(SH))))
##Function to determine all months without samples, and next time samples were collected (by station):
#StartDate of project: YYYY-MM-01
#output[1]: Site activity
#output[2]: Repro data
#output[4]: repro data dates
#Requires: Repro_samples, Repro_full, Repro_samples_noB, Start_dates
ReproSampling <- function(df) {
  #
  #Create data frame for final selected data and dates/number of samples
  Selected_repro_data <- data.frame()
  Dates_repro_data <- data.frame()
  Sites_list <- unique(df$Site)
  Site_active <- data.frame()
  Site_active_summ <- data.frame()
  
  for (j in Sites_list){
    #Filter data for site (Site_repro) and determine number reproductively active each month (Site_active)
    Site_repro <-  Repro_samples %>% filter(Site == j, MonYr >= (Start_dates %>% filter(Site == j))$Start_Date)
    Site_active_a <- Repro_full %>% filter(Site == j, MonYr >= (Start_dates %>% filter(Site == j))$Start_Date & Final_Stage != 8) %>%
      #NOTE: When converting Final_Stage to integer, each value is 1 higher due to the "0" stage factor level (0 factor = 1 integer, 1=2, etc.)
      mutate(Active = as.factor(ifelse(as.integer(Final_Stage) > 1 & as.integer(Final_Stage) < 5, "Y", "N"))) %>% 
      group_by(MonYr, Site, Station, Active) %>% summarise(Count = n()) #%>% drop_na(Active) 
    #Summarize counts of active or not active, percent of monthly sample, and average SH of each class
    Site_active_summ_i <- left_join(Site_active_a %>% group_by(MonYr, Site, Station, Active) %>% summarise(Count = sum(Count)),
                                  Repro_samples_noB %>% group_by(Site, MonYr, Station) %>% summarise(Total = sum(Samples))) %>%
      mutate(Pct = (Count/Total)*100) %>% #Adding percent of sample
      left_join(Repro_full %>% filter(Site == j & Buceph == "N") %>% 
                  mutate(Active = as.factor(ifelse(as.integer(Final_Stage) > 1 & as.integer(Final_Stage) < 5, "Y", "N"))) %>%
                  group_by(MonYr, Site, Station, Active) %>% summarise(meanSH = mean(SH, na.rm = T), sdSH = sd(SH, na.rm = T), minSH = min(SH, na.rm = T), maxSH = max(SH, na.rm = T)))
    
      #Determine number of stations for the desired site and all dates of study
    Site_active_b <- Repro_full %>% filter(Site == j, MonYr >= (Start_dates %>% filter(Site == j))$Start_Date) %>%
      mutate(Active = as.factor(ifelse(as.integer(Final_Stage) > 0 & as.integer(Final_Stage) < 4, "Y", "N"))) %>% 
      group_by(MonYr, Site, Station, Active) %>% summarise(Count = n()) %>% drop_na(Active) 
    Stations <- unique(Site_repro$Station)
    #Loop over data to determine 0s and samples for each station
    for (i in Stations) {
      Station_df_i <- filter(Site_repro, Station == i)
      Station_rle <- rle(Station_df_i$Samples == 0) #Identify each first instance of 0 or non-zero and how many rows until next change
      Station_first <- Station_rle$values == 0 & Station_rle$lengths > 1 #Select the first instance of each sequence of 0s
      Station_index <- (cumsum(Station_rle$lengths)+1)[Station_first] #Get the index of the first instance 
      Station_zeros <- Station_df_i[Station_index,] #Select the first instance of each sequence of 0s
      
          #Identify next time samples were collected
      Station_rle_n <- rle(Station_df_i$Samples == 0)
      Station_first_n <- Station_rle_n$values == 0 & Station_rle_n$lengths >= 1 
      Station_last_index <- ((cumsum(Station_rle_n$lengths))[Station_first_n == FALSE])+1
      Station_counts <- Station_df_i[Station_last_index,]
      
          #Filter to data for desired dates
      (Station_repro_data_i <- rbind(Repro_full %>% dplyr::select(Year:Sex, Estuary, Final_Stage:MonYr) %>%
                                       filter(MonYr %in% Station_zeros$MonYr & Site == j),
                                     Repro_full %>% dplyr::select(Year:Sex, Estuary, Final_Stage:MonYr) %>%
                                       filter(MonYr %in% Station_counts$MonYr & Site == j)))        
      Selected_repro_data <- rbind(Selected_repro_data, Station_repro_data_i)
      Dates_repro_data <- rbind(Dates_repro_data, Station_zeros, Station_counts) %>% drop_na(MonYr)
    }
    Site_active <- rbind(Site_active, Site_active_b)
    Site_active_summ <- rbind(Site_active_summ, Site_active_summ_i)
    }
  return(list(Site_active, Site_active_summ, Selected_repro_data, Dates_repro_data))
  }
#
temp <- ReproSampling(Repro_samples)
#
##Separate each data frame
Repro_activity <- as.data.frame(temp[1])
Repro_activity_summary <- as.data.frame(temp[2])
Repro_selected_repro <- as.data.frame(temp[3])
Repro_selected_dates <- as.data.frame(temp[4])
#
##Check output
sapply(Repro_activity, levels)
sapply(Repro_activity_summary, levels)
#
#
#Histogram plot of blank spaces for no collections then Y/N fill of activity until all active again. Ave sizes corresponding to activity changes. 
Repro_activity %>% filter(Site == "SL-C") %>%
  ggplot(aes(MonYr, group = Active, fill = Active))+
  #geom_histogram(aes(y = after_stat(count)))
  geom_bar(position = "fill")+
  lemon::facet_rep_grid(Station~.)
#
#
#Counts and prop with I/M & M = 0/4 (Mature, Count, Total, Prop) AND I/M & M = 0,1,4 (Mature2, Count2, Total2, Prop2)
#Function to complete maturity levels - change if Mature/Count column name changes
Complete_Mat <- function(df){
  df1 <- data.frame()
  for (i in unique(df$Site)) {
    df3 <- df %>%  ungroup() %>% subset(Site == i) %>% droplevels() %>%
      complete(Year, Month, Site, Station, Mature, fill = list(Count = -1))
    
    df1 <- rbind(df1, df3)
  }
  return(df1)
}
#
#####
Repro_Mat_props <- merge(Maturity %>% mutate(Mature = ifelse(Final_Stage == 4 | Final_Stage == 0, "I", "M")) %>% 
                           dplyr::select(Year, Month, Site, Station, Final_Stage, Mature) %>%
                           group_by(Year, Month, Site, Station, Mature) %>%
                           summarise(Count = n()) %>% 
                           Complete_Mat(),
                         Maturity %>% mutate(Mature = ifelse(Final_Stage == 4 | Final_Stage == 0, "I", "M")) %>% 
                           dplyr::select(Year, Month, Site, Station, Final_Stage, Mature) %>%
                           group_by(Year, Month, Site, Station) %>%
                           summarise(Total = n())) %>%
  mutate(Prop = Count/Total,
         Year = as.numeric(Year),
         Month = as.numeric(Month)) %>% mutate(Prop = case_when(Prop <0 ~ 0, TRUE ~ Prop))
#
head(Repro_Mat_props)
#
Repro_Mat_props2 <- merge(Maturity %>% mutate(Mature2 = ifelse(Final_Stage == 4 | Final_Stage == 0 | Final_Stage == 1, "I", "M")) %>% 
                            dplyr::select(Year, Month, Site, Station, Final_Stage, Mature2) %>%
                            group_by(Year, Month, Site, Station, Mature2) %>%
                            summarise(Count2 = n()) %>%
                            Complete_Mat(),
                          Maturity %>% mutate(Mature2 = ifelse(Final_Stage == 4 | Final_Stage == 0 | Final_Stage == 1, "I", "M")) %>% 
                            dplyr::select(Year, Month, Site, Station, Final_Stage, Mature2) %>%
                            group_by(Year, Month, Site, Station) %>%
                            summarise(Total2 = n())) %>%
  mutate(Prop2 = Count2/Total2,
         Year = as.numeric(Year),
         Month = as.numeric(Month)) %>% mutate(Prop2 = case_when(Prop2 <0 ~ 0, TRUE ~ Prop2))
#
head(Repro_Mat_props2)
#
#
#
#MonYr and sampling schema (Repro & Spat) with Count and Prop Mature samples
ReproSpat <- full_join(Repro_samples, Rcrt_df) %>% 
  mutate(Site = as.factor(Site),
         Type = case_when(Samples == 0 & Mean == 0 ~ "NRS&NS", 
                          Samples == 0 & Mean > 0  ~ "NRS&S",
                          Samples > 0 & Mean > 0 ~ "R&S",
                          Samples > 0 & Mean == 0 ~ "R&NS",
                          Samples > 0 & is.na(Mean) ~ "R&NSS",
                          Samples == 0 & is.na(Mean) ~ "NRS&NSS",
                          TRUE ~ NA)) %>%               
  left_join(Repro_Mat_props) %>% mutate(Mature = ifelse(is.na(Mature), "Z", Mature))
ReproSpat <- ReproSpat %>% mutate(Type = fct_relevel(Type, Sample_order))
#
ReproSpat2 <- full_join(Repro_samples, Rcrt_df) %>% 
  mutate(Site = as.factor(Site),
         Type = case_when(Samples == 0 & Mean == 0 ~ "NRS&NS", 
                          Samples == 0 & Mean > 0  ~ "NRS&S",
                          Samples > 0 & Mean > 0 ~ "R&S",
                          Samples > 0 & Mean == 0 ~ "R&NS",
                          Samples > 0 & is.na(Mean) ~ "R&NSS",
                          Samples == 0 & is.na(Mean) ~ "NRS&NSS",
                          TRUE ~ NA)) %>%  
  left_join(Repro_Mat_props2) %>% mutate(Mature2 = ifelse(is.na(Mature2), "Z", Mature2),
                                         Type = fct_relevel(Type, Sample_order))
#
ggarrange(
  ReproSpat %>% filter(Site == "SL-S" & Station == 1 & Mature != "I") %>%
    ggplot()+
    geom_tile(aes(MonYr, y = 0.5, fill = Type), height = 1, alpha = 0.3)+
    geom_point(aes(MonYr, Prop), size = 2)+
    geom_line(aes(MonYr, Prop, group = 1)) + SampFill + Base,
  ReproSpat2 %>% filter(Site == "SL-S" & Station == 1 & Mature2 != "I") %>%
    ggplot()+
    geom_tile(aes(MonYr, y = 0.5, fill = Type), height = 1, alpha = 0.3)+
    geom_point(aes(MonYr, Prop2), color = "red")+
    geom_line(aes(MonYr, Prop2, group = 1), color = "red")+ SampFill + Base,
  ncol = 1)
#
#
#
#####
###Working with ReproSpat - checking for full representation of Months/Years
ReproSpat %>% group_by(Site, Station, Year, Month) %>% filter(Site == "SL-C") %>%
  summarise(count = n()) %>% pivot_wider(names_from = Month, values_from = count) %>% print(n = Inf)
#
##Function to select data between specified type (NRS&NSS/NRS&NS/NRS&S) and next repro collections
Selected_samples <- function(df, dataType){
  df1 <- data.frame()
  df2 <- data.frame()
  #
  for (i in unique(df$Site)){
    Site_data <- data.frame(df %>% filter(Site == i))
    for (j in unique(df$Station)){
      Station_data <- data.frame(Site_data %>% filter(Station == j) %>% mutate(Type = as.factor(Type))) #Limit to data to work with
      
      #Identify periods of no repro data of specified Type
      rle_NRSNSS <- rle(Station_data$Type == dataType) #Identify runs of Types
      last_NRSNSS <- (cumsum(rle_NRSNSS$lengths))[rle_NRSNSS$values == 1] #Identify last row of each sequence of specified Type
      
      #Identify periods with repro collections
      rle_R <- rle(Station_data$Type == "R&NS" | Station_data$Type == "R&NSS" | Station_data$Type == "R&S") #Identify when repro is collected
      first_R <- (cumsum(rle_R$lengths)+1)[rle_R$values == 0] #Identify first row of each sequence of specified Type
      
      #Find upper limit for first_R list based on no repro data
      find_next_higher <- function(seq, val) {
        for (h in seq_along(seq)) {
          if (seq[h] > val) {
            return(seq[h])
          }
        }
        return(NA)  # Return NA if no higher number is found
      }
      next_number <- find_next_higher(first_R, last(last_NRSNSS))
      first_R <- first_R[first_R <= next_number] #Subset to proper data
      
      # Apply function to each element of list1, expand to next 5 months (rows) of data and append to last list then order all numbers sequentially
      findNextHighest <- function(x) {
        # Find the index of the next highest number in list2
        next_highest_index <- which.max(first_R > x)
        # If there is no such number, return NA
        if (length(next_highest_index) ==  0) {
          return(NA)
        }
        # Return the next highest number
        return(first_R[next_highest_index])
      }
      next_highest_list <- sapply(last_NRSNSS[last_NRSNSS <= next_number], findNextHighest)
      
      #Determine four months out from selected dates
      out <- c()
      for (n in seq_along(next_highest_list)){
        out2 <- which(temp$MonYr == temp[next_highest_list[n],]$MonYr + months(4))[1]
        out <- append(out, out2)
      }
      
      final_list <- c()
      # Loop over the indices of the lists
      for (m in seq_along(last_NRSNSS)) {
        # Append the element from list1
        final_list <- append(final_list, last_NRSNSS[m])
        
        # Append the element from list2
        final_list <- append(final_list, out[m])
      }
      
      #Empty list and function to add colon and commas between row numbers 
      res1 <- ""  
      for (k in seq_along(final_list)) {
        if (k %%  2 ==  0) {
          res1 <- paste0(res1, final_list[k], ",")
        } else {
          res1 <- paste0(res1, final_list[k], ":")
        }
      }
      #Remove any extra comma or colon at the end of the string
      remove_last_x <- function(res1) {
        if (substr(res1, nchar(res1), nchar(res1)) == "," | nchar(res1) == ":") {
          return(substr(res1,  1, nchar(res1) -  1))
        } else {
          return(res1)
        }
      }
      res1 <- remove_last_x(res1)
      
      #Get all rows numbers between each range of numbers
      row_numbers <- ""
      for (l in 1:nrow(rows_needed)) { 
        row <- eval(parse(text =rows_needed[l,]))
        row_numbers <- append(row_numbers, row)
      }
      row_numbers
      #Subset working df to desired rows
      df3 <- Station_data[row_numbers %>% unique(),]
      #Add in missing rows of MonYr data by selecting for missing data then binding back to df2
      df4 <- rbind(df3,
                   anti_join(ReproSpat %>% filter(Site == i & Station == j) %>% filter(MonYr %in% df3$MonYr), df3)) %>% 
        arrange(Site, Station, MonYr)
      df2 <- rbind(df2, df4)
    }
    df1 <- rbind(df1, df2)
  }
  return(df1)
}
#
test <- Selected_samples(ReproSpat %>% filter(Site == "SL-C" & Station == 3), "NRS&NSS")
#
#
#Ready to try on all data.
#
t <- rbind(test,
           anti_join(ReproSpat %>% filter(Site == "SL-C" & MonYr %in% test$MonYr), test)) %>% 
  arrange(Site, Station, MonYr)




##Trying to find last occurrence of NRS and first occurrence with R samples then select all rows between last and first
temp <- data.frame(ReproSpat %>% filter(Site == "SL-C" & Station == 3) %>% mutate(Type = as.factor(Type))) #Data to work with
rle_NRSNSS <- rle(temp$Type == "NRS&NSS") #Identify runs of Types
last_NRSNSS <- (cumsum(rle_NRSNSS$lengths))[rle_NRSNSS$values == 1] #Identify last row of each sequence of specified Type
#
rle_R <- rle(temp$Type == "R&NS" | temp$Type == "R&NSS" | temp$Type == "R&S") #Identify when repro is collected
first_R <- (cumsum(rle_R$lengths)+1)[rle_R$values == 0] #Identify first row of each sequence of specified Type
#
#Identify upper limit for first_R list
find_next_higher <- function(seq, val) {
  for (i in seq_along(seq)) {
    if (seq[i] > val) {
      return(seq[i])
    }
  }
  return(NA)  # Return NA if no higher number is found
}
# Use the function to find the next higher number
next_number <- find_next_higher(first_R, last(last_NRSNSS))
first_R <- first_R[first_R <= next_number] #Subset to proper data
#
##Function to find the next highest number in first_R after each sequence of no repro samples (last_NRSNSS)
findNextHighest <- function(x) {
  # Find the index of the next highest number in list2
  next_highest_index <- which.max(first_R > x)
  # If there is no such number, return NA
  if (length(next_highest_index) ==  0) {
    return(NA)
  }
  # Return the next highest number
  return(first_R[next_highest_index])
}

# Apply the function to each element of list1, expand to next 5 months (rows) of data and append to last list then order all numbers sequentially
next_highest_list <- sapply(last_NRSNSS[last_NRSNSS <= next_number], findNextHighest)
out <- c()
for (i in seq_along(next_highest_list)){
  out2 <- which(temp$MonYr == temp[next_highest_list[i],]$MonYr + months(4))[1]
  out <- append(out, out2)
}
temp[next_highest_list,]

#final_list <- sort(append(last_NRSNSS, next_highest_list+4))
#as.list(data.frame(do.call(rbind, Map(rbind, last_NRSNSS, next_highest_list+4))))
# Initialize an empty list to hold the merged elements
merged_list <- c()
# Loop over the indices of the lists
for (i in seq_along(last_NRSNSS)) {
  # Append the element from list1
  merged_list <- append(merged_list, last_NRSNSS[i])
  
  # Append the element from list2
  merged_list <- append(merged_list, out[i])
}


#Function to add colon and commas between numbers 
res1 <- ""  
for (i in seq_along(merged_list)) {
  if (i %%  2 ==  0) {
    res1 <- paste0(res1, merged_list[i], ",")
  } else {
    res1 <- paste0(res1, merged_list[i], ":")
  }
}
#remove any extra characters at the end of the string
remove_last_x <- function(res1) {
  if (substr(res1, nchar(res1), nchar(res1)) == "," | nchar(res1) == ":") {
    return(substr(res1,  1, nchar(res1) -  1))
  } else {
    return(res1)
  }
}
res1 <- remove_last_x(res1)
#Convert string to dataframe to work with
(rows_needed <- str_split(res1, pattern = ",") %>% as.data.frame())
#
#Get all rows numbers between each range of numbers
row_numbers <- ""
for (i in 1:nrow(rows_needed)) { 
  row <- eval(parse(text =rows_needed[i,]))
  row_numbers <- append(row_numbers, row)
}
row_numbers %>% unique()
#Subset working df to desired rows
tempor <- temp[paste(row_numbers %>% unique()),]


################
fill_missing_numbers <- function(sequence) {
  if (length(sequence) ==  2) {
    return(seq(as.numeric(sequence[1]), as.numeric(sequence[2])))
  } else {
    return(as.numeric(sequence))
  }
}
lapply(str_split(rows_needed, pattern = ","), generate_sequence)


paste0("c(", str_replace_all(rows_needed, ",", "), c("), ")")
generate_sequence <- function(set) {
  seq(set[1], set[2])
}
lapply(rows_needed, generate_sequence)
list(paste0("c(", str_replace_all(rows_needed, ",", "), c("), ")"))
