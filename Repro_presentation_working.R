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
StaColor <- scale_color_manual(name = "Stage", labels = Stages, values = cbPalette, na.value = "#999999")
#
##Color to sex
Sex <- c("F" = "Female", "M" = "Male", "M/F" = "M/F", "Z" = "Undetermined")
color_og <- c("#009E73", "#E69F00", "#9966FF", "#666666")
#Map color to Sex
names(color_og) <- c("F","M","M/F","Z")
MFCol <- scale_color_manual(name = "", labels = Sex, values = color_og) 
MFFill <- scale_fill_manual(name = "", labels = Sex, values = color_og) 

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
Maturity %>% subset(Mature == "I") %>% summarise(n())
#
##Already see a slight issue.
##Based on 46 samples staged as immature:
#
##Working from scratch:
round5 <- function(x){5*ceiling(x/5)} #Rounds x up to nearest 5
#Limited to desrired columns and check for missing data
mat_all <- Maturity %>%  mutate(Mature = as.factor(Mature), Sex = as.factor(Sex)) %>% 
  dplyr::select(SH, bins, Sex, Mature) %>% drop_na()
Amelia::missmap(mat_all) #None missing
#
#Fit model
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
#
Maturity %>% filter(Final_Stage == "1" & SH < 47) %>%
  ggplot(aes(SH))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5))+
  Base + scale_x_continuous("Shell height (mm)", expand = c(0,0)) + scale_y_continuous("Count", expand = c(0,0))
#
#
#
####Repro collections per date####
#
##Data frame of full sampling time frame - remove extra stations from all time sites and extra stations created when filling out CR
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
#Requires: Repro_samples, Repro_full, Repro_samples_noB
ReproSampling <- function(TargetSite, StartDate) {
  #
  #Create data frame for final selected data and dates/number of samples
  Selected_repro_data <- data.frame()
  Dates_repro_data <- data.frame()
  
  #Filter data for site (Site_repro) and determine number reproductively active each month (Site_active)
  Site_repro <-  Repro_samples %>% filter(Site == TargetSite, MonYr >= as.Date(paste(StartDate)))
  Site_active <- Repro_full %>% filter(Site == TargetSite, MonYr >= as.Date(paste(StartDate)) & Final_Stage != 8) %>%
    #NOTE: When converting Final_Stage to integer, each value is 1 higher due to the "0" stage factor level (0 factor = 1 integer, 1=2, etc.)
    mutate(Active = as.factor(ifelse(as.integer(Final_Stage) > 1 & as.integer(Final_Stage) < 5, "Y", "N"))) %>% 
    group_by(MonYr, Site, Station, Active) %>% summarise(Count = n()) #%>% drop_na(Active) 
  #Summarize counts of active or not active, percent of monthly sample, and average SH of each class
  Site_active_summ <- left_join(Site_active %>% group_by(MonYr, Site, Station, Active) %>% summarise(Count = sum(Count)),
                                Repro_samples_noB %>% group_by(Site, MonYr, Station) %>% summarise(Total = sum(Samples))) %>%
    mutate(Pct = (Count/Total)*100) %>% #Adding percent of sample
    left_join(Repro_full %>% filter(Site == TargetSite & Buceph == "N") %>% 
                mutate(Active = as.factor(ifelse(as.integer(Final_Stage) > 1 & as.integer(Final_Stage) < 5, "Y", "N"))) %>%
                group_by(MonYr, Site, Station, Active) %>% summarise(meanSH = mean(SH, na.rm = T), sdSH = sd(SH, na.rm = T), minSH = min(SH, na.rm = T), maxSH = max(SH, na.rm = T)))
  
  #Determine number of stations for the desired site and all dates of study
  Site_active <- Repro_full %>% filter(Site == TargetSite, MonYr >= as.Date(paste(StartDate))) %>%
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
                                     filter(MonYr %in% Station_zeros$MonYr & Site == TargetSite),
                                   Repro_full %>% dplyr::select(Year:Sex, Estuary, Final_Stage:MonYr) %>%
                                     filter(MonYr %in% Station_counts$MonYr & Site == TargetSite)))        
    Selected_repro_data <- rbind(Selected_repro_data, Station_repro_data_i)
    Dates_repro_data <- rbind(Dates_repro_data, Station_zeros, Station_counts)
  }
  return(list(Site_active, Site_active_summ, Selected_repro_data, Dates_repro_data))
}
#
temp <- ReproSampling("SL-C", "2005-02-01")
#
##Separate each data frame
SLC_activity <- as.data.frame(temp[1])
SLC_activity_summary <- as.data.frame(temp[2])
SLC_selected_repro <- as.data.frame(temp[3])
SLC_selected_dates <- as.data.frame(temp[4])
#
#
SLC_selected_dates %>% 
  ggplot(aes(MonYr, Samples, group = Station, color = as.factor(Samples)))+
  geom_point(size = 3)+
  theme_classic()
#Histogram plot of blank spaces for no collections then Y/N fill of activity until all active again. Ave sizes corresponding to activity changes. 
SLN_activity %>% 
  ggplot(aes(MonYr, group = Active, fill = Active))+
  #geom_histogram(aes(y = after_stat(count)))
  geom_bar(position = "fill")+
 lemon::facet_rep_grid(Station~.)
#
#
#
###Working with SLC as example
#Date ranges for fills
(SLC_Fill_Dates <- cbind(SLC_selected_dates %>% 
                           filter(MonYr >= "2008-9-01" & MonYr != "2020-04-01" & MonYr != "2020-05-01" & MonYr != "2020-06-01" & MonYr != "2018-08-01") %>% 
                           drop_na(MonYr) %>% arrange(Station, MonYr) %>%
                           subset(Samples == 0) %>% dplyr::select(Site, Station, MonYr) %>% mutate("from" = MonYr),
                         SLC_selected_dates %>% 
                           filter(MonYr >= "2008-9-01" & MonYr != "2020-04-01" & MonYr != "2020-05-01" & MonYr != "2020-06-01" & MonYr != "2018-08-01") %>% 
                           drop_na(MonYr) %>% arrange(Station, MonYr) %>%
                           subset(Samples > 0) %>% dplyr::select(MonYr) %>% rename("to" = MonYr)))
#
#
(SLC_active_plot <- ggplot()+
  geom_rect(data = SLC_Fill_Dates, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4)+
  geom_jitter(data = SLC_selected_repro %>% 
                filter(MonYr >= "2008-9-01" & Final_Stage != 8 & Final_Stage!= "M/F") %>% drop_na(MonYr),
                aes(MonYr, SH, color = Final_Stage), size = 2, width = 5)+
  geom_jitter(data = SLC_selected_repro %>% 
                filter(MonYr >= "2008-9-01" & (Final_Stage == 4 | Final_Stage == 0)) %>% drop_na(MonYr),
              aes(MonYr, SH, color = Final_Stage), size = 3.5, width = 5)+
  lemon::facet_rep_grid(Station~.)+
  StaColor+
  scale_y_continuous("Shell height (mm)", expand = c(0,0), limits = c(0, 80))+
  scale_x_date("", expand = c(0,0), limits = c(as.Date("2006-01-01"), as.Date("2023-12-31")), 
               date_breaks = "24 months", date_labels = "%b %Y")+
  Base + theme_f)
#
#
Repro_Mat_props <- merge(Maturity %>% mutate(Mature = ifelse(Final_Stage == 4 | Final_Stage == 0, "I", "M")) %>% 
                           dplyr::select(Year, Month, Site, Station, Final_Stage, Mature) %>%
                           group_by(Year, Month, Site, Station, Mature) %>%
                           summarise(Count = n()),
                         Maturity %>% mutate(Mature = ifelse(Final_Stage == 4 | Final_Stage == 0, "I", "M")) %>% 
                           dplyr::select(Year, Month, Site, Station, Final_Stage, Mature) %>%
                           group_by(Year, Month, Site, Station) %>%
                           summarise(Total = n())) %>%
  mutate(Prop = Count/Total,
         Year = as.numeric(Year),
         Month = as.numeric(Month))

#
#
#
####Recruitment data####
#
##Periods of no recruitment
Rcrt_match <- Rcrt_df %>% filter(Site =="SL-C" & MonYr %in% (SLC_selected_dates %>% filter(Samples == 0))$MonYr) %>%
  filter(MonYr != "2020-04-01")
#
##Mean rcrt rates during no collection time period
Rcrt_df %>% filter(Site == "LW" & Station == 3) %>%
  #filter(MonYr >= "2006-09-01" & MonYr <= "2006-10-01") %>%
  summarise(mean = mean(Mean, na.rm = T))
##and during next collections
Rcrt_df %>% filter(Site == "LW" & Station == 1) %>%
  filter(MonYr == "2006-11-01") %>%
  summarise(mean = mean(Mean, na.rm = T))

#Plot of rcrt rates (red = no rcrt). Gray = no repro samples.
Rcrt_df %>% filter(Site =="SL-N" & MonYr >= "2006-01-01" & MonYr <= "2023-12-31") %>% 
  mutate(Fill = ifelse(Mean == 0, "fill", "none")) %>% mutate(Fill = replace_na(Fill, "none")) %>%
  ggplot()+
  geom_tile(aes(MonYr, y = 10, fill = Fill), height = 20, alpha = 0.2)+
  geom_rect(data = SLC_Fill_Dates, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4)+
  geom_bar(aes(MonYr, Mean), stat = "summary", fill = "darkblue")+
  lemon::facet_rep_grid(Station~., scales = "free_y")+
  scale_fill_manual("", values = c("red", "white"))+ #Red = no rcrt
  scale_y_continuous("Average recruitment (spat/shell)", expand = c(0,0))+
  scale_x_date("", expand = c(0,0), limits = c(as.Date("2006-01-01"), as.Date("2023-12-31")), 
               date_breaks = "24 months", date_labels = "%b %Y")+
  Base + theme_f + theme(legend.position = "none")
#
#
###DF of all final selected repro data and difference in days
Final_repro <- rbind(left_join(SLC_selected_repro %>% filter(MonYr %in% (SLC_selected_dates %>% filter(Samples != 0))$MonYr),
                               SLC_activity %>% arrange(Station, MonYr) %>% #Arrange by MonYr and Station for proper day-diff cals, get MonYr as number, calc difference in days
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0)), 
                     left_join(SLN_selected_repro %>% filter(MonYr %in% (SLN_selected_dates %>% filter(Samples != 0))$MonYr), 
                               SLN_activity %>% arrange(Station, MonYr) %>% 
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0)), 
                     left_join(SLS_selected_repro %>% filter(MonYr %in% (SLS_selected_dates %>% filter(Samples != 0))$MonYr),
                               SLS_activity %>% arrange(Station, MonYr) %>% 
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0)),
                     left_join(LXN_selected_repro %>% filter(MonYr %in% (LXN_selected_dates %>% filter(Samples != 0))$MonYr), 
                               LXN_activity %>% arrange(Station, MonYr) %>% 
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0)), 
                     left_join(LXS_selected_repro %>% filter(MonYr %in% (LXS_selected_dates %>% filter(Samples != 0))$MonYr), 
                               LXS_activity %>% arrange(Station, MonYr) %>% 
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0)), 
                     left_join(LW_selected_repro %>% filter(MonYr %in% (LW_selected_dates %>% filter(Samples != 0))$MonYr), 
                               LW_activity %>% arrange(Station, MonYr) %>% 
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0)), 
                     left_join(CRE_selected_repro %>% filter(MonYr %in% (CRE_selected_dates %>% filter(Samples != 0))$MonYr), 
                               CRE_activity %>% arrange(Station, MonYr) %>% 
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0)), 
                     left_join(CRW_selected_repro %>% filter(MonYr %in% (CRW_selected_dates %>% filter(Samples != 0))$MonYr),
                               CRW_activity %>% arrange(Station, MonYr) %>% 
                                 mutate(MonYrNum = as.numeric(MonYr)) %>% mutate(across(MonYrNum, ~.- lag(.), .names = "Days_diff")) %>%  dplyr::select(MonYr:Station, Days_diff) %>% filter(Days_diff > 0))) %>% 
  drop_na(Final_Stage) %>% filter(MonYr != "2020-06-01") #Remove Covid-related samples
Final_repro %>%
  filter(Final_Stage != 8 & Final_Stage != "M/F") %>%
  ggplot()+
  geom_point(aes(MonYr, SH, color = Final_Stage), width = 2)+  
  geom_point(data = Final_repro %>% filter(Final_Stage != 2 & Final_Stage != 3, Final_Stage != 8 & Final_Stage != "M/F"),
              aes(MonYr, SH, color = Final_Stage), size = 3, width = 2)+
  scale_y_continuous("Shell height (mm)", expand = c(0,0), limits = c(0,125))+
  StaColor+
  scale_x_date("", expand = c(0,0), limits = c(as.Date("2006-01-01"), as.Date("2023-12-31")), 
               date_breaks = "24 months", date_labels = "%b %Y")+
  Base
#
#
Final_repro %>%
  filter(Final_Stage != 8 & Final_Stage != "M/F" & (Final_Stage == 2 | Final_Stage == 3 | Final_Stage == 1)) %>%
  ggplot()+
  geom_jitter(aes(as.numeric(Month), SH, color = Final_Stage), width = 0.25)+
  geom_jitter(data = Final_repro %>% filter(Final_Stage != 2 & Final_Stage != 3 & Final_Stage != 1 & Final_Stage != 8 & Final_Stage != "M/F"),
             aes(as.numeric(Month), SH, color = Final_Stage), size = 3, width = 0.25)+
  StaColor+
  scale_y_continuous(limits = c(0, 35))+
  Base
#
#
#Maturity vs shell heights 
F_repro_mat <- Final_repro %>% filter(Final_Stage != "M/F" & Final_Stage != 8) %>%
  mutate(Poss_Im = factor(ifelse(Final_Stage == 0 | Final_Stage == 4, "Indifferent", "Mature"), levels = c("Indifferent", "Mature")))
F_repro_mat %>%
  ggplot()+
  geom_jitter(aes(Poss_Im, SH, color = Final_Stage, size = Final_Stage), width = 0.25)+
  scale_y_continuous("Shell height (mm)", expand = c(0,0), limits = c(0,125))+
  scale_x_discrete("Maturity")+
  scale_size_manual("Stage", labels = Stages, values = c(4, 2, 2, 2, 2))+
  StaColor + Base
#
F_repro_mat %>% 
  ggplot(aes(Site, group = Poss_Im, fill = Poss_Im))+
  geom_bar(position = "fill")
#
F_repro_mat %>% drop_na(Days_diff) %>%
  ggplot()+
  geom_point(aes(Days_diff, SH,color = Final_Stage))+
  StaColor
#
#
###Lining up rcrt rates with no repro collections - load df of Events of missing repro samples
No_repro_events <- read.csv("../CSV/Dermo_repro/CERP_no_repro_events.csv")
No_repro_events <- No_repro_events %>% mutate(MonYr = as.Date(MonYr, fiormat = "%y-%m-%d"),
                                              Station = as.character(Station))
#Select dates from rcrt data that match event dates
t1 <- left_join(Rcrt_df %>% group_by(Site, Station), 
          No_repro_events %>% group_by(Site, Station)) %>% filter(!is.na(Event))
#Determine mean rcrt rate during no repro sample events
t2 <- t1 %>% group_by(Site, Station, Event) %>% summarise(meanRcrt = mean(Mean, na.rm = T))
#Assign Event ID to next most recent repro sample collected
t3 <- left_join(Final_repro %>% group_by(Site, Station),
                #Need next collection dates after no collections
                No_repro_events %>% group_by(Site, Station, Event) %>% slice(n()) %>% mutate(MonYr = MonYr + months(1))) %>%
  drop_na(Event)
#Final data frame relating time diff of no repro samples with mean rcrt during event 
(Rcrt_repro <- full_join(t2, 
                        t3 %>% dplyr::select(Site, Station, Event, Days_diff, Final_Stage)))
#
Rcrt_repro %>% filter(Final_Stage != 8 & Final_Stage != "M/F") %>%
  ggplot()+
  geom_jitter(aes(Days_diff, meanRcrt, color = Final_Stage, size = Final_Stage), height = 0.25)+
  scale_size_manual("Stage", labels = Stages, values = c(4, 2, 2, 2, 3))+
  scale_x_continuous("Number of days since last collection", expand = c(0,0), limits = c(0,1060))+
  scale_y_continuous("Mean recrtuiment during time period (spat/shell)", limits = c(0, 5), expand = c(0,0))+
  StaColor+ Base
#
#
#
###No repro, no spat
ReproSpat_zeros <- function(df){
  #Find unique values
  Site_list <- unique(df$Site)
  Station_list <-unique(df$Station)
  NoRepro_NoRcrt <- data.frame()
  #loop
  for (j in Site_list){
    for (i in Station_list) {
      tempdf <- df[df$Site == j & df$Station == i, ]
      tempdf_i <- tempdf %>% filter(MonYr %in% (Repro_samples %>% filter(Site == j & Station == i & Samples == 0))$MonYr) %>%
      filter(Mean == 0)
      NoRepro_NoRcrt <- rbind(NoRepro_NoRcrt, tempdf_i)
    }
  }
  return(NoRepro_NoRcrt)
}
(NoReproSpat <- ReproSpat_zeros(Rcrt_df) %>% mutate(Type = 1))
#
Rcrt_df2 <- left_join(Rcrt_df, NoReproSpat)
#
#
ReproSpat <- full_join(Repro_samples, Rcrt_df) %>% drop_na(Mean) %>%
  mutate(Type = ifelse(Samples == 0 & Mean == 0, "NR&NS", 
                       ifelse(Samples == 0 & Mean > 0, "NR&S", 
                              ifelse(Samples > 0 & Mean > 0, "R&S", 
                                     ifelse(Samples > 0 & Mean == 0, "R&NS", NA))))) %>%
  left_join(Repro_Mat_props) %>% mutate(Mature = ifelse(is.na(Mature), "Z", Mature))
#
ReproSpat %>% filter(Site == "CR-E" & Station == 1 & Mature != "I") %>%
  ggplot()+
  geom_tile(aes(MonYr, y = 0.5, fill = Type), height = 1, alpha = 0.2)+
  geom_point(aes(MonYr, Prop))+
  geom_line(aes(MonYr, Prop, group = 1))

ReproSpat %>% filter(Mature != "I") %>%
  ggplot()+
  geom_tile(aes(MonYr, y = 0.5, fill = Type), height = 1, alpha = 0.2)+
  geom_point(aes(MonYr, Prop))+
  geom_line(aes(MonYr, Prop, group = 1))+
  lemon::facet_rep_grid(Site~Station)
  #
#
#####EXTRA WORKING####
find_next_non_zero <- function(df, start_index) {
  # Find the indices of non-zero values after the start_index
  #non_zero_indices <- which(df[["Mean"]] >  0 & df[["Mean"]] != NA)[which(df[["Mean"]] >  0 & df[["Mean"]] != NA) > start_index]
  # Return the first non-zero value after the start_index, or NA if none exists
  #return(ifelse(length(non_zero_indices) >  0, df[["Mean"]][min(non_zero_indices)], NA))
  df2 <- df
  #Vector indicating presence of NRNS type
  starts <- df2$Type %in% "NRNS"
  indices <- rep(NA, nrow(df2))
  #Find next positive value after starts
  df2$NextPost <- with(df2, ifelse(starts, Type, NA))
  df2$NextPost <- with(df2, ifelse(is.na(lag(NextPost)), NextPost, NA))
  df2$NextPost <- cummax(df2$NextPost)
  #Convert to numeric
  
}#
#
#
#
#temp line: Station_df_i <- filter(Site_repro, Station == i)
Station_rle <- rle(Station_df_i$Samples == 0) #Identify each first instance of 0 or non-zero and how many rows until next change
Station_first <- Station_rle$values == 0 & Station_rle$lengths > 1 #Select the first instance of each sequence of 0s
Station_index <- (cumsum(Station_rle$lengths)+1)[Station_first] #Get the index of the first instance 
Station_zeros <- Station_df_i[Station_index,] #Select the first instance of each sequence of 0s

temp <- Rcrt_df2 %>% filter(Site == "SL-S" & Station == 1)
temp_rle <- rle(temp$Type == 1)
temp_NRNS <- temp_rle$values == 1 & temp_rle$lengths > 1 #longer than 1 month
temp_index <- (cumsum(temp_rle$lengths)+1)[temp_NRNS]
temp[temp_index,]

starts <- temp_index
indices <- rep(NA, nrow(temp))
for (i in starts){
  next_value <- min(which(temp$Mean >0)[which(temp$Mean >0) > i])
  indices[i] <- if (!is.na(next_value)) temp$Mean[next_value] else NA
}
temp$NextPost <- indices

(Rcrt_df2$NextPost <- with(Rcrt_df2, ifelse(Rcrt_df2$Type %in% "NRNS", Mean, NA)))
(Rcrt_df2$NextPost <- with(Rcrt_df2, ifelse(is.na(lag(NextPost)), NextPost, NA)))
Rcrt_df2$NextPost <- as.numeric(cummax(Rcrt_df2$NextPost))

