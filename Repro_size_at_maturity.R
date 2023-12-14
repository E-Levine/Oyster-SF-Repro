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
  mutate_at(c("Station", "Sex", "Site", "Estuary", "Final_Stage"), as.factor) %>% 
  subset(Site != "TB" & Site != "LW-R")

head(Repro_df)
summary(Repro_df)
#
#
##Proportion per stage
Repro_props <- read_excel("Output/Repro_proportions_2023 12 06.xlsx", sheet = "Sheet1", #File name and sheet name
                           skip = 0, col_names = TRUE, 
                           na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                           .name_repair = "universal") %>% 
  subset(Site != "TB" & Site != "LW-R")
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
#
Base <- theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, color = "black", family = "sans"),
        axis.text.x = element_text(size = 13, color = "black", 
                                   margin = unit(c(0.5, 0.5, 0, 0.5), "cm"), family = "sans"),
        axis.text.y = element_text(size = 14, color = "black", 
                                   margin = unit(c(0, 0.5, 0, 0), "cm"), family = "sans"),
        axis.ticks.length = unit(-0.15, "cm"), plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
#
XCate <- theme(axis.title.x = element_blank(),
               axis.text.x = element_text(color = "black", size = 14, family = "sans",
                                          margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
               plot.margin = margin(0.25, 0.5, 0.25, 0.25, "cm"))
#
theme_f <- theme(strip.text.y = element_text(color = "black", size = 11, family = "sans", face = "bold"),
                 strip.background = element_rect(fill = "#CCCCCC"),
                 panel.spacing = unit(0.75, "lines"),
                 strip.text.x = element_text(size = 10, face = "bold", family = "sans"))
#
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
Maturity %>% group_by(Sex) %>% summarise(Count = n()) %>% mutate(Pct = (Count/18235)*100)
#
##Number per SL bin - show with and without free_y for comparison
Maturity %>%
  ggplot(aes(SH, fill = Sex))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5), alpha = 0.8)+
  lemon::facet_rep_grid(Sex~., scales = 'free_y')+
  theme_classic()
#
Maturity %>% subset(Mature == "I") %>%
  ggplot(aes(SH))+
  geom_histogram(aes(y = ..count..), breaks = seq(0,90, by = 5))+
  #facet_wrap(.~Sex)+
  theme_classic()
#
##Based on 46 samples staged as immature:
####Size at maturity figure
#Need histo file, "Species code", Proportion maturity, Type, Extra
#Type 1 = all M and F in one, extra == NA; 
#Type 2 = either Male or Female, must specify extra == "M" or "F"; 
#Type 3 = Male and female with individual propMature, specify extra == 1 for 1 plot, extra == 2 for faceted
matureSL <- function(df, proportionMature, Type, extra){
  #Functions needed
  round5 <- function(x){5*ceiling(x/5)} #Rounds x up to nearest 5
  
  #df to work with - if FC, need to include "U" and use minSL M or F for maturity size
  mat_all <- df %>%  mutate(Mature = as.factor(Mature), Sex = as.factor(Sex)) %>% 
    dplyr::select(SH, bins, Sex, Mature) %>% drop_na()
  mat_M <- mat_all %>% filter(Sex != "F" & Sex != "M/F") %>% mutate(Sex = replace(Sex, Sex == "Z", "M"))
  mat_F <- mat_all %>% filter(Sex != "M" & Sex != "M/F") %>% mutate(Sex = replace(Sex, Sex == "Z", "F"))
  
  ##Color to sex
  Sex <- c("M" = "Male", "F" = "Female", "M/F" = "Hermaphrodite", "Z" = "Undetermined")
  color_og <- c("#009E73", "#E69F00", "#999999", "#CCCCCC")
  #Map color to Sex
  names(color_og) <- c("F","M","M/F")#,"U")
  MFCol <- scale_color_manual(name = "", labels = Sex, values = color_og) 
  
  ###ALL
  #Fit model
  lrSH <- glm(Mature ~ SH, family = binomial, data = mat_all)
  output <- data.frame(SH = seq(0, round5(max(mat_all$SH)), 5))
  output$Mature <- predict(lrSH, newdata = output, type = "response")
  #Get x where y = 0.5
  LD50 <- MASS::dose.p(lrSH, p = proportionMature)
  
  ###Males
  lrSHM <- glm(Mature ~ SH, family = binomial, data = mat_M)
  outputM <- data.frame(SH = seq(0, round5(max(mat_M$SH)), 5))
  outputM$Mature <- predict(lrSHM, newdata = outputM, type = "response")
  #Get x where y = p
  LD50M <- MASS::dose.p(lrSHM, p = proportionMature)
  
  ###Females
  lrSHF <- glm(Mature ~ SH, family = binomial, data = mat_F)
  outputF <- data.frame(SH = seq(0, round5(max(mat_F$SH)), 5))
  outputF$Mature <- predict(lrSHF, newdata = outputF, type = "response")
  #Get x where y = p
  LD50F <- MASS::dose.p(lrSHF, p = proportionMature)
  
  ##Plots
  All <- mat_all %>%
    ggplot(aes(SH, as.numeric(Mature)-1))+
    geom_point(aes(color = Sex), size = 3, alpha = 0.6)+
    stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                method.args = list(family = binomial), size = 1.25)+
    Base + XCate + MFCol +
    geom_vline(xintercept = LD50[[1]],linetype = "dashed", color = "black", size = 1)+
    scale_x_continuous(name = "Shell height (mm)", expand = c(0,0), limits = c(0, round5(max(mat_all$SH))), breaks = seq(0, round5(max(mat_all$SH)), by = 10))+
    scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  Single <- mat_all %>% subset(Sex == extra | Sex == "Z") %>%
    ggplot(aes(SH, as.numeric(Mature)-1))+
    geom_point(aes(color = Sex), size = 3, alpha = 0.6)+
    stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                method.args = list(family = binomial), size = 1.25)+
    Base + XCate + MFCol +
    geom_vline(xintercept = ifelse(extra == "M", LD50M[[1]], LD50F[[1]]),linetype = "dashed", color = "black", size = 1)+
    scale_x_continuous(name = "Shell height (mm)", expand = c(0,0), limits = c(0, round5(max(mat_all$SH))), breaks = seq(0, round5(max(mat_all$SH)), by = 10))+
    scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  Both <- mat_all %>%
    ggplot(aes(SH, as.numeric(Mature)-1))+
    geom_point(aes(color = Sex), size = 3, alpha = 0.6)+
    stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                method.args = list(family = binomial), size = 1.25)+
    Base + XCate + MFCol +
    geom_vline(xintercept = LD50[[1]],linetype = "dashed", color = "black", size = 1)+
    geom_vline(xintercept = LD50M[[1]],linetype = "dashed", color = "#E69F00", size = 1)+
    geom_vline(xintercept = LD50F[[1]],linetype = "dashed", color = "#009E73", size = 1)+
    scale_x_continuous(name = "Shell height (mm)", expand = c(0,0), limits = c(0, round5(max(mat_all$SH))), breaks = seq(0, round5(max(mat_all$SH)), by = 10))+
    scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  Facet <- rbind(mat_M, mat_F) %>%
    ggplot(aes(SH, as.numeric(Mature)-1))+
    geom_point(aes(color = Sex), size = 3, alpha = 0.6)+
    stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
                method.args = list(family = binomial), size = 1.25)+
    lemon::facet_rep_grid(Sex~., labeller = labeller(Sex = Sex))+
    Base + XCate + MFCol + theme_f+ theme(legend.position = "none")+
    geom_vline(data = filter(mat_all, Sex == "M"), aes(xintercept = LD50M[[1]]),linetype = "dashed", color = "black", size = 1)+
    geom_vline(data = filter(mat_all, Sex == "F"), aes(xintercept = LD50F[[1]]),linetype = "dashed", color = "black", size = 1)+
    scale_x_continuous(name = "Shell height (mm)", expand = c(0,0), limits = c(0, round5(max(mat_all$SH))), breaks = seq(0, round5(max(mat_all$SH)), by = 10))+
    scale_y_continuous(name = "Proportion mature", expand = c(0.025,0.025), limits = c(0,1))
  
  #OUTPUT
  ifelse(Type == 1,
         return(list(All, paste("All:", proportionMature,"=",LD50[1],",", "SE =",attr(LD50, "SE")))),
         ifelse(Type == 2,
                return(list(Single, ifelse(extra == "M", 
                                           paste("Males:", proportionMature,"=",LD50M[1],",", "SE =",attr(LD50M, "SE")), 
                                           paste("Females:", proportionMature,"=",LD50F[1],",", "SE =",attr(LD50F, "SE"))))),
                ifelse(Type == 3,
                       if(extra == 1){return(list(Both, 
                                                  paste("All:", proportionMature,"=",LD50[1],",", "SE =",attr(LD50, "SE")), 
                                                  paste("Males:", proportionMature,"=",LD50M[1],",", "SE =",attr(LD50M, "SE")), 
                                                  paste("Females", proportionMature,"=",LD50F[1],",", "SE =",attr(LD50F, "SE"))))}
                       else if(extra == 2){return(list(Facet, 
                                                       paste("Males:", proportionMature,"=",LD50M[1],",", "SE =",attr(LD50M, "SE")), 
                                                       paste("Females", proportionMature,"=",LD50F[1],",", "SE =",attr(LD50F, "SE"))))}
                       else{print("1 or 2 plots?")},
                       print("Need more info"))))
  
}
#
#Name SP_maturity_Prop#_PropSL_YYYY MM
matureSL(Maturity, 0.5, 1, NA)
#
#
Maturity %>%
  ggplot(aes(SH, fill = Mature))+
  geom_histogram(position = "fill")+
  #scale_x_binned(expand = c(0,0))+
  #scale_y_continuous(expand = c(0,0))+
  theme_classic()
#
#
####Repro collections per date####
#
##Data frame of full sampling timeframe
Repro_full <- rbind(Repro_df %>% filter(Estuary != "CR" & Estuary != "TB") %>% droplevels() %>% complete(Year, Month, Site, Station) %>%
                          filter(!(Site == "LW" & Station == "4") & !(Site == "LX-N" & Station == "4") & !(Site == "LX-S" & Station == "4") & 
                                   !(Site == "SL-C" & Station == "4") & !(Site == "SL-N" & Station == "4") & !(Site == "SL-S" & Station == "4")), 
                        Repro_df %>% filter(Estuary == "CR") %>% droplevels()  %>% complete(Year, Month, Site, Station) %>% 
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
#

#
##Function to determine all months without samples, and next time samples were collected (by station):
#StartDate of project: YYYY-MM-01
#output[1]: Site activity
#output[2]: Repro data
#output[2]: repro data dates
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
  #Summarize counts of active or not active, percent of montly sample, and average SH of each class
  Site_active_summ <- left_join(Site_active %>% group_by(MonYr, Site, Active) %>% summarise(Count = sum(Count)),
                                Repro_samples_noB %>% group_by(Site, MonYr) %>% summarise(Total = sum(Samples))) %>%
    mutate(Pct = (Count/Total)*100) %>% #Adding percent of sample
    left_join(Repro_full %>% filter(Site == TargetSite & Buceph == "N") %>% 
                mutate(Active = as.factor(ifelse(as.integer(Final_Stage) > 1 & as.integer(Final_Stage) < 5, "Y", "N"))) %>%
                group_by(MonYr, Site, Active) %>% summarise(meanSH = mean(SH, na.rm = T), sdSH = sd(SH, na.rm = T), minSH = min(SH, na.rm = T), maxSH = max(SH, na.rm = T)))
  
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
temp <- ReproSampling("SL-S", "2005-02-01")
#
##Separate each data frame
SLS_activity <- as.data.frame(temp[1])
SLS_activity_summary <- as.data.frame(temp[2])
SLS_selected_repro <- as.data.frame(temp[3])
SLS_selected_dates <- as.data.frame(temp[4])
#
SLC_selected_dates %>% 
  ggplot(aes(MonYr, Samples, group = Station, color = as.factor(Samples)))+
  geom_point(size = 3)+
  theme_classic()
#Histogram plot of blank spaces for no collections then Y/N fill of activity until all active again. Ave sizes corresponding to activity changes. 
SLC_activity %>% 
  ggplot(aes(MonYr, group = Active, fill = Active))+
  #geom_histogram(aes(y = after_stat(count)))
  geom_bar(position = "fill")
  #lemon::facet_rep_grid(Station~.)
#
#
#
####Recruitment####
#
#Dates of interest
LW_dates <- c("2005-09-01", "2005-10-01", "2005-11-01", "2011-05-01", "2011-06-01", "2011-07-01")
CR_dates <- c("2017-08-01", "2017-09-01", "2017-10-01", "2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01", "2022-09-01")
LX_dates <- c("2005-09-01", "2005-10-01", "2005-11-01", "2008-05-01", "2008-06-01", "2008-07-01", "2011-05-01", "2011-06-01", "2011-07-01")
SLC_dates <- c("2005-06-01", "2005-07-01", "2005-08-01", "2005-09-01", "2005-10-01", "2005-11-01", "2005-12-01", "2006-01-01","2006-02-01", "2006-03-01", "2006-04-01", "2006-05-01", "2008-08-01", "2008-09-01",
               "2008-10-01", "2008-11-01", "2008-12-01", "2009-01-01", "2009-02-01", "2013-09-01", "2013-10-01", "2013-11-01", "2013-12-01", "2014-01-01", "2014-02-01", "2017-08-01", "2017-09-01",
               "2017-10-01", "2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01")
SLN_dates <- c("2005-06-01", "2005-07-01", "2005-08-01", "2005-09-01", "2005-10-01", "2005-11-01", "2005-12-01", "2006-01-01","2006-02-01", "2006-03-01", "2006-04-01", "2006-05-01", "2006-06-01", "2006-07-01", 
               "2006-08-01","2006-09-01", "2006-10-01", "2006-11-01", "2006-12-01", "2007-01-01", "2008-08-01", "2008-09-01", "2008-10-01", "2008-11-01", "2008-12-01", "2009-01-01", "2009-02-01", "2009-03-01", 
               "2009-04-01", "2013-07-01", "2013-08-01", "2013-09-01", "2013-10-01", "2013-11-01", "2013-12-01", "2014-01-01", "2017-08-01", "2017-09-01",
               "2017-10-01", "2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01")
SLS_dates <- c("2005-06-01", "2005-07-01", "2005-08-01", "2005-09-01", "2005-10-01", "2005-11-01", "2005-12-01", "2006-01-01","2006-02-01", "2006-03-01", "2006-04-01", "2006-05-01", "2006-06-01", "2006-07-01", 
               "2006-08-01","2006-09-01", "2006-10-01", "2006-11-01", "2006-12-01", "2007-01-01", "2007-02-01", "2007-03-01", "2008-08-01", "2008-09-01", "2008-10-01", "2008-11-01", "2008-12-01", "2009-01-01", "2009-02-01", "2009-03-01", 
               "2009-04-01", "2013-07-01", "2013-08-01", "2013-09-01", "2013-10-01", "2013-11-01", "2013-12-01", "2014-01-01", "2014-02-01", "2014-03-01", "2017-08-01", "2017-09-01",
               "2017-10-01", "2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01")
#