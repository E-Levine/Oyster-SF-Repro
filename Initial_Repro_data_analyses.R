###Repro data analyses
#
##Summary: site sizes, monthly % stage, annual count/stage
#
#
#Load packages, install as needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, 
               rstatix, rcompanion, #Summary stats & multiple comparisons
               zoo, lubridate, #Dates and times
               readxl, writexl, #Reading excel files
               car, #Basic analyses
               lme4,
               ggpubr, #Arranging ggplots
               install = TRUE)
#
#
#
####Load data and create base data frames####
#
Repro_data <- read_excel("Repro_data_2023 11.xlsx", sheet = "Sheet1", #File name and sheet name
                    skip = 0, col_names = TRUE, 
                    col_types = c("date","text","text", "text", "text", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "text"), 
                    na = c(""), trim_ws = TRUE, #Values/placeholders for NAs; trim extra white space?
                    .name_repair = "universal")

#
head(Repro_data)
#
#Cleaned data frame
Repro_df <- Repro_data %>% mutate_at(c("Year", "Site", "Station", "Sex", "Final_Stage"), as.factor) %>%
  mutate(Month = factor(Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")))
head(Repro_df)
#
##Repro stage comparisons - all repro stages (mature)
Repro_comps <- Repro_df %>% filter(Final_Stage != "8" & Final_Stage != "M/F" & Final_Stage != "Z")
#
##Maturity data 
Repro_IM <- Repro_df %>% mutate(Maturity = ifelse(Final_Stage == "Z", "I", "M"))
glimpse(Repro_IM)
#
#
#
#
####Oyster sizes#####
#
#Average, min, and max sizes
(Overall_SH <- Repro_df %>% get_summary_stats("SH", type = "common"))
(Annual_SH <- left_join(Repro_df %>% group_by(Year) %>% get_summary_stats("SH", type = "common"),
                    Repro_df %>% group_by(Year, Site) %>% summarise(n = n()) %>% spread(Site, n))) 
(Site_SH <- Repro_df %>% group_by(Site) %>% get_summary_stats("SH", type = "common"))
(Crossed_SH <- Repro_df %>% group_by(Year, Site) %>% get_summary_stats("SH", type = "common") %>%
    arrange(Site))
#
ggplot(Crossed_SH, aes(Year, mean, color = Site, group = Site))+
  geom_point()+
  geom_line()+
  theme_classic()
#
ggplot(Repro_df, aes(Site, SH))+
  geom_boxplot()+
  facet_wrap(.~Year)
#
#
#glmm - working
#Random intercept model 
Size1 <- lmer(SH ~ Year + (1|Site), data = Repro_df)
#Random slope + intercept
Size2 <- lmer(SH ~ Year + (Year|Site), data = Repro_df)
#Poisson model
Size3 <- glmer(SH ~ Year + (1|Site), data = Repro_df, family = "poisson")

#
#
#
####Pct Repro Stages####
#
#Percent per repro stage
(Pct_overall <- left_join(Repro_comps %>% group_by(Year, Month, Site, Final_Stage) %>% summarise(Count = n()),
                          Repro_comps %>% group_by(Year, Month, Site) %>% summarise(Total = n())) %>%
  mutate(Pct = Count/Total))
#
ggplot(Pct_overall, aes(Month, Pct, fill = Final_Stage))+
  geom_bar(position = "fill", stat = "identity")+
  theme_classic()
#
#
Pct_overall %>% filter(Site == "LX-N") %>%
  ggplot(aes(Month, Pct, fill = Final_Stage))+
  geom_bar(position = "fill", stat = "identity")+
  facet_grid(Year~.)
#

####Size at maturity####
# NEED TO USE ALL POSSIBLE DATA
(Repro_IM_df <- Repro_IM %>% mutate(Maturity = as.factor(Maturity)) %>% 
  filter(Final_Stage != "M/F") %>%
  dplyr::select(Year:SH, Maturity))
#
All_mat <- glm(Maturity ~ SH, family = binomial, data = Repro_IM_df)
All_output <- data.frame(SH = seq(0, round(5*ceiling(max(Repro_IM_df$SH)/5)), 2.5))
All_output$Maturity <- predict(All_mat, newdata = All_output, type = "response")
All_LD50 <- MASS::dose.p(All_mat, p = 0.5)
ggplot(Repro_IM_df, aes(SH, as.numeric(Maturity)-1))+
  geom_point(size = 3, alpha = 0.6)+
  
