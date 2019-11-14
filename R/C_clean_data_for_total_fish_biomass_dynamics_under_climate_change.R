# Script to load and clean raw data set for Figure 2
####Load packages####
library(readr)
library(tidyverse)
library(dplyr)
####Read data#####
b <- as.data.frame(read_csv("data/raw/raw_montecarlo_trials_mean_percentils.csv", 
                            col_types = cols()))
####See how the data frame is structured#####
glimpse(b)
str(b)
summary(b)
#  Basic checks --------------------------------------------------------------
nrow(b)             # How many rows
str(b)              # Variables classes
attributes(b)       # Attributres
head(b)             # First rows
any(duplicated(b))  # There is any duplicated rows?
any(is.na(b))       # There are NAs in the data?
######Rename Scenarios#####
b$scenario <- replace(b$scenario, b$scenario == "baseline", "Status quo")
b$scenario <- replace(b$scenario, b$scenario == "RCP2", "RCP 2.6")
b$scenario <- replace(b$scenario, b$scenario == "RCP4", "RCP 4.5")
b$scenario <- replace(b$scenario, b$scenario == "RCP8", "RCP 8.5")
####Subset only fish species#####
b1 <- subset(b,species %in% c("X2","X3","X4","X5","X6",
                             "X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17"))

##### Sum total fish biomass , per year and scenario #####
b2 <- b1 %>% 
   group_by(year.group, scenario) %>% 
   summarise(tbmean=sum(mean), tbmin = sum(percent_05), tbmax=sum(percent_95))

#### subset for reef sharks only--------
b_sharks <- subset(b,species %in% c("X2","X3"))
#Sum by year and scenario, total reef sharks biomass
b_sharks <- b_sharks %>% 
   group_by(year.group, scenario) %>% 
   summarise(tbmean = sum(mean), tbmin = sum(percent_05), tbmax = sum(percent_95))
b_sharks$trophic_group <- "Reef sharks"

#### subset for generalist predator only--------
b_gen_pred <- subset(b,species %in% c("X4","X5","X6"))
b_gen_pred <- b_gen_pred %>% 
   group_by(year.group, scenario) %>% 
   summarise(tbmean = sum(mean), tbmin = sum(percent_05), tbmax = sum(percent_95))
b_gen_pred$trophic_group <- "Generalist predators"

#### subset for invertivore fish only--------
b_invert <- subset(b,species %in% c("X9","X10","X12","X14","X15","X16"))
b_invert <- b_invert %>% 
   group_by(year.group, scenario) %>% 
   summarise(tbmean = sum(mean), tbmin = sum(percent_05), tbmax = sum(percent_95))
b_invert$trophic_group <- "Invertivores"

#### subset for herbivore fish only--------
b_herbi <- subset(b,species %in% c("X7","X8","X11","X13","X17"))
b_herbi <- b_herbi %>% 
   group_by(year.group, scenario) %>% 
   summarise(tbmean = sum(mean), tbmin = sum(percent_05), tbmax = sum(percent_95))
b_herbi$trophic_group <- "Herbivores/Detritivores"

### Join in an unique dataframe the 4 trophic guilds --------
b_trophic_guilds <- bind_rows(b_sharks,b_gen_pred,b_invert,b_herbi)

### Modify "b_trophic_guilds" objet to obtain the relative contribution (%) #######
# of each trophic group by the year 2100 for each scenario ####
c1 <- b_trophic_guilds %>% filter(year.group == "2100")
#Drop column 4 and column 5 
c1 <- c1[,-c(1,4,5)]
# Summarise per scenario and trophic group
c2 <- c1 %>% group_by(scenario,trophic_group) %>% 
   summarise_all(funs(sum))
## calculate the relative contribution (%) of each trophic_group by scenario
c3 <- c2 %>%
   group_by(scenario, trophic_group) %>%
   summarise(tbmean) %>%
   mutate(rel.contribution = paste0(round(100 * tbmean/sum(tbmean), 1)))

#Save processed data -----------------------------------------------------
write.csv(x = b2, 
          file = "data/processed/processed_data_figure_2.csv", 
          row.names = FALSE)

write.csv(x = b_trophic_guilds, 
          file = "data/processed/processed_data_figure_biomass_dynamics_trophic_guilds.csv", 
          row.names = FALSE)

write.csv(x = c3, 
          file = "data/processed/processed_data_figure_relative_contribution_trophic_guilds.csv", 
          row.names = FALSE)


