# Script to clean data for figure : Percent variation in fish biomass respect Status quo 
####Load Packages----------
library(readr)
library(tidyverse)
##### Load raw data ----------
a<-as.data.frame(read_csv("data/raw/raw_montecarlo_trials_mean_percentils.csv", 
                                col_types = cols()))
#### Subset only fish species --------
a <- subset(a,species %in% c("X2","X3","X4","X5","X6",
                             "X7","X8","X9","X10","X11","X12","X13","X14","X15",
                             "X16","X17"))

####Rename column year.group------
names(a)[1] <- "year"
#### Rename species column's factors-------
a <- a %>%
   mutate(species = recode(species, X2 = "Negaprion brevirostris",
         X3 = "Ginglymostoma cirratum", X4 = "Lutjanus jocu", X5 ="Cephalopholis fulva",
         X6 = "Carangidae",X7 = "Acanthurus spp.",X8 = "Stegastes rocasensis", 
         X9 = "Thalassoma noronhanum",X10 = "Abudefduf saxatilis",X11="Sparisoma spp.", 
         X12 ="Melichthys niger",X13= "Kyphosus spp.", 
         X14 = "Mulloidichthys martinicus",X15 = "Holocentrus adscensionis", 
         X16 = "Haemulidae",X17= "Cryptobenthic reef fishes"))
#### Rename scenario's factors -----------
a$scenario <- replace(a$scenario, a$scenario == "baseline", "Status quo")
a$scenario <- replace(a$scenario, a$scenario == "RCP2", "RCP 2.6")
a$scenario <- replace(a$scenario, a$scenario == "RCP4", "RCP 4.5")
a$scenario <- replace(a$scenario, a$scenario == "RCP8", "RCP 8.5")
##### Subset for the year 2100--------
a_2100 <- subset(a, year==2100)
##### Subset for the year 2050--------
a_2050 <- subset(a, year==2050)

###Appling function "percent_dt_general" for 2100 year------------
#to estimate percent variation of biomass respect status quo scenario, year 2100
mean_variation = c()
for (number in ids_baseline) {
   mean_variation  = rbind( mean_variation  , 
                            percent_dt_general(a_2100[["mean"]], a_2100[["mean"]],number))
}

mean_variation =  mean_variation[,1]
a_2100$mean_variation <- mean_variation
#### 5 percentil 
min_variation = c()
for (number in ids_baseline) {
   min_variation  = rbind( min_variation  , 
                           percent_dt_general(a_2100[["percent_05"]],
                                              a_2100[["mean"]],number))
}
min_variation =  min_variation[,1]
a_2100$min_variation <- min_variation
#### 95 percentil
max_variation = c()
for (number in ids_baseline) {
   max_variation  = rbind( max_variation  , 
                           percent_dt_general(a_2100[["percent_95"]],
                                              a_2100[["mean"]],number))
}
max_variation =  max_variation[,1]
a_2100$max_variation <- max_variation

### Drop Status quo scenario for 2100 year---------
a_2100 <- subset(a_2100, scenario != "Status quo")

### Define  trophic category for fish species for 2100 year -------
a_2100$trophic_group <- c("Reef sharks", "Reef sharks","Reef sharks",
                   "Reef sharks", "Reef sharks","Reef sharks",
                   "Generalist predators", "Generalist predators","Generalist predators",
                   "Generalist predators", "Generalist predators","Generalist predators",
                   "Generalist predators", "Generalist predators","Generalist predators",
                   "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                   "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                   "Invertivores","Invertivores","Invertivores",
                   "Invertivores","Invertivores","Invertivores",
                   "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                   "Invertivores","Invertivores","Invertivores",
                   "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                   "Invertivores","Invertivores","Invertivores",
                   "Invertivores","Invertivores","Invertivores",
                   "Invertivores","Invertivores","Invertivores",
                   "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores")
### Add column with numbers of each fish species for 2100 year ---------
a_2100$numbers <- c("2","2","2",
                    "3","3","3",
                    "4","4","4",
                    "5","5","5",
                    "6","6","6",
                    "7","7","7",
                    "8","8","8",
                    "9","9","9",
                    "10","10","10",
                    "11","11","11",
                    "12","12","12",
                    "13","13","13",
                    "14","14","14",
                    "15","15","15",
                    "16","16","16",
                    "17","17","17")


###Appling function "percent_dt_general" for 2050 year ------------
#to estimate percent variation of biomass respect status quo scenario, year 2050
mean_variation = c()
for (number in ids_baseline) {
   mean_variation  = rbind( mean_variation  , 
                            percent_dt_general(a_2050[["mean"]], a_2050[["mean"]],number))
}

mean_variation =  mean_variation[,1]
a_2050$mean_variation <- mean_variation
#### 5 percentil 
min_variation = c()
for (number in ids_baseline) {
   min_variation  = rbind( min_variation  , 
                           percent_dt_general(a_2050[["percent_05"]],
                                              a_2050[["mean"]],number))
}
min_variation =  min_variation[,1]
a_2050$min_variation <- min_variation
#### 95 percentil
max_variation = c()
for (number in ids_baseline) {
   max_variation  = rbind( max_variation  , 
                           percent_dt_general(a_2050[["percent_95"]],
                                              a_2050[["mean"]],number))
}
max_variation =  max_variation[,1]
a_2050$max_variation <- max_variation

### Drop Status quo scenario year 2050 ---------
a_2050 <- subset(a_2050, scenario != "Status quo")

### Define  trophic category for fish species year 2050 -------
a_2050$trophic_group <- c("Reef sharks", "Reef sharks","Reef sharks",
                          "Reef sharks", "Reef sharks","Reef sharks",
                          "Generalist predators", "Generalist predators","Generalist predators",
                          "Generalist predators", "Generalist predators","Generalist predators",
                          "Generalist predators", "Generalist predators","Generalist predators",
                          "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                          "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                          "Invertivores","Invertivores","Invertivores",
                          "Invertivores","Invertivores","Invertivores",
                          "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                          "Invertivores","Invertivores","Invertivores",
                          "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores",
                          "Invertivores","Invertivores","Invertivores",
                          "Invertivores","Invertivores","Invertivores",
                          "Invertivores","Invertivores","Invertivores",
                          "Herbivores/Detritivores","Herbivores/Detritivores","Herbivores/Detritivores")
### Add column with numbers of each fish species for 2050 year ---------
a_2050$numbers <- c("2","2","2",
                     "3","3","3",
                     "4","4","4",
                     "5","5","5",
                     "6","6","6",
                     "7","7","7",
                     "8","8","8",
                     "9","9","9",
                     "10","10","10",
                     "11","11","11",
                     "12","12","12",
                     "13","13","13",
                     "14","14","14",
                     "15","15","15",
                     "16","16","16",
                     "17","17","17")

##### Write processsed data ------------
write.csv(a_2100,file  = "data/processed/processed_data_figure_percent_variation_year_2100.csv", row.names = F)

write.csv(a_2050,file  = "data/processed/processed_data_figure_percent_variation_year_2050.csv", row.names = F)





