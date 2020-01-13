# Script to clean data for figure : Percent variation in fish biomass respect Status quo 
####Load Packages----------
library(readr)
library(tidyverse)

##### Load raw data ----------

a<-as.data.frame(read_csv("data/raw/raw_montecarlo_trials_mean_percentils.csv", 
                          col_types = cols()))
#### Subset only nO_fish species --------
a <- subset(a,species %in% c("X1","X18","X19","X20","X21",
                             "X22","X23","X24","X25","X26","X27"))

####Rename column year.group------
names(a)[1] <- "year"
#### Rename species column's factors-------
a <- a %>%
   mutate(species = recode(species, X1= "Sea birds", X18 = "Turtles",
                           X19 = "Cephalopoda", X20 = "Panulirus spp.", X21 ="Benthic macroinvertebrates",
                           X22 = "Benthic microinvertebrates", X23 = "Siderastrea stellata", 
                           X24 = "Zooplankton", 
                           X25 = "Phytoplankton", X26 = "Digenea simplex", X27 = "Other algal turf" 
   ))
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
ids_baseline = c(1)
x = 1
for (i in c(1:10)) {x = x+4
ids_baseline[i+1] = x
}

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

##### Write processsed data ------------
write.csv(a_2100,file  = "data/processed/processed_data_figure_percent_variation_year_2100_NOFISH.csv", row.names = F)

write.csv(a_2050,file  = "data/processed/processed_data_figure_percent_variation_year_2050_NOFISH.csv", row.names = F)





