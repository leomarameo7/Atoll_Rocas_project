#### Script to modify data frame containing temperature ranges for Rocas Atoll Species####
#### Load packages ####
library(readr)
library(tidyverse)
#### Load raw data####
a<-as.data.frame(read_csv("data/raw/temperature_range_species.csv", 
                          col_types = cols()))
##### Spread temperature variable, into columns that represent statistic parameters for each species######
a <- a %>% 
   spread(key = class, value = temperature)

# Reorder levels of Species 
a$Species <- factor(a$Species,
                    levels=c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", "Cephalopholis fulva",
                             "Carangidae","Acanthurus spp.","Stegastes rocasensis","Thalassoma noronhanum",
                             "Abudefduf saxatilis", "Sparisoma spp.","Melichthys niger","Kyphosus spp.", 
                             "Mulloidichthys martinicus", "Holocentrus adscensionis","Haemulidae",
                             "Cryptobenthic reef fishes","Turtles", "Cephalopoda", "Panulirus spp.",
                             "Benthic macroinvert.","Benthic microinvert.","Siderastrea stellata",
                             "Zooplankton", "Phytoplankton" ,"Digenea simplex" , "Algal turf" ))

#### Save file in Processed data folder 

write.csv(a,file  = "data/processed/processed_data_temperature_range_species.csv", 
          row.names = F)



