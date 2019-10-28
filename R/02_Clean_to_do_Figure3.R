#Call packages
library(readr)
library (tidyverse)
library(plyr)
#read raw data
c <- read_csv("data/raw/raw_trials.csv")
# colapse columns trials in only one named "biomass" and preserving the others
c <- gather(c, "trials", "biomass", 3:252)
#duplicate first column, and rename it, id2
c$id2 <- c$.id
#rename levels of column "id2"
c <- mutate(c, id2 = revalue(id2, c("X1_RCP2" = "Sea birds",
                                    "X1_RCP4" = "Sea birds",
                                    "X1_RCP8" = "Sea birds",
                                    "X1_baseline" = "Sea birds",
                                    
                                    "X3_RCP2" = "Ginglymostoma cirratum",
                                    "X3_RCP4" = "Ginglymostoma cirratum",
                                    "X3_RCP8" = "Ginglymostoma cirratum",
                                    "X3_baseline" = "Ginglymostoma cirratum",
                                    
                                    "X2_RCP8" = "Negaprion brevirostris",
                                    "X2_RCP4" = "Negaprion brevirostris",
                                    "X2_RCP2" = "Negaprion brevirostris",
                                    "X2_baseline" = "Negaprion brevirostris",
                                    
                                    "X4_baseline" = "Lutjanus jocu",
                                    "X4_RCP2" = "Lutjanus jocu",
                                    "X4_RCP4" = "Lutjanus jocu",
                                    "X4_RCP8" = "Lutjanus jocu",
                                    
                                    "X5_RCP2" = "Cephalopholis fulva",
                                    "X5_RCP4" = "Cephalopholis fulva",
                                    "X5_RCP8" = "Cephalopholis fulva",
                                    "X5_baseline" = "Cephalopholis fulva",
                                    
                                    "X6_baseline" = "Carangidae",
                                    "X6_RCP2" = "Carangidae",
                                    "X6_RCP4" = "Carangidae",
                                    "X6_RCP8" = "Carangidae",
                                    
                                    "X7_baseline" = "Acanthurus spp.",
                                    "X7_RCP2" = "Acanthurus spp.",
                                    "X7_RCP4" = "Acanthurus spp.",
                                    "X7_RCP8" = "Acanthurus spp.",
                                    
                                    "X8_baseline" = "Stegastes rocasensis",
                                    "X8_RCP2" = "Stegastes rocasensis",
                                    "X8_RCP4" = "Stegastes rocasensis",
                                    "X8_RCP8" = "Stegastes rocasensis",
                                    
                                    "X9_baseline" = "Thalassoma noronhanum",
                                    "X9_RCP2" = "Thalassoma noronhanum",
                                    "X9_RCP4" = "Thalassoma noronhanum",
                                    "X9_RCP8" = "Thalassoma noronhanum",
                                    
                                    "X10_baseline" = "Abudefduf saxatilis",
                                    "X10_RCP2" = "Abudefduf saxatilis",
                                    "X10_RCP4" = "Abudefduf saxatilis",
                                    "X10_RCP8" = "Abudefduf saxatilis",
                                    
                                    "X11_baseline" = "Sparisoma spp.",
                                    "X11_RCP2" = "Sparisoma spp.",
                                    "X11_RCP4" = "Sparisoma spp.",
                                    "X11_RCP8" = "Sparisoma spp.",
                                    
                                    "X12_baseline" = "Melichthys niger",
                                    "X12_RCP2" = "Melichthys niger",
                                    "X12_RCP4" = "Melichthys niger",
                                    "X12_RCP8" = "Melichthys niger",
                                    
                                    "X13_baseline" = "Kyphosus spp.",
                                    "X13_RCP2" = "Kyphosus spp.",
                                    "X13_RCP4" = "Kyphosus spp.",
                                    "X13_RCP8" = "Kyphosus spp.",
                                    
                                    "X14_baseline" = "Mulloidichthys martinicus",
                                    "X14_RCP2" = "Mulloidichthys martinicus",
                                    "X14_RCP4" = "Mulloidichthys martinicus",
                                    "X14_RCP8" = "Mulloidichthys martinicus",
                                    
                                    "X15_baseline" = "Holocentrus adscensionis",
                                    "X15_RCP2" = "Holocentrus adscensionis",
                                    "X15_RCP4" = "Holocentrus adscensionis",
                                    "X15_RCP8" = "Holocentrus adscensionis",
                                    
                                    "X16_baseline" = "Haemulidae",
                                    "X16_RCP2" = "Haemulidae",
                                    "X16_RCP4" = "Haemulidae",
                                    "X16_RCP8" = "Haemulidae",
                                    
                                    "X17_baseline" = "Cryptobenthic reef fishes",
                                    "X17_RCP2" = "Cryptobenthic reef fishes",
                                    "X17_RCP4" = "Cryptobenthic reef fishes",
                                    "X17_RCP8" = "Cryptobenthic reef fishes",
                                    
                                    "X18_baseline" = "Turtles",
                                    "X18_RCP2" = "Turtles",
                                    "X18_RCP4" = "Turtles",
                                    "X18_RCP8" = "Turtles",
                                    
                                    "X19_baseline" = "Cephalopoda",
                                    "X19_RCP2" = "Cephalopoda",
                                    "X19_RCP4" = "Cephalopoda",
                                    "X19_RCP8" = "Cephalopoda",
                                    
                                    "X20_baseline" = "Panulirus spp.",
                                    "X20_RCP2" = "Panulirus spp.",
                                    "X20_RCP4" = "Panulirus spp.",
                                    "X20_RCP8" = "Panulirus spp.",
                                    
                                    "X21_baseline" = "Benthic macroinvertebrates",
                                    "X21_RCP2" = "Benthic macroinvertebrates",
                                    "X21_RCP4" = "Benthic macroinvertebrates",
                                    "X21_RCP8" = "Benthic macroinvertebrates",
                                    
                                    "X22_baseline" = "Benthic microinvertebrates",
                                    "X22_RCP2" = "Benthic microinvertebrates",
                                    "X22_RCP4" = "Benthic microinvertebrates",
                                    "X22_RCP8" = "Benthic microinvertebrates",
                                    
                                    "X23_baseline" = "Siderastrea stellata",
                                    "X23_RCP2" = "Siderastrea stellata",
                                    "X23_RCP4" = "Siderastrea stellata",
                                    "X23_RCP8" = "Siderastrea stellata",
                                    
                                    "X24_baseline" = "Zooplankton",
                                    "X24_RCP2" = "Zooplankton",
                                    "X24_RCP4" = "Zooplankton",
                                    "X24_RCP8" = "Zooplankton",
                                    
                                    "X25_baseline" = "Phytoplankton",
                                    "X25_RCP2" = "Phytoplankton",
                                    "X25_RCP4" = "Phytoplankton",
                                    "X25_RCP8" = "Phytoplankton",
                                    
                                    "X26_baseline" = "Digenea simplex",
                                    "X26_RCP2" = "Digenea simplex",
                                    "X26_RCP4" = "Digenea simplex",
                                    "X26_RCP8" = "Digenea simplex",
                                    
                                    "X27_baseline" = "Other algal turf",
                                    "X27_RCP2" = "Other algal turf",
                                    "X27_RCP4" = "Other algal turf",
                                    "X27_RCP8" = "Other algal turf")))

c <- mutate(c, .id = revalue(.id, c("X1_RCP2" = "RCP 2.6",
                                    "X1_RCP4" = "RCP 4.5",
                                    "X1_RCP8" = "RCP 8.5",
                                    "X1_baseline" = "Status quo",
                                    
                                    "X2_RCP2" = "RCP 2.6",
                                    "X2_RCP4" = "RCP 4.5",
                                    "X2_RCP8" = "RCP 8.5",
                                    "X2_baseline" = "Status quo",
                                    
                                    "X3_RCP2" = "RCP 2.6",
                                    "X3_RCP4" = "RCP 4.5",
                                    "X3_RCP8" = "RCP 8.5",
                                    "X3_baseline" = "Status quo",
                                    
                                    "X4_RCP2" = "RCP 2.6",
                                    "X4_RCP4" = "RCP 4.5",
                                    "X4_RCP8" = "RCP 8.5",
                                    "X4_baseline" = "Status quo",
                                    
                                    "X5_RCP2" = "RCP 2.6",
                                    "X5_RCP4" = "RCP 4.5",
                                    "X5_RCP8" = "RCP 8.5",
                                    "X5_baseline" = "Status quo",
                                    
                                    "X6_RCP2" = "RCP 2.6",
                                    "X6_RCP4" = "RCP 4.5",
                                    "X6_RCP8" = "RCP 8.5",
                                    "X6_baseline" = "Status quo",
                                    
                                    "X7_RCP2" = "RCP 2.6",
                                    "X7_RCP4" = "RCP 4.5",
                                    "X7_RCP8" = "RCP 8.5",
                                    "X7_baseline" = "Status quo",
                                    
                                    "X8_RCP2" = "RCP 2.6",
                                    "X8_RCP4" = "RCP 4.5",
                                    "X8_RCP8" = "RCP 8.5",
                                    "X8_baseline" = "Status quo",
                                    
                                    "X9_RCP2" = "RCP 2.6",
                                    "X9_RCP4" = "RCP 4.5",
                                    "X9_RCP8" = "RCP 8.5",
                                    "X9_baseline" = "Status quo",
                                    
                                    "X10_RCP2" = "RCP 2.6",
                                    "X10_RCP4" = "RCP 4.5",
                                    "X10_RCP8" = "RCP 8.5",
                                    "X10_baseline" = "Status quo",
                                    
                                    "X11_RCP2" = "RCP 2.6",
                                    "X11_RCP4" = "RCP 4.5",
                                    "X11_RCP8" = "RCP 8.5",
                                    "X11_baseline" = "Status quo",
                                    
                                    "X12_RCP2" = "RCP 2.6",
                                    "X12_RCP4" = "RCP 4.5",
                                    "X12_RCP8" = "RCP 8.5",
                                    "X12_baseline" = "Status quo",
                                    
                                    "X13_RCP2" = "RCP 2.6",
                                    "X13_RCP4" = "RCP 4.5",
                                    "X13_RCP8" = "RCP 8.5",
                                    "X13_baseline" = "Status quo",
                                    "X14_RCP2" = "RCP 2.6",
                                    "X14_RCP4" = "RCP 4.5",
                                    "X14_RCP8" = "RCP 8.5",
                                    "X14_baseline" = "Status quo",
                                    "X15_RCP2" = "RCP 2.6",
                                    "X15_RCP4" = "RCP 4.5",
                                    "X15_RCP8" = "RCP 8.5",
                                    "X15_baseline" = "Status quo",
                                    "X16_RCP2" = "RCP 2.6",
                                    "X16_RCP4" = "RCP 4.5",
                                    "X16_RCP8" = "RCP 8.5",
                                    "X16_baseline" = "Status quo",
                                    "X17_RCP2" = "RCP 2.6",
                                    "X17_RCP4" = "RCP 4.5",
                                    "X17_RCP8" = "RCP 8.5",
                                    "X17_baseline" = "Status quo",
                                    "X18_RCP2" = "RCP 2.6",
                                    "X18_RCP4" = "RCP 4.5",
                                    "X18_RCP8" = "RCP 8.5",
                                    "X18_baseline" = "Status quo",
                                    "X19_RCP2" = "RCP 2.6",
                                    "X19_RCP4" = "RCP 4.5",
                                    "X19_RCP8" = "RCP 8.5",
                                    "X19_baseline" = "Status quo",
                                    "X20_RCP2" = "RCP 2.6",
                                    "X20_RCP4" = "RCP 4.5",
                                    "X20_RCP8" = "RCP 8.5",
                                    "X20_baseline" = "Status quo",
                                    "X21_RCP2" = "RCP 2.6",
                                    "X21_RCP4" = "RCP 4.5",
                                    "X21_RCP8" = "RCP 8.5",
                                    "X21_baseline" = "Status quo",
                                    "X22_RCP2" = "RCP 2.6",
                                    "X22_RCP4" = "RCP 4.5",
                                    "X22_RCP8" = "RCP 8.5",
                                    "X22_baseline" = "Status quo",
                                    "X23_RCP2" = "RCP 2.6",
                                    "X23_RCP4" = "RCP 4.5",
                                    "X23_RCP8" = "RCP 8.5",
                                    "X23_baseline" = "Status quo",
                                    "X24_RCP2" = "RCP 2.6",
                                    "X24_RCP4" = "RCP 4.5",
                                    "X24_RCP8" = "RCP 8.5",
                                    "X24_baseline" = "Status quo",
                                    "X25_RCP2" = "RCP 2.6",
                                    "X25_RCP4" = "RCP 4.5",
                                    "X25_RCP8" = "RCP 8.5",
                                    "X25_baseline" = "Status quo",
                                    "X26_RCP2" = "RCP 2.6",
                                    "X26_RCP4" = "RCP 4.5",
                                    "X26_RCP8" = "RCP 8.5",
                                    "X26_baseline" = "Status quo",
                                    "X27_RCP2" = "RCP 2.6",
                                    "X27_RCP4" = "RCP 4.5",
                                    "X27_RCP8" = "RCP 8.5",
                                    "X27_baseline" = "Status quo")))
#rename column 1, 2,4
names(c)[2] <- "year"
names(c)[1] <- "scenario"
names(c)[5] <- "species"
#remove column 3
c <- c[-3]

#create object with species that I want to filter
fish <- c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", 
          "Cephalopholis fulva", "Carangidae","Acanthurus spp.", 
          "Stegastes rocasensis", "Thalassoma noronhanum",
          "Abudefduf saxatilis", "Sparisoma spp.", "Melichthys niger", 
          "Kyphosus spp.", "Mulloidichthys martinicus", 
          "Holocentrus adscensionis","Haemulidae", "Cryptobenthic reef fishes")

nofish<- c("Sea birds","Turtles","Cephalopoda","Panulirus spp.","Benthic macroinvertebrates", "Benthic microinvertebrates", 
           "Siderastrea stellata","Zooplankton",
           "Phytoplankton","Digenea simplex","Other algal turf")
#filter by species and year
d <- c %>% filter(species %in% fish & year == 2100)
f <- c %>% filter(species %in% nofish & year == 2100)
g <- c %>% filter(species %in% fish & year == 2050)
h <- c %>% filter(species %in% nofish & year == 2050)
#save the file 
write.csv(d,file  = "data/processed/processed_data_figure_box-plot_year_2100_fish.csv", row.names = F)
write.csv(f, "data/processed/processed_data_figure_box-plot_year_2100_nofish.csv", row.names = F)
write.csv(g,"data/processed/processed_data_figure_box-plot_year_2050_fish.csv", row.names = F)
write.csv(h, "data/processed/processed_data_figure_box-plot_year_2050_nofish.csv", row.names = F)