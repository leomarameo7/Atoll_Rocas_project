###Load packages####
library(ggplot2)
library(tidyverse)
library(reshape2)
library(readr)
### Load data #####
#a <- read_csv("data/raw/statusquo_simulations.csv", col_types = cols())
b <- as.data.frame(read_csv("data/processed/fitted_observed.csv", 
                            col_types = cols()))

#Drop row 8 and column 2
#a <- a[-8,]
#a <- a[,-2]
#### Stacks a set of columns into a single column of data,#####
# mantening column "year"
#mdata <- melt(a, id = "Year")
# Add fish times series values to mdata objet in a column named observed ####
#mdata$observed <- c(1.8,2.839398,3.555703,5.886178,4.704254,5.445588,3.409005,
 #                  2.22, 2.027739, 2.075025, 1.521285, 1.394551, 1.499202, 1.657619,
 #                  0.20, 0.4249130, 0.6680616, 0.7156951, 0.5769806, 0.4089686, 0.4026906,
  #                 2.12, 2.742664,7.164319,9.149915,9.723755,5.161921,4.311664,
  #                 9.86,9.786333,10.255217,7.805351,8.766668,8.356342,9.431128,
  #                 0.46, 0.4969154,0.5224870,0.3489655,0.2020732,0.2800376,0.2871034,
   #                0.20, 0.16407090, 0.15491803, 0.08046434, 0.07363402, 0.18210369, 0.22643443,
    #               0.990, 0.8824236 ,0.7432853 ,0.5155734, 1.2228339, 1.7175860, 2.4207161,
                 #  1.13 ,1.925271 ,1.844874, 1.808154 ,1.269728 ,1.199768, 1.379307,
                  # 0.27 ,0.4317996 ,0.3857143, 0.7480523, 0.9524308 ,1.0823373, 0.9447495,
                  # 0.416, 2.0951025, 3.0691033, 3.1014653, 1.4331506, 0.6825460, 0.6816634,
                  # 2.118, 4.292554, 5.240494, 4.923633, 3.816866 ,2.776960, 3.362320,
                  # 0.962, 0.6477912, 1.2264773, 1.6746996, 1.7857878, 1.3956905, 1.3872951,
                  # 0.79, 0.6154048, 0.3660933, 0.2143545, 0.2055187, 0.3238366, 0.3803063)
#### Rename columns ####

#colnames(mdata)<- c("year","species","biomass","observed_biomass")
### Convert columns values into integer or factor types
#mdata$year <- as.integer(mdata$year)
#mdata$species <- factor(mdata$species,
                       # levels = c("Ginglymostoma cirratum","Lutjanus jocu", "Cephalopholis fulva",
                                # "Carangidae","Acanthurus spp.","Stegastes rocasensis","Thalassoma noronhanum",
                                # "Abudefduf saxatilis", "Sparisoma spp.","Melichthys niger", 
                                # "Mulloidichthys martinicus", "Holocentrus adscensionis","Haemulidae",
                                # "Cryptobenthic reef fishes"))

b$species <- factor(b$species,
                        levels = c("Ginglymostoma cirratum","Lutjanus jocu", "Cephalopholis fulva",
                                   "Carangidae","Acanthurus spp.","Stegastes rocasensis","Thalassoma noronhanum",
                                   "Abudefduf saxatilis", "Sparisoma spp.","Melichthys niger", 
                                   "Mulloidichthys martinicus", "Holocentrus adscensionis","Haemulidae",
                                   "Cryptobenthic reef fishes"))

### Made plot ####
o <- ggplot(data= b) + 
   aes(x = year) + 
   geom_line(aes(y = relative_fitted ), size = 1) +
   geom_point(aes(y = relative_observed),size=1.5) +
   facet_wrap( ~ species, scales = "free") +
   theme(text=element_text(family = "Times New Roman"),
         strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
         strip.background = element_rect(fill = "white"),
         axis.text.x = element_text(face = "plain", color = "black", size = 12, angle = 0),
         axis.text.y = element_text(face = "plain", color = "black",size= 14, angle = 0),
         axis.title.y = element_text(face = "plain", color = "black",size= 18),
         axis.title.x = element_text(face = "plain", color = "black",size = 18),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(colour = "black", fill = NA, size=0.25),
         axis.line = element_line(colour = "black")) +
   scale_x_continuous(breaks = c(2012,2013,2014,2015,2016,2017,2018))+
    labs(x="Year", y = "Relative biomass")
o

#Before to run the code below, remember to run the function contained in the script "function_modify_facet_scale" 
o <- o +
   facet_wrap_custom(~species, scales = "free", ncol = 4,nrow=4, scale_overrides = list(
      scale_override(1, scale_y_continuous(limits  = c(0.5, 4.25), breaks=seq(0.5,4.25,1.25))),
      scale_override(2, scale_y_continuous(limits  = c(1.5, 2.5), breaks=seq(1.5,2.5,0.25))),
      scale_override(3, scale_y_continuous(limits  = c(0.1, 0.8), breaks=seq(0.1,0.8,0.15))),
      scale_override(4, scale_y_continuous(limits  = c(1, 7), breaks=seq(1,7,1.5))),
      scale_override(5, scale_y_continuous(limits  = c(7, 11), breaks=seq(7,11,1))),
      scale_override(6, scale_y_continuous(limits  = c(0.1, 0.7), breaks=seq(0.1,0.7,0.1))),
      scale_override(7, scale_y_continuous(limits  = c(0, 0.3), breaks=seq(0,0.3,0.1))),
      scale_override(8, scale_y_continuous(limits  = c(0.25, 2.5), breaks=seq(0.25,2.5,0.5))),
      scale_override(9, scale_y_continuous(limits  = c(1, 2), breaks = seq(1,2,0.25))),
      scale_override(10, scale_y_continuous(limits  = c(0.1,1.25), breaks = seq(0.1,1.25,0.25))),
      scale_override(11, scale_y_continuous(limits  = c(0.25, 3.25), breaks = seq(0.25,3.25,0.75))),
      scale_override(12, scale_y_continuous(limits  = c(1.5, 4.5), breaks = seq(1.5,4.5,0.5))),
      scale_override(13, scale_y_continuous(limits  = c(0.25, 1.75), breaks = seq(0.25,1.75,0.5))),
      scale_override(14, scale_y_continuous(limits  = c(0.1, 1.25), breaks = seq(0.1,1.25,0.25)))
      
   ))
o 
#### Saving plot#####

ggsave(filename = "time_series_fish_obeserved_vs_expected_modify.png", plot = o,
       path = "outputs_results/supplementary_materials/",
       width = 18, device = "png", height = 13, units = 'in', dpi = 400)

