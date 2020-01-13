#Script to do supplementary figure: fish species biomass dynamics by scenario
####Load packages---------------------------
library(ggplot2)
library(readr)
library(tidyverse)
#### Load data fish biomass temporal dynamics----------
a <- read_csv("data/processed/processed_data_figure_fish_species__biomass_dynamics.csv", 
              col_types = cols())
### Rename species#### 
a <- a %>%
   mutate(species = recode(species, X2 = "Negaprion brevirostris",
                           X3 = "Ginglymostoma cirratum", X4 = "Lutjanus jocu", X5 ="Cephalopholis fulva",
                           X6 = "Carangidae",X7 = "Acanthurus spp.",X8 = "Stegastes rocasensis", 
                           X9 = "Thalassoma noronhanum",X10 = "Abudefduf saxatilis",X11="Sparisoma spp.", 
                           X12 ="Melichthys niger",X13= "Kyphosus spp.", 
                           X14 = "Mulloidichthys martinicus",X15 = "Holocentrus adscensionis", 
                           X16 = "Haemulidae",X17= "Cryptobenthic reef fishes"))

#####Declare and re-order factor of variables species and scenario#####
a$species <- factor(a$species,
                    levels = c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", 
                               "Cephalopholis fulva", "Carangidae","Acanthurus spp.", 
                               "Stegastes rocasensis","Thalassoma noronhanum",
                               "Abudefduf saxatilis", "Sparisoma spp.", "Melichthys niger", 
                               "Kyphosus spp.", "Mulloidichthys martinicus",
                               "Holocentrus adscensionis","Haemulidae", "Cryptobenthic reef fishes"))
a$scenario <- factor(a$scenario,
                     levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

#### Plot #####

p1 <- ggplot(data = a, aes(x = year.group)) + 
   geom_ribbon( aes(ymin = percent_05 , ymax = percent_95, fill = scenario), 
                alpha = 0.4, linetype = 0) + # intervalos de confiança
   scale_fill_manual(values = c("Status quo" = "seashell3", "RCP 2.6" = "paleturquoise",
                                "RCP 4.5"= "khaki1", "RCP 8.5" = "lightsalmon1"), 
                     name = "Scenarios:", 
                     labels = c("Status quo", "RCP 2.6","RCP 4.5","RCP 8.5")) +
   geom_line(aes(y = mean, colour = scenario, linetype = scenario), size = 0.6) +
   scale_linetype_manual(values = c("Status quo" = "solid", "RCP 2.6" = "twodash","RCP 4.5" = "dotted",
                                    "RCP 8.5" = "dotdash"), name = "Scenarios:",
                         labels = c("Status quo","RCP 2.6","RCP 4.5","RCP 8.5")) +
   # geom_point(data=prova1, aes(y = observed), size=0.5, na.rm = T) +
   scale_colour_manual(values = c("Status quo" = "black", 
                                  "RCP 2.6" = "blue", 
                                  "RCP 4.5" = "orange3", 
                                  "RCP 8.5" = "red"), name = "Scenarios:",
                       labels = c("Status quo", "RCP 2.6","RCP 4.5","RCP 8.5")) +
   facet_wrap(~ species, scales = "free_y") +
   labs(x="Year", y = expression(Biomass~""~(g~m^{-2}))) +
   theme(text = element_text(family = "Times New Roman"),
         aspect.ratio = 0.9,
         strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
         axis.text.x = element_text(face = "plain", color = "black", size = 10, angle=75, hjust = 1.05),
         axis.text.y = element_text(face = "plain", color = "black",size = 10, angle = 0),
         axis.title.y = element_text(face = "plain", color = "black",size = 12),
         axis.title.x = element_text(face = "plain", color = "black",size = 12),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title = element_text(color = "black", size = 12, face = "bold"),
         legend.text = element_text(color = "black", size = 12, face = "plain"),
         legend.key.size = unit(0.8,"line"),
         legend.position = "top",
         panel.border = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_x_continuous(breaks = c(2012,2020,2040,2060,2080,2100))

p1
#### Saving figure #####
ggsave(filename = "fish_biomass_dynamics_by_scenario.png", plot = p1, 
       path = "outputs_results/supplementary_materials/",
       width= 233, height= 250 ,device = "png", units = 'mm', dpi = 400)

