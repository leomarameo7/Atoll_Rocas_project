#script made for plotting Figure of box plot , year 2100, Y axis= biomass ,
#X axis= Climate change scenarios
######Load packages######
library(readr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(tidyverse)
#####Read data######
#Read data from "processed" folder, scenario 2100 and for fish species 
d <- read_csv("data/processed/processed_data_figure_box-plot_year_2100_fish.csv", 
              col_types = cols())
#####Declare and re-order factor of variables species and scenario#####
d$species <- factor(d$species,
    levels = c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", 
             "Cephalopholis fulva", "Carangidae","Acanthurus spp.", 
             "Stegastes rocasensis","Thalassoma noronhanum",
             "Abudefduf saxatilis", "Sparisoma spp.", "Melichthys niger", 
             "Kyphosus spp.", "Mulloidichthys martinicus",
             "Holocentrus adscensionis","Haemulidae", "Cryptobenthic reef fishes"))
d$scenario <- factor(d$scenario,
      levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

#####Data frame with median values for each fish species ######
#Made data-frame with median of the Status quo scenario 
median_statusquo <- d %>%
   filter(scenario == "Status quo") %>%
   group_by(scenario,year, species) %>%
   summarise_all(funs(mean,median,sd, max, min))
#####Declare and re-order factor of scenario variable#####
median_statusquo$species <- factor(median_statusquo$species,
                                   levels = c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", 
                                            "Cephalopholis fulva", "Carangidae","Acanthurus spp.", 
                                            "Stegastes rocasensis", "Thalassoma noronhanum",
                                            "Abudefduf saxatilis", "Sparisoma spp.", "Melichthys niger", 
                                            "Kyphosus spp.", "Mulloidichthys martinicus", 
                                            "Holocentrus adscensionis",
                                            "Haemulidae", "Cryptobenthic reef fishes"))
##### Plot Figure 3 ####
p <- ggplot(d, aes(x = scenario, y = biomass, fill = scenario, facets = species)) + 
   geom_boxplot(aes(x = as.factor(scenario), y = as.numeric(biomass), fill = scenario),
                width = 0.35, fatten = 2.5,  notch = F, alpha = 0.75, outlier.shape = NA) +
   facet_wrap(~ species, scales = "free_y") +
   geom_hline(data = median_statusquo, aes(yintercept = median_statusquo$median), 
              colour = "red", lty = "longdash", lwd = 0.35) +
   theme_bw() + 
   labs(x = "Scenarios", y = expression(Biomass~""~(g~m^{-2}))) +
   theme(text = element_text(family = "Times New Roman"),
         legend.position = "none",
         strip.text.x = element_text(size = 10, color = "black", face = "bold.italic"),
         axis.text.x = element_text(size = 10,  color = "black"),
         axis.title.x = element_text(size = 15),
         axis.text.y  = element_text(size = 11,  color = "black"),
         axis.title.y = element_text(size = 15),
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_fill_brewer(palette = "RdBu", direction = -1)

#Before to run the code below, remember to run the function contained in the script "function_modify_facet_scale" 
p <- p +
   facet_wrap_custom(~species, scales = "free_y", ncol = 4,nrow = 4, scale_overrides = list(
      scale_override(1, scale_y_continuous(limits  = c(0, 0.60), breaks = seq(0,.60,0.15))),
      scale_override(2, scale_y_continuous(limits  = c(0, 12), breaks = seq(0,12,1))),
      scale_override(3, scale_y_continuous(limits  = c(0, 2.05), breaks = seq(0,2.05,0.5))),
      scale_override(4, scale_y_continuous(limits  = c(0, 0.75), breaks = seq(0,0.75,0.25))),
      scale_override(7, scale_y_continuous(limits  = c(0, 0.8), breaks = seq(0,0.8,0.2))),
      scale_override(9, scale_y_continuous(limits  = c(0, 1.5), breaks = seq(0,1.5,.25))),
      scale_override(2, scale_y_continuous(limits  = c(1, 3.5))),
      scale_override(13, scale_y_continuous(limits  = c(0, 2.5), breaks = seq(0,2.5,.5))),
      scale_override(15, scale_y_continuous(limits  = c(0, 1.75), breaks = seq(0,1.75,.5)))
      
   ))
p
#### Saving Figure 3####
ggsave(filename = "boxplot_fish_2100.png", plot = p, path = "outputs_results/supp/",
       width = 18, device = "png", height = 13, units = 'in', dpi = 400)
