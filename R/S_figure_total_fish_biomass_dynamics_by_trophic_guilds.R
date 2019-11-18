# Script to do Figure :  total fish biomass dynamics under climate chenge scenarios by trophic guilds
######Load packages#######
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(tibble)
##### Load data for plotting####
c <- read_csv("data/processed/processed_data_figure_biomass_dynamics_trophic_guilds.csv", 
              col_types = cols())
#### Convert scenario and trophic group as factor ####
c$scenario <- factor(c$scenario,
                     levels = c("Status quo", "RCP 2.6","RCP 4.5",
                                "RCP 8.5"))
c$trophic_group = factor(c$trophic_group, 
                              levels=c("Reef sharks", "Generalist predators",
                                       "Invertivores","Herbivores/Detritivores"))
###Plot ######

p <- ggplot(data= c) + aes(x = year.group) + 
   geom_ribbon( aes(ymin = tbmin , ymax = tbmax, fill = scenario), 
                alpha = 0.4, linetype = 0) + # intervalos de confiança
   scale_fill_manual(values = c("white","Status quo" = "seashell3", "RCP 2.6" = "paleturquoise",
                                "RCP 4.5"= "khaki1", "RCP 8.5" = "lightsalmon1"), 
                     name = "Scenarios:", 
                     labels = c("Status quo", "RCP 2.6","RCP 4.5","RCP 8.5")) +
   geom_line(aes(y = tbmean, colour = scenario, linetype = scenario), size = 1) +
   scale_linetype_manual(values = c("Status quo" = "solid", "RCP 2.6" = "twodash","RCP 4.5" = "dotted",
                                    "RCP 8.5" = "dotdash"), name = "Scenarios:",
                         labels = c("Status quo","RCP 2.6","RCP 4.5","RCP 8.5")) +
   # geom_point(data=prova1, aes(y = observed), size=0.5, na.rm = T) +
   scale_colour_manual(values = c("Status quo" = "black", 
                                  "RCP 2.6" = "blue", 
                                  "RCP 4.5" = "orange3", 
                                  "RCP 8.5" = "red"), name = "Scenarios:",
                       labels = c("Status quo", "RCP 2.6","RCP 4.5","RCP 8.5")) +
   facet_wrap(~ trophic_group, scales = "free")+
   labs(x="Year", y = expression(Total~fish~biomass~""~(g~m^{-2}))) +
   theme(text = element_text(family = "Times New Roman"),
         aspect.ratio = .5,
         axis.text.x = element_text(face = "plain", color = "black", size = 9, angle = 0),
         axis.text.y = element_text(face = "plain", color = "black",size = 11, angle = 0),
         axis.title.y = element_text(face = "plain", color = "black",size = 13),
         axis.title.x = element_text(face = "plain", color = "black",size = 13),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text( size = 12, color = "black", face = "bold"),
         legend.title = element_text(color = "black", size = 10, face= "bold"),
         legend.text = element_text(color = "black", size = 10, face= "plain"),
         legend.position = "top",
         legend.key.size = unit(0.75,"line"),
         panel.border = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_x_continuous(breaks = c(2012,2020,2030,2040,2050,2060,2070,2080,2090,2100)) 
p
#### Before run this part, load the function: F_modify_facet_scales.R ------
p <- p +
   facet_wrap_custom(~trophic_group, scales = "free_y", ncol = 2,nrow = 2, scale_overrides = list(
      scale_override(1, scale_y_continuous(limits  = c(0, 6), breaks = seq(0,6,1.5))),
      scale_override(2, scale_y_continuous(limits  = c(0, 7.5), breaks = seq(0,7.5,1.5))),
      scale_override(3, scale_y_continuous(limits  = c(0, 10), breaks = seq(0,10,2))),
      scale_override(4, scale_y_continuous(limits  = c(0, 15), breaks = seq(0,15,2.5)))
   ))

p

#### Saving Figure  ######
ggsave(p, file = "outputs_results/figures/total_fish_biomass_dynamics_by_trophic_guilds.png",
       units = "mm",
       width = 233, dpi = 400)



