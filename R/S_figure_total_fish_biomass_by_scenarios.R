# Script to do Figure total fish biomass dynamics under climate change scenarios 
######Load packages#######
library(readr)
library(ggplot2)
library(tidyverse)
library(tibble)
##### Load data for plotting####
b <- read_csv("data/processed/processed_data_figure_2.csv", col_types = cols())
#### Convert scenario as factor ####
b$scenario <- factor(b$scenario,
                     levels = c("Status quo", "RCP 2.6","RCP 4.5",
                                "RCP 8.5"))
#####Plot Figure #####
p <- ggplot(data= b) + aes(x = year.group) + 
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
   facet_wrap(~scenario, scales='free_y') +
   labs(x="Year", y = expression(Total~fish~biomass~""~(g~m^{-2}))) +
   #y= expression(Biomass~""~(g~m^{-2}))
   theme(text = element_text(family = "Times New Roman"),
         aspect.ratio = .5,
         strip.text.x = element_text(size = 12, color = "black", 
                                     face = "bold"),
         axis.text.x = element_text(face = "plain", color = "black", size = 10, angle = 0),
         axis.text.y = element_text(face = "plain", color = "black",size = 11, angle = 0),
         axis.title.y = element_text(face = "plain", color = "black",size = 14),
         axis.title.x = element_text(face = "plain", color = "black",size = 14),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title = element_text(color = "black", size = 13, face= "bold"),
         legend.text = element_text(color = "black", size = 12, face= "plain"),
         legend.position = "top",
         panel.border = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_x_continuous(breaks = c(2012,2020,2030,2040,2050,2060,2070,2080,2090,2100)) +
   scale_y_continuous(position = "left", limits = c(0,34), breaks = seq(0,35,5))
p

#Before to run the code below, remember to run the function contained in the script "function_modify_facet_scale" 
p <- p +
   facet_wrap_custom(~scenario, scales = "free_y", ncol = 2,nrow = 2, scale_overrides = list(
      scale_override(1, scale_y_continuous(limits  = c(10, 35), breaks = seq(10,35,5))),
      scale_override(2, scale_y_continuous(limits  = c(10, 35), breaks = seq(10,35,5))),
      scale_override(3, scale_y_continuous(limits  = c(10, 35), breaks = seq(10,35,5)))
   ))
p



#### Saving Figure 2 ######
ggsave(filename = "total_fish_biomass_dynamics_by_scenario.png", plot = p, 
       path = "outputs_results/figures/", width = 9, 
       height = 6, units = 'in', dpi = 400)
