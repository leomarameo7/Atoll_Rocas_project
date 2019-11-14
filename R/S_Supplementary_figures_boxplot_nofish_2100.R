######Load packages######
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
#####Read data######
#read data from "processed" folder, scenario 2100 and for NO fish species 
d <- read_csv("data/processed/processed_data_figure_box-plot_year_2100_nofish.csv")
#declare and re-order factor of variables species and scenario
d$species <- factor(d$species,
                    levels = c("Sea birds","Turtles","Cephalopoda","Panulirus spp.",
         "Benthic macroinvertebrates", "Benthic microinvertebrates", 
         "Siderastrea stellata","Zooplankton",
         "Phytoplankton","Digenea simplex","Other algal turf"))
d$scenario <- factor(d$scenario,
                     levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))


#####Data frame with median for each species ######
#Made data-frame with median of the Status quo scenario 
d <- d[,-3]
median_statusquo <- d %>%
   filter(scenario == "Status quo") %>%
   group_by(scenario,year,species) %>%
   summarise_all(funs(mean(biomass),median(biomass),sd(biomass), 
                      max(biomass), min(biomass)))

median_statusquo$species <- factor(median_statusquo$species,
                                   levels = c("Sea birds","Turtles","Cephalopoda",
            "Panulirus spp.","Benthic macroinvertebrates","Benthic microinvertebrates",
            "Siderastrea stellata","Zooplankton",
            "Phytoplankton","Digenea simplex","Other algal turf"))
##### boxplot for the 2100 year and NOfish species ####

p <- ggplot(d, aes(x = scenario, y = biomass, fill = scenario, facets = species)) + 
   geom_boxplot(aes(x = as.factor(scenario), y = as.numeric(biomass), fill = scenario),
                width = 0.35, fatten = 2.5,  notch = F,
                alpha = 0.75, outlier.shape = NA) +
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
p +
   facet_wrap_custom(~species, scales = "free_y", ncol = 4,nrow = 4, scale_overrides = list(
      scale_override(1, scale_y_continuous(limits  = c(0, 0.050), breaks = seq(0,.05,0.01))),
      
      scale_override(4, scale_y_continuous(limits  = c(0, 7.5), breaks = seq(0,0.75,1.5))),
      scale_override(5, scale_y_continuous(limits  = c(0, 25), breaks = seq(0,25,5))),
      scale_override(6, scale_y_continuous(limits  = c(0, 35), breaks = seq(0,35,5))),
      
      scale_override(7, scale_y_continuous(limits  = c(0, 0.95), breaks = seq(0,.95,.15))),
      scale_override(8, scale_y_continuous(limits  = c(0.1, 0.9), breaks = seq(0.1,.9,.2))),
      scale_override(9, scale_y_continuous(limits  = c(0.025, 0.15), breaks = seq(0.025,.15,0.025))),
      scale_override(10, scale_y_continuous(limits  = c(75, 250), breaks = seq(25,250,25)))
      
   ))
p
ggsave(filename = "boxplot_NOfish_2100.png", plot = p, path = "outputs_results/supp/",
       width = 18, device = "png", height = 13, units = 'in', dpi = 400)
