##### Load packages -------
library(ggplot2)
library(readr)

####Load data --------
a_2100 <- read_csv("data/processed/processed_data_figure_percent_variation_year_2100_NOFISH.csv", 
                   col_types = cols())

#### List of species' numbers#####
numbers <- as.list.factor(   c(rep(1, each=3),rep(18, each=3),rep(19, each=3),rep(20, each=3),rep(21, each=3),
            rep(22, each=3),rep(23, each=3),rep(24, each=3),rep(25, each=3),rep(26, each=3),rep(27, each=3)))
#### add list to the data frame####
a_2100$numbers <- numbers
# constrain a_2100 to a data frame object
a_2100 <- as.data.frame(a_2100)
#####Re-order scenario, trophic group and species factors#####
a_2100$scenario <- factor(a_2100$scenario,
                          levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

a_2100$species <- factor(a_2100$species,
                         levels=c("Sea birds", "Turtles","Cephalopoda","Panulirus spp.","Benthic macroinvertebrates",
                                  "Benthic microinvertebrates","Siderastrea stellata", 
                                  "Zooplankton","Phytoplankton", "Digenea simplex", "Other algal turf" ))
### Drop scenario RCP 4.5 -------
# a_2100 <- subset(a_2100, scenario != "RCP 4.5")

#### Plot -------
p <-  ggplot(a_2100, 
             aes(x = species, y = mean_variation, label = numbers)) +
   geom_bar(aes(fill=species), width = 0.85, 
            position = position_dodge(0.75), stat="identity",colour = "black") +
   geom_text(aes(y = ifelse(mean_variation < 0, 8, -8), label = numbers),
             position = position_dodge(0.75), size = 4) +
   # move numbers to the right of the geom_error bar "hjust = -0.25", in the aes() of geom_text
   # geom_errorbar(aes(ymin = min_variation, ymax = max_variation),position= position_dodge(width=1),width = 0.1) +
   geom_hline(yintercept = 0) +
   facet_wrap(~scenario, scales = "free", ncol=1, strip.position = "right") +
   theme_bw() + 
   labs(y = "Relative average biomass change (%)") +
   theme(text = element_text(family = "Times New Roman"),
         panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         panel.spacing.y  = unit(1, "lines"),
         strip.text.x = element_text(size = 10, color = "black", face = "bold"),
         strip.text.y = element_text(size = 10, color = "black", face = "bold"),
         axis.line.x = element_line(colour = "white"), 
         axis.line.y = element_line(colour = "black"),
         axis.ticks.x = element_blank(),
         legend.title = element_text(color = "black", size = 14, face = "bold"),
         legend.text = element_text(size = 10, face = "plain"),
         legend.position = "bottom",
         # plot.title = element_text(hjust= 0.5, vjust=2, size = 10 ,face = "bold"),
         legend.key.size = unit(0.75,"line"),
         axis.text.x =  element_blank(),
         axis.text.y = element_text (size = 14,colour = "black",face = "plain" ),
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 16,colour = "black",face = "plain" )) +
   scale_fill_manual(values = c("Sea birds"="wheat","Turtles"="wheat",
   "Cephalopoda"="wheat", "Panulirus spp."="wheat","Benthic macroinvertebrates"="wheat",
   "Benthic microinvertebrates"="wheat","Siderastrea stellata"="wheat","Zooplankton"="wheat",
   "Phytoplankton"="wheat", "Digenea simplex"="wheat","Other algal turf"="wheat"),
   name = "Species:", 
   labels = c("1: Sea birds","18:Turtles", "19: Cephalopoda", "20: Panulirus spp.", "21: Benthic macroinvertebrates", 
            "22: Benthic microinvertebrates", "23: Siderastrea stellata", "24: Zooplankton", "25: Phytoplankton", 
            "26: Digenea simplex","27: Other algal turf")) +
   scale_y_continuous(position = "left",
         limits=c(-100,120), breaks=seq(-100,120,20)) 
p

#Before to run the code below, remember to run the function contained in the script "function_modify_facet_scale" 
p <- p +
   facet_wrap_custom(~scenario, scales = "free_y", ncol = 1, strip.position = "right", scale_overrides = list(
      scale_override(1, scale_y_continuous(limits  = c(-40, 20), breaks = seq(-40,20,10))),
      scale_override(2, scale_y_continuous(limits  = c(-100, 40), breaks = seq(-100,40,20))),
      scale_override(3, scale_y_continuous(limits  = c(-100, 120),  breaks=seq(-100,120,30)))
   ))
p  

#### Saving  plot

ggsave(p, 
   file = "outputs_results/supplementary_materials/percent_variation_NOFISH_2100.png", 
   units = "mm",
   width= 233,
   height = 174,
   dpi = 400)
