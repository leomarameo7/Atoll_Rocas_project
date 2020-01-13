###This script allows to do the figure of trophic pyramids (relative contributions of fish biomass) 
#comparing years 2012 and 2100
###Load packages####
library(readr)
library(ggplot2)
library(tidyverse)
##### Load data for plotting####
c <- as.data.frame(read_csv("data/processed/processed_data_figure_relative_contribution_trophic_guilds.csv", 
                            col_types = cols()))
#Duplicate rows######
c1 <- c %>% slice(rep(1:nrow(c), 2))
c1$factor <- rep(c(1,2), each = 32)

### change positive values of factor 1 in negative values ######
c1$rel.contribution2 <- with(c1, ifelse(factor == 1, 
                                        -rel.contribution, rel.contribution))
### Divide by 2 , the values of rel.contribution ######
c1$rel.contribution2 <- c1$rel.contribution2/2

###
c1$trophic_group = factor(c1$trophic_group, 
                          levels = c("Herbivores/Detritivores", "Invertivores", "Generalist predators",
                                     "Reef sharks"))
c1$scenario = factor(c1$scenario, 
                     levels = c("Status quo", "RCP 2.6",
                                "RCP 4.5","RCP 8.5"))
c1$factor =factor(c1$factor)


#### Plot####
p <-  ggplot(c1, aes(x = trophic_group, y = rel.contribution2, fill = trophic_group)) +   # Fill column
   geom_bar(stat = "identity", width = 1) +   # draw the bars
   geom_text(aes(y = -0.5, label = ifelse(rel.contribution2 < 0,'', paste0(rel.contribution2*2, "%"))),
             position = position_dodge(1), size = 5) +# Draw labels
   facet_grid(year.group ~ scenario, scales = "free") +
   coord_flip() + 
   theme( panel.background = element_rect(fill = 'white', colour = 'white'),
         text = element_text(family = "Times New Roman"),
         panel.spacing = unit(0.2, "lines"), 
         strip.text.x = element_text(size = 12, color = "black", 
                                     face = "bold", hjust = 0.1),
         strip.text.y = element_text(size = 12, color = "black", 
                                     face = "bold"),
         strip.background = element_rect(fill = "white"),
         #axis.line = element_line(colour = "black"), 
         legend.title = element_text(color = "black", size = 12, face = "bold"),
         legend.text = element_text(size = 12, face = "plain"),
         legend.position = "bottom",
         legend.box.margin = margin(t = 15),
         # plot.title = element_text(hjust= 0.5, vjust=2, size = 10 ,face = "bold"),
         legend.key.size = unit(0.95,"lines"),
         axis.text.y =element_blank(),
         axis.text.x =element_blank(),
         axis.title.y =element_blank(),
         axis.title.x =element_blank(),
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_blank()) +
   scale_fill_manual(values = c("Reef sharks" = "seashell3",
                                "Generalist predators" = "chocolate3",
                                "Invertivores" = "darkgoldenrod1",
                                "Herbivores/Detritivores" = "darkolivegreen3"),
                     name = "Trophic guilds:")

p

##### Saving figure######
ggsave(p, file = "outputs_results/figures/pyramids_biomass_comparison_2012_2100.png", units = "mm",
       width = 233, dpi = 400)
