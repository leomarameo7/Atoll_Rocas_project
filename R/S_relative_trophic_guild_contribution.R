###This script allows to do the figure of trophic pyramid (relative contributions of fish biomass)
###Load packages####
library(readr)
library(ggplot2)
library(tidyverse)
##### Load data for plotting####
c <- as.data.frame(read_csv("data/processed/processed_data_figure_relative_contribution_trophic_guilds.csv", 
              col_types = cols()))
#Duplicate rows######
c1 <- rep(c, each = 2)

c1$factor <- rep(c(1,2), each = 1)

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

labels<- c()

#### Plot####
p <-  ggplot(c1, aes(x = trophic_group, y = rel.contribution, fill = trophic_group)) +   # Fill column
   geom_bar(stat = "identity", width = .75) +   # draw the bars
   coord_flip() +  # Flip axes
   facet_wrap(~ scenario, scales = "free") +
   theme_void() +
   theme(text = element_text(family = "Times New Roman"),
         strip.text.x = element_text(size = 12, color = "black", face = "bold"),
         strip.background = element_rect(fill="white"),
         #axis.line = element_line(colour = "black"), 
         legend.title = element_text(color = "black", size = 12, face = "bold"),
         legend.text = element_text(size = 12, face = "plain"),
         legend.position = "bottom",
         # plot.title = element_text(hjust= 0.5, vjust=2, size = 10 ,face = "bold"),
         legend.key.size = unit(0.95,"line"))+
   scale_fill_manual(values = c("Reef sharks"="seashell3",
                                "Generalist predators"="chocolate3",
                                "Invertivores"="darkgoldenrod1",
                                "Herbivores/Detritivores"="darkolivegreen3"),
                     name = "Trophic guilds:")

p

##### Saving figure######
ggsave(p, file = "outputs_results/figures/relative_trophic_guild_biomass_contribution_2100.png", units = "mm",
       width= 233, dpi = 400)
