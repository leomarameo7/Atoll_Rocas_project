##### Load packages -------
library(ggplot2)
library(readr)

####Load data --------
a_2100 <- read_csv("data/processed/processed_data_figure_percent_variation_year_2100.csv", 
                   col_types = cols())
#####Re-order scenario, trophic group and species factors#####
a_2100$scenario <- factor(a_2100$scenario,
                     levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

a_2100$trophic_group = factor(a_2100$trophic_group, levels=c("Reef sharks", "Generalist predators","Invertivores",
                                             "Herbivores/Detritivores"))   
a_2100$species <- factor(a_2100$species,
         levels=c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", "Cephalopholis fulva",
          "Carangidae","Acanthurus spp.","Stegastes rocasensis","Thalassoma noronhanum",
          "Abudefduf saxatilis", "Sparisoma spp.","Melichthys niger","Kyphosus spp.", 
         "Mulloidichthys martinicus", "Holocentrus adscensionis","Haemulidae",
            "Cryptobenthic reef fishes"))
### Drop scenario RCP 4.5 -------
# a_2100 <- subset(a_2100, scenario != "RCP 4.5")

#### Plot -------

p <-  ggplot(a_2100, 
             aes(x = species, y = mean_variation, label = numbers, fill = species)) +
   geom_bar(aes(fill =  species), width = 0.9, 
            position = position_dodge(1),stat="identity",colour = "black") +
   geom_text(aes(y = ifelse(mean_variation < 0, 8, -8), label = numbers),
             position = position_dodge(1), size = 3) +
   # move numbers to the right of the geom_error bar "hjust = -0.25", in the aes() of geom_text
   # geom_errorbar(aes(ymin = min_variation, ymax = max_variation),position= position_dodge(width=1),width = 0.1) +
   geom_hline(yintercept = 0) +
   facet_grid(scenario ~ trophic_group, scales = "free", space = "free", switch = "x") +
   theme_bw() + 
   labs(y = "Relative average biomass change (%)") +
   theme(text = element_text(family = "Times New Roman"),
         panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         panel.spacing.y  = unit(2.25, "lines"),
         panel.spacing.x  = unit(1.5, "lines"),
         strip.text.x = element_text(size = 10, color = "black", face = "bold"),
         strip.text.y = element_text(size = 10, color = "black", face = "bold"),
         axis.line = element_line(colour = "black"), 
         legend.title = element_text(color = "black", size = 10, face = "bold"),
         legend.text = element_text(size = 10, face = "italic"),
         legend.position = "bottom",
         # plot.title = element_text(hjust= 0.5, vjust=2, size = 10 ,face = "bold"),
         legend.key.size = unit(0.75,"line"),
         axis.text.x =  element_blank(),
         axis.text.y = element_text (size = 10,colour = "black",face = "plain" ),
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 12,colour = "black",face = "plain" )) +
   scale_fill_manual(values = c("Negaprion brevirostris"="seashell3","Ginglymostoma cirratum"="seashell3",
                                "Lutjanus jocu"="chocolate3", "Cephalopholis fulva"="chocolate3","Carangidae"="chocolate3",
                                "Acanthurus spp."="darkolivegreen3","Stegastes rocasensis"="darkolivegreen3","Thalassoma noronhanum"="darkgoldenrod1",
                                "Abudefduf saxatilis"="darkgoldenrod1", "Sparisoma spp."="darkolivegreen3","Melichthys niger"="darkgoldenrod1",
                                "Kyphosus spp."="darkolivegreen3", "Mulloidichthys martinicus"="darkgoldenrod1",
                                "Holocentrus adscensionis"="darkgoldenrod1","Haemulidae"="darkgoldenrod1",
                                "Cryptobenthic reef fishes"="darkolivegreen3"),
                     name = "Species:", 
                     labels = c("2:Negaprion brevirostris", "3:Ginglymostoma cirratum", 
                                "4:Lutjanus jocu", "5:Cephalopholis fulva", "6:Carangidae",
                                "7:Acanthurus spp.", "8:Stegastes rocasensis", "9:Thalassoma noronhanum",
                                "10:Abudefduf saxatilis", "11:Sparisoma spp.", "12:Melichthys niger", 
                                "13:Kyphosus spp.","14:Mulloidichthys martinicus", "15:Holocentrus adscensionis",
                                "16:Haemulidae","17:Cryptobenthic reef fishes")) +
   scale_y_continuous(position = "left", limits=c(-100,20), breaks=seq(-100,20,20)) 
p

#### Saving  plot

ggsave(p, file = "outputs_results/figures/percent_variation_fish_biomass_2100.png", units = "mm",
        width= 233, dpi = 400)
