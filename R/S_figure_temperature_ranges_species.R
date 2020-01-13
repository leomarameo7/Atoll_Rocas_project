# Script to do figure that represents species' thermal tolerance ranges 
##### Load packages####
library(ggplot2)
library(readr)
##### Load processed data #####
a <-as.data.frame(read_csv("data/processed/processed_data_temperature_range_species.csv", 
                          col_types = cols()))
#### variable Species as factor, reorder levels ####
a$Species <- factor(a$Species,
          levels=c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", "Cephalopholis fulva",
                   "Carangidae","Acanthurus spp.","Stegastes rocasensis","Thalassoma noronhanum",
                   "Abudefduf saxatilis", "Sparisoma spp.","Melichthys niger","Kyphosus spp.", 
                             "Mulloidichthys martinicus", "Holocentrus adscensionis","Haemulidae",
                             "Cryptobenthic reef fishes","Turtles", "Cephalopoda", "Panulirus spp.",
                             "Benthic macroinvert.","Benthic microinvert.","Siderastrea stellata",
       "Zooplankton", "Phytoplankton" ,"Digenea simplex" , "Algal turf" ))

##### Plot ####
p1<- ggplot(a, aes(x=Species, ymin = min, lower = perc10, middle = opti, upper = perc90, ymax = max)) +
   geom_boxplot(stat = "identity",na.rm=T , fill='grey81', color="black") +
   theme(text = element_text(family = "Times New Roman"),
         panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), panel.background = element_blank(),
         axis.ticks.x=element_blank(),
         axis.line = element_line(color = "black", size = 0.25, linetype = "solid"),
         legend.position = "top",
         axis.text.x=element_text(size = 18,colour = "black", face = "italic", angle=75, hjust = 1),
         axis.text.y = element_text (size = 20,colour = "black",face = "plain" ),
         axis.title.y = element_text(size = 20,colour = "black",face = "plain" ),
         axis.title.x = element_text(size = 20,colour = "black",face = "plain" )) +
   # scale_fill_manual(values=c("max"="brown1","min"="bisque1",
   # "perc10"= "darkgoldenrod", "perc90"="darkorange2"))
   scale_y_continuous(name ="Temperature (°C)\n",position = "left", 
                      limits=c(6,36), breaks=seq(6,36,2))

p1

##### Save figure #####
ggsave(filename = "temperature_ranges_species_Rocas-Atoll.png", plot = p1, 
       path = "outputs_results/supplementary_materials/",
       width = 18, device = "png", height = 13, units = 'in', dpi = 400)



