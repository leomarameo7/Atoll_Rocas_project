
# ####Total fish biomass dyamicas #####
#rename(new variable name = existing variable name)
b <- read_csv("data/processed/processed_data_figure_2.csv", col_types = cols())
# select only 2100 year
b_total_fish_biomass <- b %>%
   subset(year.group %in% "2100")
# rename columns
b_total_fish_biomass <- b_total_fish_biomass %>% 
                rename( mean = tbmean) %>% 
                 rename( min = tbmin ) %>%
                 rename( max = tbmax )
#### plot total fish biomass by scenario overlapped #####
p1 <- ggplot(data = b, aes(x = year.group)) + 
   geom_ribbon( aes(ymin = tbmin , ymax = tbmax, fill = scenario), 
                alpha = 0.4, linetype = 0) + # intervalos de confiança
   scale_fill_manual(values = c("Status quo" = "seashell3", "RCP 2.6" = "paleturquoise",
                                "RCP 4.5"= "khaki1", "RCP 8.5" = "lightsalmon1"), 
                     name = "Scenarios:", 
                     labels = c("Status quo", "RCP 2.6","RCP 4.5","RCP 8.5")) +
   geom_line(aes(y = tbmean, colour = scenario, linetype = scenario), size = 1.5) +
   scale_linetype_manual(values = c("Status quo" = "solid", "RCP 2.6" = "twodash","RCP 4.5" = "dotted",
                                    "RCP 8.5" = "dotdash"), name = "Scenarios:",
                         labels = c("Status quo","RCP 2.6","RCP 4.5","RCP 8.5")) +
   # geom_point(data=prova1, aes(y = observed), size=0.5, na.rm = T) +
   scale_colour_manual(values = c("Status quo" = "black", 
                                  "RCP 2.6" = "blue", 
                                  "RCP 4.5" = "orange3", 
                                  "RCP 8.5" = "red"), name = "Scenarios:",
                       labels = c("Status quo", "RCP 2.6","RCP 4.5","RCP 8.5")) +
   labs(x="Year", y = expression(Total~fish~biomass~""~(g~m^{-2}))) +
   theme(text = element_text(family = "Times New Roman"),
         aspect.ratio = 0.75,
         axis.text.x = element_text(face = "plain", color = "black", size = 12, angle=75, hjust = 1.05),
         axis.text.y = element_text(face = "plain", color = "black",size = 12, angle = 0),
         axis.title.y = element_text(face = "plain", color = "black",size = 14),
         axis.title.x = element_text(face = "plain", color = "black",size = 14),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title = element_text(color = "black", size = 12, face = "bold"),
         legend.text = element_text(color = "black", size = 12, face = "plain"),
         legend.key.size = unit(0.8,"line"),
         legend.position = "top",
         panel.border = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_x_continuous(breaks = c(2012,2020,2030,2035,
                                 2040,2045,2050,2055,2060,2065,2070,
                                 2075,2080,2085,2090,2100))+
scale_y_continuous(breaks = c(0,5,15,20,22.5,25,27.5,30,32.5))

p1

### Saving plot####
#Set new windo size and replot whatever plot you just made. 
dev.new(width = 10, height = 7.5, unit="in", noRStudioGD = T)
last_plot() 
#Save the plot and set the size using `dev.siz()` so you don't have to ever change that part and cannot possibly have a typo.
ggsave(filename = "total_fish_biomass_by_scenario.png",
       plot=p1, 
       device="png",
       path ="outputs_results/supplementary_materials/",
       width = dev.size()[1],
       height = dev.size()[2],
       dpi = 400)

dev.off() 


#### Load data fish biomass temporal dynamics by species and scenario ----------
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


a_1 <- a %>%
   subset(year.group %in% "2100")
is.num <- sapply(a_1, is.numeric)
a_1[is.num] <- lapply(a_1[is.num], round, 3)


#### save CSV file#####

write.csv(x = b_total_fish_biomass, 
          file = "data/processed/total_fish_species__biomass.csv", 
          row.names = FALSE)   

write.csv(x = a_1, 
          file = "data/processed/fish_species__biomass_dynamics.csv", 
          row.names = FALSE) 
