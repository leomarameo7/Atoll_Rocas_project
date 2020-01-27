### Loas package###
library(tidyverse)
### Herbivore ####
# vector biomass herbivore species 
biomass_herbivore<- c(9.86,0.46,1.13,0.46,0.79)
# a vector of weights the same length as x giving the weights to use for each element of x.
weights<- biomass_herbivore/sum(biomass_herbivore)
# a numeric vector containing the values whose mean is to be computed
tl_herbivore_species <- c(2.10,2.03,2.2,2.03,2.38)
# Compute a weighted mean of herbivore guild
herbivore_wam<-weighted.mean(tl_herbivore_species,weights)
### Invertivore####
# vector biomass invertivore species 
biomass_invert<- c(0.2,0.99,0.27,0.42,2.12,0.96)
# a vector of weights the same length as x giving the weights to use for each element of x.
weights_invert<- biomass_invert/sum(biomass_invert)
# a numeric vector containing the values whose mean is to be computed
tl_invert_species <- c(2.71,2.63,2.67,3.08,3.19,3.09)
# Compute a weighted mean of invertivore guild
invertivore_wam<-weighted.mean(tl_invert_species,weights_invert)
####Reef sharks ###
# vector biomass reef sharks species 
biomass_sharks<- c(0.17, 1.8)
# weights
weights_sharks<- biomass_sharks/sum(biomass_sharks)
# a numeric vector containing the values whose mean is to be computed
tl_sharks_species<- c(3.58,3.47)
# Compute a weighted mean of invertivore guild
sharks_wam <-weighted.mean(tl_sharks_species,weights_sharks)
#### Generalist predators ###
biomass_gp<- c(2.22, 0.2,2.12)
weights_gp<- biomass_gp/sum(biomass_gp)
tl_gp_species<- c(3.27,3.28,3.04)
gp_wam <-weighted.mean(tl_gp_species,weights_gp)
## Create a data frame ####

df <- data.frame("Average_Weighted_Trophic_level"=c(herbivore_wam,invertivore_wam,gp_wam,sharks_wam),
                 "Trophic guild" = c("Herbivores/Detritivores","Invertivores",
                                     "Generalist predators","Reef sharks"))
df <- df %>% 
   mutate_if(is.numeric,round, digits = 2)
