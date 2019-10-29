#####What thisscript do ? #####


######Load packages######
library(readr)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(tidyr)
library(magrittr)
library(ggpubr)
library(plotly)
library(dplyr)
library(tidyverse)
#####Read data######
#read data from "processed" folder, scenario 2100 and for fish species 
d <- read_csv("data/processed/processed_data_figure_box-plot_year_2100_fish.csv")
#declare and re-order factor of variables species and scenario
d$species <- factor(d$species,
    levels = c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", 
             "Cephalopholis fulva", "Carangidae","Acanthurus spp.", 
             "Stegastes rocasensis","Thalassoma noronhanum",
             "Abudefduf saxatilis", "Sparisoma spp.", "Melichthys niger", 
             "Kyphosus spp.", "Mulloidichthys martinicus",
             "Holocentrus adscensionis","Haemulidae", "Cryptobenthic reef fishes"))
d$scenario <- factor(d$scenario,
      levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

#####Data frame with median for each species ######
#Made data-frame with median of the Status quo scenario 
median_statusquo <- d %>%
   filter(scenario == "Status quo") %>%
   group_by(scenario,year, species) %>%
   summarise_all(funs(mean,median,sd, max, min))

median_statusquo$species <- factor(median_statusquo$species,
                                   levels = c("Negaprion brevirostris","Ginglymostoma cirratum","Lutjanus jocu", 
                                            "Cephalopholis fulva", "Carangidae","Acanthurus spp.", 
                                            "Stegastes rocasensis", "Thalassoma noronhanum",
                                            "Abudefduf saxatilis", "Sparisoma spp.", "Melichthys niger", 
                                            "Kyphosus spp.", "Mulloidichthys martinicus", 
                                            "Holocentrus adscensionis",
                                            "Haemulidae", "Cryptobenthic reef fishes"))



##### boxplot for the 2100 year and fish species ####

p <- ggplot(d, aes(x = scenario, y = biomass, fill = scenario, facets = species)) + 
   geom_boxplot(aes(x = as.factor(scenario), y = as.numeric(biomass), fill = scenario),
                width = 0.35, fatten = 2.5,  notch = F, alpha = 0.75, outlier.shape = NA) +
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

#####Function to modify facet scales in ggplot2
#from https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/

Facet <- ggproto(
   init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
      scales <- list()
      if (!is.null(x_scale)) {
         scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
      }
      if (!is.null(y_scale)) {
         scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
      }
      scales
   },
)

scale_override <- function(which, scale) {
   if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
      stop("which must be an integer of length 1")
   }
   
   if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
      stop("scale must be an x or y position scale")
   }
   
   structure(list(which = which, scale = scale), class = "scale_override")
}

CustomFacetWrap <- ggproto(
   "CustomFacetWrap", FacetWrap,
   init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
      # make the initial x, y scales list
      scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
      
      if(is.null(params$scale_overrides)) return(scales)
      
      max_scale_x <- length(scales$x)
      max_scale_y <- length(scales$y)
      
      # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
      for(scale_override in params$scale_overrides) {
         which <- scale_override$which
         scale <- scale_override$scale
         
         if("x" %in% scale$aesthetics) {
            if(!is.null(scales$x)) {
               if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
               scales$x[[which]] <- scale$clone()
            }
         } else if("y" %in% scale$aesthetics) {
            if(!is.null(scales$y)) {
               if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
               scales$y[[which]] <- scale$clone()
            }
         } else {
            stop("Invalid scale")
         }
      }
      
      # return scales
      scales
   }
)

facet_wrap_custom <- function(..., scale_overrides = NULL) {
   # take advantage of the sanitizing that happens in facet_wrap
   facet_super <- facet_wrap(...)
   
   # sanitize scale overrides
   if(inherits(scale_overrides, "scale_override")) {
      scale_overrides <- list(scale_overrides)
   } else if(!is.list(scale_overrides) || 
             !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
      stop("scale_overrides must be a scale_override object or a list of scale_override objects")
   }
   
   facet_super$params$scale_overrides <- scale_overrides
   
   ggproto(NULL, CustomFacetWrap,
           shrink = facet_super$shrink,
           params = facet_super$params
   )
}

#Before to run the code below, remember to run the function contained in the script "function_modify_facet_scale" 
p <- p +
   facet_wrap_custom(~species, scales = "free_y", ncol = 4,nrow = 4, scale_overrides = list(
      scale_override(1, scale_y_continuous(limits  = c(0, 0.60), breaks = seq(0,.60,0.15))),
      scale_override(2, scale_y_continuous(limits  = c(0, 12), breaks = seq(0,12,1))),
      scale_override(3, scale_y_continuous(limits  = c(0, 2.05), breaks = seq(0,2.05,0.5))),
      scale_override(4, scale_y_continuous(limits  = c(0, 0.75), breaks = seq(0,0.75,0.25))),
      scale_override(7, scale_y_continuous(limits  = c(0, 0.8), breaks = seq(0,0.8,0.2))),
      scale_override(9, scale_y_continuous(limits  = c(0, 1.5), breaks = seq(0,1.5,.25))),
      scale_override(2, scale_y_continuous(limits  = c(1, 3.5))),
      scale_override(13, scale_y_continuous(limits  = c(0, 2.5), breaks = seq(0,2.5,.5))),
      scale_override(15, scale_y_continuous(limits  = c(0, 1.75), breaks = seq(0,1.75,.5)))
      
   ))
p
ggsave(filename = "boxplot_fish_2100", plot = p, path = "outputs_results/figures/",
       width = 18, device = "png", height = 13, units = 'in', dpi = 400)
