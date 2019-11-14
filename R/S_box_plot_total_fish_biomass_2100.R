#Script to do box plot with total fish biomass by scenario at 2100 
###Load packages #######
library(ggplot2)
library(readr)
#####Load data####
r <- read_csv("data/processed/processed_data_figure_box-plot_year_2100_total_fish_biomass.csv", 
              col_types = cols())
#####Re-order scenario factors#####
r$scenario <- factor(r$scenario,
                     levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

####Plot Total fish biomass by 2100######
p <- ggplot(r, aes(x = scenario, y = biomass, fill = scenario)) + 
   geom_boxplot(aes(x = as.factor(scenario), y = as.numeric(biomass), fill = scenario),
                width = 0.35, fatten = 2.5,  notch = T, alpha = 0.75, outlier.shape = NA) +
   #geom_hline(data = median_statusquo, aes(yintercept = median_statusquo$median), 
             # colour = "red", lty = "longdash", lwd = 0.35) +
   theme_bw() + 
   labs(x = "Scenarios", y = expression(Biomass~""~(g~m^{-2}))) +
   theme(text = element_text(family = "Times New Roman"),
         aspect.ratio = 1,
         legend.position = "none",
         axis.text.x = element_text(size = 8,  color = "black"),
         axis.title.x = element_text(size = 11, margin = margin(10,0,0,0)),
         axis.text.y  = element_text(size = 10,  color = "black"),
         axis.title.y = element_text(size = 11),
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_fill_brewer(palette = "RdBu", direction = -1) +
   scale_y_continuous(limits  = c(0, 35), breaks = seq(0,35,5))

return(print(p))

####Save figure #####

ggsave(p, file = "outputs_results/figures/total_fish_biomass_boxplot_2100.png",
       units = "mm", width = 84, dpi = 400)
