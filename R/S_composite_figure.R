#Script to do the composite figure, with total fish biomass box plots and temporal dynamics
####Load packages---------------------------
library(ggplot2)
library(readr)
library(cowplot)
library(gridExtra)
library(ggpubr)
#### Load data total fish biomass temporal dynamics----------
a <- read_csv("data/processed/processed_data_figure_2.csv", col_types = cols())
#### Convert scenario as factor ####
a$scenario <- factor(a$scenario,
                     levels = c("Status quo", "RCP 2.6","RCP 4.5",
                                "RCP 8.5"))

#### Plot 1. total fish biomass dyanmics under climate change scenarios  --------
p1 <- ggplot(data = a) + aes(x = year.group) + 
   geom_ribbon( aes(ymin = tbmin , ymax = tbmax, fill = scenario), 
                alpha = 0.4, linetype = 0) + # intervalos de confiança
   scale_fill_manual(values = c("white","Status quo" = "seashell3", "RCP 2.6" = "paleturquoise",
                                "RCP 4.5"= "khaki1", "RCP 8.5" = "lightsalmon1"), 
                     name = "Scenarios:", 
                     labels = c("Status quo", "RCP 2.6","RCP 4.5","RCP 8.5")) +
   geom_line(aes(y = tbmean, colour = scenario, linetype = scenario), size = 1) +
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
         aspect.ratio = 0.5,
         axis.text.x = element_text(face = "plain", color = "black", size = 9, angle = 0),
         axis.text.y = element_text(face = "plain", color = "black",size = 9, angle = 0),
         axis.title.y = element_text(face = "plain", color = "black",size = 10),
         axis.title.x = element_text(face = "plain", color = "black",size = 10),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title = element_text(color = "black", size = 9, face = "bold"),
         legend.text = element_text(color = "black", size = 9, face = "plain"),
         legend.position = "top",
         panel.border = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_x_continuous(breaks = c(2012,2020,2030,2040,2050,2060,2070,2080,2090,2100)) +
   scale_y_continuous(position = "left", limits = c(0,36), breaks = seq(0,36,5)) +
   geom_label( x = 2011, y = 36, label = "a", size = 4, fontface = "bold",
               family = "Times New Roman", parse = F , label.size = NA)
   
p1

ggsave(p1, file = "outputs_results/figures/composite_figure1.png",
       units = "mm", width = 487.2, dpi = 600)

#####Load data figure blox plot by year 2050####
g <- read_csv("data/processed/processed_data_figure_box-plot_year_2050_total_fish_biomass.csv", 
              col_types = cols())
#####Re-order scenario factors#####
g$scenario <- factor(g$scenario,
                     levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

#######Plot-----------------
p2 <- ggplot(g, aes(x = scenario, y = biomass, fill = scenario)) + 
   geom_boxplot(aes(x = as.factor(scenario), y = as.numeric(biomass), fill = scenario),
                width = 0.35, fatten = 2.5,  notch = T, alpha = 0.75, outlier.shape = NA) +
   theme_bw() + 
   labs(x = "Scenarios", y = expression(Total~fish~biomass~""~(g~m^{-2}))) +
   theme(text = element_text(family = "Times New Roman"),
         aspect.ratio = 1,
         legend.position = "none",
         axis.text.x = element_text(size = 9,  color = "black"),
         axis.title.x = element_text(size = 10, margin = margin(10,0,0,0)),
         axis.text.y  = element_text(size = 9,  color = "black"),
         axis.title.y = element_text(size = 10),
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_fill_brewer(palette = "RdBu", direction = -1) +
   scale_y_continuous(limits  = c(20, 35), breaks = seq(20,35,2.5)) +
   geom_label(x = 4, y = 35, label = "Year 2050", size = 3,
            family = "Times New Roman", fontface = 2, fill="white") +
   annotate("text", x = 0.75, y = 35, label = "b",size = 4,
            family = "Times New Roman", fontface = 2)
p2
#### Saving composite_figure2----------
ggsave(p2, file = "outputs_results/figures/composite_figure2.png",
       units = "mm", width = 232.4, dpi = 600)

#####Load data figure blox plot by year 2100####
r <- read_csv("data/processed/processed_data_figure_box-plot_year_2100_total_fish_biomass.csv", 
              col_types = cols())
#####Re-order scenario factors#####
r$scenario <- factor(r$scenario,
                     levels = c("Status quo", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

####Plot Total fish biomass by 2100######
p3 <- ggplot(r, aes(x = scenario, y = biomass, fill = scenario)) + 
   geom_boxplot(aes(x = as.factor(scenario), y = as.numeric(biomass), fill = scenario),
                width = 0.35, fatten = 2.5,  notch = T, alpha = 0.75, outlier.shape = NA) +
   theme_bw() + 
   labs(x = "Scenarios", y = expression(Total~fish~biomass~""~(g~m^{-2}))) +
   theme(text = element_text(family = "Times New Roman"),
         aspect.ratio = 1,
         legend.position = "none",
         axis.text.x = element_text(size = 9,  color = "black"),
         axis.title.x = element_text(size = 10, margin = margin(10,0,0,0)),
         axis.text.y  = element_text(size = 9,  color = "black"),
         axis.title.y =  element_text(size = 10),
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black")) +
   scale_fill_brewer(palette = "RdBu", direction = -1) +
   scale_y_continuous(limits  = c(0, 35), breaks = seq(0,35,5)) +
   geom_label(x = 4, y = 35, label = "Year 2100", size = 3,
              family = "Times New Roman", fontface = 2, fill = "white") +
   annotate("text", x = 0.75, y = 35, label = "c", size = 4,
            family = "Times New Roman", fontface = 2)
p3

#### Saving composite_figure3----------
ggsave(p3, file = "outputs_results/figures/composite_figure3.png",
       units = "mm", width = 232.4, dpi = 600)

##### assemble multiple plots on a page--------

####### tentative
p6 <- ggdraw() +
   draw_plot(p1, x = 0.0, y = 0.5, width = 1, height = 0.5) +
   draw_plot(p2, x = 0, y = 0, width = .5, height = .5) +
   draw_plot(p3, x = 0.5, y = 0, width = .5, height = .5) +
   draw_plot_label(label = c("a", "b", "c"), size = 12,
                   x = c(0.2, 0.5, 0.25), y = c(1, 1, 0.5))
p6
###tentative
p7 <- grid.arrange(p1,                             # First row with one plot spaning over 2 columns
             arrangeGrob(p2, p3, ncol = 2), # Second row with 2 plots in 2 different columns
             nrow = 2)  
###tentative 
p5 <- grid.arrange(p1,                                    # bar plot spaning two columns
             p2, p3,                               # box plot and scatter plot
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,1), c(2,3)))

ggsave(p5, file = "outputs_results/figures/composite_figure.png",
       units = "mm", width = 174, dpi = 600)
### tentative
p10 <- ggarrange(p2, p3 ,
                ncol = 2, nrow = 1)

p11<- ggarrange(p10, p1, ncol = 1, nrow = 2)
p11

##### tentative

blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
   cowplot::theme_nothing()

grid.arrange(p1, blankPlot, p2, p3, ncol = 2, nrow = 2, 
             widths = c(4, 2))

####
library(grid)
# Move to a new page
grid.newpage()
# Create layout : nrow = 2, ncol = 2
pushViewport(viewport(layout = grid.layout(2, 2)))
# A helper function to define a region on the layout
define_region <- function(row, col){
   viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(p1, vp = define_region(1, 1:2))
print(p2, vp = define_region(2, 1.25))
print(p3, vp = define_region(2, 2))

