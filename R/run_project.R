# Script to run all project code
# Note: Refresh R session before running this script

# Start----
# 1. Run all scripts again on a fresh R section---------------------------------
# 1.1 Load and clean data----
source(file = "R/01_C_script_clean_data_for_Figure_2.R")
source(file = "R/02_C_script_clean_data_for_Figure_3.R")
#1.2 Run function for 
source(file = "R/F_modify_facet_scales.R")

# 1.3 Plot figures--------------------------------------------------------------
source(file = "R/01_S_Figure_2_total_fish_biomass_dynamics.R")
source(file = "R/02_S_Figure_3_boxplot.R")

#1.4 Run exploratory analysis and supp figures---------------------------------
source(file = "R/S_Supplementary_figures_boxplot_nofish_2100.R")
