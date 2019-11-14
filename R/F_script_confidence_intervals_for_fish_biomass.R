#Scrip to genereta coefficinet of variotion for fish species with times series 
####Load data####
dataset <- read.table("data/raw/raw_time_series_fish_Rocas_Atoll.csv", header = T, 
            sep = ",",na.strings = "NA", dec = ".", strip.white = TRUE)
View(dataset)
####'Routine with bootstrap method (random sampling with replacement)#### 
# Generating vector that contain the number of sample
a <- numeric(1000000)

#"For" loop in R that returns biomass's Coefficient of variation (CV) for fish species#####
# It works with time series' fish species  
# CV = square root of (variance/mean)
#Change the number from 1 to 14 in "dataset[.1]", to define which species you want CV 
for (i in 1:1000000){a[i] <- mean(sample(dataset[,1],7,replace = T), na.rm = T)}
     (cv <- sqrt(var(a,na.rm = T))/mean(a, na.rm = T))







