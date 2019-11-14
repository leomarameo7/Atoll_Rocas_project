# Script to do Rocas Atoll Maps
####Load packages ####
library("ggplot2")
require(raster)
library(mapdata)
library(sp)
library(maps)
library(scales)
library(mapproj)
#####
atoll <- getData("GADM",country ="BRA",level=0)
plot(atoll)
g <- plot(atoll, xlim=c(-33.84, -33.76), ylim=c(-3.9, -3.82), fill=T)


 plot(atoll, xlim=c(-40.84, -33.76), ylim=c(-40, -3.82))


map.scale(-33.84,-3.9, ratio= T,rewidth=0.29,cex=0.5)
map.axes()


lons<-c(-33.82, -33.79)
lats <-c(-3.9, -3.85)

world_map <- map_data("world")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
   geom_polygon(fill="lightgray", colour = "white")

brasil <- map_data("world", region = "brazil")

ggplot(brasil, aes(x = long, y = lat)) +
   geom_polygon(aes( group = group, fill = region))


