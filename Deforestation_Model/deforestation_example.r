
library(raster)

setwd("C:/Users/geo_flgo/Documents/GitHub/alucR/example_data/small")
#load_raster
lc <- raster("tc08_aggregated.tif")
forest <- reclassify(lc, rcl=matrix(nrow=8, ncol=2, data= c(1,1,2,0,3,0,4,0,5,0,6,0,7,0,8,0),byrow=TRUE))
prob <- stack ("suitability_stack.tif")
prob <- subset(prob, subset=1, drop=T)
protected <- raster("PAall_aggregated.tif")

demand<- c(1000,1000,1000,1000,1000,1000,1000,1000,1000) # annual deforestation

plot(prob) # probability of forest
plot(forest)

test <- def_model(forest=forest,prob=prob,protected=protected, demand=demand) 
plot(test)

