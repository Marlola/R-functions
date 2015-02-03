# Date: 21.01.2015
# author: florian.gollnow@geo.hu-berlin.de
#
#
# function:
# simple_def_model(forest, prob ,demand ,protected)
#
# argument | description
# forest | "Rasterlayer" of the initial forestcover classified as 1: forest, 0: noforest
# prob | "Rasterlayer" probabilities of deforestation continuous values between 0 and 1
# demand | vector of deforestation rate per year in pixel
# protected | optional "Rasterlayer" of protected areas classified as 1: Protected, NA: unprotected
#
# Return:
# "RasterStack" of deforestation scenarios.






simple_def_model <- function(forest, prob ,demand ,protected=c()){
  
  prob_vector <- getValues (prob) 
  
  # if a protected areas file (as "Rasterlayer" is provided, set those areas to NA in the probability file
    if (class(protected)=="RasterLayer"){
    protected_vector <- getValues(protected) 
    protected_index <- which(!is.na(protected_vector))  # get an index of the protected areas pixel/values 
    prob_vector[protected_index] <- NA 
  }
  
  
  # classify the probabilities to deforestation and no-deforestation
  # iterate over demand file (for each year)
  for (i in 1:length(demand)){
    #index the areas which are non forest
    if (i==1){ #for the first year we take the inital forest cover map to assess no forest areas 
      forest_vector <- getValues (forest) 
      forest_index <- which(forest_vector==0)
    }else{ # take the forest maps from the last iteration to assess no forest areas
      forest_vector <- getValues(tmp.raster)
      forest_index <- which(forest_vector==0)
    }
	
    #mask noforest areas (cannot be deforested)
    prob_vector[forest_index] <- NA  
    
    # get the order of the probabilities
    prob_index <- order(prob_vector, decreasing=TRUE, na.last=TRUE) # decreasing=TRUE for deforestation probabilities 
    
    deforest_index <- prob_index[1:demand[i]] # 
    tmp <- forest_vector
    
    tmp[deforest_index] <- 0 # set the newly deforested areas to no forest 
    
    tmp.raster <- setValues(forest, tmp) # convert vector back to a raster
    plot(tmp.raster, main=paste("defScenario", i))
    assign (paste("defScenario", i,sep=""), tmp.raster)
  }
  return (stack (mget (paste("defScenario", rep(1:length(demand)),sep="")))) # returns the scenarios of deforestation 
}
