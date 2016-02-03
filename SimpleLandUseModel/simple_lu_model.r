# Date: 3.02.2016
# author: florian.gollnow@geo.hu-berlin.de
#
#
# function:
# simple_lu_model(lu, suit, suitclass, elas, traj, demand,protected, writeRaster)
#
# argument | description
# lu | "Rasterlayer" of the initial forestcover classified as 1: forest, 0: noforest
# suit | "Rasterlayer" probabilities of deforestation continuous values between 0 and 1
# elas | "data.frame" , rows equal the land cover classes in lu, 1 to max(lu), while the colum represents the land use class to model. values between 0 and 1 to increase or decrease the suitability according to the previous land use cover
# traj | "data.frame" ,rows equal the land cover classes in lu, 1 to max(lu), while the colum represents the land use class to model. value 1 defines allowed transitions, value 0 no transition allowed
# demand | data.frame of land use demand, number of rows equal the modelling steps, 1 colume for the land use class
# protected | optional "Rasterlayer" of protected areas classified as 1: Protected, NA: unprotected
# writeRaster | if TRUE, writes Rasters of sceanrios calculation during processing into the woking directory, exapmple: "lu_epoche1.tif"
# Return:
# "RasterStack" of cropland expansion scenarios.

#Imortant
#make sure the rasters have have the same dimension, extent and origine.


#Example:
#landuse_aggr_res.tif
#library(raster)
#rasterOptions(tmpdir = "q:/carbiocial/florian/data/no_backup/tmp2")
#
#lu<- raster("landuse_aggr_majclip.tif")
#suit <- raster("suit_res.tif")
#protected <- raster("protectedAreas_res.tif")
#aligne extends and origine
#
#lu[Which(lu==0)] <-NA #class 0 is na right?
#elas <- data.frame(tocrop = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)) # 7 cropland stays cropland
#traj <- data.frame(tocrop = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)) #if 1 allowed, everything else not allowed
#
#demand <- data.frame(crop= c(847409+ (847409/5), 847409 +((847409/5)*2),  847409 +((847409/5)*3)+ 847409 +((847409/5)*4), 847409 +((847409/5)*5)))
#
#crop.scenario <- simple_lu_model(lu=lu, suit=suit.r, suitclass=7 ,elas=elas, traj=traj, demand=demand, protected=prot, writeRaster=FALSE) 
#
#writeRaster (crop.scenario, filename="sceanrio.tif" , bylayer=TRUE, suffix= "numbers")


#function
simple_lu_model <- function (lu, suit, suitclass ,elas, traj, demand, protected=c(), writeRaster=FALSE) {
  epoche=1
   while (epoche <= nrow(demand)){
    print (paste(epoche, date()))  
    #convert to vector
    lu_vector <-getValues(lu)
    suit_vector <- getValues (suit)
    suit_vector[is.na(lu_vector)]<- NA
    if (length(protected) > 0){
      protected_vector <- getValues (protected)
    } else { protected_vector=c() }
    
    if(epoche==1){
      lu_unique <- sort(unique (lu_vector))
    }
    
    #setNA where protected 
    protectedIndex <- !is.na(protected_vector)
    suit_vector[protectedIndex] <- NA
    #how much cropland is within protected areas?
    cropProtected <- length (which(lu_vector[protectedIndex]==suitclass))
        
    #apply elas and traj to suit_vector according to lu_vector
    for (i in lu_unique){
      ind <- which(lu_vector==i)
      suit_vector [ind] <- suit_vector[ind]+elas [i,]
      suit_vector [ind] <-ifelse (traj[i,]==1, suit_vector[ind], NA)
    }	
    #adjust demand according to croplands in protected areas (they just stay the same)
    demand.adj <- demand [epoche,] - cropProtected
    
    # allocate cropland demands
    suit.order <- order(suit_vector, na.last = TRUE, decreasing = FALSE)
    alloc.index <- suit.order[1:demand.adj]
    #allocate croplands
    lu_vector[alloc.index] <- suitclass 
    
    #convert back to raste
    lu <- setValues (lu, lu_vector)
    
    #name results
    assign(paste("scenario", epoche, sep=""), lu)
    
    if (writeRaster==TRUE){
      writeRaster(lu, paste("lu_epoche", epoche,".tif", sep=""))
    }
    
    epoche <- epoche +1
  }
  return(stack (mget (paste("scenario", rep(1:nrow(demand)),sep=""))))
}





