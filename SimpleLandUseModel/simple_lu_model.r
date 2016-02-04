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
# writeRaster | if TRUE, writes Rasters of sceanrios calculation during processing into the woking directory, exapmple: "scenario_epoche1.tif"
# loong_loop | FALSE or TRUE, two different approaches for applying the elas and traj settings. if FALSE the model should be much faster.

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
#traj <- data.frame(tocrop = c(1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1)) #if 1 allowed, everything else not allowed
#
#demand <- data.frame(crop= c(847409+ (847409/5), 847409 +((847409/5)*2),  847409 +((847409/5)*3)+ 847409 +((847409/5)*4), 847409 +((847409/5)*5)))
#
#crop.scenario <- simple_lu_model(lu=lu, suit=suit, suitclass=7 ,elas=elas, traj=traj, demand=demand, protected=protected, writeRaster=TRUE, loong_loop =FALSE) 
#
#writeRaster (crop.scenario, filename="sceanrio.tif" , bylayer=TRUE, suffix= "numbers", overwrite=TRUE)



#function
simple_lu_model <- function (lu, suit, suitclass ,elas, traj, demand, protected=c(), writeRaster=FALSE) {
  epoche=1
 
  while (epoche <= nrow(demand)){
    print (paste(epoche, date()))  
    pb <- txtProgressBar(min=0, max=6)
    setTxtProgressBar(pb, 1)
    
    #convert to vector
    
    if(epoche==1){
    lu_vector <-getValues(lu)
    suit_vector <- getValues (suit)
    suit_vector[is.na(lu_vector)]<- NA
    } else {
      lu_vector <- lu_new
    }
    
    if (length(protected) > 0){
      protected_vector <- getValues (protected)
    } 
    setTxtProgressBar(pb, 2)
    if(epoche==1){
      lu_unique <- sort(unique (lu_vector))
    }
    
#suit.n <- setValues(suit , suit_vector) 
#writeRaster(suit.n , "suit_tmp.tif")
    #setNA where protected 
    protectedIndex <- which(is.na(protected_vector)==FALSE)
    suit_vector[protectedIndex] <- NA
#suit.n <- setValues(suit , suit_vector) 
#writeRaster(suit.n , "suit_tmp.tif")
    #how much cropland is within protected areas?
    cropProtected <- length (which(lu_vector[protectedIndex]==suitclass))
    rm(protectedIndex)
	
    setTxtProgressBar(pb, 3)
#suit.v.tmp <- suit_vector   
#suit_vector <- suit.v.tmp    
    #apply elas and traj to suit_vector according to lu_vector
#if (loong_loop == TRUE){
#      for (i in lu_unique){
#      #print (i)
#      #if (elas[i,] != 0  traj[i,] != 1){
#      #print(i)
#      ind <- which(lu_vector==i)
#      #print(head(ind))
#      for (a in ind){
#        if (elas[i,] != 0){   
#        suit_vector [a] <- suit_vector[a]+elas [i,]
#        }
#        if (traj[i,] != 1){  
#        suit_vector [a] <-ifelse (traj[i,]==1, suit_vector[a], NA)
#        }}#}
#}
#if (loong_loop == FALSE){
  for (i in lu_unique){
    #if (elas[i,] != 0 | traj[i,] != 1){
    #print(i) 
	 ind <- which(lu_vector==i)
     if (elas[i,] != 0){
		#print ("elas=TRUE")
       suit_vector [ind] <- suit_vector[ind] + elas [i,]
     }
     if (traj[i,] != 1){  
	 #print ("traj=TRUE")
       suit_vector [ind] <- NA
    }
	rm(ind)}#}
#}
  
    setTxtProgressBar(pb, 4)
#suit.n <- setValues(suit , suit_vector) 
#writeRaster(suit.n , "suit_tmp.tif", overwrite=TRUE)
    #adjust demand according to croplands in protected areas (they just stay the same)
    demand.adj <- demand [epoche,] - cropProtected
    
    # allocate cropland demands
    suit.order <- order(suit_vector, na.last = TRUE, decreasing = TRUE)
    alloc.index <- suit.order[1:demand.adj]
	rm(suit.order)
    #allocate croplands
    lu_new <- lu_vector
    lu_new [alloc.index]<- suitclass
	rm(lu_vector)
	rm(alloc.index)
    #suit.al <- suit_vector
    #suit.al[alloc.index] <- suitclass 
    #suit.al[!alloc.index]<- lu_vector [!alloc.index]
    
    setTxtProgressBar(pb, 5)
    #convert back to raste
    lu.new <- setValues (lu, lu_new)
    
    #name results
    assign(paste("scenario", epoche, sep=""), lu.new)
    
    if (writeRaster==TRUE){
      writeRaster(lu.new, paste("sceanrio_epoche", epoche,".tif", sep=""), overwrite=TRUE)
    }
	rm(lu.new)
    setTxtProgressBar(pb, 6)
    epoche <- epoche +1
  }
  return(stack (mget (paste("scenario", rep(1:nrow(demand)),sep=""))))
}
