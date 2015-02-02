#author: florian.gollnow@geo.hu-berlin.de 

#Stratified Random Sampling with min.Distance on raster Data



# function
# StratifiedRandomDistance (x, min.dist, sample.size)

#argument | description
# x | rater dataset with categorical values (classes)
# min.dist | min distance between points
# sample.size | number of samples per class

# return | SpatialPointDataframe 


# takes some time to compute, because each point is drawn seperately, though as soon as one class is complete, the class is excluded from the sampling. 
# a progress bar indicates the percent of drawn random point from the total requested amount.
# if the progress bar does not change anymore, it might not be possible to solve the problem. 

#x<-tc08
#min.dist=300
#sample.n=150

StratifiedRandomDistance <- function(x, min.dist,  sample.n, progress=TRUE){ 
  pb <- txtProgressBar(min = 0, max = 100, style = 3)
  classes <- sort(unique(x))  # which unique classes               
  sample.size<- rep(sample.n, length(classes)) # vector of sample sizes to be compared to
  sample <- sampleRandom(x, 1, sp=FALSE, xy=TRUE) #initial sample
  
  repeat{
    sample.tmp <- sampleRandom(x, 1, sp=FALSE, xy=TRUE) # temporary sample
    dist<- pointDistance(sample[,c(1,2)], sample.tmp[,c(1,2)], lonlat=FALSE) # distant calculation
    #print(current.sample)
    if (all(dist >= min.dist) & sample.size[which(classes==sample.tmp[,3])] > current.sample[which(classes==sample.tmp[,3])]){ # add if min dist and sample size allows
      sample <- rbind(sample, sample.tmp)
      current.sample <- tabulate(sample[,3], nbins=length(sample.size)) # how many samples are there 
      #print(current.sample)
      if (progress==TRUE){
        setTxtProgressBar(pb, (sum(current.sample)/sum(sample.size))*100)
        }
      }
    if (sample.size[which(classes==sample.tmp[,3])] == current.sample[which(classes==sample.tmp[,3])]){
      #print(paste ("set",sample.tmp[,3], "to NA")
      x[Which(x==sample.tmp[,3])] <- NA
    }
    if (all(current.sample == sample.size)){ # stop when all sample sizes are fullfilled
      break
    }
  }
  sample.sp <-SpatialPointsDataFrame(sample[,c(1,2)], data=as.data.frame(sample), proj4string = CRS(proj4string(x)), match.ID = TRUE) # write to shapefile
  return(sample.sp) # return shape
}
