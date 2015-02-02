# R-functions

some usefull R-functions:

#Stratified Random Sampling including a minimum distance between samples

function
StratifiedRandomDistance (x, min.dist, sample.size)

argument | description
--|--
x | rater dataset with categorical values (classes)
min.dist | min distance between points
sample.size | number of samples per class


return | SpatialPointDataframe 


takes some time to compute, because each point is drawn seperately, though as soon as one class is complete, the class is excluded from the sampling. 
a progress bar indicates the percent of drawn random point from the total requested amount.
if the progress bar does not change anymore, it might not be possible to solve the problem. 



