# R-functions

some usefull R-functions:

#Stratified Random Sampling including a minimum distance between samples points

Function:   
StratifiedRandomDistance (x, min.dist, sample.size)

argument|description
---|---
x|"Rasterlayer" with categorical values (classes)
min.dist|min distance between points
sample.n|number of samples per class

return: 
SpatialPointDataframe 


Takes some time to compute, because each point is drawn seperately, though as soon as one class is complete, the class is excluded from the sampling.   
a progress bar indicates the percent of drawn random point from the total requested amount.
if the progress bar does not change anymore, it might not be possible to solve the problem. 


#Deforestation Model

deforestation model takes a estimated prbabilities for deforestation (based for example on a logistic regression analysis) and converts the most probable pixels to the class deforested, based on an a priory defined rate of deforestation.  

Function:
simple_def_model(forest, prob ,demand ,protected)

argument | description
--- | ---
forest | "Rasterlayer" of the initial forestcover classified as 1: forest, 0: noforest
prob | "Rasterlayer" probabilities of deforestation continuous values between 0 and 1
demand | vector of deforestation rate per year in pixel
protected | optional "Rasterlayer" of protected areas classified as 1: Protected, NA: unprotected

Return:
"RasterStack" of deforestation scenarios.



