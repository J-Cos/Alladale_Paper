### Geospatial Preprocessing
############################
# alladale shp provided by Chris Sandom
# trainingdata prepared in google earth engine by Jake Williams

library(terra)
library(tidyterra)
library(ggplot2)

Alladale<-vect("Data/alladale.shp")
AlladaleBuffer<-buffer(Alladale, width=10000)
#confirm buffering as expected
    ggplot() +
        geom_spatvector(data=AlladaleBuffer)+
        geom_spatvector(data=Alladale) 


writeVector(AlladaleBuffer, "Outputs/AlladaleBuffer")