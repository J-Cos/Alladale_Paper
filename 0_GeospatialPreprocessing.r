### Geospatial Preprocessing
############################
# alladale shp provided by Chris Sandom
# trainingdata prepared in google earth engine by Jake Williams

library(terra)
library(tidyterra)
library(ggplot2)


#get buffers
    Alladale<-vect("Data/alladale.shp")
    AlladaleBuffer<-buffer(Alladale, width=10000)

    StudyBuffer<-buffer(Alladale, width=1800)

    expanse(StudyBuffer)/expanse(Alladale)
    #confirm buffering as expected
        ggplot() +
            geom_spatvector(data=AlladaleBuffer)+
            geom_spatvector(data=StudyBuffer) +
            geom_spatvector(data=Alladale)



    writeVector(AlladaleBuffer, "Outputs/AlladaleBuffer")
    writeVector(StudyBuffer, "Outputs/StudyBuffer")

#get enclosures
        EnclosurePaths<-list.files("Data", pattern="^Phase.*shp$", full.names=T)
        Enclosures<-lapply(EnclosurePaths, terra::vect)
        Enclosures<-lapply(Enclosures, terra::project, crs(Alladale))
        Enclosures<-vect(Enclosures)

        values(Enclosures)<-data.frame(Phase=c(rep("I", 4), rep("II", 3), rep("III", 1), rep("IV", 1)),
                    ID=c("Ia", "Ib", "Ic", "Id", "IIa", "IIb", "IIc", "IIIa", "IVa"),
                    Start=c(rep("Pre-2007", 4), rep("Post-2007", 5)))

        Enclosures<-Enclosures[-8]

        ggplot() +
            geom_spatvector(data=StudyBuffer)+
            geom_spatvector(data=Alladale)+
            geom_spatvector(data=Enclosures, aes(color=Start))

    writeVector(Enclosures, "Outputs/Enclosures")

