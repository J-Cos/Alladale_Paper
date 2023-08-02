#########################
# Extract test and train points from polygons
##########################

#1: Dependencies
    #packages
        library(sf)
        library(terra)
        library(raster)
        library(rgdal)
        library(tidyverse)

    #load project functions
        function_files<-list.files(file.path("Code","Functions"))
        sapply(file.path("Code","Functions",function_files),source)


#2. Load Data

    # Importing satellite data #
      p21<- terra::rast(file.path("Outputs", "AllIndices_p21.tif"))
      p07<- terra::rast( file.path("Outputs", "AllIndices_p07.tif"))

    # Load training and validation polygons #
        # exported from google earth engine ("save place as") and then converted from kml to .shp at https://mygeodata.cloud/converter/kml-to-shp
            #tv1921<-MakeTrainTestSplitPolysFromShapefilePreparedFromGeeExport(data="TrainingData2019", trainProportion=0.8)
            #tv07<-MakeTrainTestSplitPolysFromShapefilePreparedFromGeeExport(data="TrainingData2007", trainProportion=0.8)

        # exported from google earth engine ("save place as")
            tv07<-MakeTrainTestSplitPolys(data="TrainingData2007.kml", trainProportion=0.8)
            tv07<-lapply(tv07, terra::project, terra::crs(p21))

            tv1921<-MakeTrainTestSplitPolys(data="TrainingData2019.kml", trainProportion=0.8)
            tv1921<-lapply(tv1921, terra::project, terra::crs(p21))



#3. Get points from polygons
    #set seed as random sampling involved
        set.seed(1) 

    train1921<-GetClassPoints(data= p21, 
                            polygons=tv1921[["CombinedTrain"]], 
                            MaxPointsPerPolygonClass=c("Other"=30))
                            
    test1921<-GetClassPoints(data= p21, 
                            polygons=tv1921[["CombinedTest"]], 
                            MaxPointsPerPolygonClass=c("Other"=30))
    train07<-GetClassPoints(data= p07, 
                            polygons=tv07[["CombinedTrain"]], 
                            MaxPointsPerPolygonClass=c("Other"=30))
                            
    test07<-GetClassPoints(data= p07, 
                            polygons=tv07[["CombinedTest"]], 
                            MaxPointsPerPolygonClass=c("Other"=30))

    TestTrain<-list("train1921"=train1921,
                "train07"=train07,
                "test1921"=test1921,
                "test07"=test07)

    TestTrain[["test07"]][["NumberCellsPerCategory"]]
    TestTrain[["test1921"]][["NumberCellsPerCategory"]]


#4. Save outputf
    #save rds for next steps
        saveRDS(TestTrain, file.path("Outputs", "TestTrainPoints.RDS")        )

    # save point shapefiles for inspection
        dir.create(file.path("Outputs", 'TestTrainPoints'))

        terra::writeVector(TestTrain[["train1921"]][["Points"]], file.path("Outputs", "TestTrainPoints",'train1921.shp'), overwrite=TRUE)
        terra::writeVector(TestTrain[["test1921"]][["Points"]], file.path("Outputs", "TestTrainPoints", 'test1921.shp'), overwrite=TRUE)
        terra::writeVector(TestTrain[["train07"]][["Points"]], file.path("Outputs", "TestTrainPoints",'train07.shp'), overwrite=TRUE)
        terra::writeVector(TestTrain[["test07"]][["Points"]], file.path("Outputs", "TestTrainPoints", 'test07.shp'), overwrite=TRUE)
                
    #save polygon images for heuristic checks
        png(file.path("Figures","TestTrainPolygons_1921.png"), height = 8.3, width = 11.7, units = 'in', res = 300)      
            par(mfrow=c(1,2))
                terra::plot(TestTrain[["train1921"]][["Points"]], "value", col=rainbow(5),  main="train")
                terra::plot(TestTrain[["test1921"]][["Points"]], "value", col=rainbow(5),  main="train")
        dev.off()

        png(file.path("Figures","TestTrainPolygons_07.png"), height = 8.3, width = 11.7, units = 'in', res = 300)      
            par(mfrow=c(1,2))
                terra::plot(TestTrain[["train07"]][["Points"]], "value", col=rainbow(5),  main="train")
                terra::plot(TestTrain[["test07"]][["Points"]], "value", col=rainbow(5),  main="train")
        dev.off()
