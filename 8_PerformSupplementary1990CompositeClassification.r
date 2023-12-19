
#########################
# testing 1990 composite classification
##########################

#1: Dependencies
    #packages
        library(terra)
        library(randomForest)
        library(tidyverse)
        library(ggplot2)
        library(tidyterra)

    #load project functions
        function_files<-list.files(file.path("Code","Functions"))
        sapply(file.path("Code","Functions",function_files),source)

    # Parameters


#2. Load Data
    # Importing satellite data #
        p90<-terra::rast(file.path("Outputs", "AllIndices_p90.tif"))
        p07<-terra::rast(file.path("Outputs", "AllIndices_p07.tif"))

    #2007 classification (also landsat5)
        mod07<-readRDS(file.path("Outputs", "RandomForestModels.RDS"))[[1]]

        Alladale<-vect("Data/alladale.shp")
        Alladale<-project(Alladale, crs(p90))
        Enclosures<-vect("Outputs/Enclosures")
        Enclosures<-project(Enclosures, crs(p90))
        StudyBuffer<-vect("Outputs/StudyBuffer")
        StudyBuffer<-project(StudyBuffer, crs(p90))




#3. classify

    #scale p90 to p07
    set.seed(1) 
    sc90 <- terra::predict(object=p90, 
                            model=mod07, 
                            type="response",
                            filename=file.path("Outputs", "sc90.tif"), 
                            format="GTiff", 
                            datatype="INT1U",
                            overwrite=TRUE)



#4. Make plot
    # Load training and validation polygons #
        TestTrain<-readRDS( file.path("Outputs", "TestTrainPoints.RDS") )

    # Importing supervised classification #
        levels(sc90)<-data.frame(1:5, levels(TestTrain[["train07"]][["PointVal_df"]]$response))

    sc90<-sc90 %>% crop (., ext(StudyBuffer)) %>% mask(StudyBuffer)

    classiifcationPlot<-ggplot() +
                geom_spatraster( data = sc90,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=Enclosures, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("green", "dark grey", "brown", "dark green", "blue"), labels=c("Grassy", "Rocky", "Scrubland", "Trees", "Water"), name = "Land\ncover\nclass", na.translate=FALSE)+
                theme_classic()

    p90_forPlot<-p90 %>% crop (., ext(StudyBuffer)) %>% mask(StudyBuffer)

    s <- terra::stretch(p90_forPlot, minq=0.05, maxq=.95)

    p90Plot<-ggplot() +
                        geom_spatraster_rgb( data = s, r=3, g=2, b=1)+
                        theme_classic()

        png(file.path("Figures","CompositeClassification1988-92.png"), height = 6, width = 12, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(cowplot::as_grob(p90Plot), x=0, y=0.5, width=1, height=0.5)+
                cowplot::draw_plot(classiifcationPlot, x=0, y=0, width=1, height=0.5)
        dev.off()