##################
## Make figure ###
##################

#1: Dependencies
    #packages
        library(terra)
        library(raster)
        library(caret)
        library(tidyverse)
        library(tidyterra)
        library(ggplot2)
        library(randomForest)

    #load project functions
        function_files<-list.files(file.path("Code","Functions"))
        sapply(file.path("Code","Functions",function_files),source)


#2. Load Data
    # Load training and validation polygons #
        TestTrain<-readRDS( file.path("Outputs", "TestTrainPoints.RDS") )
    # load fitted randomforest models
        rfMods<-readRDS(file.path("Outputs", "RandomForestModels.RDS"))

    # Importing satellite data #
        sc1921<-terra::rast(file.path("Outputs", "sc1921.tif"))
        levels(sc1921)<-data.frame(1:5, levels(TestTrain[["train1921"]][["PointVal_df"]]$response))

        sc07<-terra::rast(file.path("Outputs", "sc07.tif"))
        levels(sc07)<-data.frame(1:5, levels(TestTrain[["train07"]][["PointVal_df"]]$response))
   
    #load alladale shapefile
        Alladale<-vect("Data/alladale.shp")
        Alladale<-project(Alladale, crs(sc1921))
        enclosure<-vect("Data/175haenclosure.shp")
        enclosure<-project(enclosure, crs(sc1921))

    # get land cover change raster
        m<-as.matrix(rbind(  c(1, 2),
                            c(2, 1),
                            c(3, 2),
                            c(4, 3),
                            c(5, NA)))

        LCchange<-terra::classify(sc1921, rcl=m ) -  terra::classify(sc07, rcl=m ) 
        terra::writeRaster(LCchange, file.path("Outputs", "LandCoverChange.tif"), overwrite=TRUE)
        LCchange<-terra::as.factor(LCchange)
        levels(LCchange)<-data.frame(-2:2, c("d2", "d", "n", "u", "u2"))

#3. make landcover plots
        cbind(
            MakeAccTab(testDat=TestTrain[["test07"]]),
            MakeAccTab(testDat=TestTrain[["test1921"]])
        ) %>%
        write.csv("Figures/Table1.csv")




#3. Make landcover change plots
            p1<-ggplot() +
                geom_spatraster( data = sc07,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("green", "dark grey", "brown", "dark green", "blue"), na.translate=FALSE)+
                theme_classic()
            p2<-ggplot() +
                geom_spatraster( data = sc1921,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("green", "dark grey", "brown", "dark green", "blue"), na.translate=FALSE)+
                theme_classic()
            p3<-ggplot() +
                geom_spatraster( data = LCchange,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("dark red", "red", "grey", "green", "dark green"), na.translate=FALSE)+
                theme_classic()
        
    png(file.path("Figures","Figure2.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("2007"), x=0, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("2019/2021"), x=0.5, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Land Cover Change"), x=0.5, y=0, width=0.5, height=0.5)
    dev.off()

# 4. Alternative plots with some zoomed areas

    #1921
    png(file.path("Figures","LandCoverClassification_1921.png"), height = 8.3, width = 15, units = 'in', res = 300)
            ggplot() +
                geom_spatraster( data = sc1921,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("green", "grey", "brown", "dark green", "blue"), na.translate=FALSE)+
                theme_classic()
    dev.off()

    png(file.path("Figures","LandCoverClassification_1921_AlladaleOnly.png"), height = 8.3, width = 15, units = 'in', res = 300)
            ggplot() +
                geom_spatraster( data = crop(sc1921, ext(Alladale)),alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("green", "grey", "brown", "dark green", "blue"), na.translate=FALSE)+
                theme_classic()
    dev.off()

    #07
    png(file.path("Figures","LandCoverClassification_07.png"), height = 8.3, width = 15, units = 'in', res = 300)
            ggplot() +
                geom_spatraster( data = sc07,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("green", "grey", "brown", "dark green", "blue"), na.translate=FALSE)+
                theme_classic()
    dev.off()

    png(file.path("Figures","LandCoverClassification_07_AlladaleOnly.png"), height = 8.3, width = 15, units = 'in', res = 300)
            ggplot() +
                geom_spatraster( data = crop(sc07, ext(Alladale)),alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("green", "grey", "brown", "dark green", "blue"), na.translate=FALSE)+
                theme_classic()
    dev.off()
    #get zoomed areas
        MakePolygon<-function(e){
            v <- as.polygons(crop(sc19_masked, e), extent=TRUE)
            return(v)
        }

        ZoomedExtents<-list(
        ext(691500, 692000, 5753000, 5753500),
        ext(692000, 692500, 5755000, 5755500) ,
        ext(694000, 694500, 5754000, 5754500) ,
        ext(694750, 695250, 5750750, 5751250) 
        )

        polys<-rbind(
            MakePolygon(ZoomedExtents[[1]] ),
            MakePolygon(ZoomedExtents[[2]] ),
            MakePolygon(ZoomedExtents[[3]] ),
            MakePolygon(ZoomedExtents[[4]])
        )

        polys<-cbind(polys, data.frame(polyNames=1:length(polys)))


    # make main plots
            plot19<-ggplot() +
                geom_spatraster( data = sc19,alpha = 1, na.rm=TRUE  )+ 
                scale_fill_manual(values = plotCols, na.translate=FALSE)+
                geom_spatvector(data=polys, fill=NA, color="black")+
                geom_spatvector_text(data=polys, aes(label = polyNames), fontface = "bold", color = "black")

           plotchange<-ggplot() +
                geom_spatraster(
                    data = change,alpha = 1, na.rm=TRUE
                )+ 
                scale_fill_manual(values = c("blue", "grey", "red"), na.translate=FALSE)+
                geom_spatvector(data=polys, fill=NA, color="black")+
                geom_spatvector_text(data=polys, aes(label = polyNames), fontface = "bold", color = "black")

    #make zoomed subplots
        changePlot_l<-list()
        plot19_l<-list()
        for (i in 1: length(ZoomedExtents)) {
                changePlot_l[[i]]<-ggplot() +
                        geom_spatraster(
                            data= crop(change, ZoomedExtents[[i]]), alpha = 1, na.rm=TRUE, show.legend=FALSE
                        )+ 
                        scale_fill_manual(values = c("blue", "grey", "red"), na.translate=FALSE)
                plot19_l[[i]]<-ggplot() +
                        geom_spatraster(
                            data= crop(sc19_masked, ZoomedExtents[[i]]), alpha = 1, na.rm=TRUE, show.legend=FALSE
                        )+ 
                        scale_fill_manual(values = plotCols, na.translate=FALSE)
        }

    # save compound figure
        png(file.path("Figures",paste0("Fig3:LandCoverMaps", Sys.Date(),".png")), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(ggpubr::ggarrange(plot19, plotchange, nrow=2), x=0, y=0, width=0.5, height=1)+
                cowplot::draw_plot(ggpubr::ggarrange(plotlist=plot19_l, nrow=4, ncol=1), x=0.5, y=0, width=0.25, height=1)+
                cowplot::draw_plot(ggpubr::ggarrange(plotlist=changePlot_l, nrow=4, ncol=1), x=0.75, y=0, width=0.25, height=1)
        dev.off()
    