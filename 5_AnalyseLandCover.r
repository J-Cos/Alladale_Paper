##################
## Make figure ###
##################

#1: Dependencies
    #packages
        library(terra)
        #library(raster)
        #library(caret)
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

    # Importing supervised classification #
        sc1921<-terra::rast(file.path("Outputs", "sc1921.tif")) 
        levels(sc1921)<-data.frame(1:5, levels(TestTrain[["train1921"]][["PointVal_df"]]$response))

        sc07<-terra::rast(file.path("Outputs", "sc07.tif"))
        levels(sc07)<-data.frame(1:5, levels(TestTrain[["train07"]][["PointVal_df"]]$response))
   
    #load alladale shapefile
        Alladale<-vect("Data/alladale.shp")
        Alladale<-project(Alladale, crs(sc1921))
        Enclosures<-vect("Outputs/Enclosures")
        Enclosures<-project(Enclosures, crs(sc1921))
        StudyBuffer<-vect("Outputs/StudyBuffer")
        StudyBuffer<-project(StudyBuffer, crs(sc1921))

# 3. crop to study buffer
    sc1921<-sc1921 %>% crop (., ext(StudyBuffer)) %>% mask(StudyBuffer)
    sc07<-sc07 %>% crop (., ext(StudyBuffer)) %>% mask(StudyBuffer)


# 4.  get land cover change raster
        m<-as.matrix(rbind(  c(1, 2),
                            c(2, 1),
                            c(3, 2),
                            c(4, 3),
                            c(5, NA)))

        LCchange<-terra::classify(sc1921, rcl=m ) -  terra::classify(sc07, rcl=m ) 
        terra::writeRaster(LCchange, file.path("Outputs", "LandCoverChange.tif"), overwrite=TRUE)
        LCchange<-terra::as.factor(LCchange)
        levels(LCchange)<-data.frame(-2:2, c("d2", "d", "n", "u", "u2"))

#5. make landcover tables
    #table 1
        cbind(
            MakeAccTab(testDat=TestTrain[["test07"]]),
            MakeAccTab(testDat=TestTrain[["test1921"]])
        ) %>%
        write.csv("Figures/Table1.csv")

    #table 2
        #inside
        GetLandCoverPercents<-function(sc, inside, mask=Alladale) {
            dat<-sc %>%
                mask(mask, inverse=!inside) %>% 
                values(.) %>% 
                table(.) %>%
                `/`(sum(.)) %>%
                `*`(100) %>%
                round(., digits=2)
            return(dat)
        }
        
        Tab2base<-rbind(
            cbind(
                GetLandCoverPercents(sc07, inside=TRUE),
                GetLandCoverPercents(sc1921, inside=TRUE)
            ),
            cbind(
                GetLandCoverPercents(sc07, inside=FALSE),
            GetLandCoverPercents(sc1921, inside=FALSE)
            ),
            cbind(
                GetLandCoverPercents(sc07, inside=TRUE, mask=Enclosures),
                GetLandCoverPercents(sc1921, inside=TRUE, mask=Enclosures)
            ),
            cbind(
                GetLandCoverPercents(sc07, inside=TRUE, mask=erase(Alladale,Enclosures)),
                GetLandCoverPercents(sc1921, inside=TRUE, mask=erase(Alladale,Enclosures))
            )
        ) %>%
        as.data.frame

    Tab2base$Class<-c("Grassy", "Rocky", "Scrubland", "Trees", "Water")
    Tab2base$Area<-c(rep("Alladale",5), rep("Buffer", 5), rep("Enclosures",5), rep("Alladale excl. enclosures",5))
    names(Tab2base)[1:2]<-c("2007 area cover (%)", "2021 area cover (%)")
  
    Tab2base %>%
        as_tibble %>%
        mutate(Change=case_when(`2007 area cover (%)`> `2021 area cover (%)` ~ "-",
                                `2007 area cover (%)`< `2021 area cover (%)` ~ "+")) %>%
        select(Area, Class, `2007 area cover (%)`, `2021 area cover (%)`, Change) %>%
        write.csv("Figures/Table2.csv")
  

# x. make google earth imagery plot
library(cowplot)
library(magick)
library(ggplot2)

UkMap <-ggmap::get_stamenmap( bbox = c(left = -10.5, bottom = 49.5, right = 2, top = 59),
                        zoom = 5, maptype = "toner-background")

LocationMap<-ggmap::ggmap(UkMap) +
                        ggplot2::annotate("point", x=-4.633, y=57.869, color="red", fill="red", size=5)+
                        ggplot2::theme(                  axis.title.x = element_blank(),
                                                axis.title.y = element_blank(),
                                                axis.text= element_text(size = 12, hjust=0))
AlladaleMap<-ggplot() +
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=Enclosures, color="black", linewidth=1, fill=NA)+
                geom_spatvector(data=StudyBuffer, color="black", linewidth=1, linetype=2, fill=NA)+
                theme_classic()

# 6. Make landcover change plots
            p1<-ggplot() +
                geom_spatraster( data = sc07,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=Enclosures, color="black", linewidth=1, fill=NA)+
                ggspatial::annotation_scale() + # add scale
                scale_fill_manual(values = c("#F0E442", "#999999", "#D55E00", "#009E73", "#0072B2"), labels=c("Grassy", "Rocky", "Scrubland", "Trees", "Water"), name = "Land\ncover\nclass", na.translate=FALSE)+
                theme_classic()

            p2<-ggplot() +
                geom_spatraster( data = sc1921,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=Enclosures, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("#F0E442", "#999999", "#D55E00", "#009E73", "#0072B2"), labels=c("Grassy", "Rocky", "Scrubland", "Trees", "Water"), name = "Land\ncover\nclass", na.translate=FALSE)+
                theme_classic()+
                theme(legend.position = "none")
            p3<-ggplot() +
                geom_spatraster( data = LCchange,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=Enclosures, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("#330000", "#CC6600", "grey", "#0066CC", "#000033"), labels=c("-2", "-1", "0", "1", "2"), name = "Land\ncover\nchange", na.translate=FALSE)+
                theme_classic()
            
            zoom<-as.vector(terra::ext(Enclosures))
            p4<-ggplot() +
                geom_spatraster( data = LCchange,alpha = 1, na.rm=TRUE  )+ 
                geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
                geom_spatvector(data=Enclosures, color="black", linewidth=1, fill=NA)+
                scale_fill_manual(values = c("#330000", "#CC6600", "grey", "#0066CC", "#000033"), labels=c("-2", "-1", "0", "1", "2"), name = "Land\ncover\nchange", na.translate=FALSE)+
                theme_classic() + 
                coord_sf(xlim=c(zoom[1], zoom[2]), ylim=c(zoom[3], zoom[4]))+
                theme(legend.position = "none")

legend1 <- get_legend(p1) 
legend2 <- get_legend(p3) 
p1<-p1+theme(legend.position = "none")
p3<-p3+theme(legend.position = "none")

    center<-theme(plot.title = element_text(hjust = 0.5))

    png(file.path("Figures","Figure2.png"), height = 6, width = 12, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("2007 land cover")+center, x=0, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("2019/2021 land cover")+center, x=0, y=0, width=0.5, height=0.5)+
                cowplot::draw_grob(legend1, x=0.25, y=0.25, width=0.5, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Land cover change")+center, x=0.5, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p4+ggtitle("Land cover change, zoomed to enclosures")+center, x=0.5, y=0, width=0.5, height=0.5)+
                cowplot::draw_grob(legend2, x=0.72, y=0.25, width=0.5, height=0.5)
    dev.off()


    png(file.path("Figures","Figure1.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(LocationMap, x=0, y=0.4, width=0.4*5/3, height=0.6)+
                cowplot::draw_plot(AlladaleMap, x=0, y=0, width=0.4*5/3, height=0.35)+
                cowplot::draw_image("Data/Grassy.png", x=0.45, y=0.8, width=0.2*5/3, height=0.2)+
                cowplot::draw_image("Data/Scrubby.png", x=0.45, y=0.6, width=0.2*5/3, height=0.2)+
                cowplot::draw_image("Data/Trees.png", x=0.45, y=0.4, width=0.2*5/3, height=0.2)+
                cowplot::draw_image("Data/Rocky.png", x=0.45, y=0.2, width=0.2*5/3, height=0.2)+
                cowplot::draw_image("Data/Water.png", x=0.45, y=0, width=0.2*5/3, height=0.2)+
                cowplot::draw_plot_label(   label = c("A", "B", "C", "D", "E", "F", "G"), 
                                        size = 15, 
                                        x = c(0.17,0.17, 0.49, 0.49, 0.49, 0.49, 0.49), 
                                        y = c(1, 0.35, 1,0.8,0.6,0.4,0.2))
    dev.off()


#####################################################
#######################################################
# 7. Alternative plots with some zoomed areas

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
    