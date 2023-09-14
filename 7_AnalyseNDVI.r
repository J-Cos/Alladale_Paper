library(terra)
library(tidyverse)
library(tidyterra)
library(ggpointdensity)

    #load project functions
        function_files<-list.files(file.path("Code","Functions"))
        sapply(file.path("Code","Functions",function_files),source)
 
#load data
    modis_smoothed<-terra::rast(file.path("Outputs", "MODIS_smoothed.tif"))
    smoothedlayer_df<-readRDS(file.path("Outputs", "MODIS_smoothed_layer_df.RDS"))
    modis_monthly<-terra::rast(file.path("Outputs", "MODIS_monthly.tif"))
    monthlylayer_df<-readRDS(file.path("Outputs", "MODIS_monthly_layer_df.RDS"))

    #alladale shapefiles for overplotting on ndvi change maps
        Alladale<-vect("Data/alladale.shp")
        Alladale<-project(Alladale, crs(modis_smoothed))
        Enclosures<-vect("Outputs/Enclosures")
        Enclosures<-project(Enclosures, crs(modis_smoothed))
        StudyBuffer<-vect("Outputs/StudyBuffer")
        StudyBuffer<-project(StudyBuffer, crs(modis_smoothed))

    #load land cover data for plotting agaisnt ndvi change
        LCchange<-terra::rast(file.path("Outputs", "LandCoverChange.tif"))
        LCchange<-terra::as.factor(LCchange)
        levels(LCchange)<-data.frame(-2:2, c("d2", "d", "n", "u", "u2"))
        LCchange<-project( LCchange, crs(modis_smoothed))


# 3. crop to modis to study buffer
    modis_smoothed<-modis_smoothed %>% crop (., ext(StudyBuffer)) %>% mask(StudyBuffer)
    modis_monthly<-modis_monthly %>% crop (., ext(StudyBuffer)) %>% mask(StudyBuffer)


#calculate NDVI indices
    MAXNDVI_smooth<-getMAXNDVI(smoothedlayer_df, modis_smoothed)
    MAXNDVI_monthly<-getMAXNDVI(monthlylayer_df, modis_monthly)

    #get nnual integrated NDVI during the growing season (March–November; I-NDVI)
    INDVI_smooth<-getINDVI(smoothedlayer_df, modis_smoothed)
    INDVI_monthly<-getINDVI(monthlylayer_df, modis_monthly)    
    
    #get annual minimum (MINNDVI)
    MINNDVI_smooth<-getMINNDVI(smoothedlayer_df, modis_smoothed)
    MINNDVI_monthly<-getMINNDVI(monthlylayer_df, modis_monthly)    
    

#run mann kendal tests

    mk_MAXXNDVI_res<-terra::app(MAXNDVI_smooth, fun_kendall)
    mk_INDVI_res<-terra::app(INDVI_smooth, fun_kendall)
    mk_MINNDVI_res<-terra::app(MINNDVI_smooth, fun_kendall)

    mk_MAXXNDVI_monthly_res<-terra::app(MAXNDVI_monthly, fun_kendall)
    mk_INDVI_monthly_res<-terra::app(INDVI_monthly, fun_kendall)
    mk_MINNDVI_monthly_res<-terra::app(MINNDVI_monthly, fun_kendall)



#make NDVI change table (Table 3)
    #%age pixels increasing ndvi metrics - functions don't currently work

    NdviTab<-rbind(GetNDVIChange_df(ComparatorArea=Alladale) ,
                    GetNDVIChange_df(ComparatorArea=Enclosures, mask=Alladale) )

    NdviTab$Parameter<-paste0(sapply(strsplit(NdviTab$Parameter,"_"), `[`, 1), " % pixels")
    NdviTab<-cbind(Area=c(rep("Alladale", 3), rep("Buffer", 3), rep("Enclosures",3), rep("Alladale excl. enclosures",3)), NdviTab)
    write.csv(NdviTab, file.path("Figures", "Table3.csv"))

    NdviTab<-rbind(GetNDVIChangeMonthly_df(ComparatorArea=Alladale),
                GetNDVIChangeMonthly_df(ComparatorArea=Enclosures, mask=Alladale))
    NdviTab$Parameter<-paste0(sapply(strsplit(NdviTab$Parameter,"_"), `[`, 1), " % pixels")
    NdviTab<-cbind(Area=c(rep("Alladale", 3), rep("Buffer", 3), rep("Enclosures",3), rep("Alladale excl. enclosures",3)), NdviTab)
    write.csv(NdviTab, file.path("Figures", "TableS3.csv"))

#make some plots

    #fig 4
    p1<-plotNdviAgainstLcChange( mk_MAXXNDVI_res, MAXNDVI_smooth, LCchange)
    p2<-plotNdviAgainstLcChange( mk_INDVI_res, INDVI_smooth, LCchange)
    p3<-plotNdviAgainstLcChange( mk_MINNDVI_res, MINNDVI_smooth, LCchange)
    p4<-plotNdviAgainstLcChange( mk_MAXXNDVI_res, MAXNDVI_smooth, LCchange, mask=Alladale, ComparatorArea=Enclosures)
    p5<-plotNdviAgainstLcChange( mk_INDVI_res, INDVI_smooth, LCchange, mask=Alladale, ComparatorArea=Enclosures)
    p6<-plotNdviAgainstLcChange( mk_MINNDVI_res, MINNDVI_smooth, LCchange, mask=Alladale, ComparatorArea=Enclosures)

        png(file.path("Figures","Figure4.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI")+ theme(plot.title = element_text(hjust = 0.5)), x=0.04, y=0.5, width=0.32, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI")+ theme(plot.title = element_text(hjust = 0.5)), x=0.36, y=0.5, width=0.32, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI")+ theme(plot.title = element_text(hjust = 0.5)), x=0.68, y=0.5, width=0.32, height=0.5) +
                cowplot::draw_plot(p4, x=0.04, y=0, width=0.32, height=0.5)+
                cowplot::draw_plot(p5, x=0.36, y=0, width=0.32, height=0.5)+
                cowplot::draw_plot(p6,  x=0.68, y=0, width=0.32, height=0.5) +
                cowplot::draw_label("Buffer", x=0.02, y=0.85, size = 16, angle = 90)+
                cowplot::draw_label("Alladale", x=0.02, y=0.65, size = 16, angle = 90)+
                cowplot::draw_label("Alladale\nexcl.\nenclosures", x=0.02, y=0.38, size = 16, angle = 90)+
                cowplot::draw_label("Enclosures", x=0.02, y=0.17, size = 16, angle = 90)
        dev.off()

    #fig S3
    p1<-plotNdviAgainstLcChange( mk_MAXXNDVI_monthly_res, MAXNDVI_monthly, LCchange, xlims=c(-2048, 2048))
    p2<-plotNdviAgainstLcChange( mk_INDVI_monthly_res, INDVI_monthly, LCchange, xlims=c(-2048, 2048))
    p3<-plotNdviAgainstLcChange( mk_MINNDVI_monthly_res, MINNDVI_monthly, LCchange, xlims=c(-2048, 2048))
    p4<-plotNdviAgainstLcChange( mk_MAXXNDVI_monthly_res, MAXNDVI_monthly, LCchange, mask=Alladale, ComparatorArea=Enclosures, xlims=c(-2048, 2048))
    p5<-plotNdviAgainstLcChange( mk_INDVI_monthly_res, INDVI_monthly, LCchange, mask=Alladale, ComparatorArea=Enclosures, xlims=c(-2048, 2048))
    p6<-plotNdviAgainstLcChange( mk_MINNDVI_monthly_res, MINNDVI_monthly, LCchange, mask=Alladale, ComparatorArea=Enclosures, xlims=c(-2048, 2048))
        png(file.path("Figures","FigureS2.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI monthly")+ theme(plot.title = element_text(hjust = 0.5)), x=0.04, y=0.5, width=0.32, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI monthly")+ theme(plot.title = element_text(hjust = 0.5)), x=0.36, y=0.5, width=0.32, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI monthly")+ theme(plot.title = element_text(hjust = 0.5)), x=0.68, y=0.5, width=0.32, height=0.5) +
                cowplot::draw_plot(p4, x=0.04, y=0, width=0.32, height=0.5)+
                cowplot::draw_plot(p5, x=0.36, y=0, width=0.32, height=0.5)+
                cowplot::draw_plot(p6,  x=0.68, y=0, width=0.32, height=0.5) +
                cowplot::draw_label("Buffer", x=0.02, y=0.85, size = 16, angle = 90)+
                cowplot::draw_label("Alladale", x=0.02, y=0.65, size = 16, angle = 90)+
                cowplot::draw_label("Alladale\nexcl.\nenclosures", x=0.02, y=0.38, size = 16, angle = 90)+
                cowplot::draw_label("Enclosures", x=0.02, y=0.17, size = 16, angle = 90)
        dev.off()

    #fig 3
            p1<-plotNdviMetric(mk_MAXXNDVI_res, MAXNDVI_smooth)
            p2<-plotNdviMetric(mk_INDVI_res, INDVI_smooth)
            p3<-plotNdviMetric(mk_MINNDVI_res, MINNDVI_smooth)

            zoom<-as.vector(terra::ext(Enclosures))
            p4<-plotNdviMetric(mk_INDVI_res, INDVI_smooth) + coord_sf(xlim=c(zoom[1], zoom[2]), ylim=c(zoom[3], zoom[4]))

        png(file.path("Figures","Figure3.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI"), x=0, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI"), x=0.5, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI"), x=0, y=0, width=0.5, height=0.5) +
                cowplot::draw_plot(p4+ggtitle("I-NDVI zoomed to enclosures"), x=0.5, y=0, width=0.5, height=0.5) 
        dev.off()

    #fig S2
            p1<-plotNdviMetric(mk_MAXXNDVI_monthly_res, MAXNDVI_monthly)
            p2<-plotNdviMetric(mk_INDVI_monthly_res, INDVI_monthly)
            p3<-plotNdviMetric(mk_MINNDVI_monthly_res, MINNDVI_monthly)

            zoom<-as.vector(terra::ext(Enclosures))
            p4<-plotNdviMetric(mk_INDVI_monthly_res, INDVI_monthly) + coord_sf(xlim=c(zoom[1], zoom[2]), ylim=c(zoom[3], zoom[4]))

        png(file.path("Figures","FigureS1.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI - monthly"), x=0, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI - monthly"), x=0.5, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI - monthly"), x=0, y=0, width=0.5, height=0.5) +
                cowplot::draw_plot(p4+ggtitle("I-NDVI - monthly zoomed to enclosures"), x=0.5, y=0, width=0.5, height=0.5) 
        dev.off()