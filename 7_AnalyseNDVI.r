library(terra)
library(tidyverse)
library(tidyterra)

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
        enclosure<-vect("Data/175haenclosure.shp")
        enclosure<-project(enclosure, crs(modis_smoothed))

    #load land cover data for plotting agaisnt ndvi change
        LCchange<-terra::rast(file.path("Outputs", "LandCoverChange.tif"))
        LCchange<-terra::as.factor(LCchange)
        levels(LCchange)<-data.frame(-2:2, c("d2", "d", "n", "u", "u2"))
        LCchange<-project( LCchange, crs(mk_MAXXNDVI_res))

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

    GetNDVIChange_df(Alladale, 5000) %>%
    write.csv(., file.path("Outputs", "NDVIChangeStats_Alladale_5kmbuffer.csv"))

    GetNDVIChange_df(enclosure, 1000) %>%
    write.csv(., file.path("Outputs", "NDVIChangeStats_enclosure_1kmbuffer.csv"))

#make some plots

    #fig 4
    p1<-plotNdviAgainstLcChange( mk_MAXXNDVI_res, MAXNDVI_smooth, LCchange)
    p2<-plotNdviAgainstLcChange( mk_INDVI_res, INDVI_smooth, LCchange)
    p3<-plotNdviAgainstLcChange( mk_MINNDVI_res, MINNDVI_smooth, LCchange)

        png(file.path("Figures","Figure4.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI"), x=0, y=0, width=0.33, height=1)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI"), x=0.33, y=0, width=0.33, height=1)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI"), x=0.66, y=0, width=0.33, height=1) 
        dev.off()

    #fig S3
    p1<-plotNdviAgainstLcChange( mk_MAXXNDVI_monthly_res, MAXNDVI_monthly, LCchange, xlims=c(-2048, 2048))
    p2<-plotNdviAgainstLcChange( mk_INDVI_monthly_res, INDVI_monthly, LCchange, xlims=c(-2048, 2048))
    p3<-plotNdviAgainstLcChange( mk_MINNDVI_monthly_res, MINNDVI_monthly, LCchange, xlims=c(-2048, 2048))

        png(file.path("Figures","FigureS3.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI - monthly"), x=0, y=0, width=0.33, height=1)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI - monthly"), x=0.33, y=0, width=0.33, height=1)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI - monthly"), x=0.66, y=0, width=0.33, height=1) 
        dev.off()


            p1<-plotNdviMetric(mk_MAXXNDVI_res, MAXNDVI_smooth)
            p2<-plotNdviMetric(mk_INDVI_res, INDVI_smooth)
            p3<-plotNdviMetric(mk_MINNDVI_res, MINNDVI_smooth)
    #fig 3
        png(file.path("Figures","Figure3.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI"), x=0, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI"), x=0.5, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI"), x=0, y=0, width=0.5, height=0.5) 
        dev.off()

            p1<-plotNdviMetric(mk_MAXXNDVI_monthly_res, MAXNDVI_monthly)
            p2<-plotNdviMetric(mk_INDVI_monthly_res, INDVI_monthly)
            p3<-plotNdviMetric(mk_MINNDVI_monthly_res, MINNDVI_monthly)
    #fig S2
        png(file.path("Figures","FigureS2.png"), height = 8.3, width = 15, units = 'in', res = 300)
            cowplot::ggdraw() +
                cowplot::draw_plot(p1+ggtitle("Max NDVI - monthly"), x=0, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p2+ggtitle("I-NDVI - monthly"), x=0.5, y=0.5, width=0.5, height=0.5)+
                cowplot::draw_plot(p3+ggtitle("Min NDVI - monthly"), x=0, y=0, width=0.5, height=0.5) 
        dev.off()

#make NDVI change table (Table 3)