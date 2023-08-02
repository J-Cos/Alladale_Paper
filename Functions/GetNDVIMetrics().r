
    getMAXNDVI<-function(layer_df, modis){
        MAXNDVI_l<-list()
        for (y in unique(layer_df$year)){
            YearsLayers<-layer_df %>% 
                filter(year==y) %>%
                select(LayerName)
            
            AnnualNDVI<-modis[[YearsLayers]]
            MAXNDVI_l[[y]]<-max(AnnualNDVI, na.rm=TRUE)
            names(MAXNDVI_l[[y]])<-y
        }
        MAXNDVI<-terra::rast(MAXNDVI_l)

        return(MAXNDVI)
    }
    getINDVI<-function(layer_df, modis){
    
        INDVI_l<-list()
        for (y in unique(layer_df$year)){
            YearsGrowingSeasonLayers<-layer_df %>% 
                filter(year==y) %>%
                filter(month %in% c("03", "04", "05", "06", "07", "08", "09", "10", "11")) %>%
                select(LayerName)
            
            GrowingSeasonNDVI<-modis[[YearsGrowingSeasonLayers]]
            GrowingSeasonNDVI_PositiveVals<-ifel(GrowingSeasonNDVI<0, 0, GrowingSeasonNDVI)
            INDVI_l[[y]]<-sum(GrowingSeasonNDVI_PositiveVals, na.rm=TRUE) # sum of positive NDVI
            names(INDVI_l[[y]])<-y
        }
        INDVI<-rast(INDVI_l)
        return(INDVI)
    }
    getMINNDVI<-function(layer_df, modis){
        MINNDVI_l<-list()
        for (y in unique(layer_df$year)){
            YearsLayers<-layer_df %>% 
                filter(year==y) %>%
                select(LayerName)
            
            AnnualNDVI<-modis[[YearsLayers]]
            MINNDVI_l[[y]]<-min(AnnualNDVI, na.rm=TRUE)
            names(MINNDVI_l[[y]])<-y
        }
        MINNDVI<-rast(MINNDVI_l)
        return(MINNDVI)
    }