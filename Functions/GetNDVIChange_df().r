    proportionOfPixels<-function(x) { table(x)/sum(!is.na(x))
    }
    GetNDVIChange_df<-function(ComparatorArea, buffer=5000){
        NDVIChanges<-list()
        NDVIChanges[["MaxNDVI_Inside"]]<-GetNdviChangeRast(mk_MAXXNDVI_res, MAXNDVI_smooth) %>% 
            mask(ComparatorArea) %>% 
            #crop(ext(Alladale)) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["MaxNDVI_Outside"]]<-GetNdviChangeRast(mk_MAXXNDVI_res, MAXNDVI_smooth) %>%
            mask(buffer(ComparatorArea, width=buffer), inverse=TRUE) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["INDVI_Inside"]]<-GetNdviChangeRast(mk_INDVI_res, INDVI_smooth) %>%
            mask(ComparatorArea) %>%
            #crop(ext(Alladale)) %>%
            values %>%
            proportionOfPixels

        NDVIChanges[["INDVI_Outside"]]<-GetNdviChangeRast(mk_INDVI_res, INDVI_smooth) %>%
            mask(buffer(ComparatorArea, width=buffer), inverse=TRUE) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["MINNDVI_Inside"]]<-GetNdviChangeRast(mk_MINNDVI_res, MINNDVI_smooth) %>%
            mask(ComparatorArea) %>%
            #crop(ext(Alladale)) %>%
            values %>%
            proportionOfPixels

        NDVIChanges[["MINNDVI_Outside"]]<-GetNdviChangeRast(mk_MINNDVI_res, MINNDVI_smooth) %>%
            mask(buffer(ComparatorArea, width=buffer), inverse=TRUE) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["Both_Inside"]]<-(GetNdviChangeRast(mk_MAXXNDVI_res, MAXNDVI_smooth) + GetNdviChangeRast(mk_INDVI_res, INDVI_smooth)) %>% 
            terra::as.factor() %>%
            mask(ComparatorArea) %>% 
            #crop(ext(Alladale)) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["Both_Outside"]]<-(GetNdviChangeRast(mk_MAXXNDVI_res, MAXNDVI_smooth) + GetNdviChangeRast(mk_INDVI_res, INDVI_smooth)) %>% 
            terra::as.factor() %>%
            mask(buffer(ComparatorArea, width=buffer), inverse=TRUE) %>% 
            values %>%
            proportionOfPixels

        NDVIChange_df<-bind_rows(NDVIChanges) %>%
            cbind(names(NDVIChanges),.) 
        
        return(NDVIChange_df)
    }