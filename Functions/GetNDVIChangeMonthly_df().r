GetNDVIChangeMonthly_df<-function(ComparatorArea, mask=StudyBuffer){
        NDVIChanges<-list()
        NDVIChanges[["MaxNDVI_Inside"]]<-GetNdviChangeRast(mk_MAXXNDVI_monthly_res, MAXNDVI_monthly) %>% 
            mask(mask) %>% 
            mask(ComparatorArea) %>% 
            #crop(ext(Alladale)) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["INDVI_Inside"]]<-GetNdviChangeRast(mk_INDVI_monthly_res, INDVI_monthly) %>%
            mask(mask) %>% 
            mask(ComparatorArea) %>%
            #crop(ext(Alladale)) %>%
            values %>%
            proportionOfPixels

        NDVIChanges[["MINNDVI_Inside"]]<-GetNdviChangeRast(mk_MINNDVI_monthly_res, MINNDVI_monthly) %>%
            mask(mask) %>% 
            mask(ComparatorArea) %>%
            #crop(ext(Alladale)) %>%
            values %>%
            proportionOfPixels

        NDVIChanges[["MaxNDVI_Outside"]]<-GetNdviChangeRast(mk_MAXXNDVI_monthly_res, MAXNDVI_monthly) %>%
            mask(mask) %>% 
            mask(ComparatorArea, inverse=TRUE) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["INDVI_Outside"]]<-GetNdviChangeRast(mk_INDVI_monthly_res, INDVI_monthly) %>%
            mask(mask) %>% 
            mask(ComparatorArea, inverse=TRUE) %>% 
            values %>%
            proportionOfPixels

        NDVIChanges[["MINNDVI_Outside"]]<-GetNdviChangeRast(mk_MINNDVI_monthly_res, MINNDVI_monthly) %>%
            mask(mask) %>% 
            mask(ComparatorArea, inverse=TRUE) %>% 
            values %>%
            proportionOfPixels

        #NDVIChanges[["Both_Inside"]]<-(GetNdviChangeRast(mk_MAXXNDVI_monthly_res, MAXNDVI_monthly) + GetNdviChangeRast(mk_INDVI_monthly_res, INDVI_monthly)) %>% 
        #    terra::as.factor() %>%
        #    mask(ComparatorArea) %>% 
        #    #crop(ext(Alladale)) %>% 
        #    values %>%
        #    proportionOfPixels

        #NDVIChanges[["Both_Outside"]]<-(GetNdviChangeRast(mk_MAXXNDVI_monthly_res, MAXNDVI_monthly) + GetNdviChangeRast(mk_INDVI_monthly_res, INDVI_monthly)) %>% 
        #    terra::as.factor() %>%
        #    mask(buffer(ComparatorArea, width=buffer), inverse=TRUE) %>% 
        #    values %>%
        #    proportionOfPixels

        NDVIChange_df<-bind_rows(NDVIChanges) %>%
            cbind(names(NDVIChanges),.) 
        
        colnames(NDVIChange_df)<-c("Parameter", "Significant Decrease", "Inignificant Decrease", "Insignificant Increase", "Significant Increase")

        return(NDVIChange_df)
    }