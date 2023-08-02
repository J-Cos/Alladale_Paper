    plotNdviMetric<-function(mk_res, NdviMetric){
        p<-ggplot() +
            geom_spatraster( data = GetNdviChangeRast(mk_res, NdviMetric),alpha = 1, na.rm=TRUE  )+ 
            geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
            geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
            scale_fill_manual(values = c("dark red", "red", "green", "dark green"), na.translate=FALSE)+
            theme_classic()
        return(p)
    }
    plotNdviMetric_alladaleZoom<-function(mk_res, NdviMetric){
        p<-ggplot() +
            geom_spatraster( data = crop(GetNdviChangeRast(mk_res, NdviMetric), ext(Alladale)),alpha = 1, na.rm=TRUE  )+ 
            geom_spatvector(data=Alladale, color="black", linewidth=2, fill=NA)+
            geom_spatvector(data=enclosure, color="black", linewidth=1, fill=NA)+
            scale_fill_manual(values = c("dark red", "red", "green", "dark green"), na.translate=FALSE)+
            theme_classic()
        return(p)
    }   