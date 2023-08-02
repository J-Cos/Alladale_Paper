    plotNdviAgainstLcChange<-function(NdviMetric_res, NdviMetric, LCchange, xlims=c(-128, 128) ) {
        
        LcChange_resampled <- exactextractr::exact_resample(LCchange, NdviMetric, fun='sum', coverage_area=FALSE)
        LcChange_resampled<-mask( LcChange_resampled, NdviMetric)[[1]]
        AlladaleInOut<- mask( terra::ifel(test=is.na(LcChange_resampled), yes=1, no=1), Alladale, updatevalue=0)


        plot_df<-cbind(  values(mask(NdviMetric_res[["tau"]], NdviMetric)[[1]]),
                values(mask(NdviMetric_res[["sl"]]<0.05, NdviMetric)[[1]]),
                values(LcChange_resampled),
                values(AlladaleInOut) ) %>%
                as_tibble
        names(plot_df)<-c("NdviChange", "signif", "LcChange", "Position")

        plot<-ggplot(data=plot_df, aes(y=NdviChange, x=LcChange, color=as.factor(Position)))+
            geom_point(alpha=0.3)+
            geom_hline(yintercept=0)+ geom_vline(xintercept=0)+
            scale_color_manual(values = c("grey", "green"), na.translate=FALSE)+
            theme_classic()+
            ylim(-1,1)+
            xlim(xlims)+
            facet_wrap(~Position, ncol=1) + 
            theme(
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                legend.position="none"
            )

        return(plot)
    
    }