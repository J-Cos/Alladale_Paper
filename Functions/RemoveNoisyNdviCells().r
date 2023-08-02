RemoveNoisyNdviCells<-function(cell) {
    cell_df<-data.frame("NDVI"=cell,"RapidChangeTo"=NA, "RapidChangeFrom"=NA) %>% as_tibble
    Smoothing_df<-cell_df %>%
        mutate( ChangeFrom=NDVI-lag(NDVI),
                ChangeTo=lead(NDVI)-NDVI) %>%
        mutate( RapidChangeFrom=abs(ChangeFrom)>0.3,
                RapidChangeTo=abs(ChangeTo)>0.3) %>%
        mutate( RapidChangeFromType=RapidChangeFrom*sign(ChangeFrom),
                RapidChangeToType=RapidChangeTo*sign(ChangeTo)) %>%
        mutate( ProblemPixel=RapidChangeFrom & RapidChangeTo & RapidChangeFromType==-RapidChangeToType) %>% 
        mutate(NDVI_noiseRemoved=case_when(!ProblemPixel | is.na(ProblemPixel) ~ NDVI, ProblemPixel~NA)) 
    return(Smoothing_df$NDVI_noiseRemoved)    
}