
RemoveProblemPixels<-function(denoisedModis, smoothedModis, AcceptableRunOfNAs){
    modis_PixelsToRemove<-terra::app(denoisedModis, GetPixelsToRemove, AcceptableRunOfNAs)
    modis_smoothed_problempixelsremoved<-smoothedModis*modis_PixelsToRemove
    return(modis_smoothed_problempixelsremoved)
}