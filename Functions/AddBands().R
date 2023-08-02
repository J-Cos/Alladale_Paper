 
AddBands<-function(raster) {
    NDVI<-(raster[["NIR"]] - raster[["R"]]) / (raster[["NIR"]] + raster[["R"]])
    NBR <- (raster[["NIR"]] - raster[["SWIR2"]]) / (raster[["NIR"]] + raster[["SWIR2"]])
    MSAVI2 <- 0.5 * ((2*(raster[["NIR"]]+1)) - (((2*raster[["NIR"]])+1)*2 - 8*(raster[["NIR"]]-raster[["R"]]))*0.5)
    raster<-c(raster, NDVI, NBR, MSAVI2)
    names(raster)[8:10]<-c("NDVI", "NBR" ,"MSAVI2")
    return(raster)
}
