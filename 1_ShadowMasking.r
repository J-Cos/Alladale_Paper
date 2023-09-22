library(terra)

#load data
    elev1<-terra::rast(file.path("Data", "NASADEM_HGT_n57w005", "n57w005.hgt"))
    elev2<-terra::rast(file.path("Data", "NASADEM_HGT_n57w006", "n57w006.hgt"))
    elev<-terra::merge(elev1, elev2)

    Alladale<-terra::vect("Outputs/AlladaleBuffer")

#crop to study area
    Alladale<-terra::project(Alladale,terra::crs(elev)) 
    elev<-terra::mask(elev, Alladale)
    elev<-terra::crop(elev, terra::ext(Alladale))



#get hillshade
    x <- terra::terrain(x=elev, v=c("slope", "aspect"), unit="radians", neighbors=8)
    shade2019landsat<-terra::shade(slope=x$slope, 
                        aspect=x$aspect, 
                        angle=41.31504144, 
                        direction=161.13008284,
                        normalize=TRUE,
                        filename="Outputs/shade2019landsat.tif")
    shade2007landsat<-terra::shade(slope=x$slope, 
                        aspect=x$aspect, 
                        angle=46.26366279, 
                        direction=159.17805558,
                        normalize=TRUE,
                        filename="Outputs/shade2007landsat.tif")

    shade<-shade2019landsat<100 | shade2007landsat<100


# calculate summary stats for shaded area
    #load study area to crop to 
        StudyBuffer<-vect("Outputs/StudyBuffer")
        StudyBuffer<-project(StudyBuffer, crs(shade))

    #calulate area shaded
        StudyShade<-crop(mask(shade, StudyBuffer), StudyBuffer)
        propShaded<-table(values(StudyShade))/sum(!is.na(values(StudyShade)))
        signif(propShaded*100,2)
    
    #calculate average elevation of shaded area
        StudyElev<-crop(mask(elev, StudyBuffer), StudyBuffer)

        getElevStats<-function(shaded) {
            avElevShaded<-values(StudyElev*shaded)
            u<-mean(avElevShaded, na.rm=T)
            sd<-sd(avElevShaded, na.rm=T)
            return(signif(c("u"=u, "sd"=sd), 2))
        }
        getElevStats(StudyShade)
        getElevStats(!StudyShade)


#save output
    #terra::writeRaster(shade2019landsat<100, "Outputs/HillShadeBinary2019.tif", overwrite=TRUE)
    #terra::writeRaster(shade2007landsat<100, "Outputs/HillShadeBinary2007.tif", overwrite=TRUE)
    terra::writeRaster(shade, "Outputs/HillShadeBinary.tif", overwrite=TRUE)

