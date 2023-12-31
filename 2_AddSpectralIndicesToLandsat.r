#########################
# Add spectral indicies
##########################

#1: Dependencies
    #packages
        #library(raster)
        library(terra)
        library(tidyverse)

    #load project functions
        function_files<-list.files(file.path("Code","Functions"))
        sapply(file.path("Code","Functions",function_files),source)

#2. Load, add all spectral indices and plot
    #2019/21
        #load
            rastlist <- list.files(path = ("../Alladale_Paper/Data/LC08_L1TP_206020_20210827_20210901_02_T1"), pattern=glob2rx("*B?.TIF"), all.files= T, full.names= T)
            p21 <-  terra::rast(rastlist[1:7]) #landsat 8 has 11 bands, we just want the first 7. glob2rx() function above only gets single digit bands so we can just take the first 7.
            names(p21)<-c("Aerosol", "B", "G", "R", "NIR", "SWIR1", "SWIR2")

        #crop to alladale buffer  
            Alladale<-terra::vect("Outputs/AlladaleBuffer")
            Alladale<-terra::project(Alladale,terra::crs(p21)) 
            p21<-mask(p21, Alladale)
            p21<-crop(p21, ext(Alladale))
        
        # mask shadows
            ShadowMask<-terra::rast("Outputs/HillShadeBinary.tif")
            ShadowMask<-terra::project(ShadowMask,terra::crs(p21)) 
            ShadowMask_resampled = resample(y=p21, x=ShadowMask)
            msk <- ifel(ShadowMask_resampled >0, NA, 1)
            p21 <- mask(p21, msk)

        #calulate spectral indices
            p21<-AddBands(p21)

    #2007
        #load
            p07 <-  terra::rast("../Alladale_Paper/Outputs/Alladale2007.tif") # only 7 bands from this older landsat spacecraft - note they are not the same bands
            names(p07)<-c("B", "G", "R", "NIR", "SWIR1", "T", "SWIR2")

        #crop to alladale buffer  
            p07<-mask(p07, Alladale)
            p07<-crop(p07, ext(Alladale))
            msk<-crop(msk, ext(p07))


        # mask shadows
            p07 <- mask(p07, msk)

        #calulate spectral indices
            p07<-AddBands(p07)


    #1990
        #load
            p90 <-  terra::rast("../Alladale_Paper/Outputs/Alladale1990Composite_5y.tif")
            p90<-terra::subset(p90, c(1, 2, 3, 4, 5, 9, 6))
            names(p90)<-c("B", "G", "R", "NIR", "SWIR1", "T", "SWIR2")

        #reproject
            #p90<-terra::project(p90,terra::crs(p07)) 

        #crop to alladale buffer  
            p90<-mask(p90, Alladale)
            p90<-crop(p90, ext(msk))
            msk90<- crop(msk, p90)



        # mask shadows
            p90 <- mask(p90, msk90)

        #calulate spectral indices
            p90<-AddBands(p90)

    #is subsetting needed for NA layers?
                    #plot them to confirm
        png(file.path("Figures","LandsatBands_21.png"), height = 8.3, width = 11.7, units = 'in', res = 300)      
            terra::plot(p21, colNA="grey")
        dev.off()
        png(file.path("Figures","LandsatBands_07.png"), height = 8.3, width = 11.7, units = 'in', res = 300)      
            terra::plot(p07, colNA="grey")
        dev.off()
        png(file.path("Figures","LandsatBands_90Composite.png"), height = 8.3, width = 11.7, units = 'in', res = 300)      
            terra::plot(p90, colNA="grey")
        dev.off()

#3. save output
    #testing if i can retain band names using format argument
    #terra::writeRaster(p15, file.path("Outputs", "AllIndices_p15"), format="raster", overwrite=TRUE)
    terra::writeRaster(p21, file.path("Outputs", "AllIndices_p21.tif"), overwrite=TRUE)
    terra::writeRaster(p07, file.path("Outputs", "AllIndices_p07.tif"), overwrite=TRUE)
    terra::writeRaster(p90, file.path("Outputs", "AllIndices_p90.tif"), overwrite=TRUE)
