#To correct for environmental noise, this function smoothes NDVI values
#derived from MODIS data, following the method established by Garonna and col­
#leagues (Garonna et al., 2009). Specifically, the data for each pixel is
#checked for rapid decreases or increases (a difference of 0.3 or more
#from one date to the next) that are immediately followed by a rapid
#return to previous values. These drops in NDVI are attributed to
#environmental noise and are replaced by the average of the previous and
#following values to ‘smooth’ the annual NDVI curve for that pixel. If two
#consecutive contaminated values are present, the average of the
#closest NDVI values is calculated.

library(terra)
library(tidyverse)

    #load project functions
        function_files<-list.files(file.path("Code","Functions"))
        sapply(file.path("Code","Functions",function_files),source)


#load data
    modislist<-list.files("Data/MODIS", full.names=TRUE)
    modislist<-modislist[1:538] #taking first half of the modis tifs (second half are quality docs)
    modisMonthlylist<-list.files("Data/MODIS_Monthly", full.names=TRUE)
    modisMonthlylist<-modisMonthlylist[1:281] #taking first half of the modis tifs (second half are quality docs)
    
    #check data integrity - 16 day
        days<-str_split(modislist, pattern="_", simplify=TRUE)[,7]
        smoothedlayer_df<-data.frame(year=substr(days, 4, 7) , dayOfYear=substr(days, 8, 10) ) %>%
            as_tibble %>% 
            mutate(date=as.Date(as.integer(dayOfYear), origin="2000-01-01")) %>%
            mutate(day=substr(date, 9,10), month=substr(date, 6, 7)) %>%
            select (-date)
        smoothedlayer_df %>%
            group_by(year) %>%
            summarise(n=n()) %>% print(n=99)

    #check data integrity - monthly
        days<-str_split(modisMonthlylist, pattern="_", simplify=TRUE)[,8]
        monthlylayer_df<-data.frame(year=substr(days, 4, 7) , dayOfYear=substr(days, 8, 10) ) %>%
            as_tibble %>% 
            mutate(date=as.Date(as.integer(dayOfYear), origin="2000-01-01")) %>%
            mutate(month=substr(date, 6, 7)) %>%
            select (-date)
        monthlylayer_df %>%
            group_by(year) %>%
            summarise(n=n()) %>% print(n=99)

    #create spatraster
        modis<-terra::rast(modislist)
        monthly<-terra::rast(modisMonthlylist)

#run denoising and sothing process
    #mask everything below value 3
        #modis_NoiseRemoved1<-terra::app(modis, RemoveCellsBasedOnQuality)

    #remove noisy cells
        modis_NoiseRemoved2<-terra::app(modis, RemoveNoisyNdviCells)

    #smooth 
        modis_smoothed<-terra::approximate(modis_NoiseRemoved2, method="linear", rule=1)

    #remove entire pixels with too much noise
        modis_smoothed_problempixelsremoved<-RemoveProblemPixels(modis_NoiseRemoved2, modis_smoothed, 2)

    #add layer names to layer df
        smoothedlayer_df$LayerName<-names(modis_smoothed_problempixelsremoved)

# get layer names for monthly
    #add layer names to layer df
        monthlylayer_df$LayerName<-names(monthly)

#save
    terra::writeRaster(modis_smoothed_problempixelsremoved, file.path("Outputs", "MODIS_smoothed.tif"), overwrite=TRUE)
    saveRDS(smoothedlayer_df, file.path("Outputs", "MODIS_smoothed_layer_df.RDS"))

    terra::writeRaster(monthly, file.path("Outputs", "MODIS_monthly.tif"), overwrite=TRUE)
    saveRDS(monthlylayer_df, file.path("Outputs", "MODIS_monthly_layer_df.RDS"))

#end of code
