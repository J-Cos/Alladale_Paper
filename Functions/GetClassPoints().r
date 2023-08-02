#function to extract stratified points from a spatial polygons dataframe of n polygons
# returns a spatialpointsdataframe

  #terra
GetClassPoints<-function(data, polygons, MaxPointsPerPolygonClass=c("Other"=10)){

                #get stratiefied poitns from polygons
                print("Getting stratified points")
                  pts_list<-list()
                  for (i in 1:length(polygons)){
                    #get max points per polygon for this class of polygon
                      if ( polygons[i,]$value %in% names(MaxPointsPerPolygonClass) ) {
                        MaxPointsPerPolygon<-MaxPointsPerPolygonClass[as.character(polygons[i,]$class)]
                      } else {
                        MaxPointsPerPolygon<-MaxPointsPerPolygonClass["Other"]
                      }
                    # sample points
                      pts <- try(terra::spatSample(polygons[i], method = "random", size=MaxPointsPerPolygon))
                      if(class(pts)=="try-error") {return(print("StratifyingCellSize is too large for smallest polygon - there is some randomness in this process"))}
                      pts_list[[i]]<-pts
                    print(paste0("Polygon ", i , " (", polygons[i]$value, ") complete"))
                  }
                  allpts <- do.call("rbind", pts_list)

                #extract values from raster based on raster cell each point falls within
                print("Extracting values from raster cells")
                  trainingvals <- terra::extract(x=data, y=allpts, cells=TRUE, method="simple")
                  trainingvals <- data.frame(response = allpts$value, trainingvals)
                
                # remove raster cells that are selected multiple times
                print("Removing duplicated cells")
                  if (any(duplicated(trainingvals$cell))) {
                    print(paste0(sum(duplicated(trainingvals$cell)), " duplicated cells removed"))
                    allpts<-allpts[!duplicated(trainingvals$cell),]
                    trainingvals <- trainingvals[!duplicated(trainingvals$cell), -2]
                  }
                trainingvals <- select(trainingvals, -cell) %>%
                                  filter(complete.cases(.))
                #save point map for visual inspection
                  #png(file.path("Outputs","TrainingPoints.png"), height = 8.3, width = 11.7, units = 'in', res = 300)
                  #      terra::plot(allpts, col= c("water"="blue", "green"="green", "manmade"="black", "turf"="red"))
                  #dev.off()

                  #dir.create(file.path("Outputs","TrainingPointsShapeFile"), showWarnings=FALSE)
                  #raster::shapefile(allpts, filename=file.path("Outputs","TrainingPointsShapeFile", "TrainingPointsShapeFile"), overwrite=TRUE)

                #ensure class is a factor
                  trainingvals$response<-as.factor(trainingvals$response)
                  
                return( list( "PointVal_df"=trainingvals,
                              "NumberCellsPerCategory" = table(trainingvals$response),
                              "Points"=allpts))
                }