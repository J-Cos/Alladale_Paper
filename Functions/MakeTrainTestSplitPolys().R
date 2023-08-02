#depends on terra

MakeTrainTestSplitPolys<-function(data, trainProportion){
    #load data

    myLayers<-sf::st_layers(file.path("Data", data))[["name"]][-1]
    
    polys<-list()
    for (layer in myLayers){
        polys[[layer]]<-sf::st_read( file.path("Data", data),layer) %>%
            terra::vect()
        polys[[layer]]$value<-substr(layer, 1, 1)
        polys[[layer]]<-polys[[layer]][,"value"]
    }

    train<-test<-list()
    for (i in 1:length(polys)){
        #split into train and test
        numPolysInClass<-length(polys[[i]])
        numPolysInTrain<-floor(numPolysInClass*trainProportion)
        TrainSplit<-1:numPolysInClass %in% sample(1:numPolysInClass, numPolysInTrain)
        train[[i]]<-polys[[i]][TrainSplit]
        test[[i]]<-polys[[i]][!TrainSplit]
    }

    CombinedTrain<-terra::vect(train)
    CombinedTest<-terra::vect(test)

    return(list("CombinedTrain"=CombinedTrain, "CombinedTest"=CombinedTest))
   }
