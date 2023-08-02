MakeAccTab<-function(testDat){
    OverallProducerAccuracy<-function(){
    PA<-signif(sum(y==predictions)/length(y)*100, 3)
    return(PA)
    }

    ProducerAccuracy<- function(class){
        KnownToBeThisClass<-predictions[which(y==class)]
        PA<-signif(sum(KnownToBeThisClass==class)/length(KnownToBeThisClass)*100, 3)
        return(PA)
    }

    UserAccuracy<-function(class){
        PredictedToBeThisClass<-y[which(predictions==class)]
        UA<-signif(sum(PredictedToBeThisClass==class)/length(PredictedToBeThisClass)*100,3)
        return(UA)
    }

    if (identical(testDat,TestTrain[["test07"]])) {mod<-1} else {mod<-2}

    y<-as.vector(testDat[[1]][1])[[1]]
    predictions<-terra::predict(   rfMods[[mod]],  testDat[[1]][-1],)

    AccTab<-data.frame("Class"= c("Grassy", "Scruby", "Trees", "Rocky/Bare", "Water", "Overall"),
            "Producer accuracy" = c(ProducerAccuracy("G"), ProducerAccuracy("S"), ProducerAccuracy("T"), ProducerAccuracy("R"), ProducerAccuracy("W"),OverallProducerAccuracy()),
            "User accuracy" = c(UserAccuracy("G"), UserAccuracy("S"), UserAccuracy("T"), UserAccuracy("R"), UserAccuracy("W"),NA))

    AccTab["F1"]=signif(2/(1/AccTab[2]+ 1/AccTab[3]),3)
    return(AccTab)
}