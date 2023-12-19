#########################
# Conduct supervised classification
##########################

#1: Dependencies
    #packages
        library(terra)
        library(randomForest)
        library(tidyverse)

    #load project functions
        function_files<-list.files(file.path("Code","Functions"))
        sapply(file.path("Code","Functions",function_files),source)

    # Parameters


#2. Load Data
    # Importing satellite data #
        p21<-terra::rast(file.path("Outputs", "AllIndices_p21.tif"))
        p07<-terra::rast(file.path("Outputs", "AllIndices_p07.tif"))


    # Load training and validation points #
        TestTrain<-readRDS( file.path("Outputs", "TestTrainPoints.RDS") )

#3. Run Classifications and save output
    #fit random forests and classify
        #2019
            #tune rf
                model_tuned <- randomForest::tuneRF(
                    x=select(TestTrain[["train1921"]][["PointVal_df"]], !response), #define predictor variables
                    y=TestTrain[["train1921"]][["PointVal_df"]]$response, #define response variable
                    ntreeTry=1001,
                    mtryStart=3, 
                    stepFactor=1.5,
                    improve=0.001,
                    trace=TRUE #don't show real-time progress
                )
            #optimum mtry =3

            #error rates stabilise by 500 trees - visible with plot(mod1921)
            set.seed(1) 
            mod1921 <- randomForest::randomForest(x=TestTrain[["train1921"]][["PointVal_df"]][-1], 
                                                xtest= TestTrain[["test1921"]][["PointVal_df"]][-1] , 
                                                y=TestTrain[["train1921"]][["PointVal_df"]]$response, 
                                                ytest= TestTrain[["test1921"]][["PointVal_df"]]$response , 
                                                na.action=na.omit, 
                                                ntree=2001, 
                                                mtry=4, 
                                                confusion=TRUE)

                mod1921[["test"]][["confusion"]][,-6] %>%
                    write.csv("Figures/TableS2B.csv")

            set.seed(1) 
            mod1921 <- randomForest::randomForest(response ~ ., data=TestTrain[["train1921"]][["PointVal_df"]], na.action=na.omit, ntree=2001, mtry=4, confusion=TRUE)
            sc19 <- terra::predict(object=p21, 
                        model=mod1921, 
                        type="response",
                        filename=file.path("Outputs", "sc1921.tif"), 
                        format="GTiff", 
                        datatype="INT1U",
                        overwrite=TRUE)
        #2007
            #tune rf
                model_tuned <- randomForest::tuneRF(
                    x=select(TestTrain[["train07"]][["PointVal_df"]], !response), #define predictor variables
                    y=TestTrain[["train07"]][["PointVal_df"]]$response, #define response variable
                    ntreeTry=1001,
                    mtryStart=3, 
                    stepFactor=1.5,
                    improve=0.001,
                    trace=TRUE #don't show real-time progress
                )
            #optimum mtry =3

            #error rates stabilise by 500 trees - visible with plot(mod07)
            mod07 <- randomForest::randomForest(x=TestTrain[["train07"]][["PointVal_df"]][-1], 
                                                xtest= TestTrain[["test07"]][["PointVal_df"]][-1] , 
                                                y=TestTrain[["train07"]][["PointVal_df"]]$response, 
                                                ytest= TestTrain[["test07"]][["PointVal_df"]]$response , 
                                                na.action=na.omit, 
                                                ntree=2001, 
                                                mtry=3, 
                                                confusion=TRUE)

            mod07[["test"]][["confusion"]][,-6] %>%
                write.csv("Figures/TableS2A.csv")      

            set.seed(1) 
            mod07 <- randomForest::randomForest(response ~ ., data=TestTrain[["train07"]][["PointVal_df"]], na.action=na.omit, ntree=2001, mtry=3, confusion=TRUE)
            sc07 <- terra::predict(object=p07, 
                                    model=mod07, 
                                    type="response",
                                    filename=file.path("Outputs", "sc07.tif"), 
                                    format="GTiff", 
                                    datatype="INT1U",
                                    overwrite=TRUE)

#4. save outputs
    #saving random forest models - (classification rasters written to file by functions)
        saveRDS(list(mod07,mod1921), file.path("Outputs", "RandomForestModels.RDS"))
