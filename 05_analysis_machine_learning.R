#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(caret) #for neural networks
library(gridExtra) #to help graphing
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable

# Load data
df_wide_c_cats <- read.csv("intermediate/df_wide_c_cats.csv")
df_wide_c_cats$chlorophyll_category <- as.factor(df_wide_c_cats$chlorophyll_category)
df_wide_c_cats$do_category <- factor(df_wide_c_cats$do_category,
                                     levels=c("bad","moderate","good"))

# Calculate indices
df_wide_c_cats$NDWI <- (df_wide_c_cats$B3-df_wide_c_cats$B8)/(df_wide_c_cats$B3+df_wide_c_cats$B8)
df_wide_c_cats$NDCI <- (df_wide_c_cats$B5-df_wide_c_cats$B4)/(df_wide_c_cats$B5+df_wide_c_cats$B4)
df_wide_c_cats$NDTI <- (df_wide_c_cats$B4-df_wide_c_cats$B3)/(df_wide_c_cats$B4+df_wide_c_cats$B3)
df_wide_c_cats$NDMI <- (df_wide_c_cats$B8-df_wide_c_cats$B11)/(df_wide_c_cats$B8+df_wide_c_cats$B11)
df_wide_c_cats$MNDWI <- (df_wide_c_cats$B3-df_wide_c_cats$B11)/(df_wide_c_cats$B3+df_wide_c_cats$B11)
df_wide_c_cats$NDVI <- (df_wide_c_cats$B8-df_wide_c_cats$B4)/(df_wide_c_cats$B8+df_wide_c_cats$B4)

# Generate grids
xgb_grid <- expand.grid(nrounds = 1000,
                        max_depth = c(2,4,6,8,10,14),
                        eta = c(0.01,0.05,0.1),
                        gamma=c(0.1,1,10),
                        colsample_bytree=c(0.25,0.5,0.75,0.1),
                        min_child_weight=c(1,10,100),
                        subsample=c(0.25,0.5,0.75))
#xgb_grid <- xgb_grid[1,] #For debugging
svm_grid <- expand.grid(C=c(0.25,0.5,1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,32,64,100,150,200),
                        sigma=c(0.01,0.05,0.1,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,2,5))
#svm_grid <- svm_grid[1,] #For debugging
rf_grid <- expand.grid(nsets=c(10,20,30,40,50,60,70,80,90,100,150,500),
                       ntreeperdiv=c(10,20,30,40,50,60,70,80,90,100,150,500),
                       ntreefinal=c(10,100,200,300,400,500))
# rf_grid <- rf_grid[1,] #For debugging
ann_grid <- expand.grid(size=c(1:10),
                        decay=c(0.001,0.01,0.05,0.1,0.15,0.2,0.3,0.5),
                        bag=c(TRUE,FALSE))
#ann_grid <- ann_grid[1,] #For debugging

# Generate training and test datasets
# for a couple of different seeds
unique_ponds <- unique(df_wide_c_cats$pond)
set.seed(3)
trainIndex_3 <- sample(df_wide_c_cats$pond, round(0.5*length(unique_ponds)))
dfTrain_3 <- df_wide_c_cats[which(df_wide_c_cats$pond %in% trainIndex_3),]
dfTest_3 <- df_wide_c_cats[-which(df_wide_c_cats$pond %in% trainIndex_3),]
writeLines("Seed = 3","results/seeds.txt")
write("Train ponds: ","results/seeds.txt",append=T)
write(paste(trainIndex_3,collapse=" "),"results/seeds.txt",append=T)
write(paste0("(",nrow(dfTrain_3)," rows)"),"results/seeds.txt",append=T)
write("Test ponds: ","results/seeds.txt",append=T)
write(paste(unique(dfTest_3$pond),collapse=" "),"results/seeds.txt",append=T)
write(paste0("(",nrow(dfTest_3)," rows)"),"results/seeds.txt",append=T)
set.seed(12)
trainIndex_12 <- sample(df_wide_c_cats$pond, round(0.5*length(unique_ponds)))
dfTrain_12 <- df_wide_c_cats[which(df_wide_c_cats$pond %in% trainIndex_12),]
dfTest_12 <- df_wide_c_cats[-which(df_wide_c_cats$pond %in% trainIndex_12),]
write("Seed = 12","results/seeds.txt",append=T)
write("Train ponds: ","results/seeds.txt",append=T)
write(paste(trainIndex_12,collapse=" "),"results/seeds.txt",append=T)
write(paste0("(",nrow(dfTrain_12)," rows)"),"results/seeds.txt",append=T)
write("Test ponds: ","results/seeds.txt",append=T)
write(paste(unique(dfTest_12$pond),collapse=" "),"results/seeds.txt",append=T)
write(paste0("(",nrow(dfTest_12)," rows)"),"results/seeds.txt",append=T)

# Train machine learning models
# Categorical, seed 3
xgb_chl_3_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                   data=dfTrain_3,
                   method="xgbTree",
                   tuneGrid=xgb_grid,
                   verbosity=0,
                   tree_method="hist",
                   metric="Kappa")
df_xgb_chl_3_categorical <- data.frame(observation = dfTest_3$chlorophyll_category,
                           prediction = predict(xgb_chl_3_categorical,newdata=dfTest_3))
metrics_xgb_chl_3_categorical <- postResample(pred=df_xgb_chl_3_categorical$prediction,obs=df_xgb_chl_3_categorical$observation)
saveRDS(xgb_chl_3_categorical,"results/xgb_chl_3_categorical")
write(metrics_xgb_chl_3_categorical,"results/xgb_chl_3_categorical.txt")
write.csv(df_xgb_chl_3_categorical,"results/df_xgb_chl_3_categorical.csv",row.names=F)

svm_chl_3_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                   data=dfTrain_3,
                   method="svmRadial",
                   tuneGrid=svm_grid,
                   preProcess = c("center","scale"),
                   tuneLength=10,
                   metric = "Kappa")
df_svm_chl_3_categorical <- data.frame(observation = dfTest_3$chlorophyll_category,
                           prediction = predict(svm_chl_3_categorical,newdata=dfTest_3))
metrics_svm_chl_3_categorical <- postResample(pred=df_svm_chl_3_categorical$prediction,obs=df_svm_chl_3_categorical$observation)
saveRDS(svm_chl_3_categorical,"results/svm_chl_3_categorical")
write(metrics_svm_chl_3_categorical,"results/svm_chl_3_categorical.txt")
write.csv(df_svm_chl_3_categorical,"results/df_svm_chl_3_categorical.csv",row.names=F)

rf_chl_3_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                  data=dfTrain_3,
                  method="ordinalRF",
                  tuneGrid=rf_grid,
                  preProcess = c("center","scale"),
                  metric = "Kappa")
df_rf_chl_3_categorical <- data.frame(observation = dfTest_3$chlorophyll_category,
                           prediction = predict(rf_chl_3_categorical,newdata=dfTest_3))
metrics_rf_chl_3_categorical <- postResample(pred=df_rf_chl_3_categorical$prediction,obs=df_rf_chl_3_categorical$observation)
saveRDS(rf_chl_3_categorical,"results/rf_chl_3_categorical")
write(metrics_rf_chl_3_categorical,"results/rf_chl_3_categorical.txt")
write.csv(df_rf_chl_3_categorical,"results/df_rf_chl_3_categorical.csv",row.names=F)

ann_chl_3_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                   data=dfTrain_3,
                   method="avNNet",
                   metric="Kappa",
                   tuneGrid=ann_grid,
                   
                   preProcess = c("center","scale"),
                   trace=FALSE)
df_ann_chl_3_categorical <- data.frame(observation = dfTest_3$chlorophyll_category,
                                      prediction = predict(ann_chl_3_categorical,newdata=dfTest_3))
metrics_ann_chl_3_categorical <- postResample(pred=df_ann_chl_3_categorical$prediction,obs=df_ann_chl_3_categorical$observation)
saveRDS(ann_chl_3_categorical,"results/ann_chl_3_categorical")
write(metrics_ann_chl_3_categorical,"results/ann_chl_3_categorical.txt")
write.csv(df_ann_chl_3_categorical,"results/df_ann_chl_3_categorical.csv",row.names=F)

# Categorical, seed 12
xgb_chl_12_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                   data=dfTrain_12,
                   method="xgbTree",
                   tuneGrid=xgb_grid,
                   verbosity=0,
                   tree_method="hist",
                   metric="Kappa")
df_xgb_chl_12_categorical <- data.frame(observation = dfTest_12$chlorophyll_category,
                           prediction = predict(xgb_chl_12_categorical,newdata=dfTest_12))
metrics_xgb_chl_12_categorical <- postResample(pred=df_xgb_chl_12_categorical$prediction,obs=df_xgb_chl_12_categorical$observation)
saveRDS(xgb_chl_12_categorical,"results/xgb_chl_12_categorical")
write(metrics_xgb_chl_12_categorical,"results/xgb_chl_12_categorical")
write.csv(df_xgb_chl_12_categorical,"results/df_xgb_chl_12_categorical.csv",row.names=F)

svm_chl_12_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                   data=dfTrain_12,
                   method="svmRadial",
                   tuneGrid=svm_grid,
                   preProcess = c("center","scale"),
                   tuneLength=10,
                   metric = "Kappa")
df_svm_chl_12_categorical <- data.frame(observation = dfTest_12$chlorophyll_category,
                           prediction = predict(svm_chl_12_categorical,newdata=dfTest_12))
metrics_svm_chl_12_categorical <- postResample(pred=df_svm_chl_12_categorical$prediction,obs=df_svm_chl_12_categorical$observation)
saveRDS(svm_chl_12_categorical,"results/svm_chl_12_categorical")
write(metrics_svm_chl_12_categorical,"results/svm_chl_12_categorical.txt")
write.csv(df_svm_chl_12_categorical,"results/df_svm_chl_12_categorical.csv",row.names=F)

rf_chl_12_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                  data=dfTrain_12,
                  method="ordinalRF",
                  tuneGrid=rf_grid,
                  preProcess = c("center","scale"),
                  metric = "Kappa")
df_rf_chl_12_categorical <- data.frame(observation = dfTest_12$chlorophyll_category,
                          prediction = predict(rf_chl_12_categorical,newdata=dfTest_12))
metrics_rf_chl_12_categorical <- postResample(pred=df_rf_chl_12_categorical$prediction,obs=df_rf_chl_12_categorical$observation)
saveRDS(rf_chl_12_categorical,"results/rf_chl_12_categorical")
write(metrics_rf_chl_12_categorical,"results/rf_chl_12_categorical.txt")
write.csv(df_rf_chl_12_categorical,"results/df_rf_chl_12_categorical.csv",row.names=F)

ann_chl_12_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                   data=dfTrain_12,
                   method="avNNet",
                   metric="Kappa",
                   tuneGrid=ann_grid,
                   
                   preProcess = c("center","scale"),
                   trace=FALSE)
df_ann_chl_12_categorical <- data.frame(observation = dfTest_12$chlorophyll_category,
                                       prediction = predict(ann_chl_12_categorical,newdata=dfTest_12))
metrics_ann_chl_12_categorical <- postResample(pred=df_ann_chl_12_categorical$prediction,obs=df_ann_chl_12_categorical$observation)
saveRDS(ann_chl_12_categorical,"results/ann_chl_12_categorical")
write(metrics_ann_chl_12_categorical,"results/ann_chl_12_categorical.txt")
write.csv(df_ann_chl_12_categorical,"results/df_ann_chl_12_categorical.csv",row.names=F)

# Continuous, seed 3
xgb_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="xgbTree",
                               tuneGrid=xgb_grid,
                               verbosity=0,
                               tree_method="hist")
df_xgb_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                       prediction = predict(xgb_chl_3_continuous,newdata=dfTest_3))
metrics_xgb_chl_3_continuous <- postResample(pred=df_xgb_chl_3_continuous$prediction,obs=df_xgb_chl_3_continuous$observation)
saveRDS(xgb_chl_3_continuous,"results/xgb_chl_3_continuous")
write(metrics_xgb_chl_3_continuous,"results/xgb_chl_3_continuous.txt")
write.csv(df_xgb_chl_3_continuous,"results/df_xgb_chl_3_continuous.csv",row.names=F)

svm_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="svmRadial",
                               tuneGrid=svm_grid,
                               preProcess = c("center","scale"),
                               tuneLength=10)
df_svm_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                       prediction = predict(svm_chl_3_continuous,newdata=dfTest_3))
metrics_svm_chl_3_continuous <- postResample(pred=df_svm_chl_3_continuous$prediction,obs=df_svm_chl_3_continuous$observation)
saveRDS(svm_chl_3_continuous,"results/svm_chl_3_continuous")
write(metrics_svm_chl_3_continuous,"results/svm_chl_3_continuous.txt")
write.csv(df_svm_chl_3_continuous,"results/df_svm_chl_3_continuous.csv",row.names=F)

ann_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="avNNet",
                               tuneGrid=ann_grid,
                               preProcess = c("center","scale"),
                               trace=FALSE)
df_ann_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                       prediction = predict(ann_chl_3_continuous,newdata=dfTest_3))
metrics_ann_chl_3_continuous <- postResample(pred=df_ann_chl_3_continuous$prediction,obs=df_ann_chl_3_continuous$observation)
saveRDS(ann_chl_3_continuous,"results/ann_chl_3_continuous")
write(metrics_ann_chl_3_continuous,"results/ann_chl_3_continuous.txt")
write.csv(df_ann_chl_3_continuous,"results/df_ann_chl_3_continuous.csv",row.names=F)

# Continuous, seed 12
xgb_chl_12_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                                data=dfTrain_12,
                                method="xgbTree",
                                tuneGrid=xgb_grid,
                                verbosity=0,
                                tree_method="hist")
df_xgb_chl_12_continuous <- data.frame(observation = dfTest_12$chlorophyll,
                                        prediction = predict(xgb_chl_12_continuous,newdata=dfTest_12))
metrics_xgb_chl_12_continuous <- postResample(pred=df_xgb_chl_12_continuous$prediction,obs=df_xgb_chl_12_continuous$observation)
saveRDS(xgb_chl_12_continuous,"results/xgb_chl_12_continuous")
write(metrics_xgb_chl_12_continuous,"results/xgb_chl_12_continuous.txt")
write.csv(df_xgb_chl_12_continuous,"results/df_xgb_chl_12_continuous.csv",row.names=F)

svm_chl_12_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                                data=dfTrain_12,
                                method="svmRadial",
                                tuneGrid=svm_grid,
                                preProcess = c("center","scale"),
                                tuneLength=10)
df_svm_chl_12_continuous <- data.frame(observation = dfTest_12$chlorophyll,
                                        prediction = predict(svm_chl_12_continuous,newdata=dfTest_12))
metrics_svm_chl_12_continuous <- postResample(pred=df_svm_chl_12_continuous$prediction,obs=df_svm_chl_12_continuous$observation)
saveRDS(svm_chl_12_continuous,"results/svm_chl_12_continuous")
write(metrics_svm_chl_12_continuous,"results/svm_chl_12_continuous.txt")
write.csv(df_svm_chl_12_continuous,"results/df_svm_chl_12_continuous.csv",row.names=F)

ann_chl_12_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                                data=dfTrain_12,
                                method="avNNet",
                                tuneGrid=ann_grid,
                               preProcess = c("center","scale"),
                                trace=FALSE)
df_ann_chl_12_continuous <- data.frame(observation = dfTest_12$chlorophyll,
                                      prediction = predict(ann_chl_12_continuous,newdata=dfTest_12))
metrics_ann_chl_12_continuous <- postResample(pred=df_ann_chl_12_continuous$prediction,obs=df_ann_chl_12_continuous$observation)
saveRDS(ann_chl_12_continuous,"results/ann_chl_12_continuous")
write(metrics_ann_chl_12_continuous,"results/ann_chl_12_continuous.txt")
write.csv(df_ann_chl_12_continuous,"results/df_ann_chl_12_continuous.csv",row.names=F)



