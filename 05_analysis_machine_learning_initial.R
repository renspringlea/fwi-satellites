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
df_wide_c <- read.csv("intermediate/df_clean.csv")

# Crucially, restrict to days 1 and 3
#df_wide_c <- df_wide_c[which(df_wide_c$day %in% c(1,3,4,5)),]

# Restrict based on cell numbers
df_wide_c <- df_wide_c[which(df_wide_c$cells_count>5 &
                               df_wide_c$cells_prop>0.5),]


# Calculate indices
df_wide_c$NDWI <- (df_wide_c$B3-df_wide_c$B8)/(df_wide_c$B3+df_wide_c$B8)
df_wide_c$NDCI <- (df_wide_c$B5-df_wide_c$B4)/(df_wide_c$B5+df_wide_c$B4)
df_wide_c$NDTI <- (df_wide_c$B4-df_wide_c$B3)/(df_wide_c$B4+df_wide_c$B3)
df_wide_c$NDMI <- (df_wide_c$B8-df_wide_c$B11)/(df_wide_c$B8+df_wide_c$B11)
df_wide_c$MNDWI <- (df_wide_c$B3-df_wide_c$B11)/(df_wide_c$B3+df_wide_c$B11)
df_wide_c$NDVI <- (df_wide_c$B8-df_wide_c$B4)/(df_wide_c$B8+df_wide_c$B4)

# Calculate summary statistics
numcols <- sapply(df_wide_c,FUN=is.numeric)
df_summarystats <- as.data.frame(rbind(sapply(df_wide_c[,numcols],FUN=min),
                                       colMeans(df_wide_c[,numcols]),
                                       sapply(df_wide_c[,numcols],FUN=max)))
df_summarystats

# Do some initial chlorophyll analysis
df_wide_c_13 <- df_wide_c[which(df_wide_c$day %in% c(1,3)),]
lm_chl <- lm(chl~NDCI+day,data=df_wide_c_13)
summary(lm_chl)
g_scatterplot_ndci <- ggplot(aes(x=chl,y=NDCI,colour=as.factor(day)),data=df_wide_c_13) +
  geom_point() +
  geom_smooth(method="lm",se=F)
g_scatterplot_ndci
ggsave("preliminary_analysis/g_scatterplot_ndci.png",g_scatterplot_ndci,width=6,height=3)


# Generate grids
xgb_grid <- expand.grid(nrounds = 500,
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
unique_ponds <- unique(df_wide_c$pond)
set.seed(3)
trainIndex_3 <- sample(df_wide_c$pond, round(length(unique_ponds)))
dfTrain_3 <- df_wide_c[which(df_wide_c$pond %in% trainIndex_3),]
dfTest_3 <- df_wide_c[-which(df_wide_c$pond %in% trainIndex_3),]
nrow(dfTrain_3); nrow(dfTest_3)
writeLines("Seed = 3","results_3/seeds.txt")
write("Train ponds: ","results_3/seeds.txt",append=T)
write(paste(trainIndex_3,collapse=" "),"results_3/seeds.txt",append=T)
write(paste0("(",nrow(dfTrain_3)," rows)"),"results_3/seeds.txt",append=T)
write("Test ponds: ","results_3/seeds.txt",append=T)
write(paste(unique(dfTest_3$pond),collapse=" "),"results_3/seeds.txt",append=T)
write(paste0("(",nrow(dfTest_3)," rows)"),"results_3/seeds.txt",append=T)
set.seed(12)
trainIndex_12 <- sample(df_wide_c$pond, round(length(unique_ponds)))
dfTrain_12 <- df_wide_c[which(df_wide_c$pond %in% trainIndex_12),]
dfTest_12 <- df_wide_c[-which(df_wide_c$pond %in% trainIndex_12),]
nrow(dfTrain_12); nrow(dfTest_12)
write("Seed = 12","results_3/seeds.txt",append=T)
write("Train ponds: ","results_3/seeds.txt",append=T)
write(paste(trainIndex_12,collapse=" "),"results_3/seeds.txt",append=T)
write(paste0("(",nrow(dfTrain_12)," rows)"),"results_3/seeds.txt",append=T)
write("Test ponds: ","results_3/seeds.txt",append=T)
write(paste(unique(dfTest_12$pond),collapse=" "),"results_3/seeds.txt",append=T)
write(paste0("(",nrow(dfTest_12)," rows)"),"results_3/seeds.txt",append=T)

# Train machine learning models for chlorophyll

# Continuous, seed 3
xgb_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_3,
                               method="xgbTree",
                               tuneGrid=xgb_grid,
                               verbosity=0,
                               tree_method="hist",
                              ncores=5)

df_xgb_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                       prediction = predict(xgb_chl_3_continuous,newdata=dfTest_3))
metrics_xgb_chl_3_continuous <- postResample(pred=df_xgb_chl_3_continuous$prediction,obs=df_xgb_chl_3_continuous$observation)
saveRDS(xgb_chl_3_continuous,"results_3/xgb_chl_3_continuous")
write(metrics_xgb_chl_3_continuous,"results_3/xgb_chl_3_continuous.txt")
write.csv(df_xgb_chl_3_continuous,"results_3/df_xgb_chl_3_continuous.csv",row.names=F)

svm_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_3,
                               method="svmRadial",
                               tuneGrid=svm_grid,
                               preProcess = c("center","scale"),
                               tuneLength=10,
                              ncores=5)
df_svm_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                       prediction = predict(svm_chl_3_continuous,newdata=dfTest_3))
metrics_svm_chl_3_continuous <- postResample(pred=df_svm_chl_3_continuous$prediction,obs=df_svm_chl_3_continuous$observation)
saveRDS(svm_chl_3_continuous,"results_3/svm_chl_3_continuous")
write(metrics_svm_chl_3_continuous,"results_3/svm_chl_3_continuous.txt")
write.csv(df_svm_chl_3_continuous,"results_3/df_svm_chl_3_continuous.csv",row.names=F)

ann_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_3,
                               method="avNNet",
                               tuneGrid=ann_grid,
                               preProcess = c("center","scale"),
                               trace=FALSE)
df_ann_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                       prediction = predict(ann_chl_3_continuous,newdata=dfTest_3))
metrics_ann_chl_3_continuous <- postResample(pred=df_ann_chl_3_continuous$prediction,obs=df_ann_chl_3_continuous$observation)
saveRDS(ann_chl_3_continuous,"results_3/ann_chl_3_continuous")
write(metrics_ann_chl_3_continuous,"results_3/ann_chl_3_continuous.txt")
write.csv(df_ann_chl_3_continuous,"results_3/df_ann_chl_3_continuous.csv",row.names=F)

# Continuous, seed 12
xgb_chl_12_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                                data=dfTrain_12,
                                method="xgbTree",
                                tuneGrid=xgb_grid,
                                verbosity=0,
                                tree_method="hist",
                               ncores=5)
df_xgb_chl_12_continuous <- data.frame(observation = dfTest_12$chlorophyll,
                                        prediction = predict(xgb_chl_12_continuous,newdata=dfTest_12))
metrics_xgb_chl_12_continuous <- postResample(pred=df_xgb_chl_12_continuous$prediction,obs=df_xgb_chl_12_continuous$observation)
saveRDS(xgb_chl_12_continuous,"results_3/xgb_chl_12_continuous")
write(metrics_xgb_chl_12_continuous,"results_3/xgb_chl_12_continuous.txt")
write.csv(df_xgb_chl_12_continuous,"results_3/df_xgb_chl_12_continuous.csv",row.names=F)

svm_chl_12_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                                data=dfTrain_12,
                                method="svmRadial",
                                tuneGrid=svm_grid,
                                preProcess = c("center","scale"),
                                tuneLength=10)
df_svm_chl_12_continuous <- data.frame(observation = dfTest_12$chlorophyll,
                                        prediction = predict(svm_chl_12_continuous,newdata=dfTest_12))
metrics_svm_chl_12_continuous <- postResample(pred=df_svm_chl_12_continuous$prediction,obs=df_svm_chl_12_continuous$observation)
saveRDS(svm_chl_12_continuous,"results_3/svm_chl_12_continuous")
write(metrics_svm_chl_12_continuous,"results_3/svm_chl_12_continuous.txt")
write.csv(df_svm_chl_12_continuous,"results_3/df_svm_chl_12_continuous.csv",row.names=F)

ann_chl_12_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                                data=dfTrain_12,
                                method="avNNet",
                                tuneGrid=ann_grid,
                               preProcess = c("center","scale"),
                                trace=FALSE)
df_ann_chl_12_continuous <- data.frame(observation = dfTest_12$chlorophyll,
                                      prediction = predict(ann_chl_12_continuous,newdata=dfTest_12))
metrics_ann_chl_12_continuous <- postResample(pred=df_ann_chl_12_continuous$prediction,obs=df_ann_chl_12_continuous$observation)
saveRDS(ann_chl_12_continuous,"results_3/ann_chl_12_continuous")
write(metrics_ann_chl_12_continuous,"results_3/ann_chl_12_continuous.txt")
write.csv(df_ann_chl_12_continuous,"results_3/df_ann_chl_12_continuous.csv",row.names=F)

ggplot(aes(x=observation,y=prediction),data=df_xgb_chl_3_continuous) +
  geom_point()
ggplot(aes(x=observation,y=prediction),data=df_xgb_chl_12_continuous) +
  geom_point()

# Train machine learning models for DO

# Continuous, seed 3
xgb_do_3_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="xgbTree",
                              tuneGrid=xgb_grid,
                              verbosity=0,
                              tree_method="hist",
                              ncores=5)
df_xgb_do_3_continuous <- data.frame(observation = dfTest_3$do,
                                      prediction = predict(xgb_do_3_continuous,newdata=dfTest_3))
metrics_xgb_do_3_continuous <- postResample(pred=df_xgb_do_3_continuous$prediction,obs=df_xgb_do_3_continuous$observation)
metrics_xgb_do_3_continuous
saveRDS(xgb_do_3_continuous,"results_3/xgb_do_3_continuous")
write(metrics_xgb_do_3_continuous,"results_3/xgb_do_3_continuous.txt")
write.csv(df_xgb_do_3_continuous,"results_3/df_xgb_do_3_continuous.csv",row.names=F)

svm_do_3_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="svmRadial",
                              tuneGrid=svm_grid,
                              preProcess = c("center","scale"),
                              tuneLength=10,
                              ncores=5)
df_svm_do_3_continuous <- data.frame(observation = dfTest_3$do,
                                      prediction = predict(svm_do_3_continuous,newdata=dfTest_3))
metrics_svm_do_3_continuous <- postResample(pred=df_svm_do_3_continuous$prediction,obs=df_svm_do_3_continuous$observation)
saveRDS(svm_do_3_continuous,"results_3/svm_do_3_continuous")
write(metrics_svm_do_3_continuous,"results_3/svm_do_3_continuous.txt")
write.csv(df_svm_do_3_continuous,"results_3/df_svm_do_3_continuous.csv",row.names=F)

ann_do_3_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="avNNet",
                              tuneGrid=ann_grid,
                              preProcess = c("center","scale"),
                              trace=FALSE)
df_ann_do_3_continuous <- data.frame(observation = dfTest_3$do,
                                      prediction = predict(ann_do_3_continuous,newdata=dfTest_3))
metrics_ann_do_3_continuous <- postResample(pred=df_ann_do_3_continuous$prediction,obs=df_ann_do_3_continuous$observation)
saveRDS(ann_do_3_continuous,"results_3/ann_do_3_continuous")
write(metrics_ann_do_3_continuous,"results_3/ann_do_3_continuous.txt")
write.csv(df_ann_do_3_continuous,"results_3/df_ann_do_3_continuous.csv",row.names=F)

# Continuous, seed 12
xgb_do_12_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="xgbTree",
                               tuneGrid=xgb_grid,
                               verbosity=0,
                               tree_method="hist",
                               ncores=5)
df_xgb_do_12_continuous <- data.frame(observation = dfTest_12$do,
                                       prediction = predict(xgb_do_12_continuous,newdata=dfTest_12))
metrics_xgb_do_12_continuous <- postResample(pred=df_xgb_do_12_continuous$prediction,obs=df_xgb_do_12_continuous$observation)
saveRDS(xgb_do_12_continuous,"results_3/xgb_do_12_continuous")
write(metrics_xgb_do_12_continuous,"results_3/xgb_do_12_continuous.txt")
write.csv(df_xgb_do_12_continuous,"results_3/df_xgb_do_12_continuous.csv",row.names=F)

svm_do_12_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="svmRadial",
                               tuneGrid=svm_grid,
                               preProcess = c("center","scale"),
                               tuneLength=10)
df_svm_do_12_continuous <- data.frame(observation = dfTest_12$do,
                                       prediction = predict(svm_do_12_continuous,newdata=dfTest_12))
metrics_svm_do_12_continuous <- postResample(pred=df_svm_do_12_continuous$prediction,obs=df_svm_do_12_continuous$observation)
saveRDS(svm_do_12_continuous,"results_3/svm_do_12_continuous")
write(metrics_svm_do_12_continuous,"results_3/svm_do_12_continuous.txt")
write.csv(df_svm_do_12_continuous,"results_3/df_svm_do_12_continuous.csv",row.names=F)

ann_do_12_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="avNNet",
                               tuneGrid=ann_grid,
                               preProcess = c("center","scale"),
                               trace=FALSE)
df_ann_do_12_continuous <- data.frame(observation = dfTest_12$do,
                                       prediction = predict(ann_do_12_continuous,newdata=dfTest_12))
metrics_ann_do_12_continuous <- postResample(pred=df_ann_do_12_continuous$prediction,obs=df_ann_do_12_continuous$observation)
saveRDS(ann_do_12_continuous,"results_3/ann_do_12_continuous")
write(metrics_ann_do_12_continuous,"results_3/ann_do_12_continuous.txt")
write.csv(df_ann_do_12_continuous,"results_3/df_ann_do_12_continuous.csv",row.names=F)



ggplot(aes(x=observation,y=prediction),data=df_ann_do_3_continuous) +
  geom_point()



xgb_grid <- expand.grid(nrounds = c(50,100,150),
                        max_depth=1,
                        eta = c(0.1),
                        gamma=c(1),
                        colsample_bytree=c(1),
                        min_child_weight=1,
                        subsample=c(0.5))
unique(xgb_chl_12_continuous_test$results$eta)

trc <- trainControl(method="repeatedcv",
                    number=10,
                    repeats=3)

xgb_do_12_continuous_test <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A,
                                   data=dfTrain_12,
                                   method="xgbTree",
                                   trControl = trc,
                                   #tuneGrid=xgb_grid,
                                   verbosity=0,
                                   tree_method="hist",
                                   ncores=5)
xgb_do_12_continuous_test$bestTune
View(xgb_do_12_continuous_test$results)

df_12_test <- data.frame(observation = dfTest_12$do,
                         prediction = predict(xgb_do_12_continuous_test,newdata=dfTest_12))
postResample(pred=df_12_test$prediction,obs=df_12_test$observation)
ggplot(aes(x=observation,y=prediction),data=df_12_test) +
  geom_point()


xgb_grid <- expand.grid(nrounds = c(50,100,150),
                        max_depth=c(6),
                        eta = c(0.2),
                        gamma=c(1,3),
                        colsample_bytree=c(1),
                        min_child_weight=c(1),
                        subsample=0.8)


xgb_chl_3_continuous_test <- train(chlorophyll~B4+B5+day,
                                    data=dfTrain_3,
                                    method="xgbTree",
                                    trControl = trc,
                                    #tuneGrid=xgb_grid,
                                    verbosity=0,
                                    tree_method="hist",
                                    ncores=5)
xgb_chl_3_continuous_test$results$RMSE
xgb_chl_3_continuous_test$results$Rsquared

df_3_test_chl <- data.frame(observation = dfTest_3$chlorophyll,
                             prediction = predict(xgb_chl_3_continuous_test,newdata=dfTest_3))
postResample(pred=df_3_test_chl$prediction,obs=df_3_test_chl$observation)
ggplot(aes(x=observation,y=prediction),data=df_3_test_chl) +
  geom_point()

df_wide_c$NDCI


xgb_chl_12_continuous_test <- train(chlorophyll~B4+B5,
                                   data=dfTrain_12,
                                   method="xgbTree",
                                   trControl = trc,
                                   #tuneGrid=xgb_grid,
                                   verbosity=0,
                                   tree_method="hist",
                                   ncores=5)

xgb_chl_12_continuous_test
xgb_chl_12_continuous_test$results$Rsquared
df_12_test_chl <- data.frame(observation = dfTest_12$chlorophyll,
                         prediction = predict(xgb_chl_12_continuous_test,newdata=dfTest_12))
postResample(pred=df_12_test_chl$prediction,obs=df_12_test_chl$observation)
ggplot(aes(x=observation,y=prediction),data=df_12_test_chl) +
  geom_point()




cor(xgb_chl_3_continuous_test$results$Rsquared,
    xgb_chl_3_continuous_test$results$RMSE)
cor(xgb_chl_3_continuous_test$results$Rsquared,
    xgb_chl_3_continuous_test$results$MAE)
cor(xgb_chl_3_continuous_test$results$RMSE,
    xgb_chl_3_continuous_test$results$MAE)

bandvec <-c("B2","B3","B4","B5","B6","B7","B8","B8A")
highlyCor <- findCorrelation(cor(df_wide_c[,]),
                             cutoff=0.95)
bandvec[highlyCor]

set.seed(312)
trainIndex <- createDataPartition(df_wide_c$chl, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dfTrain_312 <- df_wide_c[trainIndex,]
dfTest_312 <- df_wide_c[-trainIndex,]

xgb_chl_312_continuous_test <- train(chl~NDCI+B2+B3+B4+B5+B6+B8A+day,
                                    data=dfTrain_312,
                                    method="xgbTree",
                                    trControl = trc,
                                    #tuneGrid=xgb_grid,
                                    verbosity=0,
                                    tree_method="hist",
                                    ncores=5)
xgb_chl_312_continuous_test <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+day,
                                     data=dfTrain_312,
                                     method="xgbTree",
                                     trControl = trc,
                                     tuneGrid=xgb_grid,
                                     verbosity=0,
                                     tree_method="hist",
                                     ncores=5)
xgb_chl_312_continuous_test <- train(chlorophyll~B2+B3+B4+B5+B6+B8A,
                                    data=dfTrain_312,
                                    method="svmRadial",
                                    trControl = trc,
                                    tuneGrid=svm_grid,
                                    preProcess = c("center","scale"),
                                    tuneLength=10)
xgb_chl_312_continuous_test <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI,
                                     data=dfTrain_312,
                                     method="svmRadial",
                                     trControl = trc,
                                     tuneGrid=svm_grid,
                                     preProcess = c("center","scale"),
                                     tuneLength=10)
xgb_chl_312_continuous_test <- train(chlorophyll~B2+B3+B4+B5+B6+B7+B8+B8A,
                                    data=dfTrain_312,
                                    method="avNNet",
                                    trControl = trc,
                                    tuneGrid=ann_grid,
                                    preProcess = c("center","scale"),
                                    trace=FALSE)

xgb_chl_312_continuous_test
xgb_chl_312_continuous_test$results$Rsquared
df_312_test_chl <- data.frame(observation = dfTest_312$chl,
                             prediction = predict(xgb_chl_312_continuous_test,newdata=dfTest_312))
postResample(pred=df_312_test_chl$prediction,obs=df_312_test_chl$observation)
ggplot(aes(x=observation,y=prediction),data=df_312_test_chl) +
  geom_point() +
  geom_abline(intercept=0, slope=1)


lm_chl_312 <- lm(do~B2+B3+B4+B5+B6+B7+B8+B8A,
                 data=dfTrain_312)
lm_chl_312 <- lm(chlorophyll~B2+B3+B4+B5+B6+B7+B8+B8A,
                 data=dfTrain_312)
lm_chl_312 <- lm(chlorophyll~I(B5/B4),
                 data=dfTrain_312)
summary(lm_chl_312)

xgb_grid <- expand.grid(nrounds = 500,
                        max_depth = c(2,4,6,8,10,14),
                        eta = c(0.01,0.05,0.1),
                        gamma=c(0.1,1,10),
                        colsample_bytree=c(0.25,0.5,0.75,0.1),
                        min_child_weight=c(1,10),
                        #min_child_weight=c(1,10,100),
                        subsample=c(0.25))
xgb_do_312_continuous_test <- train(do~B2+B3+B4+B5+B6+B8A+day,
                                     data=dfTrain_312,
                                     method="xgbTree",
                                     trControl = trc,
                                     tuneGrid=xgb_grid,
                                     verbosity=0,
                                     tree_method="hist",
                                     ncores=5)



sf_rr <- function(data,lev=NULL,model=NULL){
  pred <- data$pred
  obs <- data$obs
  RMSE_ren <- sqrt(mean((pred - obs)^2))
  R2_ren <- cor(pred,obs,use="pairwise.complete.obs")^2
  one_minus_R2_ren <- 1-R2_ren
  combined_metric <- RMSE_ren*one_minus_R2_ren
  names(RMSE_ren) <- "rr"
  RMSE_ren
}
trc <- trainControl(method="repeatedcv",
                    number=10,
                    repeats=3)#,
                    #summaryFunction=sf_rr)
xgb_grid2 <- xgb_grid[which(xgb_grid$max_depth==2 & xgb_grid$min_child_weight==1),]
xgb_do_312_continuous_test <- train(do~B2+B3+B4+B5+B8+day,
                                    data=dfTrain_312,
                                    method="xgbTree",
                                    trControl = trc,
                                    tuneGrid=xgb_grid,
                                    verbosity=0,
                                    tree_method="hist",
                                    ncores=5#,
                                    #metric="rr",
                                    
                                    #maximize=F
                                    )



xgb_do_312_continuous_test
#View(xgb_do_312_continuous_test$results)
xgb_do_312_continuous_test$results$Rsquared
df_312_test_do <- data.frame(observation = dfTest_312$do,
                              prediction = predict(xgb_do_312_continuous_test,newdata=dfTest_312))
postResample(pred=df_312_test_do$prediction,obs=df_312_test_do$observation)
ggplot(aes(x=observation,y=prediction),data=df_312_test_do) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  xlim(2,8) +
  ylim(2,8)




xgb_do_312_continuous_test <- train(do~B2+B3+B4+B5+B6+B8A,
                                    data=dfTrain_312,
                                    method="svmRadial",
                                    trControl = trc,
                                    tuneGrid=svm_grid,
                                    preProcess = c("center","scale"),
                                    tuneLength=10)
xgb_do_312_continuous_test <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A,
                                    data=dfTrain_312,
                                    method="avNNet",
                                    trControl = trc,
                                    tuneGrid=ann_grid,
                                    preProcess = c("center","scale"),
                                    trace=FALSE)