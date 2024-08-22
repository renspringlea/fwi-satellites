

# Continuous, seed 3
xgb_chl_3_continuous <- train(NDCI_b~chl+dark_day,
                              data=dfTrain_3,
                              method="xgbTree",
                              #tuneGrid=xgb_grid,
                              verbosity=0,
                              tree_method="hist",
                              ncores=5)
xgb_chl_3_continuous$results$Rsquared

df_xgb_chl_3_continuous <- data.frame(observation = dfTest_3$NDCI_b,
                                      prediction = predict(xgb_chl_3_continuous,newdata=dfTest_3))
metrics_xgb_chl_3_continuous <- postResample(pred=df_xgb_chl_3_continuous$prediction,obs=df_xgb_chl_3_continuous$observation)
metrics_xgb_chl_3_continuous
saveRDS(xgb_chl_3_continuous,"results/xgb_chl_3_continuous")
write(metrics_xgb_chl_3_continuous,"results/xgb_chl_3_continuous.txt")
write.csv(df_xgb_chl_3_continuous,"results/df_xgb_chl_3_continuous.csv",row.names=F)

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
saveRDS(svm_chl_3_continuous,"results/svm_chl_3_continuous")
write(metrics_svm_chl_3_continuous,"results/svm_chl_3_continuous.txt")
write.csv(df_svm_chl_3_continuous,"results/df_svm_chl_3_continuous.csv",row.names=F)

ann_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
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
                               tree_method="hist",
                               ncores=5)
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
saveRDS(xgb_do_3_continuous,"results/xgb_do_3_continuous")
write(metrics_xgb_do_3_continuous,"results/xgb_do_3_continuous.txt")
write.csv(df_xgb_do_3_continuous,"results/df_xgb_do_3_continuous.csv",row.names=F)

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
saveRDS(svm_do_3_continuous,"results/svm_do_3_continuous")
write(metrics_svm_do_3_continuous,"results/svm_do_3_continuous.txt")
write.csv(df_svm_do_3_continuous,"results/df_svm_do_3_continuous.csv",row.names=F)

ann_do_3_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                             data=dfTrain_3,
                             method="avNNet",
                             tuneGrid=ann_grid,
                             preProcess = c("center","scale"),
                             trace=FALSE)
df_ann_do_3_continuous <- data.frame(observation = dfTest_3$do,
                                     prediction = predict(ann_do_3_continuous,newdata=dfTest_3))
metrics_ann_do_3_continuous <- postResample(pred=df_ann_do_3_continuous$prediction,obs=df_ann_do_3_continuous$observation)
saveRDS(ann_do_3_continuous,"results/ann_do_3_continuous")
write(metrics_ann_do_3_continuous,"results/ann_do_3_continuous.txt")
write.csv(df_ann_do_3_continuous,"results/df_ann_do_3_continuous.csv",row.names=F)

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
saveRDS(xgb_do_12_continuous,"results/xgb_do_12_continuous")
write(metrics_xgb_do_12_continuous,"results/xgb_do_12_continuous.txt")
write.csv(df_xgb_do_12_continuous,"results/df_xgb_do_12_continuous.csv",row.names=F)

svm_do_12_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_12,
                              method="svmRadial",
                              tuneGrid=svm_grid,
                              preProcess = c("center","scale"),
                              tuneLength=10)
df_svm_do_12_continuous <- data.frame(observation = dfTest_12$do,
                                      prediction = predict(svm_do_12_continuous,newdata=dfTest_12))
metrics_svm_do_12_continuous <- postResample(pred=df_svm_do_12_continuous$prediction,obs=df_svm_do_12_continuous$observation)
saveRDS(svm_do_12_continuous,"results/svm_do_12_continuous")
write(metrics_svm_do_12_continuous,"results/svm_do_12_continuous.txt")
write.csv(df_svm_do_12_continuous,"results/df_svm_do_12_continuous.csv",row.names=F)

ann_do_12_continuous <- train(do~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_12,
                              method="avNNet",
                              tuneGrid=ann_grid,
                              preProcess = c("center","scale"),
                              trace=FALSE)
df_ann_do_12_continuous <- data.frame(observation = dfTest_12$do,
                                      prediction = predict(ann_do_12_continuous,newdata=dfTest_12))
metrics_ann_do_12_continuous <- postResample(pred=df_ann_do_12_continuous$prediction,obs=df_ann_do_12_continuous$observation)
saveRDS(ann_do_12_continuous,"results/ann_do_12_continuous")
write(metrics_ann_do_12_continuous,"results/ann_do_12_continuous.txt")
write.csv(df_ann_do_12_continuous,"results/df_ann_do_12_continuous.csv",row.names=F)



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


# working from here

set.seed(12312)
trainIndex <- createDataPartition(df_wide_c$chl, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dfTrain_312 <- df_wide_c[trainIndex,]
dfTest_312 <- df_wide_c[-trainIndex,]

trc <- trainControl(method="repeatedcv",
                    number=10,
                    repeats=3)
xgb_chl_312_continuous_test <- train(chl~B2+B3+B4+B5+B6+B8+dark_day,
                                     data=dfTrain_312,
                                     method="xgbTree",
                                     trControl = trc,
                                     #tuneGrid=xgb_grid,
                                     verbosity=0,
                                     tree_method="hist",
                                     ncores=5)
xgb_chl_312_continuous_test <- train(chl~NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+dark_day,
                                     data=dfTrain_312,
                                     method="xgbTree",
                                     trControl = trc,
                                     #tuneGrid=xgb_grid,
                                     verbosity=0,
                                     tree_method="hist",
                                     ncores=5)

xgb_chl_312_continuous_test$results$Rsquared
df_312_test_chl <- data.frame(observation = dfTest_312$chl,
                              prediction = predict(xgb_chl_312_continuous_test,newdata=dfTest_312))
postResample(pred=df_312_test_chl$prediction,obs=df_312_test_chl$observation)
ggplot(aes(x=observation,y=prediction),data=df_312_test_chl) +
  geom_point() +
  geom_abline(intercept=0, slope=1)




xgb_do_312_continuous_test <- train(do~NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+dark_day,
                                    data=dfTrain_312,
                                    method="xgbTree",
                                    trControl = trc,
                                    #tuneGrid=svm_grid,
                                    verbosity=0,
                                    tree_method="hist",
                                    ncores=5)
xgb_do_312_continuous_test$results$Rsquared
df_312_test_do <- data.frame(observation = dfTest_312$do,
                             prediction = predict(xgb_do_312_continuous_test,newdata=dfTest_312))
postResample(pred=df_312_test_do$prediction,obs=df_312_test_do$observation)
ggplot(aes(x=observation,y=prediction),data=df_312_test_do) +
  geom_point() +
  geom_abline(intercept=0, slope=1)



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