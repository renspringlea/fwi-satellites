# Categorical, seed 3
xgb_chl_3_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_3,
                               method="xgbTree",
                               tuneGrid=xgb_grid,
                               verbosity=0,
                               tree_method="hist",
                               metric="Kappa",
                               ncores=5)
df_xgb_chl_3_categorical <- data.frame(observation = dfTest_3$chlorophyll_category,
                                       prediction = predict(xgb_chl_3_categorical,newdata=dfTest_3))
metrics_xgb_chl_3_categorical <- postResample(pred=df_xgb_chl_3_categorical$prediction,obs=df_xgb_chl_3_categorical$observation)
saveRDS(xgb_chl_3_categorical,"results_3/xgb_chl_3_categorical")
write(metrics_xgb_chl_3_categorical,"results_3/xgb_chl_3_categorical.txt")
write.csv(df_xgb_chl_3_categorical,"results_3/df_xgb_chl_3_categorical.csv",row.names=F)

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
saveRDS(svm_chl_3_categorical,"results_3/svm_chl_3_categorical")
write(metrics_svm_chl_3_categorical,"results_3/svm_chl_3_categorical.txt")
write.csv(df_svm_chl_3_categorical,"results_3/df_svm_chl_3_categorical.csv",row.names=F)

rf_chl_3_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="ordinalRF",
                              tuneGrid=rf_grid,
                              preProcess = c("center","scale"),
                              metric = "Kappa")
df_rf_chl_3_categorical <- data.frame(observation = dfTest_3$chlorophyll_category,
                                      prediction = predict(rf_chl_3_categorical,newdata=dfTest_3))
metrics_rf_chl_3_categorical <- postResample(pred=df_rf_chl_3_categorical$prediction,obs=df_rf_chl_3_categorical$observation)
saveRDS(rf_chl_3_categorical,"results_3/rf_chl_3_categorical")
write(metrics_rf_chl_3_categorical,"results_3/rf_chl_3_categorical.txt")
write.csv(df_rf_chl_3_categorical,"results_3/df_rf_chl_3_categorical.csv",row.names=F)

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
saveRDS(ann_chl_3_categorical,"results_3/ann_chl_3_categorical")
write(metrics_ann_chl_3_categorical,"results_3/ann_chl_3_categorical.txt")
write.csv(df_ann_chl_3_categorical,"results_3/df_ann_chl_3_categorical.csv",row.names=F)

# Categorical, seed 12
xgb_chl_12_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                                data=dfTrain_12,
                                method="xgbTree",
                                tuneGrid=xgb_grid,
                                verbosity=0,
                                tree_method="hist",
                                metric="Kappa",
                                ncores=5)
df_xgb_chl_12_categorical <- data.frame(observation = dfTest_12$chlorophyll_category,
                                        prediction = predict(xgb_chl_12_categorical,newdata=dfTest_12))
metrics_xgb_chl_12_categorical <- postResample(pred=df_xgb_chl_12_categorical$prediction,obs=df_xgb_chl_12_categorical$observation)
saveRDS(xgb_chl_12_categorical,"results_3/xgb_chl_12_categorical")
write(metrics_xgb_chl_12_categorical,"results_3/xgb_chl_12_categorical")
write.csv(df_xgb_chl_12_categorical,"results_3/df_xgb_chl_12_categorical.csv",row.names=F)

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
saveRDS(svm_chl_12_categorical,"results_3/svm_chl_12_categorical")
write(metrics_svm_chl_12_categorical,"results_3/svm_chl_12_categorical.txt")
write.csv(df_svm_chl_12_categorical,"results_3/df_svm_chl_12_categorical.csv",row.names=F)

rf_chl_12_categorical <- train(chlorophyll_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="ordinalRF",
                               tuneGrid=rf_grid,
                               preProcess = c("center","scale"),
                               metric = "Kappa")
df_rf_chl_12_categorical <- data.frame(observation = dfTest_12$chlorophyll_category,
                                       prediction = predict(rf_chl_12_categorical,newdata=dfTest_12))
metrics_rf_chl_12_categorical <- postResample(pred=df_rf_chl_12_categorical$prediction,obs=df_rf_chl_12_categorical$observation)
saveRDS(rf_chl_12_categorical,"results_3/rf_chl_12_categorical")
write(metrics_rf_chl_12_categorical,"results_3/rf_chl_12_categorical.txt")
write.csv(df_rf_chl_12_categorical,"results_3/df_rf_chl_12_categorical.csv",row.names=F)

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
saveRDS(ann_chl_12_categorical,"results_3/ann_chl_12_categorical")
write(metrics_ann_chl_12_categorical,"results_3/ann_chl_12_categorical.txt")
write.csv(df_ann_chl_12_categorical,"results_3/df_ann_chl_12_categorical.csv",row.names=F)

# Categorical, seed 3
xgb_do_3_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="xgbTree",
                              tuneGrid=xgb_grid,
                              verbosity=0,
                              tree_method="hist",
                              metric="Kappa",
                              ncores=5)
df_xgb_do_3_categorical <- data.frame(observation = dfTest_3$do_category,
                                      prediction = predict(xgb_do_3_categorical,newdata=dfTest_3))
metrics_xgb_do_3_categorical <- postResample(pred=df_xgb_do_3_categorical$prediction,obs=df_xgb_do_3_categorical$observation)
saveRDS(xgb_do_3_categorical,"results_3/xgb_do_3_categorical")
write(metrics_xgb_do_3_categorical,"results_3/xgb_do_3_categorical.txt")
write.csv(df_xgb_do_3_categorical,"results_3/df_xgb_do_3_categorical.csv",row.names=F)

svm_do_3_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="svmRadial",
                              tuneGrid=svm_grid,
                              preProcess = c("center","scale"),
                              tuneLength=10,
                              metric = "Kappa")
df_svm_do_3_categorical <- data.frame(observation = dfTest_3$do_category,
                                      prediction = predict(svm_do_3_categorical,newdata=dfTest_3))
metrics_svm_do_3_categorical <- postResample(pred=df_svm_do_3_categorical$prediction,obs=df_svm_do_3_categorical$observation)
saveRDS(svm_do_3_categorical,"results_3/svm_do_3_categorical")
write(metrics_svm_do_3_categorical,"results_3/svm_do_3_categorical.txt")
write.csv(df_svm_do_3_categorical,"results_3/df_svm_do_3_categorical.csv",row.names=F)

rf_do_3_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                             data=dfTrain_3,
                             method="ordinalRF",
                             tuneGrid=rf_grid,
                             preProcess = c("center","scale"),
                             metric = "Kappa")
df_rf_do_3_categorical <- data.frame(observation = dfTest_3$do_category,
                                     prediction = predict(rf_do_3_categorical,newdata=dfTest_3))
metrics_rf_do_3_categorical <- postResample(pred=df_rf_do_3_categorical$prediction,obs=df_rf_do_3_categorical$observation)
saveRDS(rf_do_3_categorical,"results_3/rf_do_3_categorical")
write(metrics_rf_do_3_categorical,"results_3/rf_do_3_categorical.txt")
write.csv(df_rf_do_3_categorical,"results_3/df_rf_do_3_categorical.csv",row.names=F)

ann_do_3_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B3+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="avNNet",
                              metric="Kappa",
                              tuneGrid=ann_grid,
                              
                              preProcess = c("center","scale"),
                              trace=FALSE)
df_ann_do_3_categorical <- data.frame(observation = dfTest_3$do_category,
                                      prediction = predict(ann_do_3_categorical,newdata=dfTest_3))
metrics_ann_do_3_categorical <- postResample(pred=df_ann_do_3_categorical$prediction,obs=df_ann_do_3_categorical$observation)
saveRDS(ann_do_3_categorical,"results_3/ann_do_3_categorical")
write(metrics_ann_do_3_categorical,"results_3/ann_do_3_categorical.txt")
write.csv(df_ann_do_3_categorical,"results_3/df_ann_do_3_categorical.csv",row.names=F)

# Categorical, seed 12
xgb_do_12_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="xgbTree",
                               tuneGrid=xgb_grid,
                               verbosity=0,
                               tree_method="hist",
                               metric="Kappa",
                               ncores=5)
df_xgb_do_12_categorical <- data.frame(observation = dfTest_12$do_category,
                                       prediction = predict(xgb_do_12_categorical,newdata=dfTest_12))
metrics_xgb_do_12_categorical <- postResample(pred=df_xgb_do_12_categorical$prediction,obs=df_xgb_do_12_categorical$observation)
saveRDS(xgb_do_12_categorical,"results_3/xgb_do_12_categorical")
write(metrics_xgb_do_12_categorical,"results_3/xgb_do_12_categorical")
write.csv(df_xgb_do_12_categorical,"results_3/df_xgb_do_12_categorical.csv",row.names=F)

svm_do_12_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="svmRadial",
                               tuneGrid=svm_grid,
                               preProcess = c("center","scale"),
                               tuneLength=10,
                               metric = "Kappa")
df_svm_do_12_categorical <- data.frame(observation = dfTest_12$do_category,
                                       prediction = predict(svm_do_12_categorical,newdata=dfTest_12))
metrics_svm_do_12_categorical <- postResample(pred=df_svm_do_12_categorical$prediction,obs=df_svm_do_12_categorical$observation)
saveRDS(svm_do_12_categorical,"results_3/svm_do_12_categorical")
write(metrics_svm_do_12_categorical,"results_3/svm_do_12_categorical.txt")
write.csv(df_svm_do_12_categorical,"results_3/df_svm_do_12_categorical.csv",row.names=F)

rf_do_12_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_12,
                              method="ordinalRF",
                              tuneGrid=rf_grid,
                              preProcess = c("center","scale"),
                              metric = "Kappa")
df_rf_do_12_categorical <- data.frame(observation = dfTest_12$do_category,
                                      prediction = predict(rf_do_12_categorical,newdata=dfTest_12))
metrics_rf_do_12_categorical <- postResample(pred=df_rf_do_12_categorical$prediction,obs=df_rf_do_12_categorical$observation)
saveRDS(rf_do_12_categorical,"results_3/rf_do_12_categorical")
write(metrics_rf_do_12_categorical,"results_3/rf_do_12_categorical.txt")
write.csv(df_rf_do_12_categorical,"results_3/df_rf_do_12_categorical.csv",row.names=F)

ann_do_12_categorical <- train(do_category~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                               data=dfTrain_12,
                               method="avNNet",
                               metric="Kappa",
                               tuneGrid=ann_grid,
                               
                               preProcess = c("center","scale"),
                               trace=FALSE)
df_ann_do_12_categorical <- data.frame(observation = dfTest_12$do_category,
                                       prediction = predict(ann_do_12_categorical,newdata=dfTest_12))
metrics_ann_do_12_categorical <- postResample(pred=df_ann_do_12_categorical$prediction,obs=df_ann_do_12_categorical$observation)
saveRDS(ann_do_12_categorical,"results_3/ann_do_12_categorical")
write(metrics_ann_do_12_categorical,"results_3/ann_do_12_categorical.txt")
write.csv(df_ann_do_12_categorical,"results_3/df_ann_do_12_categorical.csv",row.names=F)
