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

# Load data object from previous script
xgb_chl_3_continuous <- readRDS("results_1/xgb_chl_3_continuous")

# Generate grids
xgb_grid <- expand.grid(nrounds = 1000,
                        max_depth = c(2,4,6,8,10,14),
                        eta = c(0.01,0.05,0.1),
                        gamma=c(0.1,1,10),
                        colsample_bytree=c(0.25,0.5,0.75,0.1),
                        min_child_weight=c(1,10,100),
                        subsample=c(0.25,0.5,0.75))
xgb_grid <- expand.grid(nrounds = c(50,100,1000),
                        max_depth = seq(5,7,0.5),
                        eta = seq(0.04,0.06,0.005),
                        gamma=c(9,10,11,50),
                        colsample_bytree=seq(0.05,0.15,0.025),
                        min_child_weight=c(0.9,1,1.1),
                        subsample=seq(0.7,0.8,0.1))
xgb_grid <- expand.grid(nrounds=1000,
                        max_depth=14,
                        eta=0.01,
                        gamma=1,
                        colsample_bytree=0.1,
                        min_child_weight=1,
                        subsample=0.75)
#xgb_grid <- xgb_grid[1,] #For debugging

# Generate training and test datasets
# for a couple of different seeds
unique_ponds <- unique(df_wide_c_cats$pond)
set.seed(3)
trainIndex_3 <- sample(df_wide_c_cats$pond, round(0.5*length(unique_ponds)))
dfTrain_3 <- df_wide_c_cats[which(df_wide_c_cats$pond %in% trainIndex_3),]

dfTrain_3_object <- dfTrain_3[,c("NDWI","NDCI","NDTI","NDMI","MNDWI","NDVI",
                                   "B2","B3","B4","B5","B6","B7","B8","B8A")]
dfTest_3 <- df_wide_c_cats[-which(df_wide_c_cats$pond %in% trainIndex_3),]
dfTest_3_object <- dfTest_3[,c("NDWI","NDCI","NDTI","NDMI","MNDWI","NDVI",
                               "B2","B3","B4","B5","B6","B7","B8","B8A")]
writeLines("Seed = 3","results_2/seeds.txt")
write("Train ponds: ","results_2/seeds.txt",append=T)
write(paste(trainIndex_3,collapse=" "),"results_2/seeds.txt",append=T)
write(paste0("(",nrow(dfTrain_3)," rows)"),"results_2/seeds.txt",append=T)
write("Test ponds: ","results_2/seeds.txt",append=T)
write(paste(unique(dfTest_3$pond),collapse=" "),"results_2/seeds.txt",append=T)
write(paste0("(",nrow(dfTest_3)," rows)"),"results_2/seeds.txt",append=T)
set.seed(12)
trainIndex_12 <- sample(df_wide_c_cats$pond, round(0.5*length(unique_ponds)))
dfTrain_12 <- df_wide_c_cats[which(df_wide_c_cats$pond %in% trainIndex_12),]
dfTrain_12_object <- dfTrain_12[,c("NDWI","NDCI","NDTI","NDMI","MNDWI","NDVI",
                                     "B2","B3","B4","B5","B6","B7","B8","B8A")]
dfTest_12 <- df_wide_c_cats[-which(df_wide_c_cats$pond %in% trainIndex_12),]
write("Seed = 12","results_2/seeds.txt",append=T)
write("Train ponds: ","results_2/seeds.txt",append=T)
write(paste(trainIndex_12,collapse=" "),"results_2/seeds.txt",append=T)
write(paste0("(",nrow(dfTrain_12)," rows)"),"results_2/seeds.txt",append=T)
write("Test ponds: ","results_2/seeds.txt",append=T)
write(paste(unique(dfTest_12$pond),collapse=" "),"results_2/seeds.txt",append=T)
write(paste0("(",nrow(dfTest_12)," rows)"),"results_2/seeds.txt",append=T)

# Train machine learning models
# Continuous, seed 3
xgb_chl_3_continuous <- train(x=dfTrain_3_object,
                              y=dfTrain_3$chlorophyll,
                               method="xgbTree",
                               tuneGrid=xgb_grid,
                               verbosity=0,
                               tree_method="hist",
                              ncores=5)
xgb_chl_3_continuous <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI+B2+B12+B4+B5+B6+B7+B8+B8A,
                              data=dfTrain_3,
                              method="xgbTree",
                              tuneGrid=xgb_grid,
                              verbosity=0,
                              tree_method="hist",
                              ncores=5)


df_xgb_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                       prediction = predict(xgb_chl_3_continuous_old,newdata=dfTest_3_object))

df_xgb_chl_3_continuous <- data.frame(observation = dfTest_3$chlorophyll,
                                      prediction = predict(xgb_chl_3_continuous,newdata=dfTest_3))
metrics_xgb_chl_3_continuous <- postResample(pred=df_xgb_chl_3_continuous$prediction,obs=df_xgb_chl_3_continuous$observation)
metrics_xgb_chl_3_continuous
saveRDS(xgb_chl_3_continuous,"results_2/xgb_chl_3_continuous")
write(metrics_xgb_chl_3_continuous,"results_2/xgb_chl_3_continuous.txt")
write.csv(df_xgb_chl_3_continuous,"results_2/df_xgb_chl_3_continuous.csv",row.names=F)

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
saveRDS(xgb_chl_12_continuous,"results_2/xgb_chl_12_continuous")
write(metrics_xgb_chl_12_continuous,"results_2/xgb_chl_12_continuous.txt")
write.csv(df_xgb_chl_12_continuous,"results_2/df_xgb_chl_12_continuous.csv",row.names=F)

ggplot(aes(x=observation,y=prediction),data=df_xgb_chl_3_continuous) +
  geom_point()
ggplot(aes(x=observation,y=prediction),data=df_xgb_chl_12_continuous) +
  geom_point()
