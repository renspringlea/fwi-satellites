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

# Restrict based on cell numbers
df_wide_c <- df_wide_c[which(df_wide_c$cells_count>5 & df_wide_c$cells_prop>0.5),]

# Categorise DO
df_wide_c$do_cat <- "bad"
df_wide_c[which(df_wide_c$do>4),"do_cat"] <- "good"
df_wide_c$do_cat <- as.factor(df_wide_c$do_cat)
df_wide_c$wq_agg_cat <- "bad"
df_wide_c[which(df_wide_c$do>3 &
                  df_wide_c$ph<8.5 &
                  df_wide_c$amm<0.5),"wq_agg_cat"] <- "good"
df_wide_c$wq_agg_cat <- as.factor(df_wide_c$wq_agg_cat)

# Generate training and test datasets
# for a couple of different seeds
# We'll do one naive split (A),
# Then two non-naive splits (B and C)
# For each non-naive train/test split, we're going to select 1 full day
# (one with day 1 or 3, and one with day 4 or 5)
# and 1 full pond to form the test set, then the rest in the train set
unique_ponds <- unique(df_wide_c$pond)

# Naive split
set.seed(123123123)
trainIndex_A <- createDataPartition(df_wide_c$chl, p = .7, 
                                    list = FALSE, 
                                    times = 1)
dfTrain_A <- df_wide_c[trainIndex_A,]
dfTest_A <- df_wide_c[-trainIndex_A,]

# Non-naive splits
set.seed(123123123)
test_ponds <- sample(unique_ponds,8)
test_pond_B <- test_ponds[c(1:4)]
test_pond_C <- test_ponds[c(5:8)]

set.seed(123123123)
test_day_B <- sample(c(1,3),1)
dfTest_B <- df_wide_c[which(df_wide_c$day==test_day_B |
                              df_wide_c$pond%in%test_pond_B),]
dfTrain_B <- df_wide_c[-which(df_wide_c$day==test_day_B |
                                df_wide_c$pond%in%test_pond_B),]

set.seed(123123123)
test_day_C <- sample(c(4,5),1)
dfTest_C <- df_wide_c[which(df_wide_c$day==test_day_C |
                              df_wide_c$pond%in%test_pond_C),]
dfTrain_C <- df_wide_c[-which(df_wide_c$day==test_day_C |
                                df_wide_c$pond%in%test_pond_C),]

# Use repeated CV
trc <- trainControl(method="repeatedcv",
                    number=10,
                    repeats=3)


svm_grid <- expand.grid("C"=c(10e-5,10e-4,10e-3,10e-2,0.25,0.5,10e-1,10e0,10e1,10e2,10e3,10e4,10e5),
                        "sigma"=c(0.05,0.1,0.15,0.5))

svm_do_A <- train(wq_agg_cat~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b,
                  data=dfTrain_A,
                  method="svmRadial",
                  preProcess=c("center","scale"),
                  tuneGrid=svm_grid,
                  trControl = trc)
df_svm_do_A <- data.frame(observation = dfTest_A$wq_agg_cat,
                          prediction = predict(svm_do_A,newdata=dfTest_A))
postResample(pred=df_svm_do_A$prediction,obs=df_svm_do_A$observation)


which(df_svm_do_A$prediction=="bad")
which(dfTest_A$ph>8.5)
which(dfTest_A$do<3)
which(dfTest_A$amm>0.5)

# observation is rows, prediction is columns
table(df_svm_do_A$observation,df_svm_do_A$prediction)


#########################
svm_chl_A <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b,
                  data=dfTrain_A,
                  method="svmRadial",
                  preProcess=c("center","scale"),
                  tuneGrid=svm_grid,
                  trControl = trc)
dfTrain_A$chl_pred <- predict(svm_chl_A,dfTrain_A)
dfTest_A$chl_pred <- predict(svm_chl_A,dfTest_A)

svm_do_chl_A <- train(do~temp+chl_pred,
                      data=dfTrain_A,
                      method="svmRadial",
                      preProcess=c("center","scale"),
                      tuneGrid=svm_grid,
                      trControl = trc)
df_svm_do_chl_A <- data.frame(observation = dfTest_A$do,
                          prediction = predict(svm_do_chl_A,newdata=dfTest_A))
postResample(pred=df_svm_do_chl_A$prediction,obs=df_svm_do_chl_A$observation)
g_svm_do_chl_A <- ggplot(aes(x=observation,y=prediction),data=df_svm_do_chl_A) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="do~tmp+chl_pred, svm, split A")
g_svm_do_chl_A

svm_do_chl_A_2 <- train(do~temp+chl,
                        data=dfTrain_A,
                        method="svmRadial",
                        preProcess=c("center","scale"),
                        tuneGrid=svm_grid,
                        trControl = trc)
svm_do_chl_A_2
df_svm_do_chl_A_2 <- data.frame(observation = dfTest_A$do,
                              prediction = predict(svm_do_chl_A_2,newdata=dfTest_A))
postResample(pred=df_svm_do_chl_A_2$prediction,obs=df_svm_do_chl_A_2$observation)
g_svm_do_chl_A_2 <- ggplot(aes(x=observation,y=prediction),data=df_svm_do_chl_A_2) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="do~tmp+chl, svm, split A")
g_svm_do_chl_A_2
