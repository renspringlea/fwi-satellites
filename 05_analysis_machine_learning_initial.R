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
df_wide_c <- df_wide_c[which(df_wide_c$cells_count>5 & df_wide_c$cells_prop>0.5),]

# Correct indices by brightness where appropriate
#df_wide_c$NDCI_b <- df_wide_c$NDCI_b*(df_wide_c$dark_day/max(df_wide_c$dark_day))
#df_wide_c$NDTI_b <- df_wide_c$NDTI_b*(df_wide_c$dark_day/max(df_wide_c$dark_day))
#df_wide_c$NDWI_b <- df_wide_c$NDWI_b*(df_wide_c$dark_day/max(df_wide_c$dark_day))
#df_wide_c$NDVI_b <- df_wide_c$NDVI_b*(df_wide_c$dark_day/max(df_wide_c$dark_day))
#df_wide_c$MNDWI_b <- df_wide_c$MNDWI_b*(df_wide_c$dark_day/max(df_wide_c$dark_day))
#df_wide_c$NDMI <- df_wide_c$NDMI*(df_wide_c$dark_day/max(df_wide_c$dark_day))


# Calculate summary statistics
numcols <- sapply(df_wide_c,FUN=is.numeric)
df_summarystats <- as.data.frame(rbind(sapply(df_wide_c[,numcols],FUN=min),
                                       colMeans(df_wide_c[,numcols]),
                                       sapply(df_wide_c[,numcols],FUN=max)))
df_summarystats

# Do some initial chlorophyll analysis
lm_chl <- lm(chl~NDTI_b+dark_day,data=df_wide_c)
summary(lm_chl)

g_scatterplot_ndci <- ggplot(aes(x=NDTI_b,y=chl,colour=as.factor(day)),data=df_wide_c) +
  geom_point() +
  geom_smooth(method="lm",se=F)
g_scatterplot_ndci
#ggsave("preliminary_analysis/g_scatterplot_ndci.png",g_scatterplot_ndci,width=6,height=3)

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


bandvec <-c("B2","B3","B4","B5","B6","B7","B8","B8A")
indexvec <- c("NDMI","NDWI_b","NDCI_b","NDTI_b","MNDWI_b","NDVI_b")
bothvec <- c(bandvec,indexvec)
highlyCor_bands <- findCorrelation(cor(df_wide_c[,bandvec]),
                             cutoff=0.95)
highlyCor_index <- findCorrelation(cor(df_wide_c[,indexvec]),
                             cutoff=0.95)
highlyCor_both <- findCorrelation(cor(df_wide_c[,bothvec]),
                                   cutoff=0.95)
bandvec[highlyCor_bands]
bandvec[highlyCor_index]
bothvec[highlyCor_both]

###############
### xgbTree ###
###############

# Train machine learning models for chlorophyll
xgb_chl_A <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_A,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_chl_A <- data.frame(observation = dfTest_A$chl,
                              prediction = predict(xgb_chl_A,newdata=dfTest_A))
round(postResample(pred=df_xgb_chl_A$prediction,obs=df_xgb_chl_A$observation),2)
g_xgb_chl_A <- ggplot(aes(x=observation,y=prediction),data=df_xgb_chl_A) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="chl, xgb, split A")
ggsave("results/g_xgb_chl_A.png",g_xgb_chl_A,width=6,height=4)

xgb_chl_B <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_B,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_chl_B <- data.frame(observation = dfTest_B$chl,
                           prediction = predict(xgb_chl_B,newdata=dfTest_B))
postResample(pred=df_xgb_chl_B$prediction,obs=df_xgb_chl_B$observation)
g_xgb_chl_B <- ggplot(aes(x=observation,y=prediction),data=df_xgb_chl_B) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="chl, xgb, split B")
ggsave("results/g_xgb_chl_B.png",g_xgb_chl_B,width=6,height=4)

xgb_chl_C <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_C,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_chl_C <- data.frame(observation = dfTest_C$chl,
                           prediction = predict(xgb_chl_C,newdata=dfTest_C))
postResample(pred=df_xgb_chl_C$prediction,obs=df_xgb_chl_C$observation)
g_xgb_chl_C <- ggplot(aes(x=observation,y=prediction),data=df_xgb_chl_C) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="chl, xgb, split C")
ggsave("results/g_xgb_chl_C.png",g_xgb_chl_C,width=6,height=4)

# Train machine learning models for dissolved oxygen
xgb_do_A <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_A,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_do_A <- data.frame(observation = dfTest_A$do,
                           prediction = predict(xgb_do_A,newdata=dfTest_A))
postResample(pred=df_xgb_do_A$prediction,obs=df_xgb_do_A$observation)
g_xgb_do_A <- ggplot(aes(x=observation,y=prediction),data=df_xgb_do_A) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="DO, xgb, split A")
ggsave("results/g_xgb_do_A.png",g_xgb_do_A,width=6,height=4)

xgb_do_B <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_B,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_do_B <- data.frame(observation = dfTest_B$do,
                           prediction = predict(xgb_do_B,newdata=dfTest_B))
postResample(pred=df_xgb_do_B$prediction,obs=df_xgb_do_B$observation)
g_xgb_do_B <- ggplot(aes(x=observation,y=prediction),data=df_xgb_do_B) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="DO, xgb, split B")
ggsave("results/g_xgb_do_B.png",g_xgb_do_B,width=6,height=4)

xgb_do_C <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_C,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_do_C <- data.frame(observation = dfTest_C$do,
                           prediction = predict(xgb_do_C,newdata=dfTest_C))
postResample(pred=df_xgb_do_C$prediction,obs=df_xgb_do_C$observation)
g_xgb_do_C <- ggplot(aes(x=observation,y=prediction),data=df_xgb_do_C) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="DO, xgb, split C")
ggsave("results/g_xgb_do_C.png",g_xgb_do_C,width=6,height=4)

# Train machine learning models for ph
xgb_ph_A <- train(ph~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_A,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_ph_A <- data.frame(observation = dfTest_A$ph,
                           prediction = predict(xgb_ph_A,newdata=dfTest_A))
postResample(pred=df_xgb_ph_A$prediction,obs=df_xgb_ph_A$observation)
g_xgb_ph_A <- ggplot(aes(x=observation,y=prediction),data=df_xgb_ph_A) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="ph, xgb, split A")
ggsave("results/g_xgb_ph_A.png",g_xgb_ph_A,width=6,height=4)

xgb_ph_B <- train(ph~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_B,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_ph_B <- data.frame(observation = dfTest_B$ph,
                           prediction = predict(xgb_ph_B,newdata=dfTest_B))
postResample(pred=df_xgb_ph_B$prediction,obs=df_xgb_ph_B$observation)
g_xgb_ph_B <- ggplot(aes(x=observation,y=prediction),data=df_xgb_ph_B) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="ph, xgb, split B")
ggsave("results/g_xgb_ph_B.png",g_xgb_ph_B,width=6,height=4)

xgb_ph_C <- train(ph~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_C,
                   method="xgbTree",
                   trControl = trc,
                   verbosity=0,
                   tree_method="hist",
                   ncores=5)
df_xgb_ph_C <- data.frame(observation = dfTest_C$ph,
                           prediction = predict(xgb_ph_C,newdata=dfTest_C))
postResample(pred=df_xgb_ph_C$prediction,obs=df_xgb_ph_C$observation)
g_xgb_ph_C <- ggplot(aes(x=observation,y=prediction),data=df_xgb_ph_C) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="ph, xgb, split C")
ggsave("results/g_xgb_ph_C.png",g_xgb_ph_C,width=6,height=4)

#################
### svmRadial ###
#################

svm_grid <- expand.grid("C"=c(10e-5,10e-4,10e-3,10e-2,0.25,0.5,10e-1,10e0,10e1,10e2,10e3,10e4,10e5),
                        "sigma"=c(0.05,0.1,0.15,0.5))

# Train machine learning models for chlorophyll
svm_chl_A <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_A,
                   method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                   trControl = trc)
df_svm_chl_A <- data.frame(observation = dfTest_A$chl,
                           prediction = predict(svm_chl_A,newdata=dfTest_A))
postResample(pred=df_svm_chl_A$prediction,obs=df_svm_chl_A$observation)
g_svm_chl_A <- ggplot(aes(x=observation,y=prediction),data=df_svm_chl_A) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="chl, svm, split A")
ggsave("results/g_svm_chl_A.png",g_svm_chl_A,width=6,height=4)

svm_chl_B <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_B,
                   method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                   trControl = trc)



df_svm_chl_B <- data.frame(observation = dfTest_B$chl,
                           prediction = predict(svm_chl_B,newdata=dfTest_B))
postResample(pred=df_svm_chl_B$prediction,obs=df_svm_chl_B$observation)
g_svm_chl_B <- ggplot(aes(x=observation,y=prediction),data=df_svm_chl_B) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="chl, svm, split B")
ggsave("results/g_svm_chl_B.png",g_svm_chl_B,width=6,height=4)

svm_chl_C <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+
                     NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                     dark_day,
                   data=dfTrain_C,
                   method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                   trControl = trc)
df_svm_chl_C <- data.frame(observation = dfTest_C$chl,
                           prediction = predict(svm_chl_C,newdata=dfTest_C))
postResample(pred=df_svm_chl_C$prediction,obs=df_svm_chl_C$observation)
g_svm_chl_C <- ggplot(aes(x=observation,y=prediction),data=df_svm_chl_C) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="chl, svm, split C")
ggsave("results/g_svm_chl_C.png",g_svm_chl_C,width=6,height=4)

# Train machine learning models for dissolved oxygen
svm_do_A <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                    dark_day,
                  data=dfTrain_A,
                  method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                  trControl = trc)
df_svm_do_A <- data.frame(observation = dfTest_A$do,
                          prediction = predict(svm_do_A,newdata=dfTest_A))
postResample(pred=df_svm_do_A$prediction,obs=df_svm_do_A$observation)
g_svm_do_A <- ggplot(aes(x=observation,y=prediction),data=df_svm_do_A) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="DO, svm, split A")
ggsave("results/g_svm_do_A.png",g_svm_do_A,width=6,height=4)

svm_do_B <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                    dark_day,
                  data=dfTrain_B,
                  method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                  trControl = trc)
df_svm_do_B <- data.frame(observation = dfTest_B$do,
                          prediction = predict(svm_do_B,newdata=dfTest_B))
postResample(pred=df_svm_do_B$prediction,obs=df_svm_do_B$observation)
g_svm_do_B <- ggplot(aes(x=observation,y=prediction),data=df_svm_do_B) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="DO, svm, split B")
ggsave("results/g_svm_do_B.png",g_svm_do_B,width=6,height=4)

svm_do_C <- train(do~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                    dark_day,
                  data=dfTrain_C,
                  method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                  trControl = trc)
df_svm_do_C <- data.frame(observation = dfTest_C$do,
                          prediction = predict(svm_do_C,newdata=dfTest_C))
postResample(pred=df_svm_do_C$prediction,obs=df_svm_do_C$observation)
g_svm_do_C <- ggplot(aes(x=observation,y=prediction),data=df_svm_do_C) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="DO, svm, split C")
ggsave("results/g_svm_do_C.png",g_svm_do_C,width=6,height=4)

# Train machine learning models for ph
svm_ph_A <- train(ph~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                    dark_day,
                  data=dfTrain_A,
                  method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                  trControl = trc)

df_svm_ph_A <- data.frame(observation = dfTest_A$ph,
                          prediction = predict(svm_ph_A,newdata=dfTest_A))
postResample(pred=df_svm_ph_A$prediction,obs=df_svm_ph_A$observation)
g_svm_ph_A <- ggplot(aes(x=observation,y=prediction),data=df_svm_ph_A) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="ph, svm, split A")
ggsave("results/g_svm_ph_A.png",g_svm_ph_A,width=6,height=4)

svm_ph_B <- train(ph~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                    dark_day,
                  data=dfTrain_B,
                  method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                  trControl = trc)
df_svm_ph_B <- data.frame(observation = dfTest_B$ph,
                          prediction = predict(svm_ph_B,newdata=dfTest_B))
paste(round(postResample(pred=df_svm_ph_B$prediction,obs=df_svm_ph_B$observation),2),sep="",collapse=",")
g_svm_ph_B <- ggplot(aes(x=observation,y=prediction),data=df_svm_ph_B) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="ph, svm, split B")
ggsave("results/g_svm_ph_B.png",g_svm_ph_B,width=6,height=4)

svm_ph_C <- train(ph~B2+B3+B4+B5+B6+B7+B8+B8A+
                    NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b+
                    dark_day,
                  data=dfTrain_C,
                  method="svmRadial",preProcess=c("center","scale"),tuneGrid=svm_grid,
                  trControl = trc)
df_svm_ph_C <- data.frame(observation = dfTest_C$ph,
                          prediction = predict(svm_ph_C,newdata=dfTest_C))
postResample(pred=df_svm_ph_C$prediction,obs=df_svm_ph_C$observation)
g_svm_ph_C <- ggplot(aes(x=observation,y=prediction),data=df_svm_ph_C) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(title="",subtitle="ph, svm, split C")
ggsave("results/g_svm_ph_C.png",g_svm_ph_C,width=6,height=4)

############################
### Analyse final models ###
############################

xgb_chl_B$finalModel
varImp(xgb_chl_B)
varImp(svm_chl_A)
varImp(svm_chl_B)
varImp(svm_chl_C)
varImp(svm_do_C)
varImp(svm_ph_A)

svm_chl_A
svm_chl_B
svm_chl_C
svm_do_A
svm_ph_A
