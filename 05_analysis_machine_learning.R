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
df_wide_c <- read.csv("intermediate/df_wide_c.csv")

# Calculate indices
df_wide_c$NDWI <- (df_wide_c$B3-df_wide_c$B8)/(df_wide_c$B3+df_wide_c$B8)
df_wide_c$NDCI <- (df_wide_c$B5-df_wide_c$B4)/(df_wide_c$B5+df_wide_c$B4)
df_wide_c$NDTI <- (df_wide_c$B4-df_wide_c$B3)/(df_wide_c$B4+df_wide_c$B3)
df_wide_c$NDMI <- (df_wide_c$B8-df_wide_c$B11)/(df_wide_c$B8+df_wide_c$B11)
df_wide_c$MNDWI <- (df_wide_c$B3-df_wide_c$B11)/(df_wide_c$B3+df_wide_c$B11)
df_wide_c$NDVI <- (df_wide_c$B8-df_wide_c$B4)/(df_wide_c$B8+df_wide_c$B4)

# Restrict to variables in the model
df_wide_c_model <- df_wide_c[,c(7:ncol(df_wide_c))]

# Normalise all variables
#df_wide_c_model <- as.data.frame(scale(df_wide_c_model))

# Get column IDs of the variables
col_nos <- which(colnames(df_wide_c_model)==c("ammonia","chlorophyll","do","ph","phycocyanin","temperature"))

# h/t https://github.com/tidyverse/glue/issues/108
subform <- function(formula, ...){
  args <- syms(list(...))
  subst_ <- substitute(
    substitute(formula, args),
    list(formula = formula))
  as.formula(eval(subst_),attr(formula,".Environment"))
}


# Make sure they're all numeric
sapply(df_wide_c_model,FUN=class)

# Loop neural networks over variables
for (i in col_nos){
  set.seed(3)
  trainIndex <- createDataPartition(df_wide_c_model[,i],
                                    p=0.7,
                                    times=1,
                                    list=F)
  dfTrain <- df_wide_c_model[trainIndex,]
  dfTest <- df_wide_c_model[-trainIndex,]
  
  
  # h/t https://github.com/tidyverse/glue/issues/108
  formula5 <- subform(x ~ B2+B3+B4+B5+B6+B7+B8+B8A +NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI,  
                      x = names(df_wide_c_model)[i])
  xgb.grid <- expand.grid(nrounds = 1000,
                          max_depth = c(2,4,6,8,10,14),
                          eta = c(0.01,0.05,0.1),
                          gamma=c(0.1,1,10),
                          colsample_bytree=c(0.25,0.5,0.75,0.1),
                          min_child_weight=c(1,10,100),
                          subsample=c(0.25,0.5,0.75))
  
  
  # Specify machine learning method
  ml_method <- "xgbTree"
  #ml_number <- 5
  #ml_repeats <- 100
  
  set.seed(3)
  control <- trainControl(method="repeatedcv",
                          number=ml_number,
                          repeats=ml_repeats,
                          savePredictions="all",
                          preProcOptions=c("scale"))
  
  xgbgrid <- expand.grid(eta = c(0.05,0.1,0.3,0.4),
                         max_depth=c(2,4,6,10),
                         gamma=c(0,1,10,100),
                         colsample_bytree = c(0.6,0.8),
                         min_child_weight=c(1,10,100),
                         subsample=c(0.25,1),
                         nrounds=c(50,100,150,1000))

  nn_tmp <- train(chlorophyll~NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI,
                  data=dfTrain,
                  method=ml_method,
                  tuneGrid=xgbgrid,
                  #trControl=trainControl(number=50),
                  #trControl=control,
                  verbosity=0,
                  tree_method="hist")
                  #linout=TRUE)#,
                  #maxit=100)
  
  write.csv(nn_tmp$results,paste0("results/nn_",i,".csv"))
  

  dfTest$pred_nn_tmp <- predict(nn_tmp, newdata=dfTest)
  
  success_metrics_names <- names(postResample(pred=dfTest$pred_nn_tmp,
                                              obs=dfTest[,i]))
  success_metrics <- postResample(pred=dfTest$pred_nn_tmp,
                                  obs=dfTest[,i])
  success_metrics
  
  caption_success_metrics <- paste0(
    "method = ",ml_method," (",ml_number,";",ml_repeats,")","; ",
    success_metrics_names[1]," = ",round(success_metrics[1],3),"; ",
    success_metrics_names[2]," = ",round(success_metrics[2],3),"; ",
    success_metrics_names[3]," = ",round(success_metrics[3],)
  )
  
  g_nn_tmp <- ggplot(aes(x=get(names(df_wide_c_model)[i]),y=pred_nn_tmp),data=dfTest) +
    geom_point() +
    geom_smooth(method="lm") +
    labs(x="ground truth",
         y="model prediction",
         title=names(df_wide_c_model)[i],
         caption=caption_success_metrics)
  print(g_nn_tmp)
  
  assign(paste0("g_nn_",i),g_nn_tmp, envir = .GlobalEnv)
  print(paste0("g_nn_",i))

    ggsave(filename=paste0("results/g_nn_",i,".png"),
         g_nn_tmp,
         width=6,height=4)
}


