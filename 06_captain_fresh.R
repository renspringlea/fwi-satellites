#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable

# Load data
df_cf <- read.csv("data/captainfresh.csv")

# Analyse differences between empirical and predicted values
# by variables
df_cf$absdiff <- df_cf$empirical-df_cf$predicted

df_cf_summarystats <- aggregate(absdiff~variable,data=df_cf,FUN=mean)
names(df_cf_summarystats) <- c("variable","meanabsdiff")
df_cf_summarystats$sdabsdiff <- aggregate(absdiff~variable,data=df_cf,FUN=sd)[,2]
df_cf_summarystats$meanofvar <- aggregate(empirical~variable,data=df_cf,FUN=mean)[,2]
df_cf_summarystats$meanabsdiff_relative <- df_cf_summarystats$meanabsdiff/
  df_cf_summarystats$meanofvar
df_cf_summarystats$sdabsdiff_relative <- df_cf_summarystats$sdabsdiff/
  df_cf_summarystats$meanofvar

ggplot(aes(x=absdiff),data=df_cf) +
  geom_histogram(bins=10) +
  facet_wrap(~variable,scales="free")
ggplot(aes(x=reldiff),data=df_cf) +
  geom_histogram(bins=10) +
  facet_wrap(~variable,scales="free")
ggplot(aes(x=empirical,y=predicted),data=df_cf) +
  geom_point() +
  geom_smooth(method="lm",formula=y~0+x) +
  facet_wrap(~variable,scales="free")# +
  #ylim(0,NA) +
  #xlim(0,NA)

vec_variables <- unique(df_cf$variable)
for (i in vec_variables){
  shapiro.tmp <- shapiro.test(df_cf[df_cf$variable==i,]$absdiff)
  ks.tmp <- ks.test(df_cf[df_cf$variable==i,]$absdiff, "pnorm")
  lm.tmp <- lm(predicted~empirical,data=df_cf[df_cf$variable==i,])
  print(c(i,shapiro.tmp$p.value,ks.tmp$p.value))
  print(summary(lm.tmp))
  print(cor(df_cf[df_cf$variable==i,]$empirical,df_cf[df_cf$variable==i,]$predicted))
}

summary(lm(predicted~empirical+0,data=df_cf[df_cf$variable=="ph",]))
summary(lm(predicted~empirical,data=df_cf[df_cf$variable=="ph",]))
        
df_cf_ph <- df_cf[df_cf$variable=="ph",]
cor(df_cf_ph$empirical,df_cf_ph$predicted)

lm_ph <- lm(predicted~empirical+0,data=df_cf_ph)
lm_ph$coefficients
lm_ph$residuals
lm_ph$effects
lm_ph$df.residual
lm_ph$qr

df_cf_ph$lmpred <- predict(lm_ph)

ybar <- mean(df_cf_ph$predicted)
rss <- sum((df_cf_ph$predicted-df_cf_ph$lmpred)^2)
rss2 <- sum((df_cf_ph$predicted)^2)

df_cf_ph$predicted
df_cf_ph$lmpred

tss <- sum((df_cf_ph$predicted-ybar)^2)
1-(rss/tss)
