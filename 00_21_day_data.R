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
# I'm only including Location 1 for each pond
df_br_evening <- read.csv("data_21_days/21 day data (Dyke vs Pond middle) - BR Pond - Evening.csv",skip=1)[,c(1:8)]
df_br_morning <- read.csv("data_21_days/21 day data (Dyke vs Pond middle) - BR Pond - Morning.csv",skip=1)[,c(1:8)]
df_uc_evening <- read.csv("data_21_days/21 day data (Dyke vs Pond middle) - UC Pond - Evening.csv",skip=1)[,c(1:8)]
df_uc_morning <- read.csv("data_21_days/21 day data (Dyke vs Pond middle) - UC Pond - Morning.csv",skip=1)[,c(1:8)]

# Format data nicely
df_br_evening$pond <- "BR"
df_br_morning$pond <- "BR"
df_uc_evening$pond <- "UC"
df_uc_morning$pond <- "UC"
df_br_uc <- rbind(df_br_evening,
                  df_br_morning,
                  df_uc_evening,
                  df_uc_morning)
names(df_br_uc) <- c("date","timepoint","time","temp","chl","pc","do","ph","pond")
df_br_uc$date <- as.Date(df_br_uc$date,format="%m-%d-%Y")

# Create a datetime column so we can then sort by time
df_br_uc$datetime_char <- paste0(df_br_uc$date," ",df_br_uc$time)
df_br_uc$datetime <- as.POSIXct(df_br_uc$datetime_char)
df_br_uc <- df_br_uc[order(df_br_uc$datetime),]

# Now separate by pond again
df_br <- df_br_uc[which(df_br_uc$pond=="BR"),]
df_uc <- df_br_uc[which(df_br_uc$pond=="UC"),]
df_uc <- df_uc[-1,] #Remove the unpaired row

# Now rbind again now that they're all in the correct order (sue me)
df_br_uc <- rbind(df_br,df_uc)

# Relationship between chl and do, naive
summary(lm(do~chl,data=df_br_uc))

# Relationship between chl in morning and do in evening
chl_morning <- df_br_uc[which(df_br_uc$timepoint=="Morning"),"chl"]
do_evening <- df_br_uc[which(df_br_uc$timepoint=="Evening"),"do"]
summary(lm(do_evening~chl_morning))

# Relationship between chl in morning and change in DO during day
delta_do <- df_br_uc[which(df_br_uc$timepoint=="Evening"),"do"] -
  df_br_uc[which(df_br_uc$timepoint=="Morning"),"do"]
do_t1 <- df_br_uc[which(df_br_uc$timepoint=="Morning"),"do"]
summary(lm(delta_do~chl_morning))
summary(lm(delta_do~chl_morning+do_t1))

# Relationship between chl in evening and do next morning
dates_br <- unique(df_br$date)
dates_br_df <- data.frame("date"=dates_br,
                          "chl_today_evening"=1,
                          "do_tomorrow_morning"=1)
for (i in c(1:(length(dates_br)-1))){
  today_tmp <- dates_br[i]
  tomorrow_tmp <- dates_br[c(i+1)]
  df_br_today <- df_br[which(df_br$date==today_tmp),]
  df_br_tomorrow <- df_br[which(df_br$date==tomorrow_tmp),]
  chl_today_evening <- df_br_today[which(df_br_today$timepoint=="Evening"),"chl"]
  do_tomorrow_morning <- df_br_tomorrow[which(df_br_tomorrow$timepoint=="Morning"),"do"]
  dates_br_df[i,"chl_today_evening"] <- chl_today_evening
  dates_br_df[i,"do_tomorrow_morning"] <- do_tomorrow_morning
}
dates_uc <- unique(df_uc$date)
dates_uc_df <- data.frame("date"=dates_uc,
                          "chl_today_evening"=1,
                          "do_tomorrow_morning"=1)
for (i in c(1:(length(dates_uc)-1))){
  today_tmp <- dates_uc[i]
  tomorrow_tmp <- dates_uc[c(i+1)]
  df_uc_today <- df_uc[which(df_uc$date==today_tmp),]
  df_uc_tomorrow <- df_uc[which(df_uc$date==tomorrow_tmp),]
  chl_today_evening <- df_uc_today[which(df_uc_today$timepoint=="Evening"),"chl"]
  do_tomorrow_morning <- df_uc_tomorrow[which(df_uc_tomorrow$timepoint=="Morning"),"do"]
  dates_uc_df[i,"chl_today_evening"] <- chl_today_evening
  dates_uc_df[i,"do_tomorrow_morning"] <- do_tomorrow_morning
}
dates_br_df$pond <- "BR"
dates_uc_df$pond <- "UC"
dates_df <- rbind(dates_br_df,dates_uc_df)
g1 <- ggplot(aes(x=chl_today_evening,y=do_tomorrow_morning,colour=pond),data=dates_df) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  geom_hline(yintercept=3,linetype="dashed")
g1
ggsave("data_21_days/g1.png",g1,width=6,height=4)
dates_br_df



df_wide_c <- read.csv("intermediate/df_clean.csv")
df_wide_c$do_cat <- "bad"
df_wide_c[which(df_wide_c$do>3),"do_cat"] <- "good"
table(df_wide_c$do_cat)

aggregate(chl~do_cat,FUN=mean,data=df_wide_c)

chldo <- merge(aggregate(chl~pond,FUN=mean,data=df_wide_c),
      aggregate(do~pond,FUN=mean,data=df_wide_c),
      all=T)
chldosd <- merge(aggregate(chl~pond,FUN=sd,data=df_wide_c),
               aggregate(do~pond,FUN=sd,data=df_wide_c),
               all=T)
names(chldosd)[c(2:3)] <- c("chl_sd","do_sd")
chldo <- merge(chldo,chldosd,all=T)
chldo

g2 <- ggplot(aes(x=chl,y=do),data=chldo) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  geom_hline(yintercept=3,linetype="dashed")
g2
