
######################
### Haze tinkering ###
######################

ggplot(aes(x=NDCI_b,y=NDCI),data=df_clean) +
  geom_point() +
  geom_smooth(method="lm",se=F)

ggplot(aes(x=mNDHI_b,y=dark),data=df_clean) +
  geom_point() +
  geom_smooth(method="lm",se=F)

aggregate(B2~day,FUN=mean,data=df_clean,na.rm=T)
aggregate(B3~day,FUN=mean,data=df_clean,na.rm=T)
aggregate(B4~day,FUN=mean,data=df_clean,na.rm=T)

aggregate(B3_dehazed~day,FUN=mean,data=df_clean,na.rm=T)
aggregate(B4_dehazed~day,FUN=mean,data=df_clean,na.rm=T)



day1_rgb <- ggplot() + 
  geom_spatraster_rgb(data=rast_tmp_masked1,r=4,g=3,b=2,max_col_value=3500)
day1_rgb_dehazed <- ggplot() + 
  geom_spatraster_rgb(data=rast_tmp_masked1,r=22,g=21,b=20,max_col_value=3500)
day5_rgb <- ggplot() + 
  geom_spatraster_rgb(data=rast_tmp_masked5,r=4,g=3,b=2,max_col_value=3500)
day5_rgb_dehazed <- ggplot() + 
  geom_spatraster_rgb(data=rast_tmp_masked5,r=22,g=21,b=20,max_col_value=3500)
g_haze <- grid.arrange(day1_rgb,
             day1_rgb_dehazed,
             day5_rgb,
             day5_rgb_dehazed)
ggsave("preliminary_analysis/g_haze.png",g_haze,width=12,height=10)

















aggregate(mNDHI~day,FUN=mean,data=df_clean,na.rm=T)
aggregate(dark~day,FUN=mean,data=df_clean,na.rm=T)



rast_masked_day1$mNDHI <- (rast_masked_day1$B4-rast_masked_day1$B2)/
  (rast_masked_day1$B4+rast_masked_day1$B2)
rast_masked_day2$mNDHI <- (rast_masked_day2$B4-rast_masked_day2$B2)/
  (rast_masked_day2$B4+rast_masked_day2$B2)
rast_masked_day3$mNDHI <- (rast_masked_day3$B4-rast_masked_day3$B2)/
  (rast_masked_day3$B4+rast_masked_day3$B2)
rast_masked_day4$mNDHI <- (rast_masked_day4$B4-rast_masked_day4$B2)/
  (rast_masked_day4$B4+rast_masked_day4$B2)
rast_masked_day5$mNDHI <- (rast_masked_day5$B4-rast_masked_day5$B2)/
  (rast_masked_day5$B4+rast_masked_day5$B2)

global(rast_masked_day1$mNDHI,fun="mean",na.rm=T)
global(rast_masked_day2$mNDHI,fun="mean",na.rm=T)
global(rast_masked_day3$mNDHI,fun="mean",na.rm=T)
global(rast_masked_day4$mNDHI,fun="mean",na.rm=T)
global(rast_masked_day5$mNDHI,fun="mean",na.rm=T)

mNDHI_day1 <- ggplot() +
  geom_spatraster(aes(fill=mNDHI),data=rast_masked_day1) +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons, fill=NA, colour="red")
mNDHI_day2 <- ggplot() +
  geom_spatraster(aes(fill=mNDHI),data=rast_masked_day2) +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons)
mNDHI_day3 <- ggplot() +
  geom_spatraster(aes(fill=mNDHI),data=rast_masked_day3) +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons)
mNDHI_day4 <- ggplot() +
  geom_spatraster(aes(fill=mNDHI),data=rast_masked_day4) +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons)
mNDHI_day5 <- ggplot() +
  geom_spatraster(aes(fill=mNDHI),data=rast_masked_day5) +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons)
grid.arrange(mNDHI_day1,
             mNDHI_day2,
             mNDHI_day3,
             mNDHI_day4,
             mNDHI_day5)



dark_day1 <- ggplot() +
  geom_spatraster(aes(fill=dark),data=rast_masked_day1) +
  scale_fill_viridis(limits=c(0,1200)) 
dark_day2 <- ggplot() +
  geom_spatraster(aes(fill=dark),data=rast_masked_day2) +
  scale_fill_viridis(limits=c(0,1200))
dark_day3 <- ggplot() +
  geom_spatraster(aes(fill=dark),data=rast_masked_day3) +
  scale_fill_viridis(limits=c(0,1200))
dark_day4 <- ggplot() +
  geom_spatraster(aes(fill=dark),data=rast_masked_day4) +
  scale_fill_viridis(limits=c(0,1200))
dark_day5 <- ggplot() +
  geom_spatraster(aes(fill=dark),data=rast_masked_day5) +
  scale_fill_viridis(limits=c(0,1200))
grid.arrange(dark_day1,
             dark_day2,
             dark_day3,
             dark_day4,
             dark_day5)

w <- 0.6
rast_day1$dark <- min(rast_day1$B2,
                      rast_day1$B3,
                      rast_day1$B4)
rast_day1$B4c <- (1-w)*rast_day1$B4 + w*rast_day1$B4*350/rast_day1$dark
rast_day1$B3c <- (1-w)*rast_day1$B3 + w*rast_day1$B3*350/rast_day1$dark
rast_day1$B2c <- (1-w)*rast_day1$B2 + w*rast_day1$B2*350/rast_day1$dark
rast_day5$dark <- min(rast_day5$B2,
                      rast_day5$B3,
                      rast_day5$B4)
rast_day5$B4c <- (1-w)*rast_day5$B4 + w*rast_day5$B4*350/rast_day5$dark
rast_day5$B3c <- (1-w)*rast_day5$B3 + w*rast_day5$B3*350/rast_day5$dark
rast_day5$B2c <- (1-w)*rast_day5$B2 + w*rast_day5$B2*350/rast_day5$dark

day1_rgb_unhazed <- ggplot() + 
  geom_spatraster_rgb(data=rast_day1,r=4,g=3,b=2,max_col_value=3500)
day1_rgb_hazed <- ggplot() + 
  geom_spatraster_rgb(data=rast_day1,r=15,g=16,b=17,max_col_value=3500)
day5_rgb_unhazed <- ggplot() + 
  geom_spatraster_rgb(data=rast_day5,r=4,g=3,b=2,max_col_value=3500)
day5_rgb_hazed <- ggplot() + 
  geom_spatraster_rgb(data=rast_day5,r=15,g=16,b=17,max_col_value=3500)
grid.arrange(day1_rgb_unhazed,
             day1_rgb_hazed,
             day5_rgb_unhazed,
             day5_rgb_hazed)



rast_masked_day5

aggregate(dark~day,FUN=mean,data=df_clean,na.rm=T)$dark
350/aggregate(dark~day,FUN=mean,data=df_clean,na.rm=T)$dark
350/aggregate(dark~day,FUN=mean,data=df_clean,na.rm=T)$dark



rast_masked_day1$
  
  
  mNDHI_day1


rast_masked_day1_a <- subst(rast_masked_day1$AOT,0,NA)
rast_masked_day2_a <- subst(rast_masked_day2$AOT,0,NA)
rast_masked_day3_a <- subst(rast_masked_day3$AOT,0,NA)
rast_masked_day4_a <- subst(rast_masked_day4$AOT,0,NA)
rast_masked_day5_a <- subst(rast_masked_day5$AOT,0,NA)

global(rast_masked_day1_a$AOT,fun="mean",na.rm=T)
global(rast_masked_day2_a$AOT,fun="mean",na.rm=T)
global(rast_masked_day3_a$AOT,fun="mean",na.rm=T)
global(rast_masked_day4_a$AOT,fun="mean",na.rm=T)
global(rast_masked_day5_a$AOT,fun="mean",na.rm=T)

aot_day1 <- ggplot() +
  geom_spatraster(aes(fill=AOT),data=rast_masked_day1_a) +
  scale_fill_viridis(limits=c(400,600))
aot_day2 <- ggplot() +
  geom_spatraster(aes(fill=AOT),data=rast_masked_day2_a) +
  scale_fill_viridis(limits=c(400,600))
aot_day3 <- ggplot() +
  geom_spatraster(aes(fill=AOT),data=rast_masked_day3_a) +
  scale_fill_viridis(limits=c(400,600))
aot_day4 <- ggplot() +
  geom_spatraster(aes(fill=AOT),data=rast_masked_day4_a) +
  scale_fill_viridis(limits=c(400,600))
aot_day5 <- ggplot() +
  geom_spatraster(aes(fill=AOT),data=rast_masked_day5_a) +
  scale_fill_viridis(limits=c(400,600))
grid.arrange(aot_day1,
             aot_day2,
             aot_day3,
             aot_day4,
             aot_day5)


aot_day1
aot_day2 <- ggplot() +
  geom_spatraster(aes(fill=AOT),data=rast_masked_day2) +
  scale_fill_viridis()
aot_day2
aot_day5 <- ggplot() +
  geom_spatraster(aes(fill=AOT),data=rast_day5) +
  scale_fill_viridis()
grid.arrange(aot_day1,
             aot_day2,
             aot_day5)

hist_aot_day1

mean(rast_day1$AOT)


rast_masked_day1$AOT

which(rast_day1$AOT==0)
global(rast_day3$AOT,fun="mean")

global(rast_day4$AOT,fun="mean")
global(rast_day5$AOT,fun="mean")

global(rast_day2$AOT,fun="mean")



######################
### Visualise NDCI ###
######################

rast_masked_day3$NDCI <- (rast_masked_day3$B5-rast_masked_day3$B4)/
  (rast_masked_day3$B5+rast_masked_day3$B4)
rast_masked_day1$NDCI <- (rast_masked_day1$B5-rast_masked_day1$B4)/
  (rast_masked_day1$B5+rast_masked_day1$B4)
rast_masked_day2$NDCI <- (rast_masked_day2$B5-rast_masked_day2$B4)/
  (rast_masked_day2$B5+rast_masked_day2$B4)
rast_masked_day5$NDCI <- (rast_masked_day5$B5-rast_masked_day5$B4)/
  (rast_masked_day5$B5+rast_masked_day5$B4)
rast_masked_day4$NDCI <- (rast_masked_day4$B5-rast_masked_day4$B4)/
  (rast_masked_day4$B5+rast_masked_day4$B4)

ndci_1 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day1)  +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons,fill=NA,colour="red")
ndci_2 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day2)  +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons,fill=NA,colour="red")
ndci_3 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day3)  +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons,fill=NA,colour="red")
ndci_4 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day4)  +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons,fill=NA,colour="red")
ndci_5 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day5)  +
  scale_fill_viridis() +
  geom_spatvector(data=pond_polygons,fill=NA,colour="red")
g_ndci <- grid.arrange(ndci_1,ndci_2,ndci_3,ndci_4,ndci_5)
ggsave("preliminary_analysis/g_ndci.png",g_ndci,width=15,height=15)


# Calculate haze index
# see Han et al 2016, Haze detection by using modified normalized 
# difference haze index in Beijing, Tianjin, and Hebei province
rast_tmp$mNDHI <- (rast_tmp$B4-rast_tmp$B2)/
  (rast_tmp$B4+rast_tmp$B2)