
df_wide_c$NDCI <- (df_wide_c$B5-df_wide_c$B4)/(df_wide_c$B5+df_wide_c$B4)
aggregate(NDCI~day,data=df_wide_c,FUN=mean)
df_wide_c$mishra <- 14.039 + 
  (86.155*df_wide_c$NDCI) +
  (194.325*(df_wide_c$NDCI^2))
df_wide_c_13 <- df_wide_c[which(df_wide_c$day %in% c(1,3)),]
ggplot(aes(x=chlorophyll,y=mishra),data=df_wide_c_13) +
  geom_point()

lm_chl_quadratic <- lm(chlorophyll ~ NDCI + I(NDCI^2), df_wide_c_13)
summary(lm_chl_quadratic)

rast_masked_day1$NDCI <- (rast_masked_day1$B5-rast_masked_day1$B4)/
  (rast_masked_day1$B5+rast_masked_day1$B4)
rast_masked_day3$NDCI <- (rast_masked_day3$B5-rast_masked_day3$B4)/
  (rast_masked_day3$B5+rast_masked_day3$B4)
ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day1) +
  geom_spatvector(data=pond_polygons_d,fill=NA,colour="red") +
  scale_fill_viridis()
ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day3) +
  geom_spatvector(data=pond_polygons_d,fill=NA,colour="red") +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red") +
  scale_fill_viridis()  +
  geom_spatvector_label(aes(label=ID),data=pond_polygons_d) +
  geom_spatvector_text(aes(label=polygon),data=vect_pond_names,color="blue") +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names)

extract(rast_masked_day1, pond_polygons_d,fun=mean,na.rm=T)$NDCI
sd(extract(rast_masked_day1, pond_polygons_d,fun=mean,na.rm=T)$NDCI)
extract(rast_masked_day1$NDCI, pond_polygons_d,fun=range,na.rm=T)
extract(rast_masked_day1, pond_polygons_d,fun=sd,na.rm=T)$NDCI

rast_masked_day1_pondsonly <- mask(rast_hazed_masked_scaled_day1,
                                   project(pond_polygons_d,rast_hazed_masked_scaled_day1))
rast_masked_day3_pondsonly <- mask(rast_hazed_masked_scaled_day3,
                                   project(pond_polygons_d,rast_hazed_masked_scaled_day3))
rast_masked_day1_pondsonly$NDCI <- (rast_masked_day1_pondsonly$B5-rast_masked_day1_pondsonly$B4)/
  (rast_masked_day1_pondsonly$B5+rast_masked_day1_pondsonly$B4)
rast_masked_day3_pondsonly$NDCI <- (rast_masked_day3_pondsonly$B5-rast_masked_day3_pondsonly$B4)/
  (rast_masked_day3_pondsonly$B5+rast_masked_day3_pondsonly$B4)


ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day1_pondsonly)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")

ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day3_pondsonly)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")



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
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")
ndci_2 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day2)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")
ndci_3 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day3)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")
ndci_4 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day4)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")
ndci_5 <- ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day5)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")
g_ndci <- grid.arrange(ndci_1,ndci_2,ndci_3,ndci_4,ndci_5)
ggsave("preliminary_analysis/g_ndci.png",g_ndci,width=15,height=15)


df_wide_c$NDCI <- (df_wide_c$B5-df_wide_c$B4)/
  (df_wide_c$B5+df_wide_c$B4)
summary(lm(chlorophyll~NDCI+I(NDCI^2),data=df_wide_c))

df_agg_ndci <- aggregate(NDCI~day,data=df_wide_c,FUN=mean)
df_agg_ndci$chlorophyll <- aggregate(chlorophyll~day,data=df_wide_c,FUN=mean)$chlorophyll
ggplot(aes(x=chlorophyll,y=NDCI),data=df_agg_ndci) +
  geom_point()
summary(lm(chlorophyll~NDCI,data=df_agg_ndci))

ggplot(aes(x=chlorophyll,y=NDCI,colour=as.factor(day)),data=df_wide_c) +
  geom_point() +
  geom_smooth(se=F) +
  geom_vline(xintercept=100) +
  scale_x_continuous(breaks=seq(0,650,10))

ggplot(aes(x=chlorophyll,y=do,colour=as.factor(day)),data=df_wide_c) +
  geom_point() +
  #geom_smooth(se=F) #+
  #geom_vline(xintercept=100) +
  scale_y_continuous(breaks=seq(0,15,1))



ggplot() +
  geom_spatraster(aes(fill=B4),data=rast_masked_day3)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")

ggplot() +
  geom_spatraster(aes(fill=B5),data=rast_masked_day3)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")


df_wide_c_day <- df_wide_c[which(df_wide_c$day %in% c(1,3)),]



extract(rast_masked_day1_pondsonly,vect_pond_names)
vect_pond_names_p <- project(vect_pond_names,rast_masked_day1_pondsonly)

rast_masked_day1_pondsonly_distance <- distance(rast_masked_day1_pondsonly)
ggplot() +
  geom_spatraster(aes(fill=B3),data=rast_masked_day1_pondsonly_distance) 
head(rast_masked_day1_pondsonly_distance$B3)
head(rast_masked_day1_pondsonly_distance$B8A)
head(rast_masked_day1_pondsonly_distance$NDCI)
extract(rast_masked_day1_pondsonly_distance,vect_pond_names)$B1

rast_masked_day1_pondsonly_focal <- focal(rast_masked_day1_pondsonly,
                                          9, 
                                          "modal", 
                                          na.policy="only",
                                          na.rm=T)
rast_masked_day3_pondsonly_focal <- focal(rast_masked_day3_pondsonly,
                                          9, 
                                          "modal", 
                                          na.policy="only",
                                          na.rm=T)

vect_pond_names$NDCI_day1 <- extract(rast_masked_day1_pondsonly_focal,vect_pond_names)$NDCI
vect_pond_names$NDCI_day3 <- extract(rast_masked_day3_pondsonly_focal,vect_pond_names)$NDCI
vect_pond_names


df_wide_c_13_focal <- merge(df_wide_c_13,vect_pond_names,by="pond")
df_wide_c_13_focal$NDCI_polygon <- df_wide_c_13_focal$NDCI.x
df_wide_c_13_focal$NDCI_point <- ifelse(df_wide_c_13_focal$day==1,
                                        df_wide_c_13_focal$NDCI_day1,
                                        df_wide_c_13_focal$NDCI_day3)

ggplot(aes(x=NDCI_point,y=NDCI_polygon),data=df_wide_c_13_focal) +
  geom_point() +
  xlim(0,0.5) +
  ylim(0,0.5)
ggplot(aes(x=chlorophyll,y=NDCI_point),data=df_wide_c_13_focal) +
  geom_point()

lm_chl_quadratic_point <- lm(chlorophyll ~ NDCI_point + I(NDCI_point^2), df_wide_c_13_focal)
summary(lm_chl_quadratic_point)
lm_chl_linear_point <- lm(NDCI_point~chlorophyll,data=df_wide_c_13_focal)
summary(lm_chl_linear_point)
cor.test(df_wide_c_13_focal$NDCI_point,
         df_wide_c_13_focal$chlorophyll)



ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day1_pondsonly)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")
ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day1)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")
rast_day1$NDCI <- (rast_day1$B5-rast_day1$B4)/(rast_day1$B5+rast_day1$B4)
ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_day1)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red") +
  geom_spatvector(data=pond_polygons_d,fill=NA,colour="purple",linewidth=1)
ggplot() +
  geom_spatraster(aes(fill=NDCI),data=rast_masked_day1_pondsonly_focal)  +
  scale_fill_viridis() +
  geom_spatvector(data=vect_pond_names,fill=NA,colour="red")

extract(rast_day1$NDCI,pond_polygons_d,fun=mean)
extract(rast_day1$NDCI,pond_polygons_d,fun=range)

extract(not.na(rast_day1), pond_polygons_d,fun=mean)
aggregate(cell~ID,FUN=length,data=cells(rast_masked_day1,project(pond_polygons_d,rast_masked_day2)))$cell


extract(rast_masked_day2,project(pond_polygons_d,rast_masked_day2),na.rm=TRUE)$B4
aggregate(cell~ID,FUN=length,data=cells(rast_masked_day1,project(pond_polygons_d,rast_masked_day2)))
aggregate(cell~ID,FUN=length,data=cells(rast_masked_day2,project(pond_polygons_d,rast_masked_day2)))
cells(rast_masked_day2,project(pond_polygons_d,rast_masked_day2))



mean(extract(rast_masked_day1,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,na.rm=T)
mean(extract(rast_masked_day2,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,na.rm=T)
mean(extract(rast_masked_day3,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,na.rm=T)
mean(extract(rast_masked_day4,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,na.rm=T)
mean(extract(rast_masked_day5,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,na.rm=T)

length(cells(rast_masked_day1))
length(cells(rast_masked_day2))


rast_day2
cells(rast_day2)
length(extract(rast_day2,pond_polygons_d,na.rm=TRUE)$B4)
extract(rast_day2,pond_polygons_d,cells=T,na.rm=TRUE)$B4
extract(rast_day2,pond_polygons_d,na.rm=TRUE)$B4
length(extract(rast_day2,pond_polygons_d,na.rm=TRUE)$B4)*(60^2)/100
extract(rast_masked_day2,pond_polygons_d,fun=mean,na.rm=TRUE)$B4

rast_day1
rast_masked_day1
cellSize(rast_day2)
cellSize(rast_masked_day2)
