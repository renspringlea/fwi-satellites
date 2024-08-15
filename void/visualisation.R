
# Graph cloud mask images to get our bearings
#g_cloud_1 <- ggplot() +
#geom_spatraster(data=cloud_day1) +
#scale_fill_viridis(direction=1,option="cividis") +
#ggtitle("Day 1; cloud probability")
#g_cloud_2 <- ggplot() +
#geom_spatraster(data=cloud_day2) +
#scale_fill_viridis(direction=1,option="cividis") +
#ggtitle("Day 2; cloud probability")
#g_cloud_3 <- ggplot() +
#geom_spatraster(data=cloud_day3) +
#scale_fill_viridis(direction=1,option="cividis") +
#ggtitle("Day 3; cloud probability")
#g_cloud_4 <- ggplot() +
#geom_spatraster(data=cloud_day4) +
#scale_fill_viridis(direction=1,option="cividis") +
#ggtitle("Day 4; cloud probability")
#g_cloud_5 <- ggplot() +
#geom_spatraster(data=cloud_day5) +
#scale_fill_viridis(direction=1,option="cividis") +
#ggtitle("Day 5; cloud probability")
#g_cloud <- grid.arrange(g_cloud_1,
#g_cloud_2,
#g_cloud_3,
#g_cloud_4,
#g_cloud_5,
#nrow=5)


# Generate histograms of the cloud masks to get our bearings
#hist_cloud_1 <- ggplot(aes(x=probability),data=cloud_day1) +
#geom_histogram(bins=100) +
#scale_x_continuous(breaks=seq(0,100,2))
#hist_cloud_2 <- ggplot(aes(x=probability),data=cloud_day2) +  
#geom_histogram(bins=100) +
#scale_x_continuous(breaks=seq(0,100,2))
#hist_cloud_3 <- ggplot(aes(x=probability),data=cloud_day3) +  
#geom_histogram(bins=100) +
#scale_x_continuous(breaks=seq(0,100,2))
#hist_cloud_4 <- ggplot(aes(x=probability),data=cloud_day4) +  
#geom_histogram(bins=100) +
#scale_x_continuous(breaks=seq(0,100,2))
#hist_cloud_5 <- ggplot(aes(x=probability),data=cloud_day5) +  
#geom_histogram(bins=100) +
#scale_x_continuous(breaks=seq(0,100,2))

#g_hist_clouds <- grid.arrange(hist_cloud_1,
#hist_cloud_2,
#hist_cloud_3,
#hist_cloud_4,
#hist_cloud_5,
#nrow=5)
#ggsave("preliminary_analysis/g_hist_clouds.png",g_hist_clouds,width=10,height=20)


# Generate true colour images to get our bearings
#g_day1_rgb_1 <- ggplot() +
#geom_spatraster_rgb(data=rast_day1,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 1; RGB")
#g_day1_rgb_2 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_day1,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 1; RGB; hazed")
#g_day1_rgb_3 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_masked_scaled_day1,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 1; RGB; masked; hazed")
#g_day2_rgb_1 <- ggplot() +
#geom_spatraster_rgb(data=rast_day2,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 2; RGB")
#g_day2_rgb_2 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_day2,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 2; RGB; hazed")
#g_day2_rgb_3 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_masked_scaled_day2,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 2; RGB; masked; hazed")
#g_day3_rgb_1 <- ggplot() +
#geom_spatraster_rgb(data=rast_day3,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 3; RGB")
#g_day3_rgb_2 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_day3,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 3; RGB; hazed")
#g_day3_rgb_3 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_masked_scaled_day3,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 3; RGB; masked; hazed")
#g_day4_rgb_1 <- ggplot() +
#geom_spatraster_rgb(data=rast_day4,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 4; RGB")
#g_day4_rgb_2 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_day4,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 4; RGB; hazed")
#g_day4_rgb_3 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_masked_scaled_day4,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 4; RGB; masked; hazed")
#g_day5_rgb_1 <- ggplot() +
#geom_spatraster_rgb(data=rast_day5,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 5; RGB")
#g_day5_rgb_2 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_day5,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 5; RGB; hazed")
#g_day5_rgb_3 <- ggplot() +
#geom_spatraster_rgb(data=rast_hazed_masked_scaled_day5,r=4,g=3,b=2,max_col_value=3500) +
#geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
#ggtitle("Day 5; RGB; masked; hazed")

# Combine all into a side-by-side comparison
#g_rgb_clouds <- grid.arrange(g_day1_rgb_1,
#g_day1_rgb_2,
#g_day1_rgb_3,
#g_cloud_1,
#g_day3_rgb_1,
#g_day3_rgb_2,
#g_day3_rgb_3,
#g_cloud_3,
#g_day4_rgb_1,
#g_day4_rgb_2,
#g_day4_rgb_3,
#g_cloud_4,
#g_day5_rgb_1,
#g_day5_rgb_2,
#g_day5_rgb_3,
#g_cloud_5,
#nrow=5)
ggsave("preliminary_analysis/g_rgb_clouds.png",g_rgb_clouds,width=20,height=16)




##################################################
### Visualise a few bands for a couple of days ###
##################################################
g_day2_band3 <- ggplot() +
  geom_spatraster(aes(fill=B3),data=rast_masked_day2) +
  scale_fill_viridis(direction=-1,option="mako") +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 2; Band 3 (green)")
g_day2_band8 <- ggplot() +
  geom_spatraster(aes(fill=B8),data=rast_masked_day2) +
  scale_fill_viridis(direction=-1,option="rocket") +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 2; Band 8 (near-infrared)")
g_day2_rgb <- ggplot() +
  geom_spatraster_rgb(data=rast_masked_day2,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 2; RGB")
g_day5_band3 <- ggplot() +
  geom_spatraster(aes(fill=B3),data=rast_masked_day5) +
  scale_fill_viridis(direction=-1,option="mako") +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 5; Band 3 (green)")
g_day5_band8 <- ggplot() +
  geom_spatraster(aes(fill=B8),data=rast_masked_day5) +
  scale_fill_viridis(direction=-1,option="rocket") +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 5; Band 8 (near-infrared)")
g_day5_rgb <- ggplot() +
  geom_spatraster_rgb(data=rast_masked_day5,r=4,g=3,b=2,max_col_value=2500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 5; RGB")
g_day25_band38 <- grid.arrange(g_day2_band3,
                               g_day2_band8,
                               g_day2_rgb,
                               g_day5_band3,
                               g_day5_band8,
                               g_day5_rgb,
                               nrow=2)
ggsave("preliminary_analysis/g_day25_band38.png",g_day25_band38,width=16,height=12)


extract(rast_day1,pond_polygons_d,fun=mean,na.rm=TRUE)$B4
extract(rast_day2,pond_polygons_d,fun=mean,na.rm=TRUE)$B4
extract(rast_day3,pond_polygons_d,fun=mean,na.rm=TRUE)$B4
extract(rast_day4,pond_polygons_d,fun=mean,na.rm=TRUE)$B4
extract(rast_day5,pond_polygons_d,fun=mean,na.rm=TRUE)$B4



df_testing_clouds <- data.frame(
  "band" = c(extract(rast_hazed_masked_scaled_day1,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,
             extract(rast_hazed_masked_scaled_day2,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,
             extract(rast_hazed_masked_scaled_day3,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,
             extract(rast_hazed_masked_scaled_day4,pond_polygons_d,fun=mean,na.rm=TRUE)$B4,
             extract(rast_hazed_masked_scaled_day5,pond_polygons_d,fun=mean,na.rm=TRUE)$B4),
  "prop_cells_not_na" = c(extract(not.na(rast_hazed_masked_scaled_day1), pond_polygons_d,fun=mean)$B4,
                          extract(not.na(rast_hazed_masked_scaled_day2), pond_polygons_d,fun=mean)$B4,
                          extract(not.na(rast_hazed_masked_scaled_day3), pond_polygons_d,fun=mean)$B4,
                          extract(not.na(rast_hazed_masked_scaled_day4), pond_polygons_d,fun=mean)$B4,
                          extract(not.na(rast_hazed_masked_scaled_day5), pond_polygons_d,fun=mean)$B4),
  "day"=c(rep("day_1",20),
          rep("day_2",20),
          rep("day_3",20),
          rep("day_4",20),
          rep("day_5",20)
  )
)
ggplot(aes(x=prop_cells_not_na,y=band,colour=day),
       data=df_testing_clouds[which(df_testing_clouds$prop_cells_not_na>0.8),]) +
  geom_point() +
  geom_smooth(method="lm")
