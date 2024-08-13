#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(caret) #for neural networks
library(ggplot2) #For graphing
library(viridis) #To help graphing
library(gridExtra) #To help graphing
theme_set(theme_bw()) #Because I'm fashionable

# Load ground truth data
gt_ammonia <- read.csv("data/gt_ammonia.csv")
gt_chlorophyll <- read.csv("data/gt_chlorophyll.csv")
gt_do <- read.csv("data/gt_do.csv")
gt_ph <- read.csv("data/gt_ph.csv")
gt_phycocyanin <- read.csv("data/gt_phycocyanin.csv")
gt_temperature <- read.csv("data/gt_temperature.csv")
df_dates <- read.csv("data/dates.csv")
df_pond_names <- read.csv("data/pond_names.csv")
pond_polygons <- vect("data/revised_pond_polygons.kml")

# Disaggregate pond polygons into multiple polygons
pond_polygons_d <- disagg(pond_polygons)

# Convert coordinates to decimal degrees
df_pond_names$latitude <- word(df_pond_names$point,1)
df_pond_names$longitude <- word(df_pond_names$point,2)
df_pond_names$latitude<-gsub('°',' ',df_pond_names$latitude)
df_pond_names$latitude<-gsub('\'',' ',df_pond_names$latitude)
df_pond_names$latitude<-gsub('\\"'," ",df_pond_names$latitude)
df_pond_names$latitude<-gsub('N',"",df_pond_names$latitude)
df_pond_names$longitude<-gsub('°',' ',df_pond_names$longitude)
df_pond_names$longitude<-gsub('\'',' ',df_pond_names$longitude)
df_pond_names$longitude<-gsub('\\"'," ",df_pond_names$longitude)
df_pond_names$longitude<-gsub('E',"",df_pond_names$longitude)
df_pond_names$lat <- as.numeric(conv_unit(df_pond_names$latitude,
                                          from="deg_min_sec",
                                          to="dec_deg"))
df_pond_names$lon <- as.numeric(conv_unit(df_pond_names$longitude,
                                          from="deg_min_sec",
                                          to="dec_deg"))
df_pond_names <- df_pond_names[,c("pond","polygon","lon","lat")]


# Need to correct df_pond_names and pond_polygons_d such that the ponds are labelled
# north to south (since I revised the polygon KML file)
df_pond_names$polygon <- order(df_pond_names$lat)
pond_polygons_d_centroids <- as.data.frame(geom(centroids(pond_polygons_d)))
pond_polygons_d_centroids$rank <- rank(pond_polygons_d_centroids$y)
pond_polygons_d$ID <- rank(pond_polygons_d_centroids$y)

# Convert pond points to spatvector
vect_pond_names <- as_spatvector(df_pond_names)
crs(vect_pond_names) <- crs(pond_polygons_d)


# Make sure pond and polygon IDs are correct

ggplot() +
  geom_spatvector(data=pond_polygons_d) +
  geom_spatvector(data=vect_pond_names) +
  geom_spatvector_label(aes(label=ID),data=pond_polygons_d) +
  #geom_spatvector_text(aes(label=polygon),data=vect_pond_names,color="blue") +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names)
  
# Swap ponds 11 and 12
df_pond_names[which(df_pond_names$polygon==11),]$polygon <- 100
df_pond_names[which(df_pond_names$polygon==12),]$polygon <- 11
df_pond_names[which(df_pond_names$polygon==100),]$polygon <- 12

# Save pond names to file
write.csv(df_pond_names,"intermediate/df_pond_names.csv",
          row.names=F)


# Combine ground truth data
names(gt_ammonia)[c(2:6)] <- paste0("ammonia_",names(gt_ammonia))[c(2:6)]
names(gt_chlorophyll)[c(2:6)] <- paste0("chlorophyll_",names(gt_chlorophyll))[c(2:6)]
names(gt_do)[c(2:6)] <- paste0("do_",names(gt_do))[c(2:6)]
names(gt_ph)[c(2:6)] <- paste0("ph_",names(gt_ph))[c(2:6)]
names(gt_phycocyanin)[c(2:6)] <- paste0("phycocyanin_",names(gt_phycocyanin))[c(2:6)]
names(gt_temperature)[c(2:6)] <- paste0("temperature_",names(gt_temperature))[c(2:6)]

df_gt <- cbind(gt_ammonia, gt_chlorophyll, gt_do, gt_ph, gt_phycocyanin, gt_temperature)
df_gt <- df_gt[,-c(7,13,19,25,31)]

# Match ground truth data with coords
df_gt <- merge(df_pond_names,df_gt,by="pond")

# Import sentinel-2 images as SpatRasters
filenames <- list.files("gee_tifs/")
for (i in c(1:length(filenames))){
  rast_tmp <- rast(paste0("gee_tifs/",filenames[i]))
  assign(paste0("rast_day",i), rast_tmp)
}

# Specify the coordinate that we're going to use for haze correction
#haze <- data.frame("lat"=16.661489, "lon"=81.109449)
#haze <- data.frame("lat"=16.661643, "lon"=81.109135)
haze <- data.frame("lat"=16.643152, "lon"=81.137731)
vec_haze <- vect(haze, geom=c("lon","lat"), crs="epsg:4326")
vec_haze_proj <- project(vec_haze, rast_tmp)

# get some info relevant to haze
df_haze <- rbind(extract(rast_day1,vec_haze_proj),
                 extract(rast_day2,vec_haze_proj),
                 extract(rast_day3,vec_haze_proj),
                 extract(rast_day4,vec_haze_proj),
                 extract(rast_day5,vec_haze_proj))
df_haze_day3 <- rbind(df_haze[3,],
                      df_haze[3,],
                      df_haze[3,],
                      df_haze[3,],
                      df_haze[3,])
df_haze_rel <- df_haze/df_haze_day3
df_haze_rel

# Import cloud mask images as SpatRasters
filenames_cloud <- list.files("gee_cloudmasks/")
for (i in c(1:length(filenames_cloud))){
  cloud_tmp <- rast(paste0("gee_cloudmasks/",filenames[i]))
  assign(paste0("cloud_day",i), cloud_tmp)
}

# Graph cloud mask images to get our bearings
g_cloud_1 <- ggplot() +
  geom_spatraster(data=cloud_day1) +
  scale_fill_viridis(direction=1,option="cividis") +
  ggtitle("Day 1; cloud probability")
g_cloud_2 <- ggplot() +
  geom_spatraster(data=cloud_day2) +
  scale_fill_viridis(direction=1,option="cividis") +
  ggtitle("Day 2; cloud probability")
g_cloud_3 <- ggplot() +
  geom_spatraster(data=cloud_day3) +
  scale_fill_viridis(direction=1,option="cividis") +
  ggtitle("Day 3; cloud probability")
g_cloud_4 <- ggplot() +
  geom_spatraster(data=cloud_day4) +
  scale_fill_viridis(direction=1,option="cividis") +
  ggtitle("Day 4; cloud probability")
g_cloud_5 <- ggplot() +
  geom_spatraster(data=cloud_day5) +
  scale_fill_viridis(direction=1,option="cividis") +
  ggtitle("Day 5; cloud probability")
g_cloud <- grid.arrange(g_cloud_1,
                        g_cloud_2,
                        g_cloud_3,
                        g_cloud_4,
                        g_cloud_5,
                        nrow=5)


# Generate histograms of the cloud masks to get our bearings
hist_cloud_1 <- ggplot(aes(x=probability),data=cloud_day1) +
  geom_histogram(bins=100) +
  scale_x_continuous(breaks=seq(0,100,2))
hist_cloud_2 <- ggplot(aes(x=probability),data=cloud_day2) +  
  geom_histogram(bins=100) +
  scale_x_continuous(breaks=seq(0,100,2))
hist_cloud_3 <- ggplot(aes(x=probability),data=cloud_day3) +  
  geom_histogram(bins=100) +
  scale_x_continuous(breaks=seq(0,100,2))
hist_cloud_4 <- ggplot(aes(x=probability),data=cloud_day4) +  
  geom_histogram(bins=100) +
  scale_x_continuous(breaks=seq(0,100,2))
hist_cloud_5 <- ggplot(aes(x=probability),data=cloud_day5) +  
  geom_histogram(bins=100) +
  scale_x_continuous(breaks=seq(0,100,2))


g_hist_clouds <- grid.arrange(hist_cloud_1,
                              hist_cloud_2,
                              hist_cloud_3,
                              hist_cloud_4,
                              hist_cloud_5,
                              nrow=5)
ggsave("preliminary_analysis/g_hist_clouds.png",g_hist_clouds,width=10,height=20)

# Choose the cloud mask thresholds
# (from day 1 to day 5, in order)
cloud_threshold <- c(10,30,12,22,26)



# Now repeat the process (sorry)
# mask by the cloud threshold
# and then extract mean values for each band for each pond
# Note that this requires the cloud masks and the main data tifs
# to have identical filenames per day
for (i in c(1:length(filenames))){
  # Import rasters
  rast_tmp <- rast(paste0("gee_tifs/",filenames[i]))
  cloud_tmp <- rast(paste0("gee_cloudmasks/",filenames[i]))
  
  # Adjust for haze (sorry)
  rast_tmp_hazed <- rast_tmp
  rast_tmp_hazed$B1 <- rast_tmp_hazed$B1/df_haze_rel$B1[i]
  rast_tmp_hazed$B2 <- rast_tmp_hazed$B2/df_haze_rel$B2[i]
  rast_tmp_hazed$B3 <- rast_tmp_hazed$B3/df_haze_rel$B3[i]
  rast_tmp_hazed$B4 <- rast_tmp_hazed$B4/df_haze_rel$B4[i]
  rast_tmp_hazed$B5 <- rast_tmp_hazed$B5/df_haze_rel$B5[i]
  rast_tmp_hazed$B6 <- rast_tmp_hazed$B6/df_haze_rel$B6[i]
  rast_tmp_hazed$B7 <- rast_tmp_hazed$B7/df_haze_rel$B7[i]
  rast_tmp_hazed$B8 <- rast_tmp_hazed$B8/df_haze_rel$B8[i]
  rast_tmp_hazed$B8A <- rast_tmp_hazed$B8A/df_haze_rel$B8A[i]
  rast_tmp_hazed$B9 <- rast_tmp_hazed$B9/df_haze_rel$B9[i]
  rast_tmp_hazed$B11 <- rast_tmp_hazed$B11/df_haze_rel$B11[i]
  rast_tmp_hazed$B12 <- rast_tmp_hazed$B12/df_haze_rel$B12[i]
  assign(paste0("rast_hazed_day",i), rast_tmp_hazed)
  
  
  # Perform cloud masking
  #rast_tmp_resampled <- resample(rast_tmp,cloud_tmp,method="near")
  cloud_tmp_resampled <- resample(cloud_tmp,rast_tmp,method="near")
  rast_tmp_hazed_masked <- mask(x = rast_tmp,
                          mask = cloud_tmp_resampled,
                          maskvalues = seq(cloud_threshold[i],100,1))
  assign(paste0("rast_hazed_masked_day",i), rast_tmp_hazed_masked)
  
  # Scale
  rast_tmp_hazed_masked_scaled <- terra::scale(rast_tmp_hazed_masked,
                                        center=F,
                                        scale=T)
  assign(paste0("rast_hazed_masked_scaled_day",i), rast_tmp_hazed_masked_scaled)
  
  # Extract themean  reflectance data from the ponds
  #extract_tmp <- extract(rast_tmp_masked,pond_polygons_d,fun=mean,na.rm=TRUE)
  extract_tmp <- extract(rast_tmp_hazed_masked_scaled,pond_polygons_d,fun=mean,na.rm=TRUE,
                         touches=F)
  
  names(extract_tmp)[c(2:ncol(extract_tmp))] <- 
    paste0(names(extract_tmp)[c(2:ncol(extract_tmp))],"_day",i)
  
  assign(paste0("extract_rast_day",i), extract_tmp)

}

# Generate true colour images to get our bearings
g_day1_rgb_1 <- ggplot() +
  geom_spatraster_rgb(data=rast_day1,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 1; RGB")
g_day1_rgb_2 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_day1,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 1; RGB; hazed")
g_day1_rgb_3 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_masked_scaled_day1,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 1; RGB; masked; hazed")
g_day2_rgb_1 <- ggplot() +
  geom_spatraster_rgb(data=rast_day2,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 2; RGB")
g_day2_rgb_2 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_day2,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 2; RGB; hazed")
g_day2_rgb_3 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_masked_scaled_day2,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 2; RGB; masked; hazed")
g_day3_rgb_1 <- ggplot() +
  geom_spatraster_rgb(data=rast_day3,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 3; RGB")
g_day3_rgb_2 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_day3,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 3; RGB; hazed")
g_day3_rgb_3 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_masked_scaled_day3,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 3; RGB; masked; hazed")
g_day4_rgb_1 <- ggplot() +
  geom_spatraster_rgb(data=rast_day4,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 4; RGB")
g_day4_rgb_2 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_day4,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 4; RGB; hazed")
g_day4_rgb_3 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_masked_scaled_day4,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 4; RGB; masked; hazed")
g_day5_rgb_1 <- ggplot() +
  geom_spatraster_rgb(data=rast_day5,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 5; RGB")
g_day5_rgb_2 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_day5,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 5; RGB; hazed")
g_day5_rgb_3 <- ggplot() +
  geom_spatraster_rgb(data=rast_hazed_masked_scaled_day5,r=4,g=3,b=2,max_col_value=3500) +
  geom_spatvector(data=pond_polygons_d,fill=NA,linewidth=1.5) +
  geom_spatvector_text(aes(label=pond),data=vect_pond_names,size=2) +
  ggtitle("Day 5; RGB; masked; hazed")

# Combine all into a side-by-side comparison
g_rgb_clouds <- grid.arrange(g_day1_rgb_1,
                             g_day1_rgb_2,
                             g_day1_rgb_3,
                             g_cloud_1,
                             g_day3_rgb_1,
                             g_day3_rgb_2,
                             g_day3_rgb_3,
                             g_cloud_3,
                             g_day4_rgb_1,
                             g_day4_rgb_2,
                             g_day4_rgb_3,
                             g_cloud_4,
                             g_day5_rgb_1,
                             g_day5_rgb_2,
                             g_day5_rgb_3,
                             g_cloud_5,
                             nrow=5)
ggsave("preliminary_analysis/g_rgb_clouds.png",g_rgb_clouds,width=20,height=16)

# Merge band pond data into a single data frame
# https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
df_rast <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "ID", all.x = TRUE),
       mget(ls(pattern="extract_rast_day")))
df_rast <- merge(df_rast,df_pond_names[c("pond","polygon")],
                 by.x="ID",by.y="polygon")

# Combine ground-truthed data with raster data
df_gt_rast <- merge(df_gt,df_rast,by="pond")

#Remove ID col
df_gt_rast <- df_gt_rast[,-35] 

# Order the columns correctly
namevec <- c(names(df_gt_rast)[c(1:4)],sort(names(df_gt_rast)[c(5:ncol(df_gt_rast))]))
df_gt_rast <- df_gt_rast[,namevec]
# Get the variable names
vnames <- substr(names(df_gt_rast)[c(5:ncol(df_gt_rast))],1,(nchar(names(df_gt_rast))[c(5:ncol(df_gt_rast))]-5))
vnames <- unique(vnames)

tmp_seq <- seq(5,ncol(df_gt_rast),1)
tmp_seq_split <- split(tmp_seq, as.integer(gl(length(tmp_seq), length(filenames), length(tmp_seq))))
names(tmp_seq_split) <- NULL

# Reshape to long format
df_wide <- reshape(df_gt_rast,
                   direction="long",
                   varying=tmp_seq_split,
                   v.names=vnames,
                   timevar="day",
                   idvar=c("pond"),
                   times=c(1:length(filenames)))

# Convert character columns to numeric
df_wide$ammonia <- as.numeric(df_wide$ammonia)
df_wide$chlorophyll <- as.numeric(df_wide$chlorophyll)
df_wide$do <- as.numeric(df_wide$do)
df_wide$ph <- as.numeric(df_wide$ph)
df_wide$phycocyanin <- as.numeric(df_wide$phycocyanin)
df_wide$temperature <- as.numeric(df_wide$temperature)

# Retain only complete cases
df_wide_c <- df_wide[complete.cases(df_wide),]

# Check average values by day
aggregate(B2~day,FUN=mean,df_wide_c)
aggregate(B3~day,FUN=mean,df_wide_c)
aggregate(B4~day,FUN=mean,df_wide_c)

# Save to file
write.csv(df_wide,"intermediate/df_wide.csv",row.names = F)
write.csv(df_wide_c,"intermediate/df_wide_c.csv",row.names = F)




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

###############################
### Try published equations ###
###############################
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

