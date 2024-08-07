#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(caret) #for neural networks
library(ggplot2) #For graphing
library(viridis) #To help graphing
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
  geom_spatvector_text(aes(label=polygon),data=vect_pond_names)
  


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

# Import cloud mask images as SpatRasters
filenames_cloud <- list.files("gee_cloudmasks/")
for (i in c(1:length(filenames_cloud))){
  cloud_tmp <- rast(paste0("gee_cloudmasks/",filenames[i]))
  assign(paste0("cloud_day",i), cloud_tmp)
}

# Graph cloud mask images to get our bearings
g_cloud_2 <- ggplot() +
  geom_spatraster(data=cloud_day2) +
  scale_fill_viridis(direction=1,option="cividis") +
  ggtitle("Day 2; cloud probability")
g_cloud_5 <- ggplot() +
  geom_spatraster(data=cloud_day5) +
  scale_fill_viridis(direction=1,option="cividis") +
  ggtitle("Day 5; cloud probability")
g_cloud <- grid.arrange(g_cloud_2,g_cloud_5,nrow=2)
ggsave("preliminary_analysis/g_cloud.png",g_cloud,width=8,height=8)

# Choose the cloud mask threshold
cloud_threshold <- 80

# Now repeat the process (sorry)
# mask by the cloud threshold
# and then extract mean values for each band for each pond
# Note that this requires the cloud masks and the main data tifs
# to have identical filenames per day
for (i in c(1:length(filenames))){
  rast_tmp <- rast(paste0("gee_tifs/",filenames[i]))
  cloud_tmp <- rast(paste0("gee_cloudmasks/",filenames[i]))
  
  rast_tmp_resampled <- resample(rast_tmp,cloud_tmp)
  rast_tmp_masked <- mask(x = rast_tmp_resampled,
                          mask = cloud_tmp,
                          maskvalues = seq(cloud_threshold,100,1))
  
  assign(paste0("rast_masked_day",i), rast_tmp_masked)
  
  extract_tmp <- extract(rast_tmp_masked,pond_polygons_d,fun=mean,na.rm=TRUE)
  names(extract_tmp)[c(2:ncol(extract_tmp))] <- 
    paste0(names(extract_tmp)[c(2:ncol(extract_tmp))],"_day",i)
  
  assign(paste0("extract_rast_day",i), extract_tmp)
}

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

# Save to file
write.csv(df_wide,"intermediate/df_wide.csv")
write.csv(df_wide_c,"intermediate/df_wide_c.csv")




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
  ggtitle("Day 2; true-colour (RGB)")
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
  ggtitle("Day 5; true-colour (RGB)")
g_day25_band38 <- grid.arrange(g_day2_band3,
                               g_day2_band8,
                               g_day2_rgb,
                               g_day5_band3,
                               g_day5_band8,
                               g_day5_rgb,
                               nrow=2)
ggsave("preliminary_analysis/g_day25_band38.png",g_day25_band38,width=16,height=12)




