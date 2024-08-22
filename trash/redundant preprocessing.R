
#gt_ammonia <- read.csv("data/gt_ammonia.csv")
#gt_chlorophyll <- read.csv("data/gt_chlorophyll.csv")
#gt_do <- read.csv("data/gt_do.csv")
#gt_ph <- read.csv("data/gt_ph.csv")
#gt_phycocyanin <- read.csv("data/gt_phycocyanin.csv")
#gt_temperature <- read.csv("data/gt_temperature.csv")

#df_pond_names <- read.csv("data/pond_names.csv")
#pond_polygons <- vect("data/revised_pond_polygons.kml")


# Disaggregate pond polygons into multiple polygons
#pond_polygons_d <- disagg(pond_polygons)

# Convert coordinates to decimal degrees
#df_pond_names$latitude <- word(df_pond_names$point,1)
#df_pond_names$longitude <- word(df_pond_names$point,2)
#df_pond_names$latitude<-gsub('°',' ',df_pond_names$latitude)
#df_pond_names$latitude<-gsub('\'',' ',df_pond_names$latitude)
#df_pond_names$latitude<-gsub('\\"'," ",df_pond_names$latitude)
#df_pond_names$latitude<-gsub('N',"",df_pond_names$latitude)
#df_pond_names$longitude<-gsub('°',' ',df_pond_names$longitude)
#df_pond_names$longitude<-gsub('\'',' ',df_pond_names$longitude)
#df_pond_names$longitude<-gsub('\\"'," ",df_pond_names$longitude)
#df_pond_names$longitude<-gsub('E',"",df_pond_names$longitude)
#df_pond_names$lat <- as.numeric(conv_unit(df_pond_names$latitude,
#from="deg_min_sec",
#to="dec_deg"))
#df_pond_names$lon <- as.numeric(conv_unit(df_pond_names$longitude,
#from="deg_min_sec",
#to="dec_deg"))
#df_pond_names <- df_pond_names[,c("pond","polygon","lon","lat")]


# Need to correct df_pond_names and pond_polygons_d such that the ponds are labelled
# north to south (since I revised the polygon KML file)
#df_pond_names$polygon <- order(df_pond_names$lat)
#pond_polygons_d_centroids <- as.data.frame(geom(centroids(pond_polygons_d)))
#pond_polygons_d_centroids$rank <- rank(pond_polygons_d_centroids$y)
#pond_polygons_d$ID <- rank(pond_polygons_d_centroids$y)

# Convert pond points to spatvector
#vect_pond_names <- as_spatvector(df_pond_names)
#crs(vect_pond_names) <- crs(pond_polygons_d)

# Make sure pond and polygon IDs are correct

#ggplot() +
#geom_spatvector(data=pond_polygons_d) +
#geom_spatvector(data=vect_pond_names) +
#geom_spatvector_label(aes(label=ID),data=pond_polygons_d) +
#geom_spatvector_text(aes(label=polygon),data=vect_pond_names,color="blue") +
#geom_spatvector_text(aes(label=pond),data=vect_pond_names)

# Swap ponds 11 and 12
#df_pond_names[which(df_pond_names$polygon==11),]$polygon <- 100
#df_pond_names[which(df_pond_names$polygon==12),]$polygon <- 11
#df_pond_names[which(df_pond_names$polygon==100),]$polygon <- 12

# Save pond names to file
#write.csv(df_pond_names,"intermediate/df_pond_names.csv",
#row.names=F)


# Combine ground truth data
#names(gt_ammonia)[c(2:6)] <- paste0("ammonia_",names(gt_ammonia))[c(2:6)]
#names(gt_chlorophyll)[c(2:6)] <- paste0("chlorophyll_",names(gt_chlorophyll))[c(2:6)]
#names(gt_do)[c(2:6)] <- paste0("do_",names(gt_do))[c(2:6)]
#names(gt_ph)[c(2:6)] <- paste0("ph_",names(gt_ph))[c(2:6)]
#names(gt_phycocyanin)[c(2:6)] <- paste0("phycocyanin_",names(gt_phycocyanin))[c(2:6)]
#names(gt_temperature)[c(2:6)] <- paste0("temperature_",names(gt_temperature))[c(2:6)]

#df_gt <- cbind(gt_ammonia, gt_chlorophyll, gt_do, gt_ph, gt_phycocyanin, gt_temperature)
#df_gt <- df_gt[,-c(7,13,19,25,31)]

# Match ground truth data with coords
#df_gt <- merge(df_pond_names,df_gt,by="pond")


# Specify the coordinate that we're going to use for haze correction
#haze <- data.frame("lat"=16.661489, "lon"=81.109449)
#haze <- data.frame("lat"=16.661643, "lon"=81.109135)
#haze <- data.frame("lat"=16.643152, "lon"=81.137731)
#vec_haze <- vect(haze, geom=c("lon","lat"), crs="epsg:4326")
#vec_haze_proj <- project(vec_haze, rast_tmp)

# get some info relevant to haze
#df_haze <- rbind(extract(rast_day1,vec_haze_proj),
#extract(rast_day2,vec_haze_proj),
#extract(rast_day3,vec_haze_proj),
#extract(rast_day4,vec_haze_proj),
#extract(rast_day5,vec_haze_proj))
#df_haze_day3 <- rbind(df_haze[3,],
#df_haze[3,],
#df_haze[3,],
#df_haze[3,],
#df_haze[3,])
#df_haze_rel <- df_haze/df_haze_day3
#df_haze_rel


# Adjust for haze (sorry)
#rast_tmp_hazed <- rast_tmp
#rast_tmp_hazed$B1 <- rast_tmp_hazed$B1/df_haze_rel$B1[i]
#rast_tmp_hazed$B2 <- rast_tmp_hazed$B2/df_haze_rel$B2[i]
#rast_tmp_hazed$B3 <- rast_tmp_hazed$B3/df_haze_rel$B3[i]
#rast_tmp_hazed$B4 <- rast_tmp_hazed$B4/df_haze_rel$B4[i]
#rast_tmp_hazed$B5 <- rast_tmp_hazed$B5/df_haze_rel$B5[i]
#rast_tmp_hazed$B6 <- rast_tmp_hazed$B6/df_haze_rel$B6[i]
#rast_tmp_hazed$B7 <- rast_tmp_hazed$B7/df_haze_rel$B7[i]
#rast_tmp_hazed$B8 <- rast_tmp_hazed$B8/df_haze_rel$B8[i]
#rast_tmp_hazed$B8A <- rast_tmp_hazed$B8A/df_haze_rel$B8A[i]
#rast_tmp_hazed$B9 <- rast_tmp_hazed$B9/df_haze_rel$B9[i]
#rast_tmp_hazed$B11 <- rast_tmp_hazed$B11/df_haze_rel$B11[i]
#rast_tmp_hazed$B12 <- rast_tmp_hazed$B12/df_haze_rel$B12[i]
#assign(paste0("rast_hazed_day",i), rast_tmp_hazed)

# Scale
#rast_tmp_hazed_masked_scaled <- terra::scale(rast_tmp_hazed_masked,
#center=F,
#scale=T)
#assign(paste0("rast_hazed_masked_scaled_day",i), rast_tmp_hazed_masked_scaled)


