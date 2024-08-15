#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(sf) #For converting geojson files to polygons
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(caret) #for neural networks
library(ggplot2) #For graphing
library(viridis) #To help graphing
library(gridExtra) #To help graphing
theme_set(theme_bw()) #Because I'm fashionable

# Load ground truth data
#gt_ammonia <- read.csv("data/gt_ammonia.csv")
#gt_chlorophyll <- read.csv("data/gt_chlorophyll.csv")
#gt_do <- read.csv("data/gt_do.csv")
#gt_ph <- read.csv("data/gt_ph.csv")
#gt_phycocyanin <- read.csv("data/gt_phycocyanin.csv")
#gt_temperature <- read.csv("data/gt_temperature.csv")
gt <- read.csv("data/gt.csv")
df_dates <- read.csv("data/dates.csv")
#df_pond_names <- read.csv("data/pond_names.csv")
#pond_polygons <- vect("data/revised_pond_polygons.kml")

# Load pond polygons
pond_polygon_GIL2 <- vect("data/pond_boundaries/GIL 2 kml.kml")
pond_polygon_GOW2 <- vect("data/pond_boundaries/GOW 2 kml.kml")
pond_polygon_KIS1 <- vect("data/pond_boundaries/KIS 1 kml.kml")
pond_polygon_PRA1 <- vect("data/pond_boundaries/PRA 1 kml.kml")
pond_polygon_SBR1 <- vect("data/pond_boundaries/SBR 1 kml.kml")
pond_polygon_SRI1 <- vect("data/pond_boundaries/SRI 1 kml.kml")
pond_polygon_SRI2 <- vect("data/pond_boundaries/SRI 2 kml.kml")
pond_polygon_SRK1 <- vect("data/pond_boundaries/SRK 1 kml.kml")
pond_polygon_SRV1 <- vect("data/pond_boundaries/SRV 1 kml.kml")
pond_polygon_VVR1 <- vect("data/pond_boundaries/VVR 1 kml.kml")
pond_polygon_GRA1_lines <- vect("data/pond_boundaries/GRA1.geojson")
pond_polygon_JAG1_lines <- vect("data/pond_boundaries/JAG1.geojson")
pond_polygon_JKS1_lines <- vect("data/pond_boundaries/JKS1.geojson")
pond_polygon_NRO1_lines <- vect("data/pond_boundaries/NRO1.geojson")
pond_polygon_NSR1_lines <- vect("data/pond_boundaries/NSR1.geojson")
pond_polygon_PKR1_lines <- vect("data/pond_boundaries/PKR1.geojson")
pond_polygon_PNR1_lines <- vect("data/pond_boundaries/PNR1.geojson")
pond_polygon_SNR2_lines <- vect("data/pond_boundaries/SNR2.geojson")
pond_polygon_VMS1_lines <- vect("data/pond_boundaries/VMS1.geojson")
pond_polygon_VPS3_lines <- vect("data/pond_boundaries/VPS3.geojson")
pond_polygon_GRA1 <- vect(st_cast(st_as_sf(pond_polygon_GRA1_lines),to="POLYGON"))
pond_polygon_JAG1 <- vect(st_cast(st_as_sf(pond_polygon_JAG1_lines),to="POLYGON"))
pond_polygon_JKS1 <- vect(st_cast(st_as_sf(pond_polygon_JKS1_lines),to="POLYGON"))
pond_polygon_NRO1 <- vect(st_cast(st_as_sf(pond_polygon_NRO1_lines),to="POLYGON"))
pond_polygon_NSR1 <- vect(st_cast(st_as_sf(pond_polygon_NSR1_lines),to="POLYGON"))
pond_polygon_PKR1 <- vect(st_cast(st_as_sf(pond_polygon_PKR1_lines),to="POLYGON"))
pond_polygon_PNR1 <- vect(st_cast(st_as_sf(pond_polygon_PNR1_lines),to="POLYGON"))
pond_polygon_SNR2 <- vect(st_cast(st_as_sf(pond_polygon_SNR2_lines),to="POLYGON"))
pond_polygon_VMS1 <- vect(st_cast(st_as_sf(pond_polygon_VMS1_lines),to="POLYGON"))
pond_polygon_VPS3 <- vect(st_cast(st_as_sf(pond_polygon_VPS3_lines),to="POLYGON"))
pond_polygon_GIL2$Name <- "GIL2"
pond_polygon_GOW2$Name <- "GOW2"
pond_polygon_KIS1$Name <- "KIS1"
pond_polygon_PRA1$Name <- "PRA1"
pond_polygon_SBR1$Name <- "SBR1"
pond_polygon_SRI1$Name <- "SRI1"
pond_polygon_SRI2$Name <- "SRI2"
pond_polygon_SRK1$Name <- "SRK1"
pond_polygon_SRV1$Name <- "SRV1"
pond_polygon_VVR1$Name <- "VVR1"
pond_polygon_GRA1$Name <- "GRA1"
pond_polygon_JAG1$Name <- "JAG1"
pond_polygon_JKS1$Name <- "JKS1"
pond_polygon_NRO1$Name <- "NRO1"
pond_polygon_NSR1$Name <- "NSR1"
pond_polygon_PKR1$Name <- "PKR1"
pond_polygon_PNR1$Name <- "PNR1"
pond_polygon_SNR2$Name <- "SNR2"
pond_polygon_VMS1$Name <- "VMS1"
pond_polygon_VPS3$Name <- "VPS3"


pond_polygons <- rbind(pond_polygon_GIL2,pond_polygon_GOW2,pond_polygon_KIS1,
                                pond_polygon_PRA1,pond_polygon_SBR1,pond_polygon_SRI1,
                                pond_polygon_SRI2,pond_polygon_SRK1,pond_polygon_SRV1,
                                pond_polygon_VVR1,
                                pond_polygon_GRA1,pond_polygon_JAG1,pond_polygon_JKS1,
                                pond_polygon_NRO1,pond_polygon_NSR1,pond_polygon_PKR1,
                                pond_polygon_PNR1,pond_polygon_SNR2,pond_polygon_VMS1,
                                pond_polygon_VPS3)

ggplot() +
  geom_spatvector(data=pond_polygons_fromfile) +
  geom_spatvector_text(aes(label=Name),data=pond_polygons_fromfile)

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

# Import sentinel-2 images as SpatRasters
filenames <- list.files("gee_tifs/")
for (i in c(1:length(filenames))){
  rast_tmp <- rast(paste0("gee_tifs/",filenames[i]))
  assign(paste0("rast_day",i), rast_tmp)
}

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

# Import cloud mask images as SpatRasters
filenames_cloud <- list.files("gee_cloudmasks/")
for (i in c(1:length(filenames_cloud))){
  cloud_tmp <- rast(paste0("gee_cloudmasks/",filenames_cloud[i]))
  assign(paste0("cloud_day",i), cloud_tmp)
}

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
  
  
  # Perform cloud masking
  #rast_tmp_resampled <- resample(rast_tmp,cloud_tmp,method="near")
  cloud_tmp_resampled <- resample(cloud_tmp,rast_tmp,method="near")
  rast_tmp_masked <- mask(x = rast_tmp,
                          mask = cloud_tmp_resampled,
                          maskvalues = seq(cloud_threshold[i],100,1))
  assign(paste0("rast_masked_day",i), rast_tmp_masked)
  
  # Scale
  #rast_tmp_hazed_masked_scaled <- terra::scale(rast_tmp_hazed_masked,
                                        #center=F,
                                        #scale=T)
  #assign(paste0("rast_hazed_masked_scaled_day",i), rast_tmp_hazed_masked_scaled)
  
  # Extract themean  reflectance data from the ponds
  #extract_tmp <- extract(rast_tmp_masked,pond_polygons_d,fun=mean,na.rm=TRUE)
  extract_tmp <- extract(rast_tmp_masked,pond_polygons,fun=mean,na.rm=TRUE,
                         touches=F,
                         ID=F)
  extract_tmp$pond <- pond_polygons$Name
  extract_tmp$day <- i
  
  #names(extract_tmp)[c(1:ncol(extract_tmp))] <- 
  #paste0(names(extract_tmp)[c(1:ncol(extract_tmp))],"_day",i)
  
  assign(paste0("extract_rast_day",i), extract_tmp)
  
  # Get numbers of cells
  # We can just take any band
  # Proportion of non-na cells
  cells_prop_tmp <- extract(not.na(rast_tmp_masked),pond_polygons,fun=mean,touches=F)$B3
  # Absolute number of non-na cells
  cells_count_tmp <- extract(not.na(rast_tmp_masked),pond_polygons,fun=sum,touches=F)$B3
  # Combine into data frame
  cells_tmp <- data.frame("cells_prop" = cells_prop_tmp,
                          "cells_count" = cells_count_tmp,
                          "pond" = pond_polygons$Name,
                          "day" = i)
  
  assign(paste0("cells_day",i),cells_tmp)
  
  
}

# Merge band pond data into a single data frame
# https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
#df_rast <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "ID", all.x = TRUE),
       #mget(ls(pattern="extract_rast_day")))
#df_rast <- merge(df_rast,df_pond_names[c("pond","polygon")],
                 #by.x="ID",by.y="polygon")
df_rast <- Reduce(function(dtf1, dtf2) rbind(dtf1, dtf2),
                  mget(ls(pattern="extract_rast_day")))
df_cells_prop <- Reduce(function(dtf1, dtf2) rbind(dtf1, dtf2),
                        mget(ls(pattern="cells_day")))

# Convert ground truth dataframe to wide
df_gt <- reshape(gt,idvar=c("pond","day"),v.names="value",timevar="variable",direction="wide")
names(df_gt)[c(3:8)] <- gt$variable[c(1:6)]

# Combine ground-truthed data with raster data
df_rast$pondday <- paste0(df_rast$pond,"_",df_rast$day)
df_gt$pondday <- paste0(df_gt$pond,"_",df_gt$day)
df_gt_rast <- merge(df_gt,df_rast,by="pondday")
df_gt_rast$pond <- df_gt_rast$pond.x
df_gt_rast$day <- df_gt_rast$day.x

# Combine our dataset with cell numbers
df_cells_prop$pondday <- paste0(df_cells_prop$pond,"_",df_cells_prop$day)
df_gt_rast_cells <- merge(df_gt_rast,df_cells_prop,by="pondday")

# Make the columns less messy
df_clean <- df_gt_rast_cells[,c("pondday",
                                gt$variable[c(1:6)],
                                names(extract_rast_day1)[c(1:10)],
                                "cells_count",
                                "cells_prop")]
df_clean$pond <- substr(df_clean$pondday,1,4)
df_clean$day <- as.numeric(substr(df_clean$pondday,6,6))

# Save to file
write.csv(df_clean,"intermediate/df_clean.csv",row.names = F)

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


