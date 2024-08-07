#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable

# Load data
gt_ammonia <- read.csv("data/gt_ammonia.csv")
gt_chlorophyll <- read.csv("data/gt_chlorophyll.csv")
gt_do <- read.csv("data/gt_do.csv")
gt_ph <- read.csv("data/gt_ph.csv")
gt_phycocyanin <- read.csv("data/gt_phycocyanin.csv")
gt_temperature <- read.csv("data/gt_temperature.csv")
df_dates <- read.csv("data/dates.csv")
df_pond_names <- read.csv("data/pond_names.csv")
pond_polygons <- vect("data/pond_polygons.kml")

# Disaggregate pond polygons into multiple polygons
pond_polygons_d <- disagg(pond_polygons)
pond_polygons_d$ID <- c(1:20)

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

# Convert pond points to spatvector
vect_pond_names <- as_spatvector(df_pond_names)
crs(vect_pond_names) <- crs(pond_polygons_d)

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

# Load images
rast_day1 <- rast("images/raster_day1.tif")
rast_day2 <- rast("images/raster_day2.tif")
rast_day3 <- rast("images/raster_day3.tif")
rast_day4 <- rast("images/raster_day4.tif")

# Calculate indices (sorry)
rast_day1$NDWI <- (rast_day1$B3-rast_day1$B8)/(rast_day1$B3+rast_day1$B8)
rast_day1$NDCI <- (rast_day1$B5-rast_day1$B4)/(rast_day1$B5+rast_day1$B4)
rast_day1$NDTI <- (rast_day1$B4-rast_day1$B3)/(rast_day1$B4+rast_day1$B3)
rast_day1$NDMI <- (rast_day1$B8-rast_day1$B11)/(rast_day1$B8+rast_day1$B11)
rast_day1$MNDWI <- (rast_day1$B3-rast_day1$B11)/(rast_day1$B3+rast_day1$B11)
rast_day1$NDVI <- (rast_day1$B8-rast_day1$B4)/(rast_day1$B8+rast_day1$B4)

rast_day2$NDWI <- (rast_day2$B3-rast_day2$B8)/(rast_day2$B3+rast_day2$B8)
rast_day2$NDCI <- (rast_day2$B5-rast_day2$B4)/(rast_day2$B5+rast_day2$B4)
rast_day2$NDTI <- (rast_day2$B4-rast_day2$B3)/(rast_day2$B4+rast_day2$B3)
rast_day2$NDMI <- (rast_day2$B8-rast_day2$B11)/(rast_day2$B8+rast_day2$B11)
rast_day2$MNDWI <- (rast_day2$B3-rast_day2$B11)/(rast_day2$B3+rast_day2$B11)
rast_day2$NDVI <- (rast_day2$B8-rast_day2$B4)/(rast_day2$B8+rast_day2$B4)

rast_day3$NDWI <- (rast_day3$B3-rast_day3$B8)/(rast_day3$B3+rast_day3$B8)
rast_day3$NDCI <- (rast_day3$B5-rast_day3$B4)/(rast_day3$B5+rast_day3$B4)
rast_day3$NDTI <- (rast_day3$B4-rast_day3$B3)/(rast_day3$B4+rast_day3$B3)
rast_day3$NDMI <- (rast_day3$B8-rast_day3$B11)/(rast_day3$B8+rast_day3$B11)
rast_day3$MNDWI <- (rast_day3$B3-rast_day3$B11)/(rast_day3$B3+rast_day3$B11)
rast_day3$NDVI <- (rast_day3$B8-rast_day3$B4)/(rast_day3$B8+rast_day3$B4)

rast_day4$NDWI <- (rast_day4$B3-rast_day4$B8)/(rast_day4$B3+rast_day4$B8)
rast_day4$NDCI <- (rast_day4$B5-rast_day4$B4)/(rast_day4$B5+rast_day4$B4)
rast_day4$NDTI <- (rast_day4$B4-rast_day4$B3)/(rast_day4$B4+rast_day4$B3)
rast_day4$NDMI <- (rast_day4$B8-rast_day4$B11)/(rast_day4$B8+rast_day4$B11)
rast_day4$MNDWI <- (rast_day4$B3-rast_day4$B11)/(rast_day4$B3+rast_day4$B11)
rast_day4$NDVI <- (rast_day4$B8-rast_day4$B4)/(rast_day4$B8+rast_day4$B4)

# Visualise data
ggplot() +
  geom_spatraster(aes(fill=NDWI), data=rast_day1) +
  geom_spatvector(data=pond_polygons_d) +
  geom_spatvector(data=vect_pond_names) +
  geom_spatvector_label(aes(label=pond),data=vect_pond_names,alpha=0.5) +
  geom_spatvector_label(aes(label=ID),data=pond_polygons_d,alpha=0.5)

# Extract raster info
extract_rast_day1 <- extract(rast_day1,pond_polygons_d,fun=mean)
extract_rast_day2 <- extract(rast_day2,pond_polygons_d,fun=mean)
extract_rast_day3 <- extract(rast_day3,pond_polygons_d,fun=mean)
extract_rast_day4 <- extract(rast_day4,pond_polygons_d,fun=mean)

# Combine extracted raster info
#extract_rast_day1 <- extract_rast_day1[,c("ID","NDWI","NDCI","NDTI","NDMI","MNDWI","NDVI")]
#extract_rast_day2 <- extract_rast_day2[,c("ID","NDWI","NDCI","NDTI","NDMI","MNDWI","NDVI")]
#extract_rast_day3 <- extract_rast_day3[,c("ID","NDWI","NDCI","NDTI","NDMI","MNDWI","NDVI")]
#extract_rast_day4 <- extract_rast_day4[,c("ID","NDWI","NDCI","NDTI","NDMI","MNDWI","NDVI")]
names(extract_rast_day1)[c(2:ncol(extract_rast_day1))] <- paste0(names(extract_rast_day1)[c(2:ncol(extract_rast_day1))],"_day1")
names(extract_rast_day2)[c(2:ncol(extract_rast_day2))] <- paste0(names(extract_rast_day2)[c(2:ncol(extract_rast_day2))],"_day2")
names(extract_rast_day3)[c(2:ncol(extract_rast_day3))] <- paste0(names(extract_rast_day3)[c(2:ncol(extract_rast_day3))],"_day3")
names(extract_rast_day4)[c(2:ncol(extract_rast_day4))] <- paste0(names(extract_rast_day4)[c(2:ncol(extract_rast_day4))],"_day4")

df_rast_12 <- merge(extract_rast_day1,extract_rast_day2,by="ID")
df_rast_34 <- merge(extract_rast_day3,extract_rast_day4,by="ID")
df_rast <-  merge(df_rast_12,df_rast_34,by="ID")
df_rast <- merge(df_rast,df_pond_names[c("pond","polygon")],
                 by.x="ID",by.y="polygon")

# Combine ground-truthed data with raster data
df_gt_rast <- merge(df_gt,df_rast,by="pond")

# Convert from wide to long in days
df_gt_rast <- df_gt_rast[,-35] #Remove ID col
# Remove day 5
df_gt_rast <- df_gt_rast[,-which(substr(names(df_gt_rast),
       (nchar(names(df_gt_rast))-3),
       nchar(names(df_gt_rast)))=="day5")]
# Order the columns correctly
namevec <- c(names(df_gt_rast)[c(1:4)],sort(names(df_gt_rast)[c(5:ncol(df_gt_rast))]))
df_gt_rast <- df_gt_rast[,namevec]
# Get the variable names
vnames <- substr(names(df_gt_rast)[c(5:ncol(df_gt_rast))],1,(nchar(names(df_gt_rast))[c(5:ncol(df_gt_rast))]-5))
vnames <- unique(vnames)

# Reshape to long format
df_wide <- reshape(df_gt_rast,
                   direction="long",
                   varying=list(c(5:8),c(9:12),
                                c(13:16),c(17:20),
                                c(21:24),c(25:28),
                                c(29:32),c(33:36),
                                c(37:40),c(41:44),
                                c(45:48),c(49:52),
                                c(53:56),c(57:60),
                                c(61:64),c(65:68),
                                c(69:72),c(73:76),
                                c(77:80),c(81:84),
                                c(85:88),c(89:92),
                                c(93:96),c(97:100)),
                   v.names=vnames,
                   timevar="day",
                   idvar=c("pond"),
                   times=c(1:4))

# Save to file
write.csv(df_wide,"intermediate/df_wide.csv")
