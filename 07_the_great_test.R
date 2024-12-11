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

########################
### Machine learning ###
########################

# Load data
df_wide_c <- read.csv("intermediate/df_clean.csv")

# Restrict based on cell numbers
df_wide_c <- df_wide_c[which(df_wide_c$cells_count>5 & df_wide_c$cells_prop>0.5),]

# Generate the machine learning model
# based on the original data
trc <- trainControl(method="repeatedcv",
                    number=10,
                    repeats=3)

svm_grid <- expand.grid("C"=c(10e-5,10e-4,10e-3,10e-2,0.25,0.5,10e-1,10e0,10e1,10e2,10e3,10e4,10e5),
                        "sigma"=c(0.05,0.1,0.15,0.5))

# Train machine learning models for chlorophyll
svm_chl <- train(chl~B2+B3+B4+B5+B6+B7+B8+B8A+NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b,
                 data=df_wide_c,
                 method="svmRadial",
                 preProcess=c("center","scale"),
                 tuneGrid=svm_grid,
                 trControl = trc)

# Train machine learning models for pH
svm_ph <- train(ph~B2+B3+B4+B5+B6+B7+B8+B8A+NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b,
                 data=df_wide_c,
                 method="svmRadial",
                 preProcess=c("center","scale"),
                 tuneGrid=svm_grid,
                 trControl = trc)

# Generate OLS model for chlorophyll
ols_chl <- lm(chl~B2+B3+B4+B5+B6+B7+B8+B8A+NDMI+NDWI_b+NDCI_b+NDTI_b+MNDWI_b+NDVI_b,
              data=df_wide_c)

#######################
### Obtain new data ###
#######################

# List new dates of interest
dates_of_interest <- c("2024-09-16","2024-10-02")

# Import new pond geometries and calculate regions of interest
pond_polygon_CRP1 <- vect("the_great_test/Pond Boundary files/1st cluster/1. CRP 1/CRP 1.kml")
pond_polygon_MUR1 <- vect("the_great_test/Pond Boundary files/1st cluster/2. MUR 1/MUR 1.kml")
pond_polygon_KVR3 <- vect("the_great_test/Pond Boundary files/1st cluster/3. KVR 3/KVR 3.kml")
pond_polygon_NRO1 <- vect("the_great_test/Pond Boundary files/1st cluster/4. NRO 1/NRO 1.kml")
pond_polygon_MLN1 <- vect("the_great_test/Pond Boundary files/1st cluster/5. MLN 1/MLN 1.kml")
pond_polygon_VMS1 <- vect("the_great_test/Pond Boundary files/1st cluster/6. VMS 1/VMS 1.kml")
pond_polygon_GRA1 <- vect("the_great_test/Pond Boundary files/1st cluster/7. GRA 1/GRA 1.kml")
pond_polygon_SRI2 <- vect("the_great_test/Pond Boundary files/1st cluster/8. SRI 2/SRI 2.kml")
pond_polygon_SRI1 <- vect("the_great_test/Pond Boundary files/1st cluster/9. SRI 1/SRI 1.kml")
pond_polygon_GIL1 <- vect("the_great_test/Pond Boundary files/1st cluster/10. GIL 1/GIL 1.kml")
pond_polygon_SNR2 <- vect("the_great_test/Pond Boundary files/2nd cluster/1. SNR 2/SNR 2.kml")
pond_polygon_JKS1 <- vect("the_great_test/Pond Boundary files/2nd cluster/2. JKS 1/JKS 1.kml")
pond_polygon_PNR1 <- vect("the_great_test/Pond Boundary files/2nd cluster/3. PNR 1/PNR 1.kml")
pond_polygon_GOW2 <- vect("the_great_test/Pond Boundary files/2nd cluster/4. GOW 2/GOW 2.kml")
pond_polygon_SRK1 <- vect("the_great_test/Pond Boundary files/2nd cluster/5. SRK 1/SRK 1.kml")
pond_polygon_BSK2 <- vect("the_great_test/Pond Boundary files/2nd cluster/6. BSK 2/BSK 2.kml")
pond_polygon_RRO1 <- vect("the_great_test/Pond Boundary files/2nd cluster/7. RRO 1/RRO 1.kml")
pond_polygon_VVR1 <- vect("the_great_test/Pond Boundary files/2nd cluster/8. VVR 1/VVR 1.kml")
pond_polygon_SBR1 <- vect("the_great_test/Pond Boundary files/2nd cluster/9. SBR 1/SBR 1.kml")
pond_polygon_PRA1 <- vect("the_great_test/Pond Boundary files/2nd cluster/10. PRA 1/PRA 1.kml")
pond_polygon_CRP1$Name <- "CRP1"
pond_polygon_MUR1$Name <- "MUR1"
pond_polygon_KVR3$Name <- "KVR3"
pond_polygon_NRO1$Name <- "NRO1"
pond_polygon_MLN1$Name <- "MLN1"
pond_polygon_VMS1$Name <- "VMS1"
pond_polygon_GRA1$Name <- "GRA1"
pond_polygon_SRI2$Name <- "SRI2"
pond_polygon_SRI1$Name <- "SRI1"
pond_polygon_GIL1$Name <- "GIL1"
pond_polygon_SNR2$Name <- "SNR2"
pond_polygon_JKS1$Name <- "JKS1"
pond_polygon_PNR1$Name <- "PNR1"
pond_polygon_GOW2$Name <- "GOW2"
pond_polygon_SRK1$Name <- "SRK1"
pond_polygon_BSK2$Name <- "BSK2"
pond_polygon_RRO1$Name <- "RRO1"
pond_polygon_VVR1$Name <- "VVR1"
pond_polygon_SBR1$Name <- "SBR1"
pond_polygon_PRA1$Name <- "PRA1"

great_test_pond_polygons <- rbind(pond_polygon_CRP1,
                                  pond_polygon_MUR1,
                                  pond_polygon_KVR3,
                                  pond_polygon_NRO1,
                                  pond_polygon_MLN1,
                                  pond_polygon_VMS1,
                                  pond_polygon_GRA1,
                                  pond_polygon_SRI2,
                                  pond_polygon_SRI1,
                                  pond_polygon_GIL1,
                                  pond_polygon_SNR2,
                                  pond_polygon_JKS1,
                                  pond_polygon_PNR1,
                                  pond_polygon_GOW2,
                                  pond_polygon_SRK1,
                                  pond_polygon_BSK2,
                                  pond_polygon_RRO1,
                                  pond_polygon_VVR1,
                                  pond_polygon_SBR1,
                                  pond_polygon_PRA1)

region_of_interest <- as.numeric(c(floor(ext(great_test_pond_polygons)[1]),
                                   floor(ext(great_test_pond_polygons)[3]),
                                   ceiling(ext(great_test_pond_polygons)[2]),
                                   ceiling(ext(great_test_pond_polygons)[4])))

#################################################
### Obtain sentinel-2 images and save to file ###
#################################################

# Define bounding box
bbox <- ee$Geometry$Rectangle(region_of_interest)

# Define image collection (level 2A - surface reflectances)
collection_2a <- ee$
  ImageCollection('COPERNICUS/S2_SR_HARMONIZED')$
  filterDate(dates_of_interest[(length(dates_of_interest)-1)],dates_of_interest[length(dates_of_interest)])$
  filterBounds(bbox)$
  select("B1","B2","B3","B4","B5","B6",
         "B7","B8","B8A","B9","B11","B12",
         "AOT")

# Download images to local
local_collection_2a <- ee_imagecollection_to_local(collection_2a,
                                                   region=bbox)

# Move to a better directory (sorry)
for (i in c(1:length(local_collection_2a))){
  file.rename(from=local_collection_2a[[i]]$dsn,
              to=gsub("satellites","satellites/the_great_test/gee_tifs",local_collection_2a[[i]]$dsn))
}


###########################################
### Obtain cloud masks and save to file ###
###########################################

# Define image collection (cloud probability)
collection_cloud <- ee$
  ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY')$
  filterDate(dates_of_interest[(length(dates_of_interest)-1)],dates_of_interest[length(dates_of_interest)])$
  filterBounds(bbox)

# Download images to local
local_collection_cloud <- ee_imagecollection_to_local(collection_cloud,
                                                      region=bbox)

# Move to a better directory (sorry)
for (i in c(1:length(local_collection_cloud))){
  file.rename(from=local_collection_cloud[[i]]$dsn,
              to=gsub("satellites","satellites/the_great_test/gee_cloudmasks",local_collection_cloud[[i]]$dsn))
}

#################################
### Process these new rasters ###
#################################

# Import sentinel-2 images as SpatRasters
filenames <- list.files("the_great_test/gee_tifs/")
for (i in c("20240916","20241001")){
  # Get just the images corresponding to the given date
  filenames_tmp <- filenames[grep(i,filenames)]
  
  #Create a spatraster collection
  # https://gis.stackexchange.com/questions/407623/merging-raster-in-r
  ic_tmp <- sprc(lapply(paste0("the_great_test/gee_tifs/",filenames_tmp), rast))
  
  # Merge into a mosaic
  rast_tmp <- mosaic(ic_tmp)
  
  # Project raster to coordinate system of pond polygons
  rast_tmp <- project(rast_tmp,
                      crs(great_test_pond_polygons))
  
  # Crop to the extent (plus a bit) of the pond polygons
  rast_tmp <- crop(rast_tmp,
                   ext(great_test_pond_polygons)+0.01)
    
  # Assign to the global environment
  assign(paste0("rast_test_day_",i), rast_tmp)
  
}

# Import cloud mask images as SpatRasters
filenames_cloud <- list.files("the_great_test/gee_cloudmasks/")

for (i in c("20240916","20241001")){
  
  # Get just the images corresponding to the given date
  filenames_tmp <- filenames_cloud[grep(i,filenames_cloud)]
  
  #Create a spatraster collection
  # https://gis.stackexchange.com/questions/407623/merging-raster-in-r
  ic_tmp <- sprc(lapply(paste0("the_great_test/gee_cloudmasks/",filenames_tmp), rast))
  
  # Merge into a mosaic
  rast_tmp <- mosaic(ic_tmp)
  
  # Project raster to coordinate system of pond polygons
  rast_tmp <- project(rast_tmp,
                      crs(great_test_pond_polygons))
  
  # Crop to the extent (plus a bit) of the pond polygons
  rast_tmp <- crop(rast_tmp,
                   ext(great_test_pond_polygons)+0.01)
  
  # Assign to the global environment
  assign(paste0("cloud_test_day_",i), rast_tmp)
  
}

# Make sure the rasters are looking as we expect
ggplot() + 
  geom_spatraster_rgb(r=4,g=3,b=2,max_col_value = 3000,
                      data=rast_tmp_masked20240916) +
  geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons)
ggplot() + 
  geom_spatraster_rgb(r=4,g=3,b=2,max_col_value = 3000,
                      data=rast_test_day_20241001) +
  geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons)
ggplot() + 
  geom_spatraster(data=cloud_test_day_20240916) +
  geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons)
ggplot() + 
  geom_spatraster(data=cloud_test_day_20241001) +
  geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons)


# mask by the cloud threshold
# and then extract mean values for each band for each pond
for (i in c("20240916","20241001")){
  # 1. Import rasters
  rast_tmp <- get(paste0("rast_test_day_",i))
  cloud_tmp <- get(paste0("cloud_test_day_",i))
  
  # 2. Perform cloud masking
  # Resample the cloud mask to the same resolution as the main raster
  cloud_tmp_resampled <- resample(cloud_tmp,rast_tmp,method="near")
  
  # Visualise the resampled cloud layer
  print(ggplot() + 
          geom_spatraster(data=cloud_tmp_resampled) +
          geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons) +
          scale_fill_viridis_c())
  
  # Mask clouds using 20% threshold
  msk <- ifel(cloud_tmp_resampled>20,NA,1)
  rast_tmp_masked <- mask(x = rast_tmp,
                          msk)
  
  # Visualise the masked and non-masked cells
  print(ggplot() + geom_spatraster(data=not.na(rast_tmp_masked)) +
          geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons))
  print(ggplot() + geom_spatraster(data=rast_tmp_masked) +
          geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons))
  
  # Save to the global environment
  assign(paste0("rast_tmp_masked",i), rast_tmp_masked)
  
  # 4. Calculate indices
  rast_tmp_masked$NDMI <- (rast_tmp_masked$B8-rast_tmp_masked$B11)/(rast_tmp_masked$B8+rast_tmp_masked$B11)
  rast_tmp_masked$NDWI_b <- (rast_tmp_masked$B3-rast_tmp_masked$B8)/(rast_tmp_masked$B3+rast_tmp_masked$B8)
  rast_tmp_masked$NDCI_b <- (rast_tmp_masked$B5-rast_tmp_masked$B4)/(rast_tmp_masked$B5+rast_tmp_masked$B4)
  rast_tmp_masked$NDTI_b <- (rast_tmp_masked$B4-rast_tmp_masked$B3)/(rast_tmp_masked$B4+rast_tmp_masked$B3)
  rast_tmp_masked$MNDWI_b <- (rast_tmp_masked$B3-rast_tmp_masked$B11)/(rast_tmp_masked$B3+rast_tmp_masked$B11)
  rast_tmp_masked$NDVI_b <- (rast_tmp_masked$B8-rast_tmp_masked$B4)/(rast_tmp_masked$B8+rast_tmp_masked$B4)
  rast_tmp_masked$mNDHI_b <- (rast_tmp_masked$B4-rast_tmp_masked$B2)/
    (rast_tmp_masked$B4+rast_tmp_masked$B2)
  
  # 5. Extract the mean reflectance data from the ponds
  extract_tmp <- extract(rast_tmp_masked,great_test_pond_polygons,fun=mean,na.rm=TRUE,
                         touches=F,
                         ID=F)
  extract_tmp$pond <- great_test_pond_polygons$Name
  extract_tmp$day <- i
  
  assign(paste0("extract_rast_test_day",i), extract_tmp)
  
  # Get numbers of cells
  # We can just take any band
  # Proportion of non-na cells
  cells_prop_tmp <- extract(not.na(rast_tmp_masked),
                            great_test_pond_polygons,
                            fun=mean,
                            touches=F)$B3
  
  
  # Absolute number of non-na cells
  cells_count_tmp <- extract(not.na(rast_tmp_masked),great_test_pond_polygons,fun=sum,touches=F)$B3
  # Combine into data frame
  cells_tmp <- data.frame("cells_prop" = cells_prop_tmp,
                          "cells_count" = cells_count_tmp,
                          "pond" = great_test_pond_polygons$Name,
                          "day" = i)
  
  assign(paste0("cells_test_day",i),cells_tmp)
  
  
}

# Make sure the rasters are looking as we expect
ggplot() + 
  geom_spatraster_rgb(r=4,g=3,b=2,max_col_value = 3000,
                      data=rast_tmp_masked20240916) +
  geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons)
ggplot() + 
  geom_spatraster_rgb(r=4,g=3,b=2,max_col_value = 3000,
                      data=rast_tmp_masked20241001) +
  geom_spatvector(colour="red",fill=NA,data=great_test_pond_polygons)

# Merge band pond data into a single data frame
# https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
#df_rast <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "ID", all.x = TRUE),
#mget(ls(pattern="extract_rast_day")))
#df_rast <- merge(df_rast,df_pond_names[c("pond","polygon")],
#by.x="ID",by.y="polygon")
df_rast <- Reduce(function(dtf1, dtf2) rbind(dtf1, dtf2),
                  mget(ls(pattern="extract_rast_test_day")))
df_rast <- df_rast[-which(is.nan(df_rast$B1)),]

df_cells_prop <- Reduce(function(dtf1, dtf2) rbind(dtf1, dtf2),
                        mget(ls(pattern="cells_test_day")))

########################
### Make predictions ###
########################
df_rast$chl_pred <- predict(svm_chl,
                            df_rast)
df_rast$ph_pred <- predict(svm_ph,
                            df_rast)

df_rast$chl_pred_ols <- predict(ols_chl,
                                df_rast)

# Save to file
write.csv(df_rast,"the_great_test/df_rast.csv",row.names = F)


######################################
### Compare predictions to reality ###
######################################

df_comparison_chl <- read.csv("the_great_test/The great test_chl.csv")
df_comparison_ph <- read.csv("the_great_test/The great test_ph.csv")

postResample(pred=df_comparison_chl$chlorophyll_modelpredicted,
             obs=df_comparison_chl$chlorophyll_observed)
postResample(pred=df_comparison_chl$chlorophyll_olspredicted,
             obs=df_comparison_chl$chlorophyll_observed)
postResample(pred=df_comparison_ph$pH_modelpredicted,
             obs=df_comparison_ph$pH_observed)


g1 <- ggplot(aes(x=chlorophyll_observed,chlorophyll_modelpredicted),data=df_comparison_chl) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  geom_abline(slope=1,intercept=0,linetype="dashed") +
  xlim(0,NA) + ylim(0,NA)
g1
ggsave("the_great_test/g1.png",g1,width=6,height=5)




g2 <- ggplot(aes(x=chlorophyll_observed,chlorophyll_olspredicted),data=df_comparison_chl) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  geom_abline(slope=1,intercept=0,linetype="dashed") +
  xlim(0,NA) + ylim(0,NA)
g2
ggsave("the_great_test/g2.png",g2,width=6,height=5)


colMeans(df_wide_c[,c("B2","B3","B4")],na.rm=T)
colMeans(df_rast[,c("B2","B3","B4")],na.rm=T)


