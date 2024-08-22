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
gt <- read.csv("data/gt.csv")

# Load dates
df_dates <- read.csv("data/dates.csv")

# Load pond polygons
# Some are stored as lines rather than polygons, so I st_cast these
# to polygons.
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

# Merge pond polygons into a single SpatVector
pond_polygons <- rbind(pond_polygon_GIL2,pond_polygon_GOW2,pond_polygon_KIS1,
                                pond_polygon_PRA1,pond_polygon_SBR1,pond_polygon_SRI1,
                                pond_polygon_SRI2,pond_polygon_SRK1,pond_polygon_SRV1,
                                pond_polygon_VVR1,
                                pond_polygon_GRA1,pond_polygon_JAG1,pond_polygon_JKS1,
                                pond_polygon_NRO1,pond_polygon_NSR1,pond_polygon_PKR1,
                                pond_polygon_PNR1,pond_polygon_SNR2,pond_polygon_VMS1,
                                pond_polygon_VPS3)

# Import sentinel-2 images as SpatRasters
filenames <- list.files("gee_tifs/")
for (i in c(1:length(filenames))){
  rast_tmp <- rast(paste0("gee_tifs/",filenames[i]))
  assign(paste0("rast_day",i), rast_tmp)
}

# Import cloud mask images as SpatRasters
filenames_cloud <- list.files("gee_cloudmasks/")
for (i in c(1:length(filenames_cloud))){
  cloud_tmp <- rast(paste0("gee_cloudmasks/",filenames_cloud[i]))
  assign(paste0("cloud_day",i), cloud_tmp)
}

# Choose the cloud mask thresholds
# (from day 1 to day 5, in order)
c(10,30,12,22,26)

# Now repeat the process (sorry)
# mask by the cloud threshold
# and then extract mean values for each band for each pond
# Note that this requires the cloud masks and the main data tifs
# to have identical filenames per day
for (i in c(1:length(filenames))){
  # 1. Import rasters
  rast_tmp <- rast(paste0("gee_tifs/",filenames[i]))
  cloud_tmp <- rast(paste0("gee_cloudmasks/",filenames[i]))
  
  # 2. Perform cloud masking
  # Resample the cloud mask to the same resolution as the main raster
  cloud_tmp_resampled <- resample(cloud_tmp,rast_tmp,method="near")
  
  # Mask
  rast_tmp_masked <- mask(x = rast_tmp,
                          mask = cloud_tmp_resampled,
                          maskvalues = seq(cloud_threshold[i],100,1))
  
  # 3. Perform dehazing using the dark channel prior method
  # see He et al 2011, Single image haze removal using dark channel prior
  # Hultberg et al 2018, Dehazing of Satellite Images
  # Lee et al 2016, A review on dark channel prior based image dehazing algorithms
  
  # Calculate dark channel
  rast_tmp_masked$dark <- min(rast_tmp_masked$B2,
                              rast_tmp_masked$B3,
                              rast_tmp_masked$B4)
  
  # Min-filter the dark channel
  filter_width <- 3 #number of pixels in filter
  rast_tmp_masked$dark_filtered <- focal(rast_tmp_masked$dark,
                                         w=filter_width,
                                         fun="min")
  
  # Calculate atmospheric light
  # First, we need to get the brightest 0.1% of pixels in the dark channel
  # Get the pixel value corresponding to the brightest 0.1%
  brightest_dark <- as.numeric(terra::global(x=rast_tmp_masked$dark_filtered,
                                  fun=quantile,
                                  probs=c(0.999),
                                  na.rm=T))
  
  # Get the pixels in the dark channel with brightness above that value
  # Those pixels are set to 1, and others are set to NA
  classification_matrix <- cbind(c(0,brightest_dark-1),
                                  c(brightest_dark,9999),
                                  c(0,1))
  rast_tmp_brightest_dark <- classify(rast_tmp_masked$dark_filtered,
                                      classification_matrix)
  rast_tmp_brightest_dark <- subst(rast_tmp_brightest_dark,
                                   1,
                                   1,
                                   NA)
  
  # Get the intensity *in RGB* of those bright pixels
  # The atmospheric light for each colour simply equals the maximum of that colour
  # e.g. max_green equals A_green (atmospheric light for green)
  # see Hultberg et al 2018 p 22 for explanation of why the mean is used
  rast_tmp_masked_brightest_dark <- mask(rast_tmp_masked,
                                         rast_tmp_brightest_dark)
  max_blue <- as.numeric(terra::global(rast_tmp_masked_brightest_dark$B2,fun="mean",na.rm=T))
  max_green <- as.numeric(terra::global(rast_tmp_masked_brightest_dark$B3,fun="mean",na.rm=T))
  max_red <- as.numeric(terra::global(rast_tmp_masked_brightest_dark$B4,fun="mean",na.rm=T))
  c(max_blue,max_green,max_red)
  
  # Calculate the normalised input image (I/A)
  rast_tmp_masked$B2_norm <- rast_tmp_masked$B2/max_blue
  rast_tmp_masked$B3_norm <- rast_tmp_masked$B3/max_green
  rast_tmp_masked$B4_norm <- rast_tmp_masked$B4/max_red
  
  # Calculate the normalised dark channel (min(I/A))
  # right-hand side of equation 11 in He et al 2011
  rast_tmp_masked$dark_norm <- min(rast_tmp_masked$B2_norm,
                                   rast_tmp_masked$B3_norm,
                                   rast_tmp_masked$B4_norm)
  
  # Calculate transmission map
  rast_tmp_masked$transmission <- 1-rast_tmp_masked$dark_norm

  # Calculate the scene radiance (i.e., the dehazed image)
  # equation 22 in He et al 2011
  t0 <- 0.1
  rast_tmp_masked$B2_dehazed <- ((rast_tmp_masked$B2-max_blue)/
                                   max(rast_tmp_masked$transmission,t0))+max_blue
  rast_tmp_masked$B3_dehazed <- ((rast_tmp_masked$B3-max_green)/
                                   max(rast_tmp_masked$transmission,t0))+max_green
  rast_tmp_masked$B4_dehazed <- ((rast_tmp_masked$B4-max_red)/
                                   max(rast_tmp_masked$transmission,t0))+max_red
  
  # Save to the global environment
  assign(paste0("rast_tmp_masked",i), rast_tmp_masked)
  assign(paste0("A_",i),c(max_blue,max_green,max_red))
  
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
                                names(extract_rast_day1)[c(1:42)],
                                "cells_count",
                                "cells_prop")]
df_clean$pond <- substr(df_clean$pondday,1,4)
df_clean$day <- as.numeric(substr(df_clean$pondday,6,6))

# Add the average value of the dark channel across all pixels for each day
dark1 <- global(rast_tmp_masked1$dark_filtered,fun="mean",na.rm=T)
dark2 <- global(rast_tmp_masked2$dark_filtered,fun="mean",na.rm=T)
dark3 <- global(rast_tmp_masked3$dark_filtered,fun="mean",na.rm=T)
dark4 <- global(rast_tmp_masked4$dark_filtered,fun="mean",na.rm=T)
dark5 <- global(rast_tmp_masked5$dark_filtered,fun="mean",na.rm=T)
df_clean$dark_day <- NA
df_clean[which(df_clean$day==1),"dark_day"] <- dark1
df_clean[which(df_clean$day==2),"dark_day"] <- dark2
df_clean[which(df_clean$day==3),"dark_day"] <- dark3
df_clean[which(df_clean$day==4),"dark_day"] <- dark4
df_clean[which(df_clean$day==5),"dark_day"] <- dark5


# Save to file
write.csv(df_clean,"intermediate/df_clean.csv",row.names = F)