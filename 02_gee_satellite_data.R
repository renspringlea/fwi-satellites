###############################
### Input study information ###
###############################

# Import dates of interest from data file
df_dates_of_interest <- read.csv("data/dates.csv")
dates_of_interest <- df_dates_of_interest$date

# Import region of interest from data file
df_region_of_interest <- read.csv("data/region_of_interest.csv")
region_of_interest <- df_region_of_interest$coordinate

#################################################
### Obtain sentinel-2 images and save to file ###
#################################################

# Define bounding box
bbox <- ee$Geometry$Rectangle(region_of_interest)

# Define image collection (level 2A)
collection <- ee$
  ImageCollection('COPERNICUS/S2_SR_HARMONIZED')$
  filterDate(dates_of_interest[(length(dates_of_interest)-1)],dates_of_interest[length(dates_of_interest)])$
  filterBounds(bbox)$
  select("B1","B2","B3","B4","B5","B6",
         "B7","B8","B8A","B9",
         "B11","B12")#$
  #filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE",50))

# Download images to local
local_collection <- ee_imagecollection_to_local(collection,
                                                region=bbox)

# Move to a better directory (sorry)
for (i in c(1:length(local_collection))){
  file.rename(from=local_collection[[i]]$dsn,
              to=gsub("satellites","satellites/gee_tifs",local_collection[[i]]$dsn))
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
              to=gsub("satellites","satellites/gee_cloudmasks",local_collection_cloud[[i]]$dsn))
}
