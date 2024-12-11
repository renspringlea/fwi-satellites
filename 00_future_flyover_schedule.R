#Load libraries etc
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(terra) #For spatial data analysis
library(tidyterra) #For graphing etc
library(measurements) #For converting units
library(stringr) #For converting units
library(gridExtra) #to help graphing
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable

# Get region of interest
region_of_interest <- read.csv("data/region_of_interest.csv")
region_of_interest$coordinate[c(3,4)] <- region_of_interest$coordinate[c(3,4)]+1
e <- ext(region_of_interest$coordinate[c(1,3,2,4)])
vect_roi <- vect(e)
crs(vect_roi) <- "EPSG:4326"

# Get filenames
filenames <- list.files("flyover_schedule_kmls/")

# List layers
for (i in c(1:length(filenames))){
system(paste0("ogrinfo ","flyover_schedule_kmls/",filenames[i]))
}

for (i in c(1:length(filenames))){
  vect_tmp <- read_sf(paste0("flyover_schedule_kmls/",filenames[i]),layer="NOMINAL")
  vect_tmp <- vect(vect_tmp)
  assign(paste0("flyover_schedule_kmls",i), vect_tmp)
}

flyover_schedule <- rbind(flyover_schedule_kmls1,
                          flyover_schedule_kmls2,
                          flyover_schedule_kmls3,
                          flyover_schedule_kmls4,
                          flyover_schedule_kmls5,
                          flyover_schedule_kmls6)
#known_flyover <- flyover_schedule[which(flyover_schedule$Name=="39184-1"),]
#flyover_schedule_roi <- intersect(flyover_schedule,vect_roi)
flyover_schedule_roi <- flyover_schedule[which(
  is.related(flyover_schedule,vect_roi,"intersects")),]
sort(flyover_schedule_roi$begin+(16*3600))


