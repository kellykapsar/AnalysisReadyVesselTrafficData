
# Load Libraries
library(spatstat)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)


# ---- SETUP ----

setwd("/mnt/home/kapsarke/Documents/AnalysisReadyVesselTrafficData/R")

# Read in analysis functions 
source("./1-2-AnalysisFunctions.R")

# Projection (Alaska Albers)
AA <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Destination to save files
# output_dir <- paste0("/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Monthly_Meta/")
output_dir <- paste0("/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Seasonal_Meta/")

# Source of vector files
dsn <-"/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Vector_Vessels/"

# Mask outside of AIS bounds 
ais_mask <- st_read( "../Data_Raw/ais_reshape.shp")

# Mask land inside AIS bounds
land_mask <- read_sf("../Data_Raw/AK_CAN_RUS/AK_CAN_RUS.shp") %>% st_transform(AA) %>% st_buffer(0)

# Load study area shapefile and transform it to Alaska Albers projection
study <- st_read("../Data_Raw/StudyArea_ModifiedBerkman.shp") %>% 
  st_transform(AA)

# Specify cell size 
cellsize <- 4000

# Derive all combinations of output rasters 
# yr <- 2016
yr <- commandArgs(trailingOnly = TRUE)

# month <- stringr::str_pad(1:12, 2, pad = "0")
# df <- expand.grid(month, yr)
# colnames(df) <- c("month", "year")

season <- c("winter", "spring", "summer", "fall")
df <- expand.grid(season, yr)
colnames(df) <- c("season", "year")

rasterize_ais(df,
  dsn,
  output_dir, 
  cellsize, 
  ais_mask, 
  land_mask, 
  timescale = "seasonal", 
  subset = "meta_type")
