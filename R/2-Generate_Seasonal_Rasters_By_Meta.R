
# Load Libraries
library(spatstat)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)


# ---- SETUP ----

# Read in analysis functions 
source("./R/1-2-AnalysisFunctions.R")

# Projection (Alaska Albers)
AA <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Destination to save files
output_dir <- paste0("./Data_Processed/Raster_meta/")

# Source of vector files
# dsn <-"../Data_Processed/Vector_Vessels/"
dsn <-"E:/HPCC_Backup_20250609/AIS/Data_Processed_V4/Vector/"

# Mask outside of AIS bounds 
ais_mask <- st_read( "./Data_Raw/ais_reshape.shp")

# Mask land inside AIS bounds
land_mask <- read_sf("./Data_Raw/AK_CAN_RUS/AK_CAN_RUS.shp") %>% st_transform(AA) %>% st_buffer(0)

# Load study area shapefile and transform it to Alaska Albers projection
study <- st_read("./Data_Raw/StudyArea_ModifiedBerkman.shp") %>% 
  st_transform(AA)

# Specify cell size 
cellsize <- 4000

# Derive all combinations of output rasters 
month <- stringr::str_pad(1:12, 2, pad = "0")
yr <- commandArgs(trailingOnly = TRUE)

df <- expand.grid(month, yr)
colnames(df) <- c("month", "year")

process_meta_type_rasters(df,
                          dsn,
                          study,
                          cellsize,
                          ais_mask,
                          land_mask,
                          output_dir)