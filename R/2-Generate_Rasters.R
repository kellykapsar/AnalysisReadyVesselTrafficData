
# Load Libraries
library(spatstat)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)


# ---- SETUP ----

start <- proc.time()

setwd("/mnt/home/kapsarke/Documents/AnalysisReadyVesselTrafficData/R")

# Read in analysis functions 
source("./1-2-AnalysisFunctions.R")

# Projection (Alaska Albers)
AA <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Destination to save files
output_dir <- paste0("/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Monthly_ShipType/")
# output_dir <- paste0("/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Seasonal_Meta/")

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
# month <- 02
args <- commandArgs(trailingOnly = TRUE)
yr <- args[1]
month <- args[2]
df <- expand.grid(month, yr)
colnames(df) <- c("month", "year")

# season <- c("winter", "spring", "summer", "fall")
# df <- expand.grid(season, yr)
# colnames(df) <- c("season", "year")

print(paste0("STARTING ANALYSIS FOR: ",df$year[1], "-", df$month[1]))

rasterize_ais(df,
  dsn,
  output_dir, 
  cellsize, 
  ais_mask, 
  land_mask, 
  timescale = "monthly", 
  subset = "ship_type")

end <- proc.time()
elapsed_sec <- end["elapsed"]
elapsed_min <- as.numeric(elapsed_sec) / 60

cat("\n\nElapsed time:", round(elapsed_min, 2), "minutes\n")

print(paste0(df$year[1], "-", df$month[1], " COMPLETE!"))
