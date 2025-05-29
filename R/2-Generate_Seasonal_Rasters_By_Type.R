
# Load Libraries
library(spatstat)
library(maptools)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)

# ---- SETUP ----

# Read in analysis functions 
source("1-2-AnalysisFunctions.R")

# Projection (Alaska Albers)
AA <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Destination to save files 
savedsn <- paste0("../Data_Processed/Raster/")

# Source of vector files 
dsn <-"../Data_Processed/Vector_Vessels/"

# Mask outside of AIS bounds 
ais_mask <- st_read( "../Data_Raw/ais_reshape.shp")

# Mask land inside AIS bounds
land_mask <- read_sf("../Data_Raw/AK_CAN_RUS/AK_CAN_RUS.shp") %>% st_transform(AA) %>% st_buffer(0)

# Specify cell size 
cellsize <- 4000

# Derive all combinations of output rasters 
vessel_type <- c("Cargo", "Tanker", "Fishing", "Recreation", "TugTow", "Military", "Other", "Unknown")
season <- factor(c("winter", "spring", "summer", "fall"), 
                 levels = c("winter", "spring", "summer", "fall"), 
                 ordered=T)
year <- 2015:2024

df_seasonal <- expand.grid(vessel_type, season, year)
colnames(df) <- c("vessel_type", "season", "year")


# Call function for each unique combination of ship type, season, and year 
for(i in 102:length(df$vessel_type)){
  print(df[i,])
  process_ais_seasonal(df = df[i,], dsn, savedsn, cellsize, ais_mask, land_mask)
}
