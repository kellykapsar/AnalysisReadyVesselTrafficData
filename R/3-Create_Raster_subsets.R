library(terra)

source("./R/1-2-AnalysisFunctions.R")

aggregate_rasters(folder = "/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Monthly_Meta/", 
                  group_var = c("pattern", "year"), 
                  target_months = c(06, 07, 08), 
                  output_folder = "/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Seasonal_Meta/")

aggregate_rasters(folder = "/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Monthly_Meta/", 
                  group_var = c("pattern", "year"), 
                  target_months = c(09, 10, 11), 
                  output_folder = "/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Seasonal_Meta/")
