

source("./R/1-2-AnalysisFunctions.R")

folder <- "/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/Raster_Monthly_ShipType/"
out_dir <- "/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/netCDF/"

export_shiptype_netcdfs(folder, out_dir)
