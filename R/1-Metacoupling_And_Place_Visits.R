###############################################################################
# Script: voyage_creation_parallel.R
# Author: Kelly Kapsar
# Last updated: January 2026
#
# Purpose:
# This script processes vessel AIS data to identify individual voyages, 
# summarize voyage characteristics, and classify spatial behavior types 
# (e.g., Telecoupled, Spillover, etc.). It runs in parallel for different 
# vessel types, using shapefiles as input and saving summaries and histories 
# for each vessel voyage.
#
# Inputs:
# - AIS trackline shapefiles by vessel type
# - Study area and region boundary shapefiles
# - Gate and place shapefiles (regions of interest)
#
# Outputs:
# - Vessel-level voyage summaries (.csv)
# - Visit histories of vessels by place and date (.csv)
# - Segment shapefiles per vessel type
#
# Notes:
# - Designed to run on MSU's HPCC cluster with SLURM job scheduling
# - Uses `doParallel` and `foreach` for parallelization
# - Requires external function script: 1-2-AnalysisFunctions.R
###############################################################################

# Load required libraries
library(spatstat)
library(sf)
library(dplyr)
library(tidyr)
library(stringi)
library(foreach)
library(doParallel)
library(lubridate)

#### Read in auxiliary data -----------------------------------------------

setwd("/mnt/home/kapsarke/Documents/AnalysisReadyVesselTrafficData/R/")

# Set input and output directories
dsn <-"/mnt/research/CSIS/AIS/Data_Processed_V4/Vector/"
# dsn <-"../AISProcessing_V2/Data_Processed_V4/Vector/"

savedsn <- "/mnt/research/CSIS/AIS/Data_Processed_Metacoupling/"

# Read in analysis functions 
source("./1-2-AnalysisFunctions.R")

# Define Alaska Albers Equal Area projection
AA <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Read and transform spatial boundary files
study <- st_read("../Data_Raw/StudyArea_ModifiedBerkman.shp") %>% st_transform(AA)
aisbounds <- st_read("../Data_Raw/ais_reshape_innerbuffer_15km.shp") %>% st_transform(AA)
gates <- st_read("../Data_Raw/gates_innerbuffer_15km.shp")
eez <- st_read("../Data_Raw/us_rus_boundaries.shp")

# Read and buffer locations of interest (e.g., ports, communities)
places <- st_read(paste0(savedsn, "/Places/Pacific_Arctic_Places_And_Populations_20km_buffer_no_overlap.shp")) 

# Load arguments from sb file 
args <- commandArgs(trailingOnly = TRUE)
ship_type <- args[1]

# Run main function 
metacoupling_and_place_visits_analysis(
  ship_type, dsn, savedsn, study, aisbounds, gates, places, monthly_output = TRUE, chunks = FALSE
)
