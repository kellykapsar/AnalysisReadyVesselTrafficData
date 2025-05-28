library(sf)
library(tidyverse)

# Read in analysis functions 
source("./1-2-AnalysisFunctions.R")

# Projection (Alaska Albers)
AA <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Load data collection regions polygon 
ais_bounds <- st_read("../Data_Raw/ais_reshape.shp")

# US and Russian water boundaries
national_bounds <- st_read("../Data_Raw/us_rus_boundaries.shp")

# Derived from https://live.laborstats.alaska.gov/data-pages/alaska-population-estimates
census_boroughs <- read.csv("../Data_Raw/Places/AK_BoroughsAndRegions.csv")

# Derived from https://live.laborstats.alaska.gov/data-pages/alaska-population-estimates
census_clean <- read.csv("../Data_Raw/Places/Cities and CDPs_0.csv", 
                   skip = 6,
                   header = F, 
                   col.names = c("BoroughCensusAreaFIPS", 
                                 "PlaceFIPS", 
                                 "AreaName", 
                                 "CensusTotal_2010", 
                                 "Population", 
                                 "Blank",
                                 "CensusQuarters_2010", 
                                 "CensusQuarters_2020")) %>% 
  mutate(AreaName = gsub("CDP", "", AreaName, ignore.case = T)) %>% 
  mutate(AreaName = gsub("city", "", AreaName, ignore.case = T)) %>% 
  mutate(AreaName = gsub("municipality", "", AreaName, ignore.case = T)) %>% 
  mutate(AreaName = gsub("and borough", "", AreaName, ignore.case = T)) %>%  
  mutate(AreaName = gsub("St. ", "Saint ", AreaName, ignore.case = T)) %>%  
  mutate(AreaName = gsub("\\?", "g", AreaName), 
         Population = as.numeric(gsub(",", "", Population)), 
         BoroughCensusAreaFIPS = as.numeric(BoroughCensusAreaFIPS)) %>%
  mutate(AreaName = trimws(AreaName)) %>% 
  filter(!(PlaceFIPS %in% c(0, 9999, NA)), 
         AreaName != "Balance") %>% 
  left_join(census_boroughs) %>% 
  dplyr::select(AreaName, BoroughCensusAreaName, RegionName, Population)

# Derived from: https://gis.data.alaska.gov/datasets/DCCED::communities-incorporated-and-unincorporated-cities-boroughs-cdps-localities/about
ak_locs <- read_csv("../Data_Raw/Places/AK_Communities_-_Incorporated_and_Unincorporated_Cities.csv") %>% 
  mutate(country = "USA") %>% 
  filter(!is.na(x), 
         !is.na(y)) %>% 
  st_as_sf(coords = c("x", "y"), crs=4326) %>% 
  st_transform(st_crs(AA)) 

ak <- left_join(ak_locs, census_clean, by=c("CommunityName" = "AreaName")) %>% 
  dplyr::select(CommunityName, 
                country, 
                BoroughCensusAreaName, 
                RegionName, 
                Population) %>% 
  rename(community = CommunityName, 
         district = BoroughCensusAreaName, 
         region = RegionName, 
         population = Population) %>% 
  filter(!(region %in% c("Southeast")), 
         !(is.na(population)), 
         # Remove duplicate inland locations 
         !(community %in% c("North Lakes", "South Lakes"))) 

reddog <- data.frame(
            community = "Red Dog Mine Port",
            country = "USA", 
            district = "Northwest Arctic Borough", 
            region = "Northern", 
            population = 0, 
            x =  -441015.88, 
            y = 1988071.588
          ) %>% 
  st_as_sf(coords = c("x", "y"), crs=st_crs(AA)) 

big_dio <- data.frame(
  community = "Big Diomede",
  country = "Russia", 
  district = "Chukotsky", 
  region = "Chukotka", 
  population = 0, 
  id = "big_diomede",
  x =  -683069.41, 
  y = 1840561.58
) %>% 
  st_as_sf(coords = c("x", "y"), crs=st_crs(AA)) %>% 
  st_buffer(dist = 20000) %>% 
  st_intersection(national_bounds) %>% 
  dplyr::select(-Shape_Leng,
                -Shape_Area, 
                -name)
big_dio <- big_dio[1,]




# Manually generated 
ru <- read.csv("../Data_Raw/Places/russian_places_20240108.csv") %>% 
  rename(community = name, 
         population = population_nearest100) %>% 
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>% 
  st_transform(st_crs(AA)) %>% 
  dplyr::select(colnames(ak)) %>% 
  filter(!(is.na(population)))


places <- rbind(ak, ru, reddog) 

places$id <- tolower(gsub(" ", "_", places$community))
places$id <- gsub("-", "_",  places$id)
places$id <- gsub("'", "",  places$id)

lil_dio <- places %>% filter(id %in% c("diomede")) %>%
  st_buffer(dist = 20000) %>% 
  st_intersection(national_bounds) %>% 
  mutate(id = "little_diomede") %>% 
  dplyr::select(-Shape_Leng,
                -Shape_Area, 
                -name)
lil_dio <- lil_dio[2,]



places <- places %>% filter(!(id %in% c("diomede")))

# Not sure if World Port Index is useful for filtering large ports out 
# Seems to have a combo of large and small ports 
# ports <- st_read("../Data_Raw/Places/World_Port_Index/World_Port_Index.shp") %>%
#   st_transform(st_crs(ais_bounds)) %>% 
#   mutate(in_bounds = st_intersects(., ais_bounds, sparse=F)[,1]) %>% 
#   filter(in_bounds == TRUE)

# st_write(places, 
#           "../Data_Processed/Places/Pacific_Arctic_Places_And_Populations.shp", 
#           row.names=F, append=F)


places_buff_no_overlap <- split_overlapping_buffers(places, dis = 20000) %>% 
  left_join(st_drop_geometry(places), ., by = c("id"))

places_buff_no_overlap <- rbind(places_buff_no_overlap, big_dio, lil_dio)

places_buff_no_overlap$matt_places <- ifelse(places_buff_no_overlap$region %in% c("Northern", 
                                                  "Chukotka"), 1, 
                             ifelse(places_buff_no_overlap$district %in% c("Yukon-Koyukuk Census Area", 
                                                           "Bethel Census Area", 
                                                           "Bristol Bay Borough",
                                                           "Dillingham Census Area", 
                                                           "Kusilvak Census Area", 
                                                           "Lake and Peninsula Borough"), 1, 0))

places_buff_no_overlap$n_pac <- ifelse(places_buff_no_overlap$region %in% c("Northern", "Chukotka"), 1, 0)

places_buff_no_overlap$n_pac_hub <- ifelse(places_buff_no_overlap$n_pac == T, 
                           ifelse(places_buff_no_overlap$population > 2000, 1, 0), 0)


st_write(places_buff_no_overlap, 
         "../Data_Processed/Places/Pacific_Arctic_Places_And_Populations_20km_buffer_no_overlap.shp", 
         row.names=F, append=F)

############# Testing impact of different place buffer sizes on community intersections
# Note: requires tracklines object of vector data... 

buffs <- data.frame(size = c(1000, 2000, 5000, 10000), 
                    nPlaces = rep(NA, 4))

for(i in 1:length(buffs$size)){

  places_buff <- places %>% 
    st_buffer(buffs$size[i])
  
  temp <- st_intersects(tracklines, places_buff, sparse=F) %>% as.data.frame()
  colnames(temp) <- places$community

  
  temp_red <- temp[,which(colSums(temp) != 0)]
  
  v <- places[places$community %in% colnames(temp_red),]
  buffs$nPlaces[i] <- length(v$community)
}


