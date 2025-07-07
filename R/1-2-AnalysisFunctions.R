
#' Master function for voyage analysis
#'
#' This function orchestrates the full process of analyzing vessel voyages, including:
#' - Loading vessel shapefiles based on the specified type.
#' - Filtering and processing vessel tracks to focus on the study area and relevant data.
#' - Annotating tracklines with intersections (e.g., study area, gates, places of interest).
#' - Summarizing voyages by vessel and year, including spatial and temporal metadata.
#' - Classifying voyages based on spatial interaction type (e.g., spillover, telecoupled).
#' - Creating visit history data for each voyage (start, end, and places visited).
#' - Writing output shapefiles and CSVs for the results.
#'
#' @param vessel_type Character. The type of vessel (e.g., "Tanker", "Cargo"). Used to filter shapefiles and generate outputs.
#' @param dsn Character. The directory path containing the shapefiles for vessel tracks.
#' @param savedsn Character. The directory path where the output shapefiles and CSVs should be saved.
#' @param study sf object. A spatial object representing the study area boundary.
#' @param aisbounds sf object. A spatial object representing the AIS data collection boundaries.
#' @param gates sf object. A spatial object representing the gates or other locations of interest for the study.
#' @param places sf object. A spatial object with places of interest (e.g., ports, communities).
#' @param chunks Logical. If TRUE, splits output shapefiles into chunks; if FALSE, writes the entire dataset in a single file. Default is FALSE.
#'
#' @return NULL. This function performs analysis and writes output files but does not return a value.
#' @export
#'
#' @examples
#' # Example usage
#' voyage_analysis_master("Tanker", "path/to/data", "path/to/save", study_area_sf, ais_bounds_sf, gates_sf, places_sf)
metacoupling_and_place_visits_analysis <- function(vessel_type, dsn, savedsn, study, aisbounds, gates, places, monthly_output = FALSE, chunks = FALSE) {
  print(paste0("BEGIN PROCESSING: ", vessel_type))
  
  # Step 1: Load vessel shapefiles
  tracklines <- load_vessel_shapefiles(dsn, vessel_type)
  print(paste0(vessel_type, ": step 1 complete"))
  
  # Step 2: Remove tracklines outside the AIS boundary
  tracklines <- tracklines[st_intersects(tracklines, aisbounds, sparse = FALSE), ]
  print(paste0(vessel_type, ": step 2 complete"))
  
  # Step 3: Annotate tracklines with relevant intersections (study, gates)
  # and with places that the vessel stops 
  tracklines <- annotate_tracklines(tracklines, study, gates, eez, places)
  tracklines <- identify_stops(tracklines, places)
  print(paste0(vessel_type, ": step 3 complete"))
  
  # Step 4: Summarize voyages by vessel and year and classify metacoupling type
  vessel_summary <- summarize_voyages(tracklines, places, study) %>% 
    classify_meta_type()
  
  write.csv(vessel_summary, 
            paste0("../Data_Processed/Vessel_Summaries/vessel_summaries", vessel_type, ".csv"))
  
  print(paste0(vessel_type, ": step 4 complete"))
  
  # Step 5: Clean up tracklines for writing shapefile
  tracklines_clean <- clean_tracklines_for_output(tracklines, vessel_summary, places, study)
  print(paste0(vessel_type, ": step 5 complete"))

  # Step 9: Write output files (CSV and shapefiles)
  write_output_files(tracklines_clean, savedsn, vessel_type, monthly_output, chunks)
  return(tracklines_clean)
  print(paste0(vessel_type, ": COMPLETE"))
  
}



#' Load shapefiles for a given vessel type
#'
#' @param dsn Character. Directory path where shapefiles are stored.
#' @param vessel_type Character. Vessel type to match in filenames.
#'
#' @return An sf object containing vector data for ships of a given vessel_type
#' @export
load_vessel_shapefiles <- function(dsn, vessel_type) {
  filelist <- intersect(list.files(dsn, pattern = '.shp', full.names = TRUE),
            list.files(dsn, pattern = vessel_type, full.names = TRUE))
  
  linemonths <- lapply(filelist, function(x) st_read(x, quiet=T))
  
  tracklines <- do.call(rbind, linemonths) 
  
  return(tracklines)
}


#' Filter shapefiles to only vessels that intersect the study area
#'
#' @param df An sf object containing vector data for ships of a given vessel_ty
#' @param study sf object representing the study area boundary.
#'
#' @return An sf object of vessel tracks intersecting the study area.
#' @export
#' Filter shapefiles to only vessels that intersect the study area
#'
#' @param filelist Character vector of shapefile paths.
#' @param study sf object representing the study area boundary.
#'
#' @return An sf object of vessel tracks intersecting the study area.
#' @export
filter_ships_in_study_area <- function(tracklines, study) {
  # Identify all unique ships that enter the study area in the study period
  linemonths <- id_ships_in_study(trackline, study = study)
  study_tracklines <- tracklines %>%
    group_by(scrmblm) %>%
    summarize(in_study = ifelse(any(in_study), 1, 0)) %>% 
    filter(in_study == TRUE)

}


#' Identify ships intersecting the study area
#'
#' Reads a shapefile and checks which ship tracklines intersect the defined study area.
#' Returns a summary indicating if a ship was in the study area at least once during a year.
#'
#' @param df sf object with vector lines for a vessel type 
#' @param study sf object. Study area polygon used to test intersection.
#'
#' @return A data frame with vessel identifiers (`scrmblm`), year, and a binary `in_study` flag.
#' @export
id_ships_in_study <- function(vectormonth, study){
  
  print(paste0("Identifying study area ships from: ", filepath))
  
  # Isolate only lines in the broader Bering Strait region
  linesinstudy <- as.logical(st_intersects(vectormonth, study, sparse=FALSE))
  
  shipsinstudy <- data.frame(
    scrmblm = vectormonth$scrmblm, 
    in_study = linesinstudy, 
    year = lubridate::year(vectormonth$Tm_Strt)
  ) %>% 
    group_by(scrmblm, year) %>% 
    summarize(in_study = ifelse(any(in_study), TRUE, FALSE))
  
  return(shipsinstudy)
}


#' Annotate tracklines with intersections (study area, gates, places)
#'
#' @param tracklines sf object of vessel tracklines.
#' @param study sf object representing the study area.
#' @param places sf object with locations of interest.
#' @param gates sf object of gate polygons.
#'
#' @return A list with the annotated tracklines.
#' @export
annotate_tracklines <- function(tracklines, study, gates, eez, places) {
  
  # Identify if trackline began or ended at a known place 
  begin_coords <- as.data.frame(st_drop_geometry(tracklines[, c("x_first", "y_first")]))
  end_coords <- as.data.frame(st_drop_geometry(tracklines[, c("x_last", "y_last")]))
  
  begin_loc <- st_intersects(st_as_sf(begin_coords, coords=c("x_first", "y_first"), crs=AA), places, sparse=F) %>% as.data.frame()
  end_loc <- st_intersects(st_as_sf(end_coords, coords=c("x_last", "y_last"), crs=AA), places, sparse=F) %>% as.data.frame()
  
  colnames(begin_loc) <- places$id
  colnames(end_loc) <- places$id
  
  # Apply function to determine the matching location
  begin_loc$intersected_location <- apply(begin_loc, 1, function(x) {
    x_logical <- as.logical(x)  # convert row to logical vector
    true_locs <- names(begin_loc)[which(x_logical)]
    if (length(true_locs) == 1) {
      return(true_locs)
    } 
    else {
      return(NA)
    }
  })
  
  end_loc$intersected_location <- apply(end_loc, 1, function(x) {
    x_logical <- as.logical(x)  # convert row to logical vector
    true_locs <- names(end_loc)[which(x_logical)]
    if (length(true_locs) == 1) {
      return(true_locs)
    } 
    else {
      return(NA)
    }
  })
  
  tracklines$begin_loc <- begin_loc$intersected_location
  tracklines$end_loc <- end_loc$intersected_location
  
  # Determine intersections with study area/eez/national waters 
  tracklines$berstr <- c(st_intersects(tracklines, study, sparse = FALSE))
  
  # To determine whether ther first and last point of a track line are inside 
  # the study area. 
  # for(i in 1:length(tracklines$newsegd)){
  #   print(i)
  #   trackline <- tracklines[i,]
  #   t <- id_start_stop(trackline, study = study)
  #   tracklines$berstr_start[i] <- t$startin
  #   tracklines$berstr_stop[i] <- t$stopin
  # }
    
  # Intersect with study area boundaries and usa/rus eezs
  tracklines$berstr_only <- c(st_within(tracklines, study, sparse = FALSE))
  tracklines$usa_only <- c(st_intersects(tracklines, eez[eez$name == "USA",], sparse = FALSE))
  tracklines$rus_only <- c(st_intersects(tracklines, eez[eez$name == "RUS",], sparse = FALSE))
  
  # Modify fields to remove lines that intersect both usa and rus
  t <- which(tracklines$usa_only == T & tracklines$rus_only == T)
  tracklines$rus_only[t] <- F
  tracklines$usa_only[t] <- F
  
  gates_df <- st_intersects(tracklines, gates, sparse = FALSE) %>% data.frame()
  colnames(gates_df) <- gates$name
  tracklines <- cbind(tracklines, gates_df)

  return(tracklines)
}


#' Annotate tracklines with intersections (places)
#'
#' @param trackline sf object of a single vessel trackline
#' @param study sf object with boundaries of study area.
#'
#' @return A list with binary TRUE/FALSE for whether the vessel started and/or stopped its voyage inside the study area 
#' @export
#' 
id_start_stop <- function(trackline, study){
  voyagecoords <- as.data.frame(st_coordinates(trackline)[,1:2])
  startin <- st_intersects(st_as_sf(voyagecoords[1,], coords=c("X", "Y"), crs=AA), study, sparse=F)[1,1]
  stopin <- st_intersects(st_as_sf(voyagecoords[length(voyagecoords$X),], coords=c("X", "Y"), crs=AA), study, sparse=F)[1,1]
  list(startin = startin, stopin = stopin)
}

#' Annotate tracklines with intersections (places)
#'
#' @param tracklines sf object of vessel tracklines.
#' @param places sf object with locations of interest.
#'
#' @return A list with the annotated tracklines and reduced place intersection data.
#' @export
#' 
identify_stops <- function(tracklines, places){
  stopped_tracklines <- tracklines %>% filter(stppd_s == 1)
  
  place_intersects <- st_intersects(stopped_tracklines, places, sparse = FALSE) %>% as.data.frame()
  colnames(place_intersects) <- places$id
  place_intersects_red <- place_intersects %>%
    select(where(~ sum(.) != 0))
  stopped_tracklines <- cbind(stopped_tracklines, place_intersects_red)
  
  go_tracklines <- tracklines %>% filter(stppd_s != 1)
  
  if(sum(length(go_tracklines$newsegd),length(stopped_tracklines$newsegd)) != length(tracklines$newsegd)){
    print("SOME LINES DID NOT HAVE A STOP OR GO STATUS.")
    return(NULL)
  }
  
  new_tracklines <- bind_rows(stopped_tracklines, go_tracklines) %>% arrange(scrmblm, Tm_Strt)
  
  return(new_tracklines)
}


#' Create long-format table of place visits by voyage
#'
#' @param tracklines sf object of annotated vessel tracklines.
#'
#' @return A data frame with `scrmblm`, `place`, `date`, and visit type.
#' @export
create_place_visits_df <- function(t) {
  place_visits <- t %>%
    st_drop_geometry() %>%
    select(scrmblm, Tm_Strt, any_of(places$id)) %>%
    gather(place, visited, -scrmblm, -Tm_Strt) %>%
    filter(visited == TRUE) %>%
    mutate(date = format(as.POSIXct(Tm_Strt), "%Y-%m-%d"),
           type = "place")  %>% 
    rename(datetime = Tm_Strt) %>% 
    select(-visited)
  
  gate_crossings <- t %>%
    st_drop_geometry() %>% 
    select(scrmblm, Tm_Strt, RusN, RusS, USAN, USAS, berstr) %>%
    gather(place, visited, -scrmblm, -Tm_Strt) %>%
    mutate(type = "cross_gate",
           date = format(as.POSIXct(Tm_Strt), "%Y-%m-%d")) %>% 
    filter(visited == TRUE) %>% 
    rename(datetime = Tm_Strt) %>% 
    select(-visited)
  
  # Identify gaps in transmission between segments 
  begin_end <- t %>%
    st_drop_geometry() %>% 
    arrange(scrmblm, Tm_Strt) %>%
    group_by(scrmblm) %>%
    mutate(
      prev_end = lag(Tim_End),
      gap = as.numeric(difftime(Tm_Strt, prev_end, units = "mins")),
      new_segment = if_else(is.na(gap) | gap > 0, 1, 0),
      segment_id = cumsum(new_segment)
    ) %>%
    group_by(scrmblm, segment_id) %>% 
    summarize(begin_time = first(Tm_Strt),
              end_time = last(Tim_End),
              begin_loc = first(begin_loc), 
              end_loc = last(end_loc)) %>% 
    ungroup()
  
  begin <- begin_end %>% 
    dplyr::select(scrmblm, begin_time, begin_loc) %>% 
    mutate(type = "begin_transmission",
           date = format(as.POSIXct(begin_time), "%Y-%m-%d")) %>% 
    rename(place = begin_loc, 
           datetime = begin_time) 
    
  
  end <- begin_end %>% 
    dplyr::select(scrmblm, end_time, end_loc) %>% 
    mutate(type = "end_transmission",
           date = format(as.POSIXct(end_time), "%Y-%m-%d")) %>% 
    rename(place = end_loc, 
           datetime = end_time)  
  
  
  df <- rbind(place_visits, gate_crossings, begin, end) %>% 
    arrange(scrmblm, datetime) %>% 
    dplyr::select(-datetime) %>% 
    unique()
  
  return(df)
}


#' Summarize voyage characteristics and movements
#'
#' @param tracklines sf object of annotated vessel tracks.
#' @param places sf object with place metadata.
#' @param study sf object of the study boundary.
#'
#' @return A summarized data frame of voyages by vessel and year.
#' @export
summarize_voyages <- function(tracklines, places, study) {
  
  npac <- intersect(colnames(tracklines), places$id[places$n_pac == TRUE])
  npac_hub <- intersect(colnames(tracklines), places$id[places$n_pac_hub == TRUE])
  bs_places <- intersect(colnames(tracklines), places$id[st_within(places, study, sparse = FALSE)])
  econ_places <- intersect(colnames(tracklines), places$id[places$mtt_plc == TRUE])
  place_cols <- intersect(colnames(tracklines), places$id)
  
  t <- tracklines %>%
    st_drop_geometry() %>%
    arrange(Tm_Strt) %>%
    mutate(year = year(Tm_Strt), month = month(Tm_Strt)) %>%
    group_by(scrmblm, year) %>%
    summarize(
      start_date = first(Tm_Strt),
      end_date = last(Tim_End),
      ndays = as.numeric(round(difftime(end_date, start_date, units = "days"), 2)),
      ship_type = first(AIS_Typ),
      ship_code = first(Shp_Typ),
      flag_country = first(Country),
      n_stops = as.integer(sum(stppd_s)),
      length_k = sum(Lngth_K),
      usa_only = all(usa_only == TRUE),
      rus_only = all(rus_only == TRUE),
      ent_rus_n = first(RusN == TRUE),
      ent_rus_s = first(RusS == TRUE),
      ent_usa_n = first(USAN == TRUE),
      ent_usa_s = first(USAS == TRUE),
      lea_rus_n = last(RusN == TRUE),
      lea_rus_s = last(RusS == TRUE),
      lea_usa_n = last(USAN == TRUE),
      lea_usa_s = last(USAS == TRUE),
      # Start_BS = first(berstr == TRUE),
      # End_BS = last(berstr == TRUE),
      any_berstr = any(berstr == TRUE),
      only_berstr = all(berstr_only == TRUE),
      # across(place_cols, ~ any(. == TRUE, na.rm = TRUE)),
      stop_npac = any(rowSums(across(all_of(npac), ~ . == TRUE)) > 0, na.rm=T),
      stop_econ = any(rowSums(across(all_of(econ_places), ~ . == TRUE)) > 0, na.rm=T),
      stop_npac_hub = any(rowSums(across(all_of(npac_hub), ~ . == TRUE)) > 0, na.rm=T),
      stop_berstr = any(rowSums(across(all_of(bs_places), ~ . == TRUE)) > 0, na.rm=T),
      # crossings = sum(RusN) + sum(RusS) + sum(USAN) + sum(USAS), 
      # geometry = st_union(geometry), 
      .groups = "drop"
    )  # %>%
    # mutate(
    #   Start_In = ifelse(rowSums(across(starts_with("Ent_"))) == 0 & !Start_BS, TRUE, FALSE),
    #   End_In = ifelse(rowSums(across(starts_with("Lea_"))) == 0 & !End_BS, TRUE, FALSE),
    #   weird = ifelse(Start_In & crossings > 1 | End_In & crossings > 1 |
    #                    Start_BS & crossings > 1 | End_BS & crossings > 1 | crossings > 2, TRUE, FALSE)
    # )
  
  colnames(t) <- tolower(colnames(t))
  t$eez <- ifelse(t$usa_only == T & t$rus_only == F, "USA", 
           ifelse(t$usa_only == F & t$rus_only == T, "Russia", "Both"))
  
  t <- t %>% dplyr::select(-usa_only, -rus_only)
  return(t)
}

#' Classify voyages into spatial interaction types
#'
#' @param vessel_summary Data frame summarizing voyages.
#'
#' @return A modified data frame with `meta_type` column added.
#' @export
classify_meta_type <- function(vessel_summary) {
  gate_names <- c("ent_usa_s", "ent_usa_n", "ent_rus_s", "ent_rus_n",
                  "lea_usa_s", "lea_usa_n", "lea_rus_s", "lea_rus_n")
  vessel_summary %>%
    mutate(meta_type = ifelse(any_berstr == FALSE, NA, 
                       ifelse(only_berstr == TRUE, "Intracoupled",
                       ifelse(stop_berstr == FALSE, "Spillover",
                       ifelse(rowSums(across(all_of(gate_names), ~ . == TRUE)) > 0,
                              "Telecoupled", "Pericoupled")))))
}


#' Clean and prepare trackline data for shapefile export
#'
#' This function prepares vessel trackline data for writing to shapefiles by:
#' - Lowercasing column names
#' - Replacing `NA` values in place visit columns with `FALSE`
#' - Creating summary flags for vessel stops in specified regions (North Pacific, hubs, Bering Strait)
#' - Selecting a streamlined set of columns for output
#'
#' @param tracklines An `sf` object or data frame containing vessel tracklines and associated attributes.
#' @param places An `sf` object with place metadata. Must contain `id`, `n_pac`, and `n_pac_hub` columns.
#' @param study An `sf` object representing the study area polygon, used to define Bering Strait places.
#'
#' @return A cleaned `data.frame` or `sf` object with selected and standardized columns for export.
#' @export
#'
#' @examples
#' cleaned <- clean_tracklines_for_output(tracklines, places, study)
clean_tracklines_for_output <- function(tracklines, vessel_summary, places, study) {
  
  # Identify relevant columns based on place metadata
  npac <- intersect(colnames(tracklines), places$id[places$n_pac == TRUE])
  npac_hub <- intersect(colnames(tracklines), places$id[places$n_pac_hub == TRUE])
  bs_places <- intersect(colnames(tracklines), places$id[st_within(places, study, sparse = FALSE)])
  place_cols <- intersect(colnames(tracklines), places$id)
  
  # Standardize column names
  colnames(tracklines) <- tolower(colnames(tracklines))
  
  # Clean and enhance tracklines
  tracklines_clean <- tracklines %>% 
    mutate(
      ship_type = ais_typ,
      ship_code = shp_typ,
      flag_country = country,
      year = lubridate::year(tm_strt),
      month = stringr::str_pad(lubridate::month(tm_strt), width =2, pad = "0"),
      across(all_of(place_cols), ~ ifelse(is.na(.), FALSE, .))
    ) %>% 
    mutate(
      stop_npac = rowSums(across(all_of(npac), ~ .)) > 0,
      stop_npac_hub = rowSums(across(all_of(npac_hub), ~ .)) > 0,
      stop_berstr = rowSums(across(all_of(bs_places), ~ .)) > 0
    ) # %>% 
    # dplyr::select(
    #   newsegd, scrmblm, year, month, tm_strt, tim_end, 
    #   sog_mdn, sog_men, ship_type, ship_code, flag_country, destntn, 
    #   stppd_s, npoints, x_first, y_first, 
    #   x_last, y_last, dst_dst, dst_cnt, dst_stt, 
    #   orgn_or, orgn_cn, orgn_st, dst_typ, dst_cnf, lngth_k, 
    #   berstr, berstr_only, usa_only, rus_only, 
    #   usas, usan, russ, rusn, 
    #   stop_npac, stop_npac_hub, stop_berstr
    # )
  
  vs <- vessel_summary %>% select(scrmblm, year, meta_type)
  
  tracklines_clean <- left_join(tracklines_clean, vs, by = c("scrmblm", "year") )
  
  return(tracklines_clean)
}


#' Write output shapefiles an
#'
#' @param tracklines sf object of vessel tracks.
#' @param vessel_summary Data frame of voyage summaries.
#' @param voyage_histories Data frame of visit history.
#' @param savedsn Character. Output directory for shapefiles.
#' @param vessel_type Character. Current vessel type.
#' @param monthly_output Logical. Whether to write shapefiles divided by months.
#' @param chunks Logical. Whether to write shapefiles in parts.
#'
#' @return NULL. Writes files to disk.
#' @export
write_output_files <- function(tracklines, savedsn, vessel_type, monthly_output = FALSE, chunks) {
  if(monthly_output == FALSE){
    if (chunks) {
      metalines <- split(tracklines, ceiling(seq_along(tracklines$newsegd) / round(length(tracklines$newsegd) / 3)))
      for (i in seq_along(metalines)) {
        st_write(metalines[[i]], paste0(savedsn, "VesselLines_", vessel_type, "_part_", i, ".gpkg"), layer = "tracklines", delete_layer = TRUE)
      }
    } else {
      st_write(tracklines, paste0(savedsn, "VesselLines_", vessel_type, ".gpkg"), layer = "tracklines", delete_layer = TRUE)
    }
  }
  if(monthly_output == TRUE){
    unique_combos <- tracklines %>%
      st_drop_geometry() %>% 
      distinct(year, month, ship_type)
    
    for (i in seq_len(nrow(unique_combos))) {
      this_year <- unique_combos$year[i]
      this_month <- unique_combos$month[i]
      this_type <- unique_combos$ship_type[i]
      
      subset_data <- tracklines %>%
        filter(year == this_year, month == this_month, ship_type == this_type)
      
      if (nrow(subset_data) > 0) {
        filename <-  paste0(savedsn, sprintf("VesselLines_%s_%s_%s.gpkg", this_year, this_month, this_type))
        st_write(subset_data, filename, layer = "tracklines", delete_layer = TRUE)
      }
    }
  }
}

##############################################################################
########################### ECON SUMMARY FUNCTIONS ########################### 
##############################################################################

summarize_output_econ <- function(tracklines, vessel_summary){
  # Filter to include only vessels that stop in one of the Econ locations in a given year
  econ_keys <- vessel_summary %>%
    filter(stop_econ == TRUE) %>%
    distinct(year, scrmblm)
  
  econ_tracklines <- tracklines %>%
    mutate(year = lubridate::year(Tm_Strt)) %>% 
    semi_join(econ_keys, by = c("year", "scrmblm"))
  
  econ_summaries <- vessel_summary %>% 
    filter(stop_econ == TRUE) 
  
  vessel_history_econ <- create_place_visits_df(econ_tracklines)
  
  write.csv(vessel_summary, paste0("../Data_Processed/Econ/vessel_summaries_econ_", vessel_type, ".csv"))
  write.csv(vessel_history_econ, paste0("../Data_Processed/Econ/vessel_histories_econ_", vessel_type, ".csv"))
  
  print(paste0(vessel_type, ": history and summary saved."))
}

##############################################################################
########################### RASTERIZATION FUNCTION ########################### 
##############################################################################

#' Create AIS Raster from Vector Files
#'
#' Converts a list of AIS geopackage file paths to a raster with masking and unit conversion.
#'
#' @param files A character vector of file paths to read and convert to raster.
#' @param cellsize Numeric value for raster resolution (in meters).
#' @param ais_mask A `terra` raster object used as the initial mask.
#' @param land_mask A `terra` raster object used to mask out land areas.
#' @param output_name A character string for the output raster filename (including path).
#' @param layer_name A character string for the raster layer name.
#'
#' @return Writes the resulting raster to disk.
#' @export
# make_ais_raster <- function(ships, cellsize, ais_mask, land_mask, output_name, layer_name) {
#   library(terra)
#   library(sf)
# 
#   # Create an empty raster grid using ais_mask extent and given cell size
#   ext <- terra::ext(ais_mask)
#   template <- terra::rast(ext, resolution = cellsize, crs = terra::crs(ais_mask))
# 
#   # Convert raster to polygons (cells)
#   cell_polygons <- as.polygons(template) %>% st_as_sf()
#   cell_polygons$id <- 1:nrow(cell_polygons)
#   
#   # Intersect lines with cells
#   intersections <- st_intersection(st_make_valid(ships), st_make_valid(cell_polygons))
#   
#   # Calculate length of each intersected line segment
#   intersections$seg_length <- as.numeric(st_length(intersections))/1000
#   
#   # Aggregate total length per cell
#   length_by_cell <- intersections |>
#     st_drop_geometry() |>
#     group_by(id) |>
#     summarise(total_length = sum(as.numeric(seg_length)))  # ensure numeric units
#   
#   # Assign values to raster
#   result_raster <- template
#   values(result_raster) <- 0
#   result_raster[length_by_cell$id] <- length_by_cell$total_length
# 
#   # Mask and crop to AIS area
#   rast <- terra::mask(result_raster, ais_mask) |> terra::crop(ais_mask)
#   
#   # Mask land areas
#   rast_noland <- terra::mask(rast, land_mask, inverse = TRUE, touches = FALSE)
#   
#   # Name the layer
#   names(rast_noland) <- layer_name
#   
#   # Write result
#   terra::writeRaster(rast_noland, filename = output_name, overwrite = TRUE)
# 
# }  
#   
  
  



#' Create AIS Raster from Vector Files
#'
#' Converts a list of AIS geopackage file paths to a raster with masking and unit conversion.
#'
#' @param files A character vector of file paths to read and convert to raster.
#' @param cellsize Numeric value for raster resolution (in meters).
#' @param ais_mask A `terra` raster object used as the initial mask.
#' @param land_mask A `terra` raster object used to mask out land areas.
#' @param output_name A character string for the output raster filename (including path).
#' @param layer_name A character string for the raster layer name.
#'
#' @return Writes the resulting raster to disk.
#' @export
make_ais_raster <- function(ships, cellsize, ais_mask, land_mask, output_name, layer_name) {
  library(maptools)

  moSHP <- as(ships, "Spatial")
  moPSP <- as.psp(moSHP)

  extentAOI <- as.owin(list(xrange = c(-2550000, 550000), yrange = c(235000, 2720000)))
  allMask <- as.mask(extentAOI, eps = cellsize)
  moPXL <- pixellate.psp(moPSP, W = allMask)
  moRAST <- terra::rast(moPXL) / 1000
  terra::crs(moRAST) <- AA

  rast <- terra::mask(x = moRAST, mask = ais_mask) %>% terra::crop(., ais_mask)
  rast_noland <- terra::mask(x = rast, mask = land_mask, inverse = TRUE, touches = FALSE)
  names(rast_noland) <- layer_name

  writeRaster(rast_noland, filename = output_name, overwrite=T)
}


#' This function processes AIS `.gpkg` files and rasterizes them based on a specified 
#' temporal scale (monthly or seasonal) and a subset category (e.g., `ship_type`, `meta_type`). 
#' It handles winter seasons crossing years (e.g., winter 2015 = Dec 2014 + Jan 2015 + Feb 2015).
#'
#' @param df Optional. A data frame specifying combinations of `year`, `month` or `season`, and subset category.
#'           If `NULL`, combinations are auto-generated from available `.gpkg` files.
#' @param dsn Character. Path to the directory containing `.gpkg` files.
#' @param output_dir Character. Directory where output rasters should be saved.
#' @param cellsize Numeric. Size of raster grid cells.
#' @param ais_mask Spatial object. Study area mask used for rasterization.
#' @param land_mask Spatial object. Land mask used for rasterization.
#' @param timescale Character. Either `"monthly"` or `"seasonal"`. Controls the temporal resolution of outputs.
#' @param subset Character. Column to subset data by, e.g., `"ship_type"` or `"meta_type"`.
#'
#' @return Raster `.tif` files saved to the `output_dir`.
#' @export
rasterize_ais <- function(df = NULL,
                          dsn,
                          output_dir, 
                          cellsize, 
                          ais_mask, 
                          land_mask, 
                          timescale = c("monthly", "seasonal"), 
                          subset = c("ship_type", "meta_type")) {
  
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  timescale <- match.arg(timescale)
  subset <- match.arg(subset)
  
  # Generate df if NULL
  if (is.null(df)) {
    files <- list.files(dsn, pattern = "\\.gpkg$", full.names = FALSE)
    year <- str_extract(files, "\\d{4}")
    month <- str_extract(files, "_(\\d{2})_") |> str_remove_all("_")

    sample_file <- list.files(dsn, pattern = "\\.gpkg$", full.names = TRUE)[1]
    sample_data <- suppressMessages(st_read(sample_file, quiet = TRUE))

    if (!subset %in% names(sample_data)) {
      stop(paste("Subset column", subset, "not found in sample data"))
    }

    subset_values <- unique(sample_data[[subset]])
    year <- unique(year[!is.na(year)])

    if (timescale == "monthly") {
      month <- unique(month[!is.na(month)])
      df <- expand.grid(month = month, year = year, stringsAsFactors = FALSE)
      names(df) <- c(subset, "month", "year")
    } else {
      season_levels <- c("winter", "spring", "summer", "fall")
      df <- expand.grid(season = season_levels, year = year, stringsAsFactors = FALSE)
      names(df) <- c("season", "year")
    }
  }
  
  for (i in 1:nrow(df)) {
    d <- df[i, ]
    print(d)
    
    if (timescale == "monthly") {
      filelist <- intersect(
        list.files(dsn, pattern = as.character(d$year), full.names = TRUE),
        list.files(dsn, pattern = paste0("_", sprintf("%02d", as.integer(as.character((d$month)))), "_"), full.names = TRUE)
      )
      files <- filelist[grepl(".gpkg", filelist)]
      
    } else {
      if (tolower(d$season) == "winter") {
        prev_year <- as.numeric(as.character(d$year[1])) - 1
        this_year <- as.numeric(as.character(d$year[1]))
        
        files_prev <- list.files(dsn, pattern = as.character(prev_year), full.names = TRUE)

        files_this <- list.files(dsn, pattern = as.character(this_year), full.names = TRUE)
        
        files_dec <- grep("_12_", files_prev, value = TRUE)
        
        files_jan_feb <- grep("_01_|_02_", files_this, value = TRUE)
        
        files <- c(files_dec, files_jan_feb)
        
      } else {
        season_pattern <- switch(tolower(d$season),
                                 "spring" = "_03_|_04_|_05_",
                                 "summer" = "_06_|_07_|_08_",
                                 "fall"   = "_09_|_10_|_11_",
                                 stop("Invalid season"))
        
        files <- list.files(dsn, pattern = as.character(d$year), full.names = TRUE)
        files <- grep(season_pattern, files, value = TRUE)
      }
    }
    
    if (length(files) == 0) {
      message(paste0("No data for ", d[[subset]], " in ", timescale, " of ", d$year))
      next
    }
    
    print("Input files:")
    print(files)
        
    ships <- lapply(files, st_read, quiet = TRUE) %>% do.call(bind_rows, .)

    if (nrow(ships) > 0) {
      layer_suffix <- if (timescale == "monthly") {
        paste0("_", d$year, "_", sprintf("%02d", as.integer(as.character(d$month))))
      } else {
        paste0("_", d$year, "_", d$season)
      }
      output_suffix <- paste0(layer_suffix, ".tif")

      if (subset %in% names(ships)) {
        nested <- ships %>% nest_by(across(all_of(subset)), .keep = TRUE)

        for (j in 1:nrow(nested)) {
          group_val <- as.character(nested[[subset]][[j]])
          layer_name <- paste0(group_val, layer_suffix)
          output_name <- file.path(output_dir, paste0(group_val, output_suffix))

          print(output_name)

          make_ais_raster(
            ships = nested$data[[j]],
            cellsize = cellsize,
            ais_mask = ais_mask,
            land_mask = land_mask,
            output_name = output_name,
            layer_name = layer_name
          )
        }
      } else {
        warning(paste("Subset column", subset, "not found in ship data"))
      }
    }
  }
}


##############################################################################
######################### PLACE TESSELATION FUNCTION ######################### 
##############################################################################

#' Split overlapping buffers while preserving non-overlapping ones
#'
#' Buffers a point layer and resolves overlaps by splitting them evenly using constrained Voronoi polygons.
#' Only overlapping buffers are processed; non-overlapping buffers are left intact. The final result includes
#' one polygon per input point, merged if necessary. The Voronoi bounding box is automatically expanded
#' to fully encompass each buffer group.
#'
#' @param pts An `sf` object of point geometries. Must contain an `id` column for point identity.
#' @param dist Numeric. Buffer distance in meters.
#'
#' @return An `sf` object with non-overlapping polygons per point ID.
#' @examples
#' \dontrun{
#' pts <- st_read("points.shp")
#' result <- split_overlapping_buffers(pts, dist = 20000)
#' }
#' @export
split_overlapping_buffers <- function(pts, dist = 20000) {
  library(sf)
  library(dplyr)
  library(lwgeom)
  library(igraph)
  
  if (!inherits(pts, "sf")) stop("`pts` must be an sf object.")
  if (!"id" %in% names(pts)) stop("Input sf object must contain an 'id' column.")
  
  # Step 1: Buffer all points
  buffers <- st_buffer(pts, dist = dist)
  buffers$id <- pts$id  # retain original ID
  
  # Step 2: Find intersecting buffers
  touches <- st_intersects(buffers)
  
  # Step 3: Build edge list from intersection pairs
  edge_list <- do.call(rbind, lapply(seq_along(touches), function(i) {
    if (length(touches[[i]]) == 0) return(NULL)
    cbind(from = i, to = touches[[i]])
  }))
  
  # If no overlaps exist, just return original buffers merged per ID
  if (is.null(edge_list)) {
    return(buffers %>% group_by(id) %>% summarise(geometry = st_union(geometry), .groups = "drop"))
  }
  
  # Create graph from edge list
  g <- graph_from_edgelist(edge_list, directed = FALSE)
  
  # Component membership for each buffer
  comps <- components(g)$membership
  
  # Step 4: Process each connected component
  results <- list()
  for (comp_id in unique(comps)) {
    idx <- which(comps == comp_id)
    group_buffers <- buffers[idx, ]
    group_pts <- pts[idx, ]
    
    if (nrow(group_buffers) == 1) {
      # No overlaps — keep buffer as-is
      results[[length(results) + 1]] <- group_buffers
    } else {
      # Overlaps — resolve with Voronoi
      
      # Expand bounding box to cover all buffer area
      bbox_expanded <- st_bbox(group_pts) + c(-dist, -dist, dist, dist)
      bbox_polygon <- st_as_sfc(bbox_expanded)
      
      # Voronoi tessellation within expanded envelope
      voronoi_raw <- st_voronoi(st_union(group_pts), envelope = bbox_polygon)
      voronoi_sf <- st_sf(geometry = st_collection_extract(voronoi_raw), crs = st_crs(pts))
      
      # Clip Voronoi to buffer union
      buffer_union <- st_union(group_buffers)
      constrained_voronoi <- st_intersection(voronoi_sf, buffer_union)
      
      # Attach ID to Voronoi cells via spatial join
      constrained_voronoi <- st_join(constrained_voronoi, group_pts)
      
      # Final intersection with original buffers
      final_polys <- st_intersection(constrained_voronoi, group_buffers)
      
      results[[length(results) + 1]] <- final_polys
    }
  }
  
  # Step 5: Combine and clean
  final_result <- do.call(bind_rows, results)
  final_result <- st_make_valid(final_result)
  
  # Step 6: Merge polygons by ID
  final_result_merged <- final_result %>%
    group_by(id) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  return(final_result_merged)
}

##############################################################################
######################### POST-PROCESSING FUNCTIONS ######################### 
##############################################################################

# Load and bind shapefiles for a vessel type
load_vessel_data <- function(type) {
  files <- intersect(
    list.files("../Data_Processed/Vector_Vessels/", pattern = ".shp", full.names = TRUE),
    list.files("../Data_Processed/Vector_Vessels/", pattern = type, full.names = TRUE)
  )
  if (length(files) == 1) {
    return(st_read(files))
  } else if (length(files) > 1) {
    return(do.call(rbind, lapply(files, st_read)))
  } else {
    warning(paste("No files found for vessel type:", type))
    return(NULL)
  }
}


vessel_colors <- c(
  "Cargo" = "#0072B2",         
  "Tanker" = "#D55E00",        
  "Fishing" = "#009E73",       
  "Recreation" = "#56B4E9",    
  "TugTow" = "#CC79A7",        
  "Military" = "#F0E442",      
  "Other" = "#999999",         
  "Unknown" = "#000000"        
)


# Define color palette for metacoupling categories
meta_colors <- c(
  "Intracoupled" = "#FDBF6F",   
  "Pericoupled" = "#984EA3",    
  "Telecoupled" = "#4DAF4A",    
  "Spillover" = "#E7298A"       
)
