get_scenario <- function(file) {
  # Function takes the .obs filename
  # Returns the scenario (tx_py_dz)
  # Assumptions: files are named with scenario tx_py OR tx_py_dzz
  
  if (grepl("t._p._d..", file)) {
    # We have drainage in the scenario
    scenario <- str_extract(file, "t._p._d..")
  } else {
    # No drainage in the scenario
    scenario <- str_extract(file, "t._p.")
  }
  
  return(scenario)
  
}

combine_data <- function(df, scenario) {
  
  df$date <- as.Date(paste(df$Year, df$Month, df$Day, sep = "-"))
  # Doing filter later for the sake of proper max swe calcs / hydrologic yrs TBD
  # Make column with the scenario info tx_py_dzz
  df['scenario'] = scenario
  
  return(df)
  
}

prep_soil_moisture <- function(df) {
  
  df <- df %>%
    pivot_df_daily() %>%
    reorganize_df_daily()
}

det_cropland_areas <- function(station, class_acronym, data_dir) {
  
  # Output of this function, cropland_areas, looks like:
  #    d00 | d01 | d02 | ... | d10 
  # 1  xxx | xxx | xxx | ... | xxx
  # 2  xxx | xxx | xxx | ... | xxx
  # ..............................
  # 58 xxx | xxx | xxx | ... | xxx
  #, where each row contains the cropland area of a specific HRU
  #  over the different drainage scenarios
  #, where xxx = 0.0000 indicates that that HRU is not cropland
  #  in that scenario
  
  # Assumptions: undesired dates are the first 5 years and last (incomplete) yr
  #            : desired format is as above
  
  # ASSUMPTION: prj folders are named as station_classacronym
  dir <- paste(station, class_acronym, sep = "_")
  prj_files <- list.files(paste0(data_dir, "prj/", dir), full.names = TRUE)
  
  # Determine which HRUs are cropland for each scenario -----
  prj_areas    <- map(prj_files, readPrjParameters, "Shared hru_area")
  prj_sdmax    <- map(prj_files, readPrjParameters, "Shared Sdmax")
  # ASSUMPTION: same HRU names throughout the different scenarios
  # readPrjHRUnames doesn't work for the Winnipeg prj files
  if (grep("Winnipeg", station)) {
    prj_hrunames <- c('Fallow', 'Grassland', 'Shrubland', 'Forest', 
                      'Corn', 'Cereals', 'Legumes', 'Oilseed', 
                      'Root crop', 'Wetland', 'Water', 'Channel')
  } else {
    prj_hrunames <- readPrjHRUnames(prj_files[1])
  }
  
  cropland_areas <- as.data.frame(matrix(0, length(prj_hrunames), length(prj_files)))
  names(cropland_areas) <- str_extract(str_extract(prj_files, "t._p._d.."), "d..")
  
  # Using prj_sdmax, determine which of the wetland HRUs
  # have been drained (aka have a sdmax = 0)
  # Store the area of those HRUs in cropland_areas
  for (i in 1:ncol(cropland_areas)) {
    sd_max <- prj_sdmax[[i]]
    areas <- unlist(prj_areas[[i]])
    
    if (station == "Winnipeg") {
      
      # Only have 12 HRUs. Different than the rest:
      cropland_areas[5,i] <- areas[5]
      cropland_areas[6,i] <- areas[6]
      cropland_areas[7,i] <- areas[7]
      cropland_areas[8,i] <- areas[8]
      cropland_areas[9,i] <- areas[9]
      
    } else {
      
      # ASSUMPTION: HRUs 2 and 7 ARE ALWAYS CROPLAND
      cropland_areas[2,i] <- areas[2]
      cropland_areas[7,i] <- areas[7]
      # ASSUMPTION: THERE ARE 58 HRUs AND THE WETLANDS ARE HRUS 11 TO 56
      for (j in 11:56) {
        if (sd_max[j] == 0) {
          cropland_areas[j,i] <- areas[j]
        }
      }
      
    }
    
  }
  
  return(cropland_areas)
  
}

wtr_yr <- function(df, start_month = 9) {
  
  # Making a water year
  
  offset <- ifelse(df$Month >= start_month, 1, 0)
  adj_year <- df$Year - 1 + offset
  return(adj_year)
  
}