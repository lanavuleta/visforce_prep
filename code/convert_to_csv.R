#' Reorganize dat, txt data and write to csv
#'
#' @description
#' The convert_to_csv function transforms the input, organizes it in the desired
#' format, and writes a total of 4 files in a folder named by the class acronym: 
#' 2 files containing daily data:
#'    1 file containing all scenarios, 1 file containing a subset of scenarios
#' AND 
#' 2 files containing yearly data:
#'    1 file containing all scenarios, 1 file containing a subset of scenarios 
#'    
#' @param data_dir The path from current working directory to the raw_data file
#' @param class_dir The name of the directory containing the .dat, .txt files
#'   of interest
#'   
#' @return None
#'   
#' @examples
#' convert_to_csv("../Visforce_data/data/raw_data/", "winnipeg_SM/")
#' 
convert_to_csv <- function(class_dir, data_dir) {
  
  # Setup ----------------------------------------------------------------------
  
  class_info <- read.csv(paste0(data_dir, "class_info.csv"))
  
  class <- gsub("^(.*/)*(.*)$", "\\2", class_dir)
  
  # Using the class folder name to determine the class acronym and station
  # for later use
  for (i in 1:nrow(class_info)) {
    if (grepl(class_info$class_acronym[[i]], class, ignore.case = FALSE)) {
      class_acronym <- class_info$class_acronym[[i]]
      station <- class_info$station[[i]]
      break
    }
  }
  
  dirs <- list.dirs(class_dir)[-1]
  
  # Reading in data ------------------------------------------------------------ 
  
  # Assuming (justifiably) that first folder holds dat files, second holds txt
  files_dat <- list.files(dirs[[1]], full.names = TRUE)
  files_txt <- list.files(dirs[[2]], full.names = TRUE)
  
  data_dat <- map(files_dat, read.table, skip = 0, header = TRUE)
  data_txt <- map(files_txt, read.table, header = TRUE)
  
  # Reorganizing and calculating data ------------------------------------------
  # Yield a list of the scenarios covered (tx_py_dzz) to be able to match the txt data
  scenarios <- map(files_txt, get_scenario)
  
  # data_txt contains one dataframe with all of the .txt data, row bound
  data_txt <- data_txt %>%
    map2_df(scenarios, combine_data)
  
  # Outputs from txt files, containing daily data
  basinflow <- calc_basinflow(data_txt) 
  
  # Outputs from txt files, containing yearly data
  swe_doy   <- calc_swe_doy(data_txt) 
  swe_val   <- calc_swe_val(data_txt)   
  peak_flow <- calc_peak_flow(data_txt)
  
  # Outputs from dat files, containing daily data
  cropland_areas <- det_cropland_areas(station, class_acronym, data_dir)
  soil_moisture  <- data_dat %>%
    map2_df(scenarios, calc_soil_moisture, cropland_areas) %>%
    prep_soil_moisture()
  
  data_yearly <- rbind(swe_doy, swe_val, peak_flow)
  data_daily  <- rbind(basinflow, soil_moisture)
  
  # Selecting the columns that Visforce team will publish to dashboard ----------
  # (Remaining scenarios will be available to users upon request)
  
  # Visforce team only gets temp scenarios      0    2    4    6
  #                         precip scenarios  -20    0  +20 
  #                         drainage scenarios  0   20   40   60   80  100
  data_yearly_fewer_scen <- select(data_yearly, 
                                   -(ends_with("d01") | ends_with("d03") |
                                       ends_with("d05") | ends_with("d07") |
                                       ends_with("d09")),
                                   -(contains("t2")   | contains("t4") | 
                                       contains("t6")),
                                   -(contains("p3")   | contains("p5")))
  data_daily_fewer_scen  <- select(data_daily,  
                                   -(ends_with("d01") | ends_with("d03") |
                                       ends_with("d05") | ends_with("d07") |
                                       ends_with("d09")),
                                   -(contains("t2")   | contains("t4") | 
                                       contains("t6")),
                                   -(contains("p3")   | contains("p5")))
  
  # Writing final data to csv files ---------------------------------------------
  
  file_name_yearly <- paste(class_acronym, station, "yearly", sep = "_")
  file_name_daily  <- paste(class_acronym, station, "daily",  sep = "_")
  
  write.csv(data_yearly, 
            paste0("../visforce_data/data/clean_data/", 
                   class_acronym,
                   "/",
                   file_name_yearly, 
                   "_full.csv"), 
            row.names = FALSE)
  
  write.csv(data_daily, 
            paste0("../visforce_data/data/clean_data/", 
                   class_acronym,
                   "/",
                   file_name_daily, 
                   "_full.csv"), 
            row.names = FALSE)
  
  write.csv(data_yearly_fewer_scen, 
            paste0("../visforce_data/data/clean_data/", 
                   class_acronym,
                   "/",
                   file_name_yearly, 
                   ".csv"), 
            row.names = FALSE)
  
  write.csv(data_daily_fewer_scen, 
            paste0("../visforce_data/data/clean_data/",
                   class_acronym,
                   "/",
                   file_name_daily, 
                   ".csv"), 
            row.names = FALSE)

}
