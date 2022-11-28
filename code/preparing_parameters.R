# basinflow = Streamflow.mm. from txt files
calc_basinflow <- function(df) {
  
  # ASSUMPTION: Data already in mm. Working from .txt file
  
  df <- select(df, date, basinflow = Streamflow.mm., scenario)
  
  df <- df %>%
    pivot_df_daily() %>%
    reorganize_df_daily()
  
  return(df)
  
}

# Area weight over all HRUs (done by Zhihua in txt files)
# For each hydrologic year (Sept 1 - Aug 31), select amount of max SWE
calc_swe_val <- function(df) {

  # ASSUMPTION: Data already in mm. Working from .txt file
  
  df <- df %>% 
    select(date, swe = SWE.mm., everything()) %>%
    mutate(wtr_yr = wtr_yr(df))
  
  # Getting the max SWE value by using max()
  max_vals <- df %>%
    group_by(scenario, year = wtr_yr) %>%
    summarise(max_swe = max(swe))
  
  df <- select(max_vals, year, max_swe, scenario)
  
  df <- df %>%
    pivot_df_yearly() %>%
    reorganize_df_yearly()
  
  return(df)
  
}

# Area weight over all HRUs
# For each hydrologic year (Sept 1 - Aug 31), select date of max SWE
# If Multiple days reach the max, select the date that occurs the latest in the year
calc_swe_doy <- function(df) {
  
  # ASSUMPTION: Data already in mm. Working from .txt file
  
  df <- df %>% 
    select(date, swe = SWE.mm., everything()) %>%
    mutate(wtr_yr = wtr_yr(df))
  
  # Getting the max SWE value and date by using max()
  max_vals <- df %>%
    group_by(scenario, wtr_yr) %>%
    summarise(max_swe = max(swe)) %>%
    left_join(df, by = c("wtr_yr", "max_swe" = "swe", "scenario")) %>%
    ungroup()
  
  df <- max_vals %>%
    group_by(scenario, year = wtr_yr, max_swe) %>%
    summarise(date_max_swe = max(date)) %>%
    select(year, date_max_swe, scenario) %>%
    ungroup()
  
  df$date_max_swe <- format(df$date_max_swe, "%m-%d")
  
  df <- df %>%
    pivot_df_yearly() %>%
    reorganize_df_yearly()
  
  return(df)
  
}

# For each hydrologic year (Sept 1 - Aug 31), select amount of peak flow
calc_peak_flow <- function(df) {
  
  # ASSUMPTION: Data already in mm. Working from .txt file
  
  df <- df %>%
    select(date, basinflow = Streamflow.mm., everything()) %>%
    mutate(wtr_yr = wtr_yr(df))
  
  # Getting the max SWE value and date by using max()
  max_vals <- df %>%
    group_by(scenario, year = wtr_yr) %>%
    summarise(peak_flow = max(basinflow))
  
  df <- select(max_vals, year, peak_flow, scenario)
  
  df <- df %>%
    pivot_df_yearly() %>%
    reorganize_df_yearly()
  
  return(df)
  
}

# Determine which HRUs are croplands for each scenario
# Calculate area weighted average over the cropland HRUs
# Set all values outside of April to Sept to NA
calc_soil_moisture <- function(df, scenario, cropland_areas) {
  # Setting it up so that it takes a map fn. List of data_dat and scenarios
  
  # Reorganize df
  df$date <- as.Date(paste(df$Year, df$Month, df$Day, sep = "-"))
  df <- df %>%
    select_if(grepl(c("SoilMoisture"), names(.))) %>%
    cbind.data.frame(date = df$date) %>%
    select(date, everything())
  
  scenario_drainage <- str_extract(scenario, "d..")
  
  # cropland_areas contains 11 columns, one for each drainage scenario
  # Here we are selecting the correct column based on the current df's drainage scenario
  areas <- unlist(select(cropland_areas, contains(scenario_drainage)))
  area_total <- sum(areas)
  
  # df contains 59 columns, where 2:58 are for the different HRUs
  # Multiply each data column (ie soil moisture val for that specific HRU) 
  # by its weight (hru area/total hru area) so that we can sum over each
  # datetime and get an area weighted average soil moisture
  
  # IF a data column's associated cropland_area value is 0, that HRU is NOT
  # cropland and its values will go to 0
  for (i in 2:ncol(df)) {
    weight <- areas[[i-1]]/area_total
    df[i] <- df[i]*weight
  }
  
  # Add up the weighted values for a final weighted soil moisture
  sum <- rowSums(df[,c(2:ncol(df))])
  df <- cbind.data.frame(date = df$date, soil_moisture = sum)
  df$scenario <- scenario
  
  # Only care about values from May to Sept. NA for dates outside of May-Sept
  df$soil_moisture[which(( format(df$date, "%m-%d") < "04-01" |
                           format(df$date, "%m") > "09"))] <- NA  
  
  return(df)
  
}