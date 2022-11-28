# Handling the phydap data. The data used is the raw data and is only altered by
# aggregation from hourly to daily

library(ncdf4)
library(stringr)

phydap_clean <- function(file) {
  
  # Files are consistently named "...../phydap/PHyDAP_RDRS21_hourly_variable.nc"
  variable <- paste("phydap", 
                    str_split(tail(str_split(tail(str_split(file,
                                                            "/")[[1]], 1),
                                             "_")[[1]], 1),
                              "\\.")[[1]][1], 
                    sep = "_")
  
  info <- nc_open(file)
  id   <- ncvar_get(info, "HYBAS_ID") # each is distinct
  hr   <- ncvar_get(info, "time_hrs") # each is distinct
  
  print(info)
  
  print("Reading and transposing data...")
  data <- t(ncvar_get(info))
  print("Data read complete.")
  colnames(data) <- id
  
  hr_d <- format(as.POSIXct(hr*3600, origin='1900-01-01 00:00:00'), "%Y-%m-%d")
  
  data_daily <- data %>%
    rowsum(group = hr_d)
  
  data_daily <- data_daily %>%
    as.data.frame() %>%
    mutate(date = rownames(.),
           dummy_date = paste(sprintf("%02d", year(date) - (year(date[1])-1)), 
                              sprintf("%03d", yday(date)), 
                              sep = "-"),
           variable = variable) %>%
    select(date, dummy_date, variable, everything())
  
  # First and last day do not have values for the entire day
  data_daily <- data_daily[2:(nrow(data_daily) - 1),]
  
  print("Writing file...")
  write.csv(data_daily, 
            paste0("../visforce_data/data/clean_data/spatial/phydap/", variable, ".csv"), 
            row.names = FALSE)
  print("File write complete.")
  
  nc_close(info)
}


# Want evap, rainfall, runoff, snowmelt in daily values
files <- str_subset(list.files("../visforce_data/data/raw_data/phydap", 
                               full.names = T),
                    "evap|rainfall|runoff|snowmelt")

map(files, phydap_clean)

