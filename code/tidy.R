pivot_df_daily <- function(df) {
  
  # Remove undesired dates
  df <- df %>%
    filter(date >= as.Date("1965-01-01"), date < as.Date("2006-01-01"))
  
  # Reorganize data to be in the desired format
  df$variable <- names(df)[2] 
  
  df <- df %>%
    pivot_wider(names_from = scenario, values_from = 2)
  
  return(df)
  
}

pivot_df_yearly <- function(df) {
  
  # Remove undesired dates
  df <- df %>%
    filter(year >= 1965, year < 2006)
  
  # Reorganize data to be in the desired format
  df$variable <- names(df)[2] 
  
  df <- df %>%
    pivot_wider(names_from = scenario, values_from = 2)
  
  return(df)
  
}

reorganize_df_daily <- function(df) {
  
  df$dummy_date <- paste(sprintf("%02d", year(df$date) - (year(df$date[1])-1)), 
                         format(df$date, "%j"), 
                         sep = "-")
  
  df <- select(df, date, dummy_date, variable, everything())
  
  return(df)
  
}

reorganize_df_yearly <- function(df) {
  
  df$dummy_year <- sprintf("%02d", df$year - (df$year[1] - 1))
  
  df <- select(df, year, dummy_year, variable, everything())
  
  return(df)
  
}