#devtools::install_github("CentreForHydrology/CRHMr")
library(CRHMr)
library(stringr)
library(lubridate)
library(purrr)
library(plyr)
library(dplyr)
library(here)
library(R.utils)
library(tidyr)

source("code/convert_to_csv.R")
source("code/tidy.R")
source("code/preparing_parameters.R")
source("code/setup.R")

# Takes a directory containing all the .dat and .txt files for a class
# Creates a csv of the desired data:
#       date | dummy_date | `variable` | t1_p1_dzz | t1_p2_dzz | ..
# yyyy-mm-dd |     yy-ddd | variable1  | xxxxxxxxx | xxxxxxxxx | ..
# ............................................................................
# yyyy-mm-dd |     yy-ddd | variable2  | xxxxxxxxx | xxxxxxxxx | ..
# Assumptions: undesired dates are the first 5 years and last (incomplete) yr
#            : desired format is as above

data_dir   <- "../visforce_data/data/raw_data/"
class_dirs <- list.dirs(paste0(data_dir, "dat_txt_files"), recursive = F)

# Create daily and yearly files containing basinflow, swe_doy, swe_val,
# peak_flow, soil_moisture values. Additionally, create daily and yearly files
# containing the same values, but a subset of the scenarios.
map(class_dirs, convert_to_csv, data_dir)
