# File to convert the ArcMap tif files for the vsmb data into xy latitude 
# longitude format, in which there is a file with a crop and grass value for
# lat/long point

library(stars)
library(ggplot2)
library(gridExtra)
library(raster)
library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(terra)

# Preparing crop data ----------------------------------------------------------
crop_wst <- raster("../visforce_data/data/raw_data/masaki/masaki_projected/VSMB_crop_2_PR_84_csxy.tif")
crop_wst <- as.data.frame(cbind(coordinates(crop_wst), `crop_wst_mm_yr-1` = values(crop_wst)))
crop_wst <- filter(crop_wst, y >= 49)

# Something was going on with the coordinates and making issues for me. This is
# ugly but a quick way to see if the coordinates are still consistent between
# the pesticide and vsmb data (pud data from pesticides.R). Note that this tif
# file comes from ArcMap, as does the tif file for the pud/wpoi data. In ArcMap,
# these files are listed as having the same coordinates
# all(unique(crop_wst$y) %in% unique(pud$y))
#all(unique(crop_wst$x) %in% unique(pud$x))

# For some reason it works with the data straight from the environment but NOT with
# the data saved as csv
#all(unique(crop_wst$y) %in% unique(pud_data$y))
#all(unique(crop_wst$x) %in% unique(pud_data$x))

# Quick check to make sure that coordinates are still consistent
identical(crop_wst$x, pud_data$x)
identical(crop_wst$y, pud_data$y)

# Preparing grass data ---------------------------------------------------------
grass_ws <- raster("../visforce_data/data/raw_data/masaki/masaki_projected/VSMB_grass_2_PR_84_csxy.tif")
grass_ws <- as.data.frame(cbind(coordinates(grass_ws), `grass_ws_mm_yr-1` = values(grass_ws)))
grass_ws <- filter(grass_ws, y >= 49)

# Quick check to make sure that coordinates between the vsmb files are still
# consistent
identical(crop_wst$x, grass_ws$x)
identical(crop_wst$y, grass_ws$y)

# Combining --------------------------------------------------------------------
vsmb <- full_join(crop_wst, grass_ws)
vsmb_smaller <- vsmb %>%
  # These values are determined from the original VSMB file
  filter((x >= -115.466440 & x <= -110.004764) & (y <= 54.986286))

write.csv(vsmb_smaller, "../visforce_data/data/clean_data/vsmb_recharge.csv", row.names = FALSE)

all_spatial_data <- full_join(datapp, dataww) %>%
  full_join(vsmb)

write.csv(all_spatial_data, "../visforce_data/data/clean_data/all_spatial_data.csv", row.names = FALSE)

