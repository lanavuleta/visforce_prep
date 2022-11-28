# https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r

# This is ugly but that's okay. Variable names are ugly and there is a lot of 
# copy-pasting instead of function creating. Decided it was fine for this.

# Goal here it to take the ArcMap rasters (where pesticide data has been
# reprojected to WGS1984 and cell size has been maintained at km2) and to create
# two files (one for wpoi data, one for pud data), where each file has a list 
# of lat/long and the wpoi/pud value for that pt.

# This data format was recommended by Kevin. Ehsan has been struggling with it,
# and so a conversion to shapefile data is being considered.

library(raster)
library(sp)
library(rgdal)
library(terra)
library(stars)
library(dplyr)

start_folder <- "../visforce_data/data/clean_data/spatial_data/xy_format/"

pud_folder  <- paste0(start_folder, "raster_arcmap/pesticides_projected_km2/pud/")
wpoi_folder <- paste0(start_folder, "raster_arcmap/pesticides_projected_km2/wpoi/")


fungpp <- raster(list.files(pud_folder, ".*fung.*tif$", full.names = T))
fungww <- raster(list.files(wpoi_folder, ".*fung.*tif$", full.names = T))
herbpp <- raster(list.files(pud_folder, ".*herb.*tif$", full.names = T))
herbww <- raster(list.files(wpoi_folder, ".*herb.*tif$", full.names = T))
insepp <- raster(list.files(pud_folder, ".*insect.*tif$", full.names = T))
inseww <- raster(list.files(wpoi_folder, ".*insect.*tif$", full.names = T))

coords_fungpp <- coordinates(fungpp)
coords_fungww <- coordinates(fungww)
coords_herbpp <- coordinates(herbpp)
coords_herbww <- coordinates(herbww)
coords_insepp <- coordinates(insepp)
coords_inseww <- coordinates(inseww)

# Want to make sure that everything worked well by checking the coords. Eek!
identical(coords_fungpp, coords_fungww)
identical(coords_herbpp, coords_herbww)
identical(coords_insepp, coords_inseww)
identical(coords_fungpp, coords_herbpp)
identical(coords_fungpp, coords_insepp)

# Colin requested that these columns have the units in the colname so that Ehsan
# remembers to put units somewhere in the data visualization
fungpp <- as.data.frame(cbind(coords_fungpp, 
                              `pud_fungicide_kg_km-2`   = values(fungpp)))
fungww <- as.data.frame(cbind(coords_fungww,
                              wpoi_fungicide            = values(fungww)))
herbpp <- as.data.frame(cbind(coords_herbpp,
                              `pud_herbicide_kg_km-2`   = values(herbpp)))
herbww <- as.data.frame(cbind(coords_herbww,
                              wpoi_herbicide            = values(herbww)))
insepp <- as.data.frame(cbind(coords_insepp,
                              `pud_insecticide_kg_km-2` = values(insepp)))
inseww <- as.data.frame(cbind(coords_inseww,
                              wpoi_insecticide          = values(inseww)))

# There are issues with the original pesticide files under the US-Canada border.
# Just get rid of all data under the border to fix. That data is not needed anyways
pud_data <- fungpp %>%
  left_join(herbpp) %>%
  left_join(insepp) %>%
  filter(y >= 49)

wpoi_data <- fungww %>%
  left_join(herbww) %>%
  left_join(inseww) %>%
  filter(y >= 49)

write.csv(pud_data, 
          paste0(start_folder, "pud.csv"), 
          row.names = F)
write.csv(wpoi_data, 
          paste0(start_folder, "wpoi.csv"),  
          row.names = F)

# Check ------------------------------------------------------------------------
# Sometimes something happened when data was written to csv, and data was 
# compromised. Need to check if the above worked
pud  <- read.csv(paste0(start_folder, "pud.csv"))
wpoi <- read.csv(paste0(start_folder, "wpoi.csv"))

# Determining the unique lat/long values, determining the distance between
# those lat/long values, then determining how many different distances there
# are between values.
# Are looking for one unique value for each of these lines, to indicate that 
# there is a consistent difference between points.
unique(diff(unique(pud$x)))
unique(diff(unique(pud$y)))
unique(diff(unique(wpoi$x)))
unique(diff(unique(wpoi$y)))

# Just another extra check
identical(pud$x, wpoi$x)
identical(pud$y, wpoi$y)


