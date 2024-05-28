library(raster)
library(sp)
library(terra)


# load data, take small subset, and convert to SpatRaster
load("Intermediate_rasters/cdl_data")
# Convert RasterLayer or similar object to SpatRaster
counties_data <- bl_cdl[[1]][1:3]
c1 <- as(counties_data[[1]], "SpatRaster")
c2 <- as(counties_data[[2]], "SpatRaster")
c3 <- as(counties_data[[3]], "SpatRaster")
plot(c1)
plot(c2)
plot(c3)

# test strategies to combine these SpatRaster layers
test_merge <- terra::merge(c2,c3, na.rm = TRUE)
plot(test_merge)

# background values are set to 0 so taking max at overlap should allow us to
# keep the values we want since it will take any value other than 0
test_mosaic <- terra::mosaic(c2,c3, fun = "max")
plot(test_mosaic)

# cover not what we want here it looks like
test_cover <- terra::cover(c2,c3, values = 0)

# try getting rid of 0 value before merging, maybe this will be faster?
subset_raster <- c2
subset_raster[subset_raster <= 0] <- NA
# Print subsetted raster
plot(subset_raster)

subset_raster3 <- c3
subset_raster3[subset_raster3 <= 0] <- NA

test_merge <- terra::merge(subset_raster, subset_raster3, na.rm = TRUE)
plot(test_merge)


# Function to subset raster to keep only cells with values > 0
subset_raster <- function(r) {
  r[r <= 0] <- NA
  return(r)
}

# Apply the function to each raster in the list
subset_raster_list <- lapply(counties_data, subset_raster)
spat_collection <- sprc(subset_raster_list)

counties_merge <- terra::merge(subset_raster_list, na.rm = TRUE)

