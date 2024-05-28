library(CropScapeR)
library(dplyr)
library(sf)
library(terra)
library(raster)
library(ggplot2)


# take raw list of cld_data layers as raster class objects and transform to
# spatRaster objects where each layer is a year

# load in raw data
load("cdl_data")

#--------------------------- Step 1 Convert to SpatRaster ---------------------#

# Function to convert a nested list of raster objects to SpatRaster
convert_to_SpatRaster <- function(nested_rasters) {
  # Apply conversion to each raster in each sublist
  converted_rasters <- lapply(nested_rasters, function(sublist) {
    lapply(sublist, function(raster) {
      # Convert RasterLayer or similar object to SpatRaster
      as(raster, "SpatRaster")
    })
  })

  return(converted_rasters)
}

bl_cdl_SpatRasters <- convert_to_SpatRaster(bl_cdl)
save(bl_cdl_SpatRasters, file = "SpatRasters_data")

# Function to merges rasters within each sublist and combine the results
# convert to SpatRasterCollection first to merge sublists
merge_sublist_rasters <- function(nested_rasters) {
  # Initialize a list to hold the merged rasters
  merged_rasters_list <- list()

  # Iterate over each sublist
  for (i in seq_along(nested_rasters)) {
    # Retrieve the sublist
    rast_list <- nested_rasters[[i]]

    # Check if there are multiple rasters to merge
    if (length(rast_list) > 1) {
      # Merge the rasters in the sublist
      rast_list <- sprc(rast_list)
      combined_rasters <- terra::mosaic(rast_list, fun = "modal")
    } else {
      # If only one raster, no need to merge
      combined_rasters <- rast_list[[1]]
    }

    # Store the merged raster in the list
    merged_rasters_list[[i]] <- combined_rasters
  }

  return(merged_rasters_list)
}


test_rast <- merge_sublist_rasters(sub_cdl)

# with many SpatRasters, make a SpatRasterCollection from a list

sub_cdl <- bl_cdl_SpatRasters[[4]]
rast_list <- sprc(sub_cdl)
combined_rasters <- terra::mosaic(rast_list, fun = "modal")
plot(combined_rasters)

r1 <- bl_cdl_SpatRasters[[1]][[3]]
r2 <- bl_cdl_SpatRasters[[1]][[4]]
template<- terra::project(r2, r1, align =TRUE)
r2_aligned<- terra::project(r2, template)
r_merged<- terra::merge(r1,r2)
plot(r_merged)
plot(rast_list)
class(rast_list)

cdl_SpatRaster_merged <- merge_and_combine_rasters(bl_cdl_SpatRasters)

# Function to check the resolution of SpatRasters in the first sublist
check_resolutions <- function(nested_rasters) {
  # Access the first sublist of the nested list
  first_sublist <- nested_rasters[[2]]

  # Get the resolutions of each SpatRaster in the first sublist
  resolutions <- lapply(first_sublist, function(raster) {
    res(raster)  # Returns a vector with the x and y resolution
  })

  return(resolutions)
}

check_resolutions(bl_cdl_SpatRasters)
# 2008 and 2009 actually at 56m resolution?



#------------------------ Step 2 Link Data ------------------------------------#

# Calculate the frequency of pixel values for each year
pixel_counts <- lapply(1:nlayers(cdl_stack), function(i) {
  freq <- freq(cdl_stack[[i]], useNA="no") # Calculate frequency, excluding NAs
  freq <- as.data.frame(freq)
  data.frame(Year=years[i], Value=freq$value, Count=freq$count)
})

# Combine into a single data frame
pixel_counts_df <- do.call(rbind, pixel_counts)
names(pixel_counts_df)[2] <- "value"

# load the crop code reference data
data("linkdata")

# join code with crop name
crop_data <- dplyr::left_join(pixel_counts_df, linkdata,
                              by = c('value' = 'MasterCat'))
crop_data

# Filter for just a few crop to show change
specific_values <- c(1, 4, 21, 23, 24, 36)
filtered_data <- crop_data %>% filter(value %in% specific_values)
