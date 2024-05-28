library(CropScapeR)
library(dplyr)
library(sf)
library(terra)
library(raster)
library(ggplot2)

#--------------------- Update API function to return SpatRaster ---------------#
# Get CDL Data using a bounding box
GetCDLDataST <- function(fips, year, tol_time, save_path, readr){
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&fips=', fips)
  data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

  if(class(data)[1] != 'response') stop(paste0('No response from the server. Error message from server is:\n', data$message))
  dataX <- httr::content(data, 'text')
  num <- gregexpr('returnURL', dataX)
  url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

  if(!is.null(save_path)){
    file_info <- httr::GET(url2, httr::write_disk(path = save_path, overwrite = T))
    message(paste0('Data is saved at:', save_path))
    if(isTRUE(readr)){
      outdata <- terra::rast(save_path)
    }else{
      outdata <- NULL
    }
  }else{
    if(!isTRUE(readr)) warning('readr focred to be TRUE, because no save_path is provided. \n')
    outdata <- terra::rast(url2)
  }
  return(outdata)
}

#______________________________________________________________________________
#                                  Test with One Year
#______________________________________________________________________________


# try getting data for target states
ut_cdl <- GetCDLDataST(fips = 49, year = 2010, tol_time = 120, save_path = NULL, readr = TRUE)
plot(ut_cdl)

id_cdl <- GetCDLDataST(fips = 16, year = 2010, tol_time = 120, save_path = NULL, readr = TRUE)
plot(id_cdl)

wy_cdl <- GetCDLDataST(fips = 56, year = 2010, tol_time = 120, save_path = NULL, readr = TRUE)
plot(wy_cdl)

#------------------ Try removing 0s and replace with NA ----------------------#
subset_ut <- ut_cdl
subset_ut[subset_ut <= 0] <- NA

subset_id <- id_cdl
subset_id[subset_id <= 0] <- NA

subset_wy <- wy_cdl
subset_wy[subset_wy <= 0] <- NA

merge_states <- merge(subset_ut, subset_id, subset_wy, na.rm = TRUE)

plot(merge_states)


#----------------------- Try masking with Watershed --------------------------#
bl_watershed <- st_read("shapefiles")
bl_watershed_trans <- st_transform(bl_watershed, crs = st_crs(merge_states))
mask_test <- mask(merge_states, bl_watershed_trans)
plot(mask_test)
unique(mask_test)

#______________________________________________________________________________
#                                  Try with All Years
#______________________________________________________________________________
# Load necessary libraries
library(httr)
library(terra)

# Define the function
GetCDLDataST <- function(fips, year, tol_time, save_path = NULL, readr = TRUE) {
  # Initialize a list to store the results
  results <- list()

  # Iterate over each combination of fips and year
  for (f in fips) {
    for (y in year) {
      # Construct the URL
      url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', y, '&fips=', f)

      # Make the GET request
      data <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)

      # Check if the request was successful
      if (class(data)[1] != 'response') {
        warning(paste0('No response from the server for FIPS: ', f, ' and Year: ', y, '. Error message: ', data$message))
        next
      }

      # Extract the URL for the data
      dataX <- httr::content(data, 'text')
      num <- gregexpr('returnURL', dataX)
      url2 <- substr(dataX, num[[1]][1]+10, num[[1]][2]-3)

      # Define the output object
      outdata <- NULL

      # Handle saving and reading of the data
      if (!is.null(save_path)) {
        # Define the file path for saving
        file_path <- file.path(save_path, paste0("CDL_FIPS_", f, "_Year_", y, ".tif"))

        # Save the file
        file_info <- httr::GET(url2, httr::write_disk(path = file_path, overwrite = TRUE))
        message(paste0('Data is saved at:', file_path))

        # Read the data if required
        if (isTRUE(readr)) {
          outdata <- terra::rast(file_path)
        }
      } else {
        warning('readr forced to be TRUE, because no save_path is provided. \n')
        outdata <- terra::rast(url2)
      }

      # Store the result in the list
      results[[paste0("FIPS_", f, "_Year_", y)]] <- outdata
    }
  }

  return(results)
}

# Example usage
fips <- c("16", "56", "49")
year <- as.character(2008:2023)
tol_time <- 180
save_path <- NULL
readr <- TRUE

cdl_by_state <- GetCDLDataST(fips, year, tol_time, save_path, readr)
id_all_years <- c(cdl_by_state[1:16])
id_all_years
