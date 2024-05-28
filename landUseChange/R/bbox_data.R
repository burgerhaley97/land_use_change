# load packages
library(CropScapeR)
library(dplyr)
library(sf)
library(terra)
library(raster)

#--------------------- Update API function to return SpatRaster ---------------#
# Get CDL Data using a bounding box
GetCDLDataB <- function(box, year, tol_time, save_path, readr){
  box <- paste0(box, collapse = ',')
  url <- paste0('https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=', year, '&bbox=', box)

  url2 <- tryCatch(httr::GET(url, httr::timeout(tol_time)), error = function(x) x)
  if(class(url2)[1] != 'response') stop(paste0('No response from the server.\nError message from server is: "', url2$message, '"'))

  url2X <- httr::content(url2, as = 'text')
  num <- gregexpr('returnURL', url2X)[[1]]
  url2 <- substr(url2X, num[1]+10, num[2]-3)

  if(!is.null(save_path)){
    message(paste0('Data is saved at:', save_path))
    file_info <- httr::GET(url2, httr::write_disk(path = save_path, overwrite = T))
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


#---------------------- Get bbox data for all years --------------------------#
# Function to fetch CDL data for multiple years
fetch_cdl_data_years <- function(aoi, years, tol_time = 120, save_path = NULL) {
  # Initialize a list to store data or NA for each year
  yearly_data <- vector("list", length(years))
  names(yearly_data) <- years

  # Loop through each year and fetch data
  for (i in seq_along(years)) {
    year <- years[i]
    yearly_data[[year]] <- tryCatch({
      GetCDLDataB(box = st_bbox(aoi), year = year,
                  tol_time = tol_time, save_path = save_path, readr = TRUE)
    }, error = function(cond) {
      message(paste('NA returned for', year, '. Data request encounters the following error:'))
      message(cond$message)  # Display the error message
      return(NA)  # Return NA if an error occurs
    })
  }

  return(yearly_data)
}

# get target CRS and make sure CRS of AOI matches
targetCRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
bl_watershed <- st_read("shapefiles")
crs(bl_watershed) == targetCRS
aoi <- sf::st_transform(bl_watershed, targetCRS)
years <- as.character(c(2008:2023))

# get data and save list of rasters
cdl_bbox_data <- fetch_cdl_data_years(aoi = aoi, years = years)
save(cdl_bbox_data, file = "Intermediate_rasters//cdl_bbox_data")

#--------------------- Mask the Bbox with bl Watershed polygon ----------------#
# Mask a list of SpatRaster objects with an sf object, ensure matching CRS
mask_rasters_with_sf <- function(raster_list, sf_object) {
  # Get the CRS of the sf object
  sf_crs <- st_crs(sf_object)$wkt

  masked_rasters <- lapply(raster_list, function(raster) {
    # Check if CRS matches, and if not, reproject the raster
    if (crs(raster) != sf_crs) {
      raster <- project(raster, sf_crs)
    }

    # Apply the mask
    masked_raster <- mask(raster, sf_object)
    return(masked_raster)
  })
  return(masked_rasters)
}

# mask the bbox list of rasters with bl watershed polygon
cdl_masked <- mask_rasters_with_sf(cdl_bbox_data, bl_watershed)

# save output of bbox mask
save(cdl_masked, file = "Intermediate_rasters//cdl_bbox_masked")

# convert list to single raster with years from 2010 to 2023 (same resolution)
bl_raster <- terra::rast(cdl_masked[3:16])

# Save the raster to a new file
terra::writeRaster(bl_raster, "Intermediate_rasters//bl_raster_later_years.tif", overwrite = TRUE)

#--------------------- Get 30m res for 2008/2009 -----------------------------#
# Define the bounding box
bbox <- st_bbox(aoi)

# Specify the path to your TIF file
tif_file_path_2009 <- "raw_rasters//2009_30m_cdls//2009_30m_cdls.tif"
tif_file_path_2008 <- "raw_rasters//2008_30m_cdls//2008_30m_cdls.tif"

# Read the raster data and crop to the defined extent
raster_data_2009 <- terra::rast(tif_file_path_2009)
raster_data_2008 <- terra::rast(tif_file_path_2008)
cropped_raster_2009 <- crop(raster_data_2009, bbox)
cropped_raster_2008 <- crop(raster_data_2008, bbox)

# Check and plot the cropped raster
plot(cropped_raster_2009)
plot(cropped_raster_2008)

# Mask with Bl watershed polygon
early_years <- c(cropped_raster_2008, cropped_raster_2009)
values(early_years) <- values(early_years)
names(early_years) <- c("2008", "2009")
sf_crs <- st_crs(bl_watershed)$wkt
raster <- terra::project(early_years, sf_crs)
cdl_masked_early <- terra::mask(raster, bl_watershed)

## Save the early and later years together in one raster
ext(early_years) == ext(cdl_bbox_data[1])
# Calculate the intersecting extent
common_extent <- intersect(ext(cdl_masked_early), ext(bl_rasters))

# Align to a common extent
bl_rasters_aligned <- resample(cdl_masked_early, bl_rasters)
cdl_masked_early_aligned <- bl_rasters_aligned

# Recrop after alignment to ensure perfect matching
cdl_masked_early_cropped <- crop(cdl_masked_early_aligned, common_extent)
bl_rasters_cropped <- crop(bl_rasters, common_extent)
bl_cdl_all_years <- c(cdl_masked_early_cropped, bl_rasters_cropped)

# check that they look okay
plot(bl_cdl_all_years)

# save the final data set with all years from 2008 to 2023
terra::writeRaster(bl_cdl_all_years, "final_cdl_rasters//bl_raster_all_years.tif", overwrite = TRUE)

