# use CropScrapeR
# install.packages("CropScapeR")
install.packages("tigris")
library(tigris)
library(CropScapeR)
library(dplyr)
library(sf)
library(terra)
library(raster)
library(ggplot2)
# help from: https://tmieno2.github.io/R-as-GIS-for-Economists/CropScapeR.html


#---------------------- Example 1: Using a Bounding Box ----------------------#
# get sf shape file data for a county to start
UT_county <- tigris::counties(state = "UT", cb = TRUE) %>%
  st_as_sf() %>%
  filter(NAME %in% "Cache")
class(UT_county)

# download CDL data for bounding box of cache county
# this will likely give some area outside of the county you want
  cdl_cache <- GetCDLData(
    aoi = UT_county,
    year = "2018",
    type = "b"
  )

# view entire bounding box with terra package
terra::crs(cdl_cache)

plot(cdl_cache)

# use raster::mask() to look at just the sf object you want
cdl_cache_masked <- UT_county %>%
  #--- change the CRS first to that of the raster data ---#
  st_transform(., projection(cdl_cache)) %>%
  #--- mask the values outside the sf (turn them into NA) ---#
  raster::mask(cdl_cache, .)

plot(cdl_cache_masked)


#---------------------- Example 2: Using FIPS Codes ----------------------#
# can also download the data for a country directly with the 5 digit FIPS code
# this will likely give some area outside of the county you want

ut_years <- as.character(c(2008:2023))

cdl_cache <- GetCDLData(
  aoi = 49005,
  year = ut_years[1],
  type = "f"
)

plot(cdl_cache)

# look at frequency of crops across our area of interest and link crop data
crop_freq <- raster::freq(cdl_cache)

crop_data <-crop_freq %>%
  # matrix to data.frame
  data.frame(.) %>%
  # find share
  mutate(share = count/sum(count))

# load the crop code reference data
data("linkdata")

# join code with crop name
crop_data <- dplyr::left_join(crop_data, linkdata, by = c('value' = 'MasterCat'))
crop_data


#---------------------- Example 3: Data for All Years ----------------------#

## Write function that gets all the raster layers for 2008 to 2023
get_cdl_data <- function(years_vector) {
  cdl_data <- lapply(years_vector, function(year) {
    GetCDLData(
      aoi = 49005,
      year = year,
      type = "f"
    )
  })
  return(cdl_data)
}

ut_years <- as.character(c(2008:2022))
cdl_data_list <- get_cdl_data(ut_years)

# 2009 is not correct extent
cdl_data_list[[2]] <- crop(cdl_data_list[[2]], extent(cdl_data_list[[1]]))

# Combine the raster stacks into a single RasterStack object
cdl_stack <- do.call(raster::stack, cdl_data_list[-2])
class(cdl_stack)
nlayers(cdl_stack)
plot(cdl_stack[[1]])

## visualize changes in frequency counts over time

# RasterStack with layers from 2008 to 2023
years <- 2008:2023
years <- years[-2]

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
crop_data <- dplyr::left_join(pixel_counts_df, linkdata, by = c('value' = 'MasterCat'))
crop_data

# Filter for just a few crop to show change
specific_values <- c(1, 4, 21, 23, 24, 36)
filtered_data <- crop_data %>% filter(value %in% specific_values)

ggplot(filtered_data, aes(x = Year, y = Count, color = as.factor(Crop))) +
  geom_line() +
  labs(title = "Pixel Frequency", x = "Year", y = "Frequency", color = "Value") +
  theme_minimal()


#-------------------- Example 4: Data for all 3 States  ----------------------#

# function to get data for all years for Utah, Wyoming, and Idaho
get_cdl_data <- function(years_vector, aoi) {
  cdl_data <- lapply(years_vector, function(year) {
    # Retrieve CDL data for a given AOI and year
    GetCDLData(
      aoi = aoi,
      year = year,
      type = "f"
    )
  })
  return(cdl_data)
}

ut_years <- as.character(c(2008:2023))
state_fips <- c(49, 56, 16)

# get data for the three states
wy_data <- get_cdl_data(ut_years, aoi = 56)
plot(wy_data[[1]])
ut_data <- get_cdl_data(ut_years, aoi = 49)
id_data <- get_cdl_data(ut_years, aoi = 16)

# check that extents are the same
# Wyoming true
flat_raster_list <- lapply(wy_data, function(x) x[[1]])
initial_extent <- extent(flat_raster_list[[1]])
all_same_extent <- all(sapply(flat_raster_list,
                              function(r) identical(extent(r), initial_extent)))
all_same_extent

# Utah True
flat_raster_list <- lapply(ut_data, function(x) x[[1]])
initial_extent <- extent(flat_raster_list[[1]])
all_same_extent <- all(sapply(flat_raster_list,
                              function(r) identical(extent(r), initial_extent)))
all_same_extent

# Idaho True
flat_raster_list <- lapply(id_data, function(x) x[[1]])
initial_extent <- extent(flat_raster_list[[1]])
all_same_extent <- all(sapply(flat_raster_list,
                              function(r) identical(extent(r), initial_extent)))
all_same_extent

# Combine the raster layers into a single RasterStack object
# Utah stack
ut_stack <- do.call(raster::stack, ut_data)
class(ut_stack)
nlayers(ut_stack)

# Wyoming stack
wy_stack <- do.call(raster::stack, wy_data)
class(wy_stack)
nlayers(wy_stack)

# Idaho stack
id_stack <- do.call(raster::stack, id_data)
class(id_stack)
nlayers(id_stack)


# merge where first argument's value get priority in overlap
# try limiting output of extent?
combined_stack <- raster::merge(ut_stack, wy_stack, id_stack)


# crop stack using bear lake water shed shape file
# transform to spatial polygon first?
# save cropped RasterStack to a file for later use


# Calculate pixel frequency per value and link crop name


#-------------------- Example 4: Data for all 3 States  ----------------------#

# get data for just counties around bear lake
bl_watershed <- st_read("shapefiles")
plot(bl_watershed$geometry)

# try bounding box
# Create the bounding box
bbox <- st_bbox(bl_watershed)

# To create an sf object from the bounding box
bl_bbox <- st_as_sfc(bbox)
save("bl_bbox", file = "bl_bbox")
plot(bl_bbox)
plot(bl_watershed$geometry, add = TRUE)

# get counties
options(tigris_class = "sf")
counties <- counties(cb = TRUE)
plot(counties$geometry)
selected_counties <- counties %>%
  filter(STATEFP %in% c("16", "56", "49"))
plot(selected_counties$geometry)
plot(bl_watershed$geometry, add = TRUE)

# Get only counties that intersect with the watershed polygons
intersects_matrix <- st_intersects(selected_counties, bl_watershed, sparse = FALSE)
bl_counties <- selected_counties[apply(intersects_matrix, 1, any), ]
bl_counties <- bl_counties %>%
  mutate(FPS = paste(STATEFP, COUNTYFP, sep = ""))
county_FPS <- bl_counties$FPS

# function to get data for all years for Utah, Wyoming, and Idaho
# returns a list of lists where each sub-list contains the cdl data for each
# county for a given year
get_cdl_data <- function(years_vector, aoi) {
  cdl_data <- lapply(years_vector, function(year) {
    lapply(aoi, function(aoi_single) {
      GetCDLData(
        aoi = aoi_single,
        year = year,
        type = "f"
      )
    })
  })
  return(cdl_data)
}


years <- as.character(c(2008:2023))
bl_cdl <- get_cdl_data(years, aoi = county_FPS)
save(bl_cdl, file = "cdl_data")
load("cdl_data")



#----------------- Try bbox around counties ----------------------------------#
# get data for just counties around bear lake
bl_watershed <- st_read("shapefiles")
plot(bl_watershed$geometry)
sub_years <- as.character(c(2012:2023))

## try getting the data using the watershed polygon
cdl_2023 <- GetCDLData(
  aoi = bl_watershed,
  year = "2023",
  type = "b"
)

cdl_list <- list(cdl_2008, cdl_2009, cdl_2013, cdl_2014, cdl_2016, cdl_2018,
                 cdl_2019, cdl_2020)

# use raster::mask() to look at just the sf object you want
cdl_cache_masked <- bl_watershed %>%
  #--- change the CRS first to that of the raster data ---#
  st_transform(., projection(cdl_cache)) %>%
  #--- mask the values outside the sf (turn them into NA) ---#
  raster::mask(cdl_cache, .)

plot(cdl_cache_masked)

cdl_cache_masked


# function to get data for all years for bl_watershed object
get_cdl_bbox <- function(years_vector, aoi) {
  cdl_data <- lapply(years_vector, function(year) {
    # Retrieve CDL data for a given AOI and year
    GetCDLData(
      aoi = aoi,
      year = year,
      type = "b"
    )
  })
  return(cdl_data)
}

cdl_bbox_data <- get_cdl_bbox(sub_years, bl_watershed)


