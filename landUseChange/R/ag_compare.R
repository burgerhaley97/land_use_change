# load packages
library(lattice)
library(terra)
# install.packages("magick")
library(magick)


# load data
cdl_all_years <- terra::rast("final_cdl_rasters//bl_raster_all_years.tif")

# look at frequency of crops across our area of interest and link crop data
crop_freq <- terra::freq(cdl_all_years[[4]])

# make data frame of pixel counts with GEOID
crop_data <-crop_freq %>%
  # matrix to data.frame
  data.frame(.)

GEOID <- c(1:nrow(crop_data))

crop_data <- crop_data %>%
  mutate(GEOID = GEOID)
crop_data

# load the crop code reference data
crop_values <- reclass[ ,1:2]
names(crop_values) <- c("value", "crop")
head(crop_values)

# join code with crop name
crop_data <- dplyr::left_join(crop_data, crop_values, by = c('value' = 'value'))
crop_data

# need to reclassify before applying this ag mask
cdl_reclass <- new_class(crop_data, reclass, GEOID, Crop, Count = count, New_Crop)

# Define the values you want to treat as non-agricultural

ag_mask <- c(0, 61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 190, 195, NaN)

# Create a logical raster where matching values are 0 and others are 1
ag_raster_2008 <- terra::ifel(cdl_all_years[[1]] %in% ag_mask, 0, 1)
ag_raster_all <- terra::ifel(cdl_all_years %in% ag_mask, 0, 1)

# Plot some comparisons between the years of ag vs other area
plot(ag_raster_2008)
plot(ag_raster_all)


#------------------------- Save these plots as a GIF -------------------------#
# add gif of non ag vs ag pixels over time
# Define the years
years <- 2008:2023

# Directory to save the images
dir.create("plots")

# Loop through each layer, plot, and save as PNG
for (i in 1:nlyr(ag_raster_all)) {
  layer <- ag_raster_all[[i]]  # Extract the i-th layer
  year <- years[i]  # Corresponding year
  plot_title <- paste("Year:", year)

  # Create the plot and save it as a PNG file
  png_filename <- paste0("plots/plot_", year, ".png")
  png(png_filename)
  plot(layer, main = plot_title, col = c("white", "black"), legend = FALSE)
  dev.off()
}

# Create a GIF from the PNG files
png_files <- list.files("plots", pattern = "plot_.*\\.png", full.names = TRUE)
gif <- image_read(png_files)
gif <- image_animate(gif, fps = 1) # Set frames per second

# Save the GIF
image_write(gif, "plots/animation.gif")

# Clean up
unlink("plots", recursive = TRUE)


# add gif of all pixels over time



