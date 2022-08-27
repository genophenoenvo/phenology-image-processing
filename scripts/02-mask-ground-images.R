# Create snow mask from Feb/Mar and calculate gcc/rcc for all images
# library(opencv)
# library(jpeg)
library(countcolors)
library(dplyr)
# library(imager)
library(ggplot2)

folder <- "data_raw/NEON.D01.HARV.DP1.00042/"
pics <- list.files(folder)

# Testing imagr
# imgFeb <- imager::load.image("data_raw/NEON.D01.HARV.DP1.00042/NEON.D01.HARV.DP1.00042_2021_02_01_120005.jpg")
# dim(imgFeb)
# class(imgFeb)
# str(imgFeb)
# hsv_imgFeb <- imager::RGBtoHSV(imgFeb)
# dim(hsv_imgFeb)
# str(hsv_imgFeb)
# hist(hsv_imgFeb[,,,1]) # Hue
# hist(hsv_imgFeb[,,,2]) # Saturation
# hist(hsv_imgFeb[,,,3]) # Value
# length(hsv_imgFeb[1,1,,1])

# Test countcolors (https://cran.r-project.org/web/packages/countcolors/vignettes/Introduction.html)

# Loop through first 100 dates and determine percentage of white/snow

out <- matrix(NA, nrow = length(first100), ncol = 5)
first100 <- paste0(folder, pics[1:100])
for(i in 1:length(first100)){
  kmeans.clusters <- colordistance::getKMeanColors(first100[i], 
                                                   n = 2, 
                                                   plotting = FALSE)
  color_mat <- colordistance::extractClusters(kmeans.clusters)
  color_mat$mean <- rowMeans(color_mat[,1:3])
  out[i,] <- c(unlist(color_mat[which.max(color_mat$mean),]))
}

# Create output dataframe
out_df <- data.frame(out) %>%
  setNames(colnames(color_mat)) %>%
  mutate(image = pics[1:100], .before = R,
         date = as.Date(stringr::str_extract(image, "[0-9]{4}\\_[0-9]{2}\\_[0-9]{2}"),
                        format = "%Y_%m_%d")) %>%
  arrange(Pct)
str(out_df)

# Plot whiteness percentage

ggplot(out_df) +
  geom_point(aes(x = date, y = Pct))

quantile(out_df$Pct, 0.5)

# Pick 90th percentile
snow_image <- jpeg::readJPEG(paste0(folder, out_df$image[90]))
center.spherical <- c(unlist(out_df[90,3:5]))

# Find all the pixels within a 10% radius
snow_spherical <- countcolors::sphericalRange(snow_image, 
                                              center = center.spherical, 
                                              radius = 0.10, 
                                              color.pixels = TRUE, 
                                              target.color = "white",
                                              plotting = FALSE)


countcolors::changePixelColor(snow_image, snow_spherical$pixel.idx,
                              target.color = "magenta")

### Create mask
# any 1's in the original image?
any(snow_spherical$original.img == 1) # none

# Extract TRUE if differences in any channel              
mask_3d <- snow_spherical$original.img != snow_spherical$indicator.img
str(mask_3d)

anyT <- function(x){
  any(x == TRUE)
}

mask <- apply(mask_3d, 1:2, FUN = anyT)
str(mask)

# save mask?


#### For loop through 2021 images ####

# Functions
# Create masking function for apply
mask_image <- function(x, mask) {
  if(any(dim(x) != dim(mask))) stop('image and mask of different dimensions')
  initial <- x * mask
  initial[initial == 0] <- NA
  return(initial)
}

# Create function to calculate gcc
calc_gcc <- function(x) { 
  # x is a 3D array with R, G, and B as the 3rd margin
  if(dim(x)[3] != 3) stop('incorrect image dimensions')
  
  R <- mean(x[,,1], na.rm = TRUE)
  G <- mean(x[,,2], na.rm = TRUE)
  B <- mean(x[,,3], na.rm = TRUE)
  
  gcc <- G/(R+G+B)
  rcc <- R/(R+G+B)
  return(c(gcc, rcc))
}

image_gcc <- data.frame(image = character(length(pics)),
                        gcc = numeric(length(pics)),
                        rcc = numeric(length(pics)))

for(i in 1:length(pics)){
  # Calculate gcc and rcc in all of snow_spherical$pixel.idx
  in_image <- jpeg::readJPEG(paste0(folder, pics[i]))
  
  # Apply mask
  out <- array(NA, dim = dim(in_image))
  out[] <- apply(in_image, 3, FUN = mask_image, mask = mask)
  
  # Calculate gcc
  image_gcc$image[i] <- pics[i]
  image_gcc[i, 2:3] <- calc_gcc(out)
  
}

image_gcc <- image_gcc %>%
  mutate(.before = gcc,
         date = as.Date(stringr::str_extract(image, 
                                             "[0-9]{4}\\_[0-9]{2}\\_[0-9]{2}"),
                      format = "%Y_%m_%d"))
str(image_gcc)

ggplot(image_gcc) +
  geom_point(aes(x = date, y = gcc, col = "gcc")) +
  geom_point(aes(x = date, y = rcc, col = "rcc")) +
  scale_color_manual(values = c("forestgreen", "orchid"))


