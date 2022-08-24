# Create snow mask from Feb/Mar and calculate gcc/rcc for all images
library(opencv)
library(jpeg)
# library(countcolors)
library(imager)

imgFeb <- imager::load.image("data_raw/NEON.D01.HARV.DP1.00042/NEON.D01.HARV.DP1.00042_2021_02_01_120005.jpg")

dim(imgFeb)
class(imgFeb)
str(imgFeb)
hsv_imgFeb <- imager::RGBtoHSV(imgFeb)
dim(hsv_imgFeb)
str(hsv_imgFeb)
hist(hsv_imgFeb[,,,1]) # Hue
hist(hsv_imgFeb[,,,2]) # Saturation
hist(hsv_imgFeb[,,,3]) # Value

length(hsv_imgFeb[1,1,,1])

# Test countcolors
colordistance::plotPixels("data_raw/NEON.D01.HARV.DP1.00042/NEON.D01.HARV.DP1.00042_2021_02_01_120005.jpg", 
                          lower = NULL, upper = NULL, n = 5000)

kmeans.clusters <- colordistance::getKMeanColors("data_raw/NEON.D01.HARV.DP1.00042/NEON.D01.HARV.DP1.00042_2021_02_01_120005.jpg", 
                                                 n = 5, plotting = FALSE)
color_mat <- colordistance::extractClusters(kmeans.clusters)


center.spherical <- c(unlist(color_mat[3, 1:3])) # Center color for spherical range


feb <- jpeg::readJPEG("data_raw/NEON.D01.HARV.DP1.00042/NEON.D01.HARV.DP1.00042_2021_02_01_120005.jpg")

# Find all the pixels within a 10% radius
feb.spherical <- countcolors::sphericalRange(feb, 
                                             center = center.spherical, 
                                             radius = 0.15, 
                                             color.pixels = FALSE, 
                                             plotting = FALSE)
names(feb.spherical)
feb.spherical$img.fraction
countcolors::changePixelColor(feb, feb.spherical$pixel.idx, 
                              target.color = "magenta")
