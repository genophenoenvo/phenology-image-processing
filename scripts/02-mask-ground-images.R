# Create snow mask from Feb/Mar and calculate gcc/rcc for all images
library(opencv)
library(jpeg)
# library(countcolors)
library(imager)

imgFeb <- imager::load.image("data_raw/NEON.D01.HARV.DP1.00042/NEON.D01.HARV.DP1.00042_2021_02_01_120005.jpg")

dim(imgFeb)
str(imgFeb)
hsv_imgFeb <- imager::RGBtoHSV(imgFeb)
dim(hsv_imgFeb)
str(hsv_imgFeb)
hist(hsv_imgFeb[,,,1]) # Hue
hist(hsv_imgFeb[,,,2]) # Saturation
hist(hsv_imgFeb[,,,3]) # Value

length(hsv_imgFeb[1,1,,1])



