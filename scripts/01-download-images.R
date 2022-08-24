# Download images using `phenocamapi`
library(data.table)
library(phenocamapi)
library(jpeg)

# Obtain phenocam metadata
phenos <- get_phenos()

str(phenos) # data.table and data.frame
colnames(phenos)

phenos$site[grep("HARV.DP1", phenos$site)]

# Can also use functions to get rois, timeseries

# Obtain midday images for NEON.D01.HARV.DP1.00042
if(!dir.exists('data_raw/NEON.D01.HARV.DP1.00042')){
  dir.create('data_raw')
  dir.create('data_raw/NEON.D01.HARV.DP1.00042')
}
rawdir <- 'data_raw/NEON.D01.HARV.DP1.00042'

download_midday_images(site = 'NEON.D01.HARV.DP1.00042', # which site
                       y = 2021, # which year(s)
                       months = 1:12, # which month(s)
                       days = 1:31, # which days on month(s)
                       download_dir = rawdir) # where files should download to

