# Select a greenness
# See how many pixels are green

library(countcolors) # https://cran.r-project.org/web/packages/countcolors/vignettes/Introduction.html
library(dplyr)
library(ggplot2)
library(phenocamapi)


#### Get phenocam gcc timeseries for canopy ROI ####
phenos <- get_phenos() %>%
  filter(grepl("HARV", site))

# NEON.D01.HARV.DP1.00033 is canopy camera

#rois <- get_rois () %>%
#  filter(grepl("HARV", site))

rois <- data.table::fread('data/rois.csv') %>%
  filter(grepl("HARV", site))

# NEON.D01.HARV.DP1.00033_DB_1000 is the deciduous broadleaf roi
# NEON.D01.HARV.DP1.00042_UN_1000 is the understory

harv_un <- get_pheno_ts(site = 'NEON.D01.HARV.DP1.00042',
                        vegType = 'UN',
                        roiID = 1000,
                        type = '1day')



#### Try calcluating gcc by pixel
folder <- "data_raw/NEON.D01.HARV.DP1.00042/"
pics <- list.files(folder)
pics_df <- data.frame(filename = pics) %>%
  mutate(date = as.Date(stringr::str_extract(filename, "[0-9]{4}\\_[0-9]{2}\\_[0-9]{2}"),
                        format = "%Y_%m_%d"),
         month = lubridate::month(date),
         year = lubridate::year(date)) 
count(pics_df, year)

calc_gcc_pixel <- function(vec){
  return(vec[2]/(vec[1] + vec[2] + vec[3]))
}

pics_df$gcc_above_threshold <- c()

for(i in 1:nrow(pics_df)) {
  
  # Obtain file name
  fn <- pics_df$filename[i]
  image <- jpeg::readJPEG(paste0(folder, fn))
  
  # Calculate gcc by pixel
  gcc_mat <- apply(image, MARGIN = c(1, 2), FUN = calc_gcc_pixel)
  
  # Percent of pixels with gcc >= 0.4
  pics_df$gcc_above_threshold[i] <- length(which(gcc_mat >= 0.4))/(dim(gcc_mat)[1]*dim(gcc_mat)[2])

  print(paste("Finished with", fn))
}

ggplot(pics_df, aes(x = date, y = gcc_above_threshold)) +
  geom_point()

