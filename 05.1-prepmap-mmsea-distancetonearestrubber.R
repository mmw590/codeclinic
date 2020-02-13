# ---
# title: "Calculate distance to nearest plantations"
# author: "Maria Wang"
# date: "1 May 2018"
# output: html_document
# ---

# description:


#------------------------------#
### maps and data.
# MMSEA
#------------------------------#
rm(list=ls())
setwd('C:/Users/bop17mw/Google Drive/1-Uni-of-Sheffield/1-PhD/RubberExpansionModel') # Working from desktop in office
memory.limit(300000)

######------------------ Load packages ----------------------#######
library(tidyverse)
#library(sp)
library(raster)
#library(rgeos)
#library(rgdal)
#library(data.table)
#library(GADMTools)
#library(dplyr)

######------------------ Dataset and management ----------------------#######
#GISfolder <- 'Z:/edwards_lab1/Shared/Maps/'
GISfolder <- 'C:/Users/bop17mw/Desktop/GIS_Files/'

# Set crs ####
crs_wgs <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

#### Hurni MMSEA Map ---- #####
mmsea_allyears <- stack("C:/Users/bop17mw/Desktop/GIS_Files/Jeff Fox/byyear/mmsea_allyears_wgs.tif")

#### Reclassify to make a binary raster with only plantations
lcc <- unique(values(mmsea_allyears[[1]]))
reclassify.df <- data.frame(oldval=9, newval=1) %>% arrange(oldval)
system.time(mmsea2000 <- subs(mmsea_allyears[[1]], reclassify.df, by='oldval', which='newval',subsWithNA=TRUE))
unique(values(mmsea2000))
plot(mmsea2000)

writeRaster(mmsea2000, filename=paste0(GISfolder, 'Jeff Fox/byyear/mmsea2000_wgs_rubberonly_usethis.tif'))
mmsea2000 <- raster(paste0(GISfolder, 'Jeff Fox/byyear/mmsea2000_wgs_rubberonly_usethis.tif'))

#i think this code didn't work
# retable <- data.frame(oldval=c(0,9,10), to=c(8,9,999), becomes=(NA))
# retable[2,3] <- 1
# retable
# mmsea2000_rub <- reclassify(mmsea_allyears[[1]], rcl=retable, filename=paste0(GISfolder, 'Jeff Fox/byyear/mmsea2000_wgs_rubberonly.tif'))
#

?raster::distance

system.time(distance(mmsea2000, filename=paste0(GISfolder, 'Jeff Fox/byyear/mmsea2000_wgs_disttonearestrubber2.tif')))

mmsea_distrub <- raster(paste0(GISfolder, 'Jeff Fox/byyear/mmsea2000_wgs_disttonearestrubber2.tif'))
