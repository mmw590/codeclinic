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
memory.limit(300000)

######------------------ Load packages ----------------------#######
library(tidyverse)
library(raster)

######------------------ Dataset and management ----------------------#######

# Set crs ####
crs_wgs <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

#### Hurni MMSEA Map ---- #####
mmsea_allyears <- stack("mmsea_allyears_wgs.tif")

#### Reclassify to make a binary raster with only plantations
lcc <- unique(values(mmsea_allyears[[1]]))
reclassify.df <- data.frame(oldval=9, newval=1) %>% arrange(oldval)
system.time(mmsea2000 <- subs(mmsea_allyears[[1]], reclassify.df, by='oldval', which='newval',subsWithNA=TRUE)) #76s
unique(values(mmsea2000))
plot(mmsea2000)

system.time(distance(mmsea2000, filename='mmsea2000_wgs_disttonearestrubber2.tif')) #takes a long time (maybe a week?) #maybe resolution too fine

mmsea_distrub <- raster(paste0(GISfolder, 'Jeff Fox/byyear/mmsea2000_wgs_disttonearestrubber2.tif'))
