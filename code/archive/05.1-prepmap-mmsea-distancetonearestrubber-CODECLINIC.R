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
library(raster)

######------------------ Dataset and management ----------------------#######

#### Hurni MMSEA Map ---- #####
mmsea2000 <- raster('mmsea2000_wgs_rubberonly_usethis.tif')
plot(mmsea2000)

system.time(distance(mmsea2000, filename='mmsea2000_wgs_disttonearestrubber2.tif')) #takes a long time (maybe a week?) #maybe resolution too fine

#mmsea_distrub <- raster(paste0(GISfolder, 'Jeff Fox/byyear/mmsea2000_wgs_disttonearestrubber2.tif'))
