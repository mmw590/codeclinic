# for use in HPC - use mclapply
# Script to calculate total number of cells in the buffer for each individual cell, for calculating propBuffer
# Try with different buffer sizes


# module load apps/R/3.5.1/gcc-4.8.5
# R
# cd ch2
# qsub job_ch2.sh

# http://docs.hpc.shef.ac.uk/en/latest/sharc/software/apps/R.html

########################################################
#rm(list=ls())

#### Folder paths ####
#setwd('/data/bop17mw/ch2/')

# packages
library(data.table)
library(parallel)
library(dplyr)

# # /home/bop17mw/R/x86_64-pc-linux-gnu-library/3.5/data.table/libs
# update.packages(lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.5/data.table/libs")


# this fxn returns total number of cells in the buffer, so can calculate proportion (changes with sample size)
totalCells_buffer  <- function(x, buffersize=0.1) #x = cell position
{
  buffersize <- as.numeric(buffersize)   #can adjust the buffer size. try 0.5, 1.
  xcoord <- as.numeric(x[, "POINT_X"])
  ycoord <- as.numeric(x[, "POINT_Y"])

  # filter out cells within 1 unit buffer either side of the cell (square buffer ok??)
  sq_buffer_posi <- dplyr::filter(MMSEA_all, POINT_X >= (xcoord-buffersize) & POINT_X <= xcoord+buffersize, ###don't forget to change allcells back to mmsea)_cells!!1 '#####
                                  POINT_Y >= ycoord-buffersize & POINT_Y <= ycoord+buffersize)

  #calculate euclidian distance within square.makes circle
  Circ_buffer_posi <- dplyr::mutate(sq_buffer_posi,
                                    eucDisttocell = sqrt( (xcoord - POINT_X)^2 + (ycoord - POINT_Y)^2 ) )  %>%
    dplyr::filter(., eucDisttocell <= buffersize)#then filter out those outside the buffer distance

  pr_length <- nrow(Circ_buffer_posi) #no. of cells present in buffer
  return(pr_length)
} #fxn end



## Load dataset ####
#MMSEA_all <- fread('MMSEA_cells_all.csv') #21072672 obs, 2.7Gb
MMSEA_all <- fread('MMSEA_cells_10k.csv') #for code clinic testing, 10K obs, 1.16mb

#### Parameters for mclapply ####
# for final results, use all cells and 4 cores
ncells = nrow(MMSEA_all)
ncores = 4

print(ncells); print(ncores)

#MMSEA_all <- MMSEA_all[1:ncells]


#### calculate no. of cells within buffer. ####
NCells_buffer0.1 <- mclapply(1:ncells, function(i, ...) {totalCells_buffer(x=MMSEA_all[i, ], buffersize=0.1)} , mc.cores=ncores, mc.set.seed=TRUE)
MMSEA_all <- cbind(MMSEA_all, NCells_buffer0.1=as.numeric(NCells_buffer0.1) )
rm(NCells_buffer0.1)
gc()

# I AM CURRENTLY RUNNING THIS ON SHARC WITH THE FULL DATASET (21072672 obs, 2.7Gb)
# With 26GB memory and 4 cores, it needed >50 hours (my latest attempt was aborted as I had only allocated 50 hrs)
# Submitted a new job on 13/02/2020 with 32GB memory, 4 cores, and 96 hours.


# range: check there are no 0s: can't divide 0.
print('for buffersize=0.1/0.5/1, ranges of ncells are:')
print(range(MMSEA_all$NCells_buffer0.1))


# write data
fwrite(MMSEA_all, 'MMSEA_cells_all_buffer.csv')


