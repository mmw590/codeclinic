#!/bin/bash
#$ -cwd                                 # Run job from current directory

# Request 4 cores from ShARC's scheduler (max 16 cores on a public node), smp is all cores on same node
#$ -pe smp 16

#Requests 50Gig real (rmem) memory -- this is per task, with a 3GB file you'll still want to request more depending on how much data the functions you are using hold in the RAM. Maximum allowed is 256G
#$ -l rmem=12G #

# Requests 96 hours (the maximum) per task
#$ -l h_rt=96:00:00 #20m for 366K rows

#Send notifications to this email:
#$ -M mwangmeihua1@sheffield.ac.uk

#Send notifications for 'b' begin, 'e' end , 'a' abort
#$ -m bea

#Set the name of the task
#$ -N NCells_buffer

# Tell programs that use the OpenMP library to use 4 cores (1 thread per core). I'm not sure if you need this for R but no harm putting it in?
export OMP_NUM_THREADS=1

#Load R
module load apps/R/3.5.1/gcc-4.8.5     # Recommended to load a specific version of R

#Run parallelised R script - 1st directory points to the R script, second directory is the log file to be created for each task
R CMD BATCH  --no-save --no-restore /data/bop17mw/ch2/06-NCells_buffer_mclapply.R /data/bop17mw/ch2/logs/NCells_buffer.${JOB_ID}

# test with 10K cells, Max vmem         = 6.982G,  Wallclock Time   = 00:03:49, 2 cores in mclapply
# test with 1000K cells, 32G, smp 2 cores, more than 2 hour, 10G far
# test with 1000K cell, 26G, smp 4 cores, more than 8hr to fin even 1 buffer ize.
# test with all cells, 26G, smp 4 cores, 50h (not enough), Max vmem         = 21.347G
# test with all cells, 12G, smp 16 cores, 96h
