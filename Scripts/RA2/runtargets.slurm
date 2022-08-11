#!/bin/bash
#SBATCH --job-name=dhs_daav_ra2_SA
#SBATCH --output=%J.out
#SBATCH --error=%J.err
#SBATCH --time=60
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --mem=5G

## Initialize work environment like
## source /etc/profile
## module add ...
## Load modules
module load singularity
## Export value of DEBUGME environemnt var to slave
## Run R:

## we merge R output with stdout from SLURM, which gets then logged via --output option
singularity exec r.tidyverse.sif Rscript -e "targets::tar_make()"