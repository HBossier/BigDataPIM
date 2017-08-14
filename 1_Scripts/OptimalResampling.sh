#!/bin/sh
#
#
#PBS -N LeverageS3
#PBS -o output/output.file
#PBS -e error/error.file
#PBS -m a
#PBS -l walltime=20:00:00
#PBS -l vmem=40GB
#

#----------------------------------------------------#
# MODULES TO LOAD IN
module load R/3.3.1-intel-2016b
#----------------------------------------------------#


#----------------------------------------------------#
# LOCATION OF SCRIPT TO RUN
srcdir=/user/scratch/gent/gvo000/gvo00022/vsc40728/BIGPIM/Leverage
cd $srcdir
#----------------------------------------------------#

#----------------------------------------------------#
# WHICH SCENARIO
SCEN=3
echo 'scenario ' $SCEN
#----------------------------------------------------#


#----------------------------------------------------#
# GO TIME!
Rscript OptimalResampling.R ${PBS_ARRAYID} "${srcdir}"/results "HPC" "$SCEN"
#----------------------------------------------------#


echo "job finished"
