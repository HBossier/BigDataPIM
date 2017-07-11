#!/bin/sh
#
#
#PBS -N NonOptimalS2
#PBS -o output/output.file
#PBS -e error/error.file
#PBS -m a
#PBS -l walltime=16:00:00
#PBS -l vmem=40GB
#

#----------------------------------------------------#
# MODULES TO LOAD IN
module load R/3.3.1-intel-2016b
#----------------------------------------------------#


#----------------------------------------------------#
# LOCATION OF SCRIPT TO RUN
srcdir=/user/scratch/gent/gvo000/gvo00022/vsc40728/BIGPIM/NonOptimal
cd $srcdir
#----------------------------------------------------#

#----------------------------------------------------#
# WHICH SCENARIO
SCEN=2
echo 'scenario ' $SCEN
#----------------------------------------------------#


#----------------------------------------------------#
# GO TIME!
Rscript NonOptimalResampling.R ${PBS_ARRAYID} "${srcdir}"/results "HPC" "$SCEN"
#----------------------------------------------------#


echo "job finished"
