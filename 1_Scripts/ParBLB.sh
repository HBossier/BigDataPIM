#!/bin/sh
#
#
#PBS -N BLBS4
#PBS -o output/output.file
#PBS -e error/error.file
#PBS -m a
#PBS -l walltime=11:30:00
#PBS -l vmem=40GB
#PBS -l nodes=1:ppn=1
#

#----------------------------------------------------#
# MODULES TO LOAD IN
module load R/3.3.1-intel-2016b
#----------------------------------------------------#


#----------------------------------------------------#
# LOCATION OF SCRIPT TO RUN
srcdir=/user/scratch/gent/gvo000/gvo00022/vsc40728/BIGPIM/BLB
cd $srcdir
#----------------------------------------------------#

#----------------------------------------------------#
# WHICH SCENARIO
SCEN=4
echo 'scenario ' $SCEN
#----------------------------------------------------#


#----------------------------------------------------#
# WHICH SIMULATION: now in PBS_ARRAYID
# NSIM=${1}
#----------------------------------------------------#


#----------------------------------------------------#
# ARGUMENT ORDER
# - PBS_ARRAYID = simulation
# - SIMID (DEPRECATED)
# - Results directory
# - Machine
# - Data location
# - Scenario
#----------------------------------------------------#


#----------------------------------------------------#
# GO TIME!
Rscript ParBLB.R ${PBS_ARRAYID} "${srcdir}"/results "HPC" "${srcdir}"/Data "$SCEN"
#----------------------------------------------------#


echo "job finished"
