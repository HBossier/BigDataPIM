#!/bin/sh
#
#
#PBS -N BLBS3
#PBS -o output/output.file
#PBS -e error/error.file
#PBS -m a
#PBS -l walltime=01:00:00
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
SCEN=1
echo 'scenario ' $SCEN
#----------------------------------------------------#


#----------------------------------------------------#
# WHICH SIMULATION
NSIM=1
#----------------------------------------------------#


#----------------------------------------------------#
# ARGUMENT ORDER
# - PBS_ARRAYID is here a node for each bag
# - SIMID
# - Results directory
# - Machine
# - Data location
# - Scenario
#----------------------------------------------------#


#----------------------------------------------------#
# GO TIME!
Rscript ParBLB.R ${PBS_ARRAYID} "$NSIM" "${srcdir}"/results "HPC" "${srcdir}"/Data/SCEN_"$SCEN" "$SCEN"
#----------------------------------------------------#


echo "job finished"
