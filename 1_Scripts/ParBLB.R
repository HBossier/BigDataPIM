####################
#### TITLE:     Run resampling scheme for large N: bag of little bootstraps
#### Contents:  
####
#### Source Files: //Mastat/Thesis
#### First Modified: 04/06/2017
#### Notes:
#################


##
##########
### Notes
##########
##

# In this script, we run the bag of little bootstraps (BLB) algorithm on large PIM datasets
# Data has been split up in different parts, to increase efficiency.


##
##########
### Preparation
##########
##

# Reset working memory
rm(list = ls())

# Take arguments from master file
args <- commandArgs(TRUE)

# ID of bag
BAGID <- try(as.numeric(as.character(args)[1]), silent=TRUE)

# ID of simulation
SIMID <- try(as.numeric(as.character(args)[2]), silent=TRUE)

# Results
ResultsDir <- try(as.character(args)[3], silent=TRUE)

# Which machine: HPC or LOCAL
MACHINE <- try(as.character(input)[4],silent=TRUE)

# Data location
DATALOCATION <- try(as.character(input)[5],silent=TRUE)

# Scenario
SCEN <- try(as.numeric(as.character(args)[6]), silent=TRUE)

# If no machine is specified (it gives error), then it has to be this machine!
if(class(MACHINE)=='try-error'){
  BAGID <- 1
  MACHINE <- "LOCAL"
  nsim <- 500
  SIMID <- 1
  SCEN <- 1
  ResultsDir <- 'D:/Users/Han/Dropbox/Mastat/Thesis/Results/Univariate/NonOptimalLocal'
}
print(ResultsDir)

# Seed
# Starting seed
StartingSeed <- 11 * SCEN
set.seed(StartingSeed + (StartingSeed * ID))


# Libraries
library(pim)
library(dplyr)
library(ggplot2)
library(nleqslv)
library(parallel)


##
##########
### Get the data and fit PIM
##########
##

# Depending on scenario, different parameter values
if(SCEN == 1){
  alpha <- 5
  sigma <- 1
  trueBeta <- alpha/(sqrt(2) * sigma)
}
if(SCEN == 2){
  u <- 10
  alpha <- 10
  sigma <- 5
  trueBeta <- alpha/(sqrt(2) * sigma)
}
# Third scenario: reflects design of real study (digital screen usage on mental well being, several control variables)
if(SCEN == 3){
  alpha_1 <- -0.43
  sigma <- 9.51
  trueBeta <- alpha_1/(sqrt(2) * sigma)
}


# Try to read in data of simulation and ID
SelectedData <- try(read.table(paste(DATALOCATION,'/SCEN_', SCEN, '/BLBdata_scen_', SCEN,'_sim_', SIMID, '_bag_', BAGID, '.txt', sep = ''),
                           header = TRUE), silent = TRUE)
if(class(SelectedData) == 'try-error'){
  print(paste('No data detected in simulation ', SIMID, '. Bag ', BAGID, sep = ''))
}else{
  # Try to fit PIM. If fails, return coordinates.
  if(SCEN %in% c(1,2)){
    PIMfit <- try(pim(formula = Y ~ X, data = SelectedData,
                      link = 'probit', model = 'difference'), silent = TRUE)
    if(class(PIMfit) == 'try-error'){
      print(paste0('Error in fitting PIM at sim ',SIMID, '. Bag ',BAGID,'.'))
    }else{
      # Save beta value if no error.
      PIMvalues <- PIMfit@coef
      # Save sandwich variance estimator
      PIMvariances <- PIMfit@vcov
        colnames(PIMvariances) <- "sVariance.X"
    }
  }
  # Different model for scenario 3
  if(SCEN == 3){
    PIMfit <- try(pim(formula = Y ~ X_smartph_hrs + X_sex + X_econArea + X_ethnic, data = SelectedData,
                      link = 'probit', model = 'difference'), silent = TRUE)
    if(class(PIMfit) == 'try-error'){
      print(paste0('Error in fitting PIM at sim ',SIMID, '. Bag ',BAGID,'.'))
    }else{
      # Names of the objects
      namesObject <- tidy(PIMfit@coef) %>% data.table::transpose() %>% slice(.,1)
      
      # Save beta value if no error.
      PIMvalues <- tidy(PIMfit@coef) %>% data.table::transpose() %>% slice(.,2)
      names(PIMvalues) <- namesObject
      
      # Save sandwich variance estimator (only variance on diagonal)
      PIMvariances <- tidy(diag(PIMfit@vcov)) %>% data.table::transpose() %>% slice(2)
      names(PIMvariances) <- namesObject
    }
    
    # Collect estimated beta values and variance with info about simulation and bag
    BetaValues <- data.frame(beta = PIMvalues,
                             sVariance = PIMvariances,
                             Bag = BAGID,
                             Sim = SIMID,
                             TrueBeta = trueBeta)
  }
  
  # Write results with simulation and bag
  write.table(BetaValues, file = paste(ResultsDir,'/SCEN_', SCEN, '/', SIMID, '/BLB_beta_SCEN_', SCEN ,'_simID_', SIMID, '_bagID_', BAGID, '.txt', sep = ''), 
              row.names = FALSE, col.names = TRUE)
}



