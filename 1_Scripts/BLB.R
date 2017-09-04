####################
#### TITLE:     Run resampling scheme for large N: bag of little m out of n bootstraps
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

# In this script, we run the bag of little m out of n bootstraps (BLmnB) algorithm on large PIM datasets


##
##########
### Preparation
##########
##

# Reset working memory
rm(list = ls())

# Results directory
ResultsDir <- 'D:/Users/Han/Dropbox/Mastat/Thesis/Results/Univariate/NonOptimalLocal'
DataPartition <- "/Volumes/1_5_TB_Han_HDD/Mastat/Thesis/BigDataPIM/BLB/DataPartition"

# Libraries
library(pim)
library(dplyr)
library(ggplot2)
library(nleqslv)
library(parallel)

# BOOLEAN for development = TRUE or preparing BmnB on HPC = FALSE
DEVELOPMENT <- FALSE

# Scenario
SCEN <- 4

# Seed
StartingSeed <- 11 * SCEN

# Global variables: univariate simple linear regression 
n <- 250000

# Depending on scenario, different parameter values
if(SCEN == 1){
  u <- 1
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
  # Added intercept, but not really needed for PIM.
  alpha_0 <- 46.72
  alpha_1 <- -0.43
  # Control variables: sex (0 = female, 1 = male)
  sex <- c(0,1)
  alpha_sex <- 4.55
  # Economic area (deprived = 1, otherwise 0)
  econArea <- c(0,1)
  alphaEcon <- -0.45
  # Ethnic background (white = 0, otherwise 1)
  ethnic <- c(0,1)
  alpha_ethnic <- 0.30
  # Predictor will be discrete scale from 0 --> 7
  u <- 7
  # Sigma based on dataset
  sigma <- 9.51
  trueBeta <- alpha_1/(sqrt(2) * sigma)
}
# Fourth scenario: one that should work according to Thas et al. 2012
if(SCEN == 4){
  alpha <- 1
  u <- 10
  sigma <- 5
  trueBeta <- alpha/(sqrt(2) * sigma)
}

# Different data generating models in different scenario's
if(SCEN %in% c(1,2,4)){
  # Generate predictor
  X <- runif(n = n, min = 0.1, max = u)
  
  # Generate data
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)
  
  # In data frame
  OrigData <- data.frame(Y = Y, X = X)
}
if(SCEN == 3){
  # Predictors: proportions come from ExporatoryAnalysis.R (observed proportions in study of mental well being)
  X_smartph_hrs <- sample(x = c(0:u), size = n, replace = TRUE)
  X_sex <- sample(x = sex, size = n, replace = TRUE, prob = c(1-0.4758, 0.4758))
  X_econArea <- sample(x = econArea, size = n, replace = TRUE, prob = c(1-0.4348, 0.4348))
  X_ethnic <- sample(x = ethnic, size = n, replace = TRUE, prob = c(1-0.2408, 0.2408))
  
  # Observed data
  Y <- alpha_0 + alpha_1*X_smartph_hrs + alpha_sex*X_sex +
    alphaEcon*X_econArea + alpha_ethnic*X_ethnic +
    rnorm(n = n, mean = 0, sd = sigma)
  
  # Data frame
  OrigData <- data.frame(Y = Y, X_smartph_hrs, X_sex, X_econArea, X_ethnic)
}


##
##########
### Testing the BLB method
##########
##

# Only execute in stage of development
if(SCEN == 1){
  print('Development only for model 1')
  if(isTRUE(DEVELOPMENT)){
    # Step 1: create S disjoint subsets of size b from original data frame
    # Number of bootstraps per bag
    boots <- 50
    # Max size of the bags
    bootCutOff <- 500
    S <- 1
    sizeB <- dim(OrigData)[1]
    # Constraint: b cannot be longer than bootCutOff
    while(sizeB > bootCutOff){
      sizeB <- dim(OrigData)[1] / S
      S <- S + 1
    }
    
    # Slow approach: single core operations
    # Step 2: loop over the subsets (the bags)
    bag_estimate <- c()
    for(i in 1:S){
      print(i)
      start <- i + (i - 1) * sizeB
      end <- start + sizeB
      subsetData <- OrigData %>% slice(start:end)
      # Step 3: bootstrap k times into the subset
      bootstrap_est <- c()
      for(k in 1:boots){
        bootsample <- sample_n(tbl = subsetData, size = sizeB, replace = TRUE)
        # Try to fit PIM. If fails, return message. 
        value <- try(pim(formula = Y ~ X, data = bootsample, 
                         link = 'probit', model = 'difference')@coef, silent = TRUE)
        if(class(value) == 'try-error'){
          print(paste0('Error in bag ',i, '. Bootstrap ',k,'. Message = ', attr(value,"condition")))
          next
        }else{
          # Save recorded value if no error.
          bootstrap_est <- c(bootstrap_est, value)
        }
      }
      bag_estimate_tmp <- mean(bootstrap_est)
      bag_estimate <- c(bag_estimate, bag_estimate_tmp)
    }
    
    
    
    # Faster approach: parallel, using as many cores as possible
    # Step 2: loop over the subsets (the bags)
    # First define a function
    BBBfunction <- function(sID, boots, sizeB){
      start <- sID + (sID - 1) * sizeB
      end <- start + sizeB
      subsetData <- OrigData %>% slice(start:end)
      # Step 3: bootstrap k times into the subset
      bootstrap_est <- c()
      for(k in 1:boots){
        bootsample <- sample_n(tbl = subsetData, size = sizeB, replace = TRUE)
        # Try to fit PIM. If fails, return message. 
        value <- try(pim(formula = Y ~ X, data = bootsample, 
                         link = 'probit', model = 'difference')@coef, silent = TRUE)
        if(class(value) == 'try-error'){
          print(paste0('Error in bag ',sID, '. Bootstrap ',k,'. Message = ', attr(value,"condition")))
          next
        }else{
          # Save recorded value if no error.
          bootstrap_est <- c(bootstrap_est, value)
        }
      }
      bag_estimate <- mean(bootstrap_est)
      return(bag_estimate)
    }
    
    # Detect and start the workers
    P <- detectCores(logical = FALSE) # physical cores
    cl <- makeCluster(P)
    
    # Initialize them with the OrigData and load library tidyverse
    clusterExport(cl, "OrigData")
    clusterEvalQ(cl, library(tidyverse))
    clusterEvalQ(cl, library(pim))
    clusterEvalQ(cl, library(nleqslv))
    
    # Now run the BBBfunction
    bag_estimate_results <- clusterApply(cl, 1:(S-1), fun = BBBfunction, boots = boots, sizeB = sizeB)
    bag_estimate <- do.call(rbind, bag_estimate_results)
    
    stopCluster(cl)
    
    mean(bag_estimate, na.rm = TRUE)
  }
}

##
##########
### Preparing the BLB method for HPC
##########
##

# If preparing for HPC, then start here 
if(!isTRUE(DEVELOPMENT)){
  rm(OrigData)
  # Possible to use up to 100 cores simultaneously
    # Hence, ideally subdivide in 100 parts, each part is a bag.
    # Number of bags (S)
  cores <- S <- 100
  # 100 bootstraps/bag
  boots <- 100
  # Max size of the bags
  sizeB <- n / cores
  
  # Vector of number of simulations
  nsim_vec <- 1:1000
  
  # Progress
  progress <- floor(quantile(nsim_vec, probs = c(seq(0,0.9,by = .1))))

  # Generate a new complete dataset for each simulation
  # Then split in S parts.
  for(i in 1:length(nsim_vec)){
    if(i %in% progress) print(paste0('At ', i/length(nsim_vec) * 100, '%'))
    # Start with creating data: same seed as non optimal resampling method
    set.seed(StartingSeed + (StartingSeed * i))
    
    # Different data generating models in different scenario's
    if(SCEN %in% c(1,2,4)){
      # Generate predictor
      X <- runif(n = n, min = 0.1, max = u)
      
      # Generate data
      Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)
      
      # In data frame
      OrigData <- data.frame(Y = Y, X = X)
    }
    if(SCEN == 3){
      # Predictors: proportions come from ExporatoryAnalysis.R (observed proportions in study of mental well being)
      X_smartph_hrs <- sample(x = c(0:u), size = n, replace = TRUE)
      X_sex <- sample(x = sex, size = n, replace = TRUE, prob = c(1-0.4758, 0.4758))
      X_econArea <- sample(x = econArea, size = n, replace = TRUE, prob = c(1-0.4348, 0.4348))
      X_ethnic <- sample(x = ethnic, size = n, replace = TRUE, prob = c(1-0.2408, 0.2408))
      
      # Observed data
      Y <- alpha_0 + alpha_1*X_smartph_hrs + alpha_sex*X_sex +
        alphaEcon*X_econArea + alpha_ethnic*X_ethnic +
        rnorm(n = n, mean = 0, sd = sigma)
      
      # Data frame
      OrigData <- data.frame(Y = Y, X_smartph_hrs, X_sex, X_econArea, X_ethnic)
    }
    
    # Now split up in S parts
    for(j in 1:S){
      start <- j + (j - 1) * sizeB
      end <- start + sizeB
      subsetData <- OrigData %>% slice(start:end)
      write.table(x = subsetData, file = paste(DataPartition,'/SCEN_', SCEN, '/BLBdata_scen_', SCEN,'_sim_', i, '_bag_', j, '.txt', sep = ''), 
                  row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
  }
}
