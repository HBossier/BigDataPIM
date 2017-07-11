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

# Not yet integrated within PIM package.

##
##########
### Preparation
##########
##

# Reset working memory
rm(list = ls())

# Take arguments from master file
args <- commandArgs(TRUE)

# ID of simulation
ID <- try(as.numeric(as.character(args)[1]), silent=TRUE)

# Results
ResultsDir <- try(as.character(args)[2], silent=TRUE)

# Which machine: HPC or LOCAL
MACHINE <- try(as.character(input)[3],silent=TRUE)

# If no machine is specified (it gives error), then it has to be this machine!
if(class(MACHINE)=='try-error'){
  MACHINE <- "LOCAL"
  nsim <- 1000
  ID <- 1:nsim
  ResultsDir <- 'D:/Users/Han/Dropbox/Mastat/Thesis/Results/Univariate/NonOptimalLocal'
}
print(ResultsDir)

# Seed
StartingSeed <- 11


# Libraries
library(pim)
library(dplyr)
library(ggplot2)
library(nleqslv)
library(parallel)


# Global variables: univariate simple linear regression 
n <- 250000
u <- 1
alpha <- 5
sigma <- 2
trueBeta <- alpha/(sqrt(2) * sigma)

# Number of bootstraps per bag
boots <- 50
# Max size of the bags
bootCutOff <- 500

##
##########
### Testing the BLB method
##########
##

# Generate predictor
X <- runif(n = n, min = 0.1, max = u)
# Generate data
Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)
# In data frame 
OrigData <- data.frame(Y = Y, X = X)

# Step 1: create S disjoint subsets of size b from original data frame
# Constraint: b cannot be longer than bootCutOff
S <- 1
sizeB <- dim(OrigData)[1]
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


##
##########
### Fitting a PIM: HPC code 
##########
##

# If running on HPC, then start here 
if(MACHINE == "HPC"){
  
  # Vector of number of resampling loops
  nRSloops_vec <- floor(seq(10,1000,length.out = 10))
  
  # Vector of number of selected datapoints K per iteraton 
  K_vec <- floor(seq(10,1000,length.out = 10))
  
  # Number of pairs/combinations between number of resampling loops and sampled data 
  pairs <- expand.grid(nRSloops = nRSloops_vec, K = K_vec)
  nPairs <- dim(pairs)[1]
  
  # Generate predictor
  X <- runif(n = n, min = 0.1, max = u)
  # Generate data
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)
  # In data frame 
  OrigData <- data.frame(Y = Y, X = X)
  
  # Recorded time: per simulation 
  recordedTime <- array(NA, dim = nPairs)
  # True beta value: initialize with NULL values 
  BetaValues <- data.frame(beta = NULL, K = NULL, nRSloops = NULL)
  
  # Progress in simulations  
  progress <- floor(seq(1, nPairs, length.out = 11)[-1])
  
  # Fit the model nPairs times
  for(i in 1:nPairs){
    # Check progress 
    if(i %in% progress) print(paste0('At ', which(i == progress, arr.ind = TRUE)*10, '%'))
    
    # Set new seed according to simulation ID and pair 
    set.seed(StartingSeed + (StartingSeed * ID) + log(i))
    
    # Get number of resampling loops and number of selected datapoints 
    nRSloops <- pairs[i,'nRSloops']
    K <- pairs[i,'K']
    
    # Gather beta values inside resampling scheme 
    beta_loop <- c()
    
    # Start recording time
    StartTime <- Sys.time()
    
    # Start resampling scheme 
    for(l in 1:nRSloops){
      # Sample rows from original data with 1/n probability (i.e. weight = NULL)
      SelectedData <- dplyr::sample_n(OrigData, size = K, replace = FALSE, weight = NULL)
      
      # Try to fit PIM. If fails, return message. 
      value <- try(pim(formula = Y ~ X, data = SelectedData, 
                       link = 'probit', model = 'difference')@coef, silent = TRUE)
      if(class(value) == 'try-error'){
        print(paste0('Error in sim ',i, '. Iteration ',l,'. Message = ', attr(value,"condition")))
        next
      }else{
        # Save recorded value if no error.
        beta_loop <- c(beta_loop, value)
      }
    }
    # Recorded time 
    recordedTime[i] <- difftime(time1 = Sys.time(), time2 = StartTime, units = 'mins')
    
    # Collect beta values with info about nRSloops and K
    BetaValues <- bind_rows(BetaValues, 
                            data.frame(beta = beta_loop,
                                       K = K,
                                       nRSloops = nRSloops))
    
  }
  
  
  # Write results of beta and recorded time 
  write.table(BetaValues, file = paste(ResultsDir, '/uni_beta_vector_simID_',ID, '.txt', sep = ''), row.names = FALSE, col.names = FALSE)
  write.table(recordedTime, file = paste(ResultsDir, '/uni_time_vector_simID_',ID, '.txt', sep = ''), row.names = FALSE, col.names = FALSE)
  
}
