####################
#### TITLE:     Develop optimal weighted resampling scheme for large N PIM
#### Contents:  
####
#### Source Files: //Mastat/Thesis
#### First Modified: 19/08/2017
#### Notes:
#################


##
##########
### Notes
##########
##

# While we have non optimal resampling schemes in NonOptimalResampling.R, 
# we try to develop an optimal scheme.
# Originaly, we tried to do this by mimizing the trace of the var covar matrix.
# See Wang and Ma (3017, arXiv: 1702.01166v1) for an example using logistic regression.

# However, as this is based on maximum likelihood theory, I do not see how to implement this in a semi-parametric framework
# Hence, I will try something else.
# First I will do ordinary linear regression and get leverage scores for all observations.
# These are the strength of influence all observations have on the OLS.
# Then I will normalise these leverag scores to 1 and use these as subsampling probabilities.
# The assumption is that important observations for linear regression, will also be important in PIM.

# Note:
# SSP = subsampling probabilities


# Then, instead of estimating the PIM parameters using an unweighted score function,
# we will test a weighted version here.
# The weights correspond to the average subsampling weights of both observations in the pseudo-observation

# NOTE: no weighted variance-covariance estimation...


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
MACHINE <- try(as.character(args)[3],silent=TRUE)

# Different scenario's
SCEN <- try(as.numeric(as.character(args)[4]), silent = TRUE)

# If no machine is specified (it gives error), then it has to be this machine!
if(is.na(MACHINE)){
  MACHINE <- "HPC"
  MACHINE <- "LOCAL"
  nsim <- 1000
  ID <- 1:nsim
  ResultsDir <- 'D:/Users/Han/Dropbox/Mastat/Thesis/Results/Univariate/NonOptimalLocal'
  # Scenario
  SCEN <- 1
}
print(ResultsDir)

# Starting seed
StartingSeed <- 11 * SCEN
set.seed(StartingSeed + (StartingSeed * ID))

# Libraries
#library(pim)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(nleqslv)

# Custom functions
# Weighted score function
PIM.ScoreFunctionWeighted <- function(Z, PO, weights){
  U.func <- function(beta, Z, PO, weights){
    Zbeta <- c(Z%*%beta)
    colSums(Z*dnorm(Zbeta) * weights * (PO - pnorm(Zbeta))/c(pnorm(Zbeta)*(1-pnorm(Zbeta))))
  }
  return(U.func)
}

# Global variables: sample size
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


##
##########
### Generate data
##########
##

##
##########
### Fitting a PIM: HPC code
##########
##

# If running on HPC, then start here
if(MACHINE == "HPC"){
  # Vector of number of resampling loops
  nRSloops_vec <- floor(seq(10,1000,length.out = 10))
  nRSloops_vec <- nRSloops_vec[2:3]
  
  # Vector of number of selected datapoints K per iteraton
  K_vec <- floor(seq(10,1000,length.out = 10))
  K_vec <- K_vec[2:3]
  
  # Number of pairs/combinations between number of resampling loops and sampled data
  pairs <- expand.grid(nRSloops = nRSloops_vec, K = K_vec)
  nPairs <- dim(pairs)[1]
  
  # Different data generating models in different scenario's
  if(SCEN %in% c(1,2,4)){
    # Generate predictor
    X <- runif(n = n, min = 0.1, max = u)
    
    # Generate data
    Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)
    
    # In data frame
    OrigData <- data.frame(Y = Y, X = X)
    
    # Now fit linear regression to obtain leverage scores
    LM <- lm(Y ~ X, data = OrigData)
    
    # Then get fitted values and normalize these
    weights <- hatvalues(LM) / sum(hatvalues(LM))
    OrigData$Weights <- weights
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
    
    # Again, fit linear regression, obtain leverage scores and normalise
    LM <- lm(Y ~ X_smartph_hrs + X_sex + X_econArea + X_ethnic, data = OrigData)
    weights <- hatvalues(LM) / sum(hatvalues(LM))
    OrigData$Weights <- weights
  }
  
  # Recorded time: per simulation
  recordedTime <- array(NA, dim = nPairs)
  # Estimated values: initialize with NULL
  BetaValues <- data.frame(beta = NULL, K = NULL, nRSloops = NULL, TrueBeta = NULL)
  
  # Progress in simulations
  progress <- floor(seq(1, nPairs, length.out = 11)[-1])
  
  # Get the weighted estimators nPairs times
  for(i in 1:nPairs){
    # Check progress
    if(i %in% progress) print(paste0('At ', which(i == progress, arr.ind = TRUE)*10, '%'))
    
    # Set new seed according to simulation ID and pair
    set.seed(StartingSeed + (StartingSeed * ID) + log(i))
    
    # Get number of resampling loops and number of selected datapoints
    nRSloops <- pairs[i,'nRSloops']
    K <- pairs[i,'K']
    
    # Gather beta values and variance estimators inside resampling scheme
    beta_loop <- beta_sVar <- c()
    
    # Start recording time
    StartTime <- Sys.time()
    
    # Start resampling scheme
    for(l in 1:nRSloops){
      # Sample rows from original data with leverage scores as probability (i.e. weight = Weights)
      if(SCEN %in% c(1,2,4)){
        SelectedData <- dplyr::sample_n(OrigData, size = K, replace = TRUE, weight = Weights)
      }
      if(SCEN == 3){
        # While statement: check that at least both values of each control variable are sampled (otherwise matrix = singular)
        SelectedData <- dplyr::sample_n(OrigData, size = K, replace = TRUE, weight = Weights)
        CheckRange <- select(SelectedData, X_sex, X_econArea, X_ethnic)
        while(sum(colSums(apply(CheckRange, 2, range))) != 3){
          SelectedData <- dplyr::sample_n(OrigData, size = K, replace = TRUE, weight = Weights)
          CheckRange <- select(SelectedData, X_sex, X_econArea, X_ethnic)
        }
      }
      
      # Construct PO, and estimate weighted parameters
      if(SCEN %in% c(1,2,4)){
        # PO
        IndX <- SelectedData$X %>% data.frame('X' = ., 'Xweight' = SelectedData$Weights) %>% mutate(index = 1:length(SelectedData$X))
        PseudoObs <- data.frame(expand.grid('Y' = SelectedData$Y,'Yprime' = SelectedData$Y),
                         expand.grid('IY' = 1:length(SelectedData$Y),'IYprime' = 1:length(SelectedData$Y))) %>%
                    rowwise() %>% mutate(X = IndX[which(IndX$index == IY),'X'],
                          Xprime = IndX[which(IndX$index == IYprime),'X'],
                          Xweight = IndX[which(IndX$index == IY),'Xweight'],
                          XprimeWeight = IndX[which(IndX$index == IYprime),'Xweight']) %>%
                    filter(IY != IYprime) %>% select(-IY,-IYprime) %>%
                    mutate(PO = ifelse(Y < Yprime, 1,
                          ifelse(Y == Yprime,0.5,0)))
        # Filter only those observations I(Y <= Yprime)
        IndPseudObs <- PseudoObs %>% filter(PO > 0)
        
        # Calculate Z and weight of Z, based on mean
        IndPseudObs <- mutate(IndPseudObs, Z = Xprime - X, weightZ = (XprimeWeight + Xweight)/2)
        
        # Initial beta value for one predictor: one number
        beta <- matrix(0, ncol = 1, nrow = 1)
        # Z vector with length number of pseudo obs
        Z <- IndPseudObs %>% select(Z) %>% as.matrix(., ncol = 1)
        # Z times beta: matrix of [nPseudo x 1]
        Zbeta <- c(Z%*%beta) %>% as.matrix(., ncol = 1)
        # Pseudo observations
        PO <- IndPseudObs %>% select(PO)
        # Weights
        weightZ <- IndPseudObs %>% select(weightZ) %>% as.matrix(., ncol = 1)
        
        # Solve weighted score function
        PIM_coef_w <- nleqslv(x = rep(0,ncol(Z)), PIM.ScoreFunctionWeighted(Z = Z, PO = PO, weights = weightZ), Z=Z, PO=PO, weights = weightZ)$x

        # If error, don't save        
        if(class(PIMfit) == 'try-error'){
          print(paste0('Error in sim ',i, '. Iteration ',l,'.'))
          next
        }else{
          # Save estimated beta value if no error.
          beta_loop <- c(beta_loop, PIM_coef_w)
        }
      }
      # Different model for scenario 3
      if(SCEN == 3){
        PIMfit <- try(pim(formula = Y ~ X_smartph_hrs + X_sex + X_econArea + X_ethnic, data = SelectedData,
                          link = 'probit', model = 'difference'), silent = TRUE)
        if(class(PIMfit) == 'try-error'){
          print(paste0('Error in sim ',i, '. Iteration ',l,'.'))
          next
        }else{
          # Names of the objects
          namesObject <- tidy(PIMfit@coef) %>% data.table::transpose() %>% slice(.,1)
          
          # Save beta value if no error.
          PIMvalues <- tidy(PIMfit@coef) %>% data.table::transpose() %>% slice(.,2)
          names(PIMvalues) <- namesObject
          beta_loop <- bind_rows(beta_loop, PIMvalues)
          
          # Save sandwich variance estimator (only variance on diagonal)
          PIMvariances <- tidy(diag(PIMfit@vcov)) %>% data.table::transpose() %>% slice(2)
          names(PIMvariances) <- namesObject
          beta_sVar <- bind_rows(beta_sVar, PIMvariances)
        }
      }
    }
    # Recorded time
    recordedTime[i] <- difftime(time1 = Sys.time(), time2 = StartTime, units = 'mins')
    
    # Collect estimated beta values with info about nRSloops and K
    BetaValues <- bind_rows(BetaValues,
                            data.frame(beta = beta_loop,
                                       K = K,
                                       nRSloops = nRSloops,
                                       TrueBeta = trueBeta))
  }
  # Write results of beta and recorded time
  write.table(BetaValues, file = paste(ResultsDir, '/weighted_leverage_beta_vector_simID_',ID, '_SCEN_', SCEN, '.txt', sep = ''), row.names = FALSE, col.names = TRUE)
  write.table(recordedTime, file = paste(ResultsDir, '/weighted_leverage_time_vector_simID_',ID, '_SCEN_', SCEN, '.txt', sep = ''), row.names = FALSE, col.names = TRUE)
}











