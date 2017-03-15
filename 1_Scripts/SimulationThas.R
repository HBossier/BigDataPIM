####################
#### TITLE:     Replicating simulations of Thas et al. 2012 - PIM
#### Contents:  Raw Code
####
#### Source Files: //Mastat/Thesis
#### First Modified: 04/03/2017
#### Notes:
#################


##
##########
### Notes
##########
##

# In this script, we try to replicate the simulations from Thas, O., De Neve, J. Clement, L. and Ottoy, J.P. (2012) Probabilistic index models
# We first generate data according to
  # (1) the normal linear model with homoscedastic variance
  # (2) the normal linear model with heteroscedastic variance
  # (3) the exponential model

# We then fit the PIM model and calculate the:
  # (1) average estimated beta parameters
  # (2) the sample variance of the estimated beta parameters
  # (3) the average of the sandwich variance PIM estimates
  # (4) the emperical coverage of the 95% CI for beta.



##
##########
### Preparation
##########
##

# Reset working memory
rm(list = ls())

# working directory
wd <- '/Users/hanbossier/Dropbox/Mastat/Thesis/RCodePIM/'
setwd(wd)

# seed
set.seed(1990)

# Libraries
library(pim)
library(dplyr)
library(ggplot2)
library(nleqslv)
library(data.table)

# Global variables 
n <- 25
u <- 1
alpha <- 1
sdX <- 1

# Number of simulations
nsim <- 1000


##
##########
### Fitting a PIM
##########
##

# In the first step, we try to fit a PIM on one simulation only

# Step one: generate data
X <- runif(n = n, min = 0.1, max = u)
Y <- alpha*X + rnorm(n = n, mean = 0, sd = sdX) 
TrueBeta <- alpha/sqrt(2*sdX**2)

# We can first try using the PIM package
PIMfit <- pim(formula = Y ~ X, data = data.frame(Y = Y, X = X), link = 'probit', model = 'difference')
summary(PIMfit)
PIMfit@coef

# Manually
# Step one: create the set of pseudo-observations
IndX <- X %>% data.frame('X' = .) %>% mutate(index = 1:length(X))

PseudoObs <- data.frame(expand.grid('Y' = Y,'Yprime' = Y),
                expand.grid('IY' = 1:length(Y),'IYprime' = 1:length(Y))) %>%
                rowwise() %>% mutate(X = IndX[which(IndX$index == IY),'X'],
                Xprime = IndX[which(IndX$index == IYprime),'X']) %>%
                filter(IY != IYprime) %>% select(-IY,-IYprime) %>% 
                mutate(PO = ifelse(Y < Yprime,1,
                                   ifelse(Y == Yprime,0.5,0)))
# Check sum of PO
PseudoObs %>% select(PO) %>% colSums()

# Filter only those observations I(Y <= Yprime)
IndPseudObs <- PseudoObs %>% filter(PO > 0)

# Calculate Z
IndPseudObs <- mutate(IndPseudObs, Z = Xprime - X)

# Initial beta value for one predictor: one number
beta <- matrix(0, ncol = 1, nrow = 1)
# Z vector with length number of pseudo obs
Z <- IndPseudObs %>% select(Z) %>% as.matrix(., ncol = 1)
# Z times beta: matrix of [nPseudo x 1]
Zbeta <- c(Z%*%beta) %>% as.matrix(., ncol = 1)
# Pseudo observations
PO <- IndPseudObs %>% select(PO)

# Estimating equation
colSums(Z*dnorm(Zbeta) * (PO - pnorm(Zbeta) / c(pnorm(Zbeta)*(1-pnorm(Zbeta)))))

# To solve, have estimating equation in function
PIM.ScoreFunction <- function(Z, PO){
  U.func <- function(beta, Z, PO){ 
    Zbeta <- c(Z%*%beta)
    colSums(Z*dnorm(Zbeta)*(PO - pnorm(Zbeta))/c(pnorm(Zbeta)*(1-pnorm(Zbeta))))
  }
  return(U.func)
}

coef <- nleqslv(x = rep(0,ncol(Z)), PIM.ScoreFunction(Z = Z, PO = PO), Z=Z, PO=PO)$x

data.frame('PIM' = PIMfit@coef, 'Manual' = coef)
# Check why Z is defined as XPrime - X instead of X - XPrime




##
##########
### Normal linear regression
##########
##

# Estimating equation in function
PIM.ScoreFunction <- function(Z, PO){
  U.func <- function(beta, Z, PO){ 
    Zbeta <- c(Z%*%beta)
    colSums(Z*dnorm(Zbeta)*(PO - pnorm(Zbeta))/c(pnorm(Zbeta)*(1-pnorm(Zbeta))))
  }
  return(U.func)
}

# Generate the predictor values here, or inside for loop?
# This is equally spaced between [0,u]
X <- runif(n = n, min = 0.1, max = u)

betaValues <- data.frame('PIM' = matrix(NA, nrow = nsim),
                         'Manual' = matrix(NA, nrow = nsim))

# Run the simulations
for(i in 1:nsim){
  # First generate the predictor values.
  # This is equally spaced between [0,u]
  X <- runif(n = n, min = 0.1, max = u)
  
  # Generate data
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sdX) 
  
  # PIM package beta parameter
  betaValues[i,1] <- pim(formula = Y ~ X, link = 'probit', model = 'difference')@coef
  
  # Manually
  # Step one: create the set of pseudo-observations
  IndX <- X %>% data.frame('X' = .) %>% mutate(index = 1:length(X))
  
  PseudoObs <- data.frame(expand.grid('Y' = Y,'Yprime' = Y),
                          expand.grid('IY' = 1:length(Y),'IYprime' = 1:length(Y))) %>%
    rowwise() %>% mutate(X = IndX[which(IndX$index == IY),'X'],
                         Xprime = IndX[which(IndX$index == IYprime),'X']) %>%
    filter(IY != IYprime) %>% select(-IY,-IYprime) %>% 
    mutate(PO = ifelse(Y < Yprime,1,
                       ifelse(Y == Yprime,0.5,0)))
  
  # Filter only those observations:= I(Y <= Yprime)
  IndPseudObs <- PseudoObs %>% filter(PO > 0)
  # Step two: calculate Z
  Z <- mutate(IndPseudObs, Z = Xprime - X) %>% select(Z) %>% as.matrix(., ncol = 1)
  # Pseudo observations
  PO <- IndPseudObs %>% select(PO)
  
  # Step 3: estimation
  betaValues[i,2] <- nleqslv(x = rep(0,ncol(Z)), PIM.ScoreFunction(Z = Z, PO = PO), Z = Z, PO = PO)$x

}

# Check some quick results
apply(betaValues, 2, summary)
apply(betaValues, 2, mean)


##
##########
### compare speed
##########
##


# Compare time difference between manual and package approach.
nsim <- 5000

# Run the speed test simulation: PART ONE
t1 <- Sys.time()
set.seed(1990)
for(i in 1:nsim){
  # First generate the predictor values.
  # This is equally spaced between [0,u]
  X <- runif(n = n, min = 0.1, max = u)
  
  # Generate data
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sdX) 
  
  # PIM package beta parameter
  speedTest <- pim(formula = Y ~ X, link = 'probit', model = 'difference')@coef
}
PackageSpeed <- Sys.time() - t1


# Run the speed test simulation: PART TWO
t1 <- Sys.time()
set.seed(1990)
for(i in 1:nsim){
  # First generate the predictor values.
  # This is equally spaced between [0,u]
  X <- runif(n = n, min = 0.1, max = u)
  
  # Generate data
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sdX) 

  # Create the set of pseudo-observations
  IndX <- X %>% data.frame('X' = .) %>% mutate(index = 1:length(X))
  
  PseudoObs <- data.frame(expand.grid('Y' = Y,'Yprime' = Y),
                          expand.grid('IY' = 1:length(Y),'IYprime' = 1:length(Y))) %>%
    rowwise() %>% mutate(X = IndX[which(IndX$index == IY),'X'],
                         Xprime = IndX[which(IndX$index == IYprime),'X']) %>%
    filter(IY != IYprime) %>% select(-IY,-IYprime) %>% 
    mutate(PO = ifelse(Y < Yprime,1,
                       ifelse(Y == Yprime,0.5,0)))
  
  # Filter only those observations:= I(Y <= Yprime)
  IndPseudObs <- PseudoObs %>% filter(PO > 0)
  # Calculate Z
  Z <- mutate(IndPseudObs, Z = Xprime - X) %>% select(Z) %>% as.matrix(., ncol = 1)
  # Pseudo observations
  PO <- IndPseudObs %>% select(PO)
  
  # Estimation
  speedTest <- nleqslv(x = rep(0,ncol(Z)), PIM.ScoreFunction(Z = Z, PO = PO), Z = Z, PO = PO)$x
}
ManualSpeed <- Sys.time() - t1


data.frame('Package speed' = PackageSpeed,
           'Manual speed' = ManualSpeed)


