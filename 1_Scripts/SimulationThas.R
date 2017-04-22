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
#library(data.table)

# Global variables
n <- 25
u <- 1
alpha <- 1
sigma <- 1

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
Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)
TrueBeta <- alpha/(sqrt(2) * sigma)

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
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)

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
### Compare speed
##########
##


# Let us make a new function to calcuclate the pseudo-observations
CreatePO <- function(Y, X){
  if(class(X)!='data.frame') X <- data.frame('X' = X)
  IndX <- X %>% mutate(index = 1:length(X))
  Yvalues <- data.frame(expand.grid('Y' = Y,'Yprime' = Y),
                        expand.grid('IY' = 1:length(Y),'IYprime' = 1:length(Y)))
  # Now create P0 and select the I:= Y <= YPrime
  POdoubles <-  Yvalues %>%  mutate(PO = ifelse(Y < Yprime,1,
            ifelse(Y == Yprime,0.5,0))) %>% filter(PO > 0)
  # Note that we have Y and Y in the data frame. We delete these now.
  POsingles <- POdoubles %>% filter(IY != IYprime)

  # Now we need to add the X and Xprime variables, we use the indicators for Y and X to do this.
  PO <- POsingles %>% rowwise() %>% mutate(X = IndX[which(IndX$index == IY),'X'],
            Xprime = IndX[which(IndX$index == IYprime),'X'])

  return(PO %>% select(-IY,-IYprime))
}

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
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)

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
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sigma)
  # Create the pseudo-observations
  Observations <- CreatePO(Y = Y, X = X)
  # Calculate Z
  Z <- mutate(Observations, Z = Xprime - X) %>% select(Z) %>% as.matrix(., ncol = 1)
  # Pseudo observations
  PO <- Observations %>% select(PO)

  # Estimation
  speedTest <- nleqslv(x = rep(0,ncol(Z)), PIM.ScoreFunction(Z = Z, PO = PO), Z = Z, PO = PO)$x
}
ManualSpeed <- Sys.time() - t1


data.frame('Package speed' = PackageSpeed,
           'Manual speed' = ManualSpeed,
           'Number of simulations' = nsim,
           'Sample size' = n)


# For now: better to use package!


##
##########
### Replicate results of Thas et al.
##########
##


# Simulate over 1000 simulations in which we vary alpha, u, sigma and n:
alpha <- c(1,10)
sigma <- c(1,5)
u <- c(1,10)
n <- c(25,50,200)

combinations <- expand.grid('alpha' = alpha,'sigma' = sigma,'u' = u, 'n' = n)

BetaValues <- array(NA, dim = c(nsim, dim(combinations)[1]))

# loop over the combinations
for(c in 1:dim(combinations)[1]){
  # Print status
  print(paste0('@: ', combinations[c,]))
  # Set the parameters
  nSim <- combinations[c,'n']
  uSim <- combinations[c,'u']
  alphaSim <- combinations[c,'alpha']
  sigmaSim <- combinations[c,'sigma']

  # Generate predictor
  X <- runif(n = nSim, min = 0.1, max = uSim)

  # Fit the model nsim times
  for(i in 1:nsim){
    # Generate data
    Y <- alphaSim*X + rnorm(n = nSim, mean = 0, sd = sigmaSim)

    # PIM package beta parameter
    value <- try(pim(formula = Y ~ X, link = 'probit', model = 'difference')@coef, silent = TRUE)
    if(class(value) == 'try-error'){
      print(paste0('Error in sim ',i, ' c = ', c, '. Message = ', attr(value,"condition")))
      next
    }else{
      BetaValues[i,c] <- value
    }
  }
}

# Dimension
BetaValues
dim(BetaValues)

# Average beta hat and variance
combinations <- combinations %>% mutate(beta = round(alpha/(sqrt(2) * sigma), digits = 3))
ReplResults <- data.frame(combinations, AvBetaHat = round(colMeans(BetaValues), digits = 5),
              VarBetaHat = round(apply(BetaValues, 2, var), digits = 5))


# Why is the combination alpha = 10 and u = 10 not good?






