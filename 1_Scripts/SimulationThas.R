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


# working directory
wd <- '/Users/hanbossier/Dropbox/Mastat/Thesis/RCodePIM/'
setwd(wd)

# seed
set.seed(1990)

# Libraries
library(pim)
library(dplyr)
library(ggplot2)

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
PIMfit <- pim(formula = Y ~ X, link = 'probit', model = 'difference')
summary(PIMfit)
PIMfit@coef
pnorm(PIMfit@coef)
PIMfit@coef/(1+PIMfit@coef)

##
##########
### Normal linear regression
##########
##

# First generate the predictor values.
# This is equally spaced between [0,u]
X <- runif(n = n, min = 0.1, max = u)

PIMpack_beta <- c()

# Run the simulations
for(i in 1:nsim){
  # First generate the predictor values.
  # This is equally spaced between [0,u]
  X <- runif(n = n, min = 0.1, max = u)
  
  # Generate data
  Y <- alpha*X + rnorm(n = n, mean = 0, sd = sdX) 
  
  # PIM package beta parameter
  PIMpack_beta <- c(PIMpack_beta, pim(formula = Y ~ X, link = 'probit', model = 'difference')@coef)
  
  # Manually
  # Step one: create the set of pseudo-observations
  IndX <- X %>% data.frame('X' = .) %>% mutate(index = 1:length(X))

  PseudoObs <- data.frame(expand.grid('Y' = Y,'Yprime' = Y),
             expand.grid('IY' = 1:length(Y),'IYprime' = 1:length(Y))) %>%
            rowwise() %>% mutate(X = IndX[which(IndX$index == IY),'X'],
                                 Xprime = IndX[which(IndX$index == IYprime),'X']) %>%
            filter(IY != IYprime) %>% select(-IY,-IYprime) 
  # Calculate Z
  PseudoObs <- mutate(PseudoObs, Z = X - Xprime)
  
      
      
      
}




### OLDER CODE

mutate(IndY,X = IndX[which('index' == IndY$Y),'index'])

IndY$Y == IndX$index
which(IndX$index == IndY$Y,arr.ind = TRUE)
apply(array(IndY$Y, dim = c(length(IndY$Y),1)), 1, FUN = function(x){return(IndX[which(IndX$index == x),'X'])})
apply(matrix(IndY$Y, ncol = 1), 1, FUN = function(x){return(IndX[which(IndX$index == x),'X'])})

IndX[which(IndX$index == 2),'X']

IndX[which('index' == 2),'X']
mutate(IndY, test = IndX$index)

data.frame(expand.grid('Y' = Y,'Yprime' = Y),expand.grid('IY' = 1:length(Y),'IYprime' = 1:length(Y))) %>%
  filter(IY != IYprime)

SelectX <- function(x, X){
  IndX <- X %>% data.frame('X' = .) %>% mutate(index = 1:length(X))
  return(IndX[which(IndX$index == x),'X'])
}

data.frame(expand.grid('Y' = Y,'Yprime' = Y),
           expand.grid('IY' = 1:length(Y),'IYprime' = 1:length(Y))) %>%
  mutate(., apply(matrix(.$IY, ncol = 1), 1, FUN = function(x){return(IndX[which(IndX$index == x),'X'])})) %>%
  mutate(., apply(matrix(.$IYprime, ncol = 1), 1, FUN = function(x){return(IndX[which(IndX$index == x),'X'])})) 

apply(matrix(test$Y, ncol = 1), 1, FUN = function(x){return(IndX[which(IndX$index == x),'X'])})

apply(matrix(IndY$Y, ncol = 1), 1, FUN = function(x){return(IndX[which(IndX$index == x),'X'])}) %>% length()




IndY <- expand.grid('Y' = 1:length(Y),'Yprime' = 1:length(Y)) %>% 
  filter(Y != Yprime)
IndY <- expand.grid(Y,Y)
expand.grid('Y' = Y,'Yprime'=Y) %>% # NEED TO REMOVE THE DUPLICATES=
  mutate(,X = 
           apply(matrix(IndY$Y, ncol = 1), 1, FUN = function(x){return(IndX[which(IndX$index == x),'X'])})
  )
mutate(ind = if_else(Y <= Yprime, true = 1, false = 0)) %>% 
  filter(ind == 1) %>% select(Y, Yprime) %>% mutate(Z = )

IndY






a <- c(1:5)
b <- c(4:8)
expand.grid(a,b)
expand.grid(a,a)

summary(PIMpack_beta)
mean(PIMpack_beta)


mean((exp(PIMpack_beta)/(1+exp(PIMpack_beta))))
exp(1.22)/(1+exp(1.22))
