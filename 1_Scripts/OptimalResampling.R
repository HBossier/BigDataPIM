####################
#### TITLE:     Develop optimal resampling scheme for large N PIM
#### Contents:  
####
#### Source Files: //Mastat/Thesis
#### First Modified: 11/07/2017
#### Notes:
#################


##
##########
### Notes
##########
##

# While we have non optimal resampling schemes in NonOptimalResampling.R, 
# we try to develop an optimal scheme based on mimizing the trace of the var covar matrix.

# See Wang and Ma (3017, arXiv: 1702.01166v1) for an example using logistic regression.

# Note:
  # SSP = subsampling probabilities

##
##########
### Preparation
##########
##

# Reset working memory
rm(list = ls())

# Libraries
library(pim)
library(dplyr)
library(ggplot2)
library(nleqslv)

# Starting seed
seed <- 11
set.seed(seed)

# Sample size
n <- 250000


##
##########
### Generate data
##########
##

# Using parameters of real study (digital screen usage on mental well being, several control variables)
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



##
##########
### Algorithm: step one 
##########
##

# Obtain initial estimates for beta and var covariance matrix using uniform SSP (subsampling probabilities)



##
##########
### Replicate algorithm of Wang and Ma based on logistic regression
##########
##

# Large dataset
LargeN <- 10000000

# First do logistic regression on mtcars dataset
LogReg <- glm(am ~ hp + wt, data = mtcars, family = binomial)
summary(LogReg)

# Let us use these parameters to generate large dataset
Xhp <- sample(x = mtcars$hp, size = LargeN, replace = TRUE)
Xwt <- sample(x = mtcars$wt, size = LargeN, replace = TRUE)
probabilities <- exp(LogReg$coefficients[1] + Xhp * LogReg$coefficients[2] + Xwt * LogReg$coefficients[3]) / (1 + exp(LogReg$coefficients[1] +Xhp * LogReg$coefficients[2] + Xwt * LogReg$coefficients[3]))
Y <- rbinom(n = LargeN, size = 1, prob = probabilities)

# Now try logistic regression on this large dataset
largeLogReg <- glm(Y ~ Xhp + Xwt, family = binomial)
summary(largeLogReg)











