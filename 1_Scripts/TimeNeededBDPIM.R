####################
#### TITLE:     Measure time needed for Big Data PIM.
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


# Run 500 (parallel) simulations in which n increases from 10 to 100.000
# Save the amount of time needed.


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
K <- as.numeric(as.character(args)[1])

# Output
OutputDir <- as.character(args)[2]
print(OutputDir)

# seed
set.seed(1990 * K)

# Libraries
library(pim)
library(nleqslv)


# Global variables: normal linear model
n <- round(seq(10,25000,length.out = 25), 0)
u <- 1
alpha <- 5
sigma <- 1
trueBeta <- alpha/(sqrt(2) * sigma)


##
##########
### Run the simulations
##########
##

# Recorded time
recordedTime <- array(NA, dim = length(n))
# True beta value
BetaValues <- array(NA, dim = length(n))

# Fit the model length(n) times
for(i in 1:length(n)){
  # Start recording time
  StartTime <- Sys.time()
  # Generate predictor
  X <- runif(n = n[i], min = 0.1, max = u)
  # Generate data
  Y <- alpha*X + rnorm(n = n[i], mean = 0, sd = sigma)

  # PIM package beta parameter
  value <- try(pim(formula = Y ~ X, link = 'probit', model = 'difference')@coef, silent = TRUE)
  if(class(value) == 'try-error'){
    print(paste0('Error in sim ',i, ' c = ', c, '. Message = ', attr(value,"condition")))
    next
  }else{
    BetaValues[i] <- value
    recordedTime[i] <- difftime(time1 = Sys.time(), time2 = StartTime, units = 'mins')
  }
  # Intermediate step in which we write the time recorded and beta value to folder
  write.table(BetaValues[i], file = paste(OutputDir, '/separate/beta_N_', n[i], '_K_', K, '.txt', sep = ''), row.names = FALSE, col.names = FALSE)
  write.table(recordedTime[i], file = paste(OutputDir, '/separate/time_N_', n[i], '_K_', K, '.txt', sep = ''), row.names = FALSE, col.names = FALSE)
}

# Save the recorded beta and time values in one file
write.table(BetaValues, file = paste(OutputDir, '/beta_', K, '.txt', sep = ''), row.names = FALSE, col.names = FALSE)
write.table(recordedTime, file = paste(OutputDir, '/time_', K, '.txt', sep = ''), row.names = FALSE, col.names = FALSE)






