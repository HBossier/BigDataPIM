####################
#### TITLE:     Run resampling scheme for large N
#### Contents:  
####
#### Source Files: //Mastat/Thesis
#### First Modified: 22/04/2017
#### Notes:
#################


##
##########
### Notes
##########
##

# In this script, we run for I times the resampling scheme with 1/n probability assigned 
# to all observations for being selected within an iteration.

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
ID <- as.numeric(as.character(args)[1])

# Results
ResultsDir <- as.character(args)[2]
print(ResultsDir)

# # working directory
# wd <- '/Users/hanbossier/Dropbox/Mastat/Thesis/RCodePIM/'
# setwd(wd)

# # Specify output directory
# OutputDir <- '/Users/hanbossier/Dropbox/Mastat/Thesis/Results/Univariate/'

# Seed
StartingSeed <- 11


# Libraries
library(pim)
library(dplyr)
library(ggplot2)
library(nleqslv)



# Global variables: univariate simple linear regression 
n <- 250000
u <- 1
alpha <- 5
sigma <- 2
trueBeta <- alpha/(sqrt(2) * sigma)

# # Number of simulations
# nsim <- 1000



##
##########
### Fitting a PIM
##########
##

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
	nRSloops <- nRSloops_vec[i]
	K <- K_vec[i]

	# Gather beta values inside resampling scheme 
	beta_loop <- c()

	# Start recording time
	StartTime <- Sys.time()

	# Start resampling scheme 
	for(l in 1:nRSloops){
		# Sample rows from original data with 1/n probability (i.e. weight = NULL)
		SelectedData <- dplyr::sample_n(OrigData, size = K, replace = TRUE, weight = NULL)

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









