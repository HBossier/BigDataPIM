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

# In this script, we run for J times the resampling scheme with 1/n probability assigned 
# to all observations for being selected within an iteration.

# We test several scenario's.

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
MACHINE <- try(as.character(args)[3],silent=TRUE)

# Different scenario's
SCEN <- try(as.numeric(as.character(args)[4], silent = TRUE))

# If no machine is specified (it gives error), then it has to be this machine!
if(is.na(MACHINE)){
	MACHINE <- "LOCAL"
	nsim <- 1000
  	ID <- 1:nsim
  	ResultsDir <- 'D:/Users/Han/Dropbox/Mastat/Thesis/Results/Univariate/NonOptimalLocal'
  	# Scenario
  	SCEN <- 2
}
print(ResultsDir)

# Starting seed
StartingSeed <- 11 * SCEN

# Libraries
library(pim)
library(dplyr)
library(ggplot2)
library(nleqslv)


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
if(SCEN == 3){
	### NEED TO IMPLEMENT THIRD SCENARIO
  u <- 7
  alpha_1 <- -0.5
  alpha_0 <- 47.5
  sigma <- 2
  trueBeta <- alpha_1/(sqrt(2) * sigma)

  	X <- sample(x = c(0:u), size = n, replace = TRUE)
	# Generate data
	Y <- alpha_0 + alpha_1*X + rnorm(n = n, mean = 0, sd = sigma)

}



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
	# Estimated values: initialize with NULL 
	BetaValues <- data.frame(beta = NULL, sVariance = NULL, K = NULL, nRSloops = NULL, TrueBeta = NULL)

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

		# Gather beta values and variance estimators inside resampling scheme 
		beta_loop <- beta_sVar <- c()
		
		# Start recording time
		StartTime <- Sys.time()

		# Start resampling scheme 
		for(l in 1:nRSloops){
			# Sample rows from original data with 1/n probability (i.e. weight = NULL)
			SelectedData <- dplyr::sample_n(OrigData, size = K, replace = FALSE, weight = NULL)

			# Try to fit PIM. If fails, return coordinates. 
			PIMfit <- try(pim(formula = Y ~ X, data = SelectedData, 
			                  link = 'probit', model = 'difference'), silent = TRUE)
			if(class(PIMfit) == 'try-error'){
			  print(paste0('Error in sim ',i, '. Iteration ',l,'.'))
			  next
			}else{
			  # Save beta value if no error.
			  beta_loop <- c(beta_loop, PIMfit@coef)
			  # Save sandwich variance estimator
			  beta_sVar <- c(beta_sVar, PIMfit@vcov)
			}
		}
		# Recorded time 
		recordedTime[i] <- difftime(time1 = Sys.time(), time2 = StartTime, units = 'mins')
		
		# Collect estimated beta values and variance with info about nRSloops and K
		BetaValues <- bind_rows(BetaValues, 
		                data.frame(beta = beta_loop,
		                           sVariance = beta_sVar, 
		                           K = K,
		                           nRSloops = nRSloops,
		                           TrueBeta = trueBeta))
	}
	# Write results of beta and recorded time 
	write.table(BetaValues, file = paste(ResultsDir, '/uni_beta_vector_simID_',ID, '_SCEN_', SCEN, '.txt', sep = ''), row.names = FALSE, col.names = TRUE)
	write.table(recordedTime, file = paste(ResultsDir, '/uni_time_vector_simID_',ID, '_SCEN_', SCEN, '.txt', sep = ''), row.names = FALSE, col.names = TRUE)
}



##
##########
### Fitting a PIM: locally
##########
##

# If running locally, then start here 
if(MACHINE == "LOCAL"){
	# Start j loop over all (or some if testing code) simulations 
	for(j in 1:2){	
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
		# Estimated values: initialize with NULL 
		BetaValues <- data.frame(beta = NULL, sVariance = NULL, K = NULL, nRSloops = NULL, TrueBeta = NULL)

		# Progress in simulations  
		progress <- floor(seq(1, nPairs, length.out = 11)[-1])

		# Fit the model nPairs times
		for(i in 1:nPairs){
			# Check progress 
			if(i %in% progress) print(paste0('Doing simulation: ',ID[j],'. At ', which(i == progress, arr.ind = TRUE)*10, '%'))
			
			# Set new seed according to simulation ID and pair 
			set.seed(StartingSeed + (StartingSeed * ID[j]) + log(i))

			# Get number of resampling loops and number of selected datapoints 
			nRSloops <- pairs[i,'nRSloops']
			K <- pairs[i,'K']

			# Gather beta values and variance estimators inside resampling scheme 
			beta_loop <- beta_sVar <- c()

			# Start recording time
			StartTime <- Sys.time()

			# Start resampling scheme 
			for(l in 1:nRSloops){
				# Sample rows from original data with 1/n probability (i.e. weight = NULL)
				SelectedData <- dplyr::sample_n(OrigData, size = K, replace = FALSE, weight = NULL)

				# Try to fit PIM. If fails, return message. 
				PIMfit <- try(pim(formula = Y ~ X, data = SelectedData, 
									link = 'probit', model = 'difference'), silent = TRUE)
			  	if(class(PIMfit) == 'try-error'){
			  		print(paste0('Error in sim ',i, '. Iteration ',l,'. Message = ', attr(PIMfit,"condition")))
			  		next
			  	}else{
			  		# Save beta value if no error.
			  		beta_loop <- c(beta_loop, PIMfit@coef)
			  		# Save sandwich variance estimator
			  		beta_sVar <- c(beta_sVar, PIMfit@vcov)
			  	}
			}
			# Recorded time 
			recordedTime[i] <- difftime(time1 = Sys.time(), time2 = StartTime, units = 'mins')

			# Collect estimated beta values and variance with info about nRSloops and K
			BetaValues <- bind_rows(BetaValues, 
							data.frame(beta = beta_loop,
							  sVariance = beta_sVar, 
							  K = K,
							  nRSloops = nRSloops,
							  TrueBeta = trueBeta))
		}

		# Write results of beta and recorded time 
		write.table(BetaValues, file = paste(ResultsDir, '/uni_beta_vector_local_simID_',ID[j], '_SCEN_', SCEN, '.txt', sep = ''), row.names = FALSE, col.names = TRUE)
		write.table(recordedTime, file = paste(ResultsDir, '/uni_time_vector_local_simID_',ID[j], '_SCEN_', SCEN, '.txt', sep = ''), row.names = FALSE, col.names = TRUE)

	}
}

