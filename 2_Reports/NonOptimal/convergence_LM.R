####################
#### TITLE:   Rate of convergence for OLS - uniform subsampling
#### Contents:  
####
#### Source Files: //Mastat/Thesis
#### First Modified: 01/09/2017
#### Notes:
#################


##
##########
### Notes
##########
##


##
##########
### Preparation
##########
##

# libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(RColorBrewer)
library(boot)
library(parallel)


# Extra function (obtained from https://github.com/dewittpe/qwraps2/blob/master/R/lazyload_cache.R) to load in cached objects
lazyload_cache_labels <- function(labels, path = "./cache/", envir = parent.frame(), verbose = TRUE, filter, full.names = TRUE, ...) {
  files <- do.call(list.files, list(path = path, pattern = paste0("^(", paste(labels, collapse = "|"), ")_[0-9a-f]{32}\\.rdx$"), full.names = full.names, ...))
  files <- gsub("\\.rdx$", "", files)
  
  lfound <- sapply(lapply(labels, grepl, x = files), any)
  
  if (!all(lfound)) {
    files <- do.call(list.files, list(path = path, pattern = "_[0-9a-f]{32}\\.rdx$", full.names = FALSE, ...))
    files <- gsub("_[0-9a-f]{32}\\.rdx$", "", files)
    message(paste0("label(s)\n", paste(paste0("  ", labels[!lfound]), collapse = "\n"), "\nnot found in path '", path, "\n\n",
                   "Available labels:\n", paste(paste0("  ", files), collapse = "\n")))
    warning("Nothing loaded", call. = FALSE)
  } else {
    
    if (!verbose) {
      sapply(files, lazyLoad, envir = envir, filter = filter)
    } else {
      sapply(files, 
             function(x, envir, filter) { 
               message(paste("Lazyloading", x))
               lazyLoad(x, envir = envir, filter = filter) },
             envir = envir, 
             filter = filter)
    } 
  }
  invisible()
}

# Location of data
datLoc <- "/Volumes/1_5_TB_Han_HDD/Mastat/Thesis/BigDataPIM/NonOptimal/LMwrepl/results/"

# number of simulations 
nsim <- 1000

# Confidence level (for CI section)
level <- 0.95

# Sample size
n <- 250000

# Vector of number of resampling loops
nRSloops_vec <- floor(seq(10,1000,length.out = 10))[10]

# Vector of number of selected datapoints K per iteraton 
K_vec <- floor(seq(10,2500,length.out = 25))

# Number of pairs/combinations between number of resampling loops and sampled data 
pairs <- expand.grid(B = nRSloops_vec, K = K_vec)

# Scenario parameters
alpha_1 <- -0.43

# Sigma based on dataset
sigma <- 9.51
trueBeta <- alpha_1/(sqrt(2) * sigma)

# Model (called SCEN in script)
SCEN <- 3

##
##########
### Load in data
##########
##

# Data frame with all values over 1000 simulations
valuesAllSim <- timeAllSim <- data.frame() %>% tbl_df()

# Helper variables
progress <- floor(seq(1,nsim,length.out = 11)[-1])
S3_colnames <- c("beta..Intercept.", "beta.X_smartph_hrs",
                 "beta.X_sex", "beta.X_econArea", "beta.X_ethnic",
                 "se..Intercept._se", "se.X_smartph_hrs_se",
                 "se.X_sex_se", "se.X_econArea_se", "se.X_ethnic_se",
                 "K", "nRSloops", "TrueAlhpa")
LM_scaledSD <- data.frame() %>% tbl_df()

# Read in data
for(i in 1:nsim){
  ValSim <- read.table(file = paste0(datLoc, 'LM_uni_beta_vector_simID_', i, '_SCEN_', SCEN, '.txt'),
                       col.names = S3_colnames, header = TRUE) %>% 
    tbl_df()
  ValSim <- rename(ValSim, alpha = beta.X_smartph_hrs, TrueAlpha = TrueAlhpa)
  # CI using SD of beta with scaling
  sdScale_tmp <- ValSim %>% group_by(K, TrueAlpha) %>% 
    summarise(AvgAlpha = mean(alpha, na.rm = TRUE),  
              sdAlpha = sd(alpha, na.rm = TRUE)) %>%
    mutate(CIlow =  AvgAlpha - (qnorm(0.025, lower.tail = FALSE) * sdAlpha  * sqrt(K/n)),
           CIup = AvgAlpha + (qnorm(0.025, lower.tail = FALSE) * sdAlpha * sqrt(K/n)),
           type = 'scaledSD') %>% 
    ungroup() %>% rowwise() %>% 
    mutate(coverage_ind = ifelse(TrueAlpha >= CIlow && TrueAlpha <= CIup, 1, 0),
           sim = i)
  
  # Collect the values over all simulations
  LM_scaledSD <- bind_rows(LM_scaledSD,sdScale_tmp)
}


##
##########
### Plot CI EC
##########
##

ToPlotCI <- LM_scaledSD %>%  filter(K %in% c(217, 943, 2500)) %>% 
  group_by(K) %>%
  slice(seq(1, nsim, length.out = 100)) %>% ungroup()
ToPlotCI$K <- factor(ToPlotCI$K, levels = c(217, 943, 2500), labels = c('K = 217', 'K = 943', 'K = 2500'))

# Random CIs
ggplot(ToPlotCI, aes(x = TrueAlpha)) + geom_vline(aes(xintercept = TrueAlpha), size = .8, alpha = .8) +
  geom_point(aes(x = AvgAlpha, y = sim, colour = factor(coverage_ind)), size = 0.5) + 
  geom_segment(aes(x = CIlow, xend = CIup, y = sim, yend = sim, colour = factor(coverage_ind), alpha = factor(coverage_ind)), size = 0.9) + 
  facet_wrap(~ K) +
  scale_alpha_manual("Contains true value", values = c(1,0.8), labels = c("NO", "YES")) +
  scale_colour_manual("Contains true value", values = c('#d95f02', '#1b9e77'), labels = c("NO", "YES")) +
  scale_x_continuous("alpha") + scale_y_continuous("simulation") +
  theme(legend.position="bottom") +
  ggtitle("100 random selected simulations with the 95% CI")

# Plot with coverage 
ToPlotEC_LM <- LM_scaledSD %>% group_by(K) %>% summarise(EC = mean(coverage_ind)) %>%
                  mutate(scale = round(sqrt(K/n), 4), 
                         ID = row_number(),
                         type = 'LM')
ggplot(ToPlotEC_LM, aes(x = scale, y = EC)) + geom_point() + geom_line() +
  theme_bw()

# Let us add PIM estimator for scenario 3
UniCache = as.character('/Users/hanbossier/Dropbox/Mastat/Thesis/RCodePIM/2_Reports/NonOptimal/NonOptimal_cache/html')

# CI coverage: scaled SE
lazyload_cache_labels(labels = paste0('calculate-m-out-of-n-CI-model-', SCEN), path = UniCache)
ToPlotEC_PIM <- scaledSD %>% filter(nRSloops == 1000) %>% group_by(K) %>% summarise(EC = mean(coverage_ind)) %>%
  mutate(scale = round(sqrt(K/n), 4), 
         ID = row_number(),
         type = 'PIM')

ToPlotEC <- bind_rows(ToPlotEC_LM, ToPlotEC_PIM)

# Plot
ToPlotEC <- rename(ToPlotEC, Type = type)
ToPlotEC$Type <- factor(ToPlotEC$Type, levels = c('LM', 'PIM'), labels = c('OLS', 'PIM'))
ggplot(ToPlotEC, aes(x = scale, y = EC, group = Type)) + 
  geom_point(aes(colour = Type), size = .7) + 
  geom_line(aes(colour = Type), size = 0.8) +
  scale_x_continuous('Scaling factor: sqrt(K/n)') + 
  geom_hline(yintercept = 0.95) +
  scale_y_continuous('Empirical coverage of the 95% CI', breaks = c(0,0.25,0.5,0.75,0.95)) +
  ggtitle('Increasing K from 10 to 2500 in uniform subsampling',
          subtitle = 'n = 250,000') +
  theme_bw()





