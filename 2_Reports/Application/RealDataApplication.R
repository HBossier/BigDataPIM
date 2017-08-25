####################
#### TITLE: Real data application: effect of digital screen usage on mental well being (Przybylski et al., 2017)
#### Contents:  
####
#### Source Files: //Mastat/Thesis
#### First Modified: 25/08/2017
#### Notes:
#################


##
##########
### Notes
##########
##


# Reset working memory
rm(list = ls())

# Working directory
OS <- Sys.info()['machine']
if(OS == 'x86_64'){
  machine <- 'MAC'
  wd <- '/Users/hanbossier/Dropbox/Mastat/Thesis/BigDataPIM'
}else{
  machine <- 'WINDOWS'
  wd <- 'D:/Users/Han/Dropbox/Mastat/Thesis/BigDataPIM'
}
setwd(wd)

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(pim)
library(parallel)
library(broom)



##
###############
### Exploring data
###############
##

# Read in the dataset
dataWB <- read.csv(file = paste(wd, '/OSF_Przybylski2017/data.csv', sep = ''))

# Summary
head(dataWB)
tail(dataWB)
dim(dataWB)
summary(dataWB)


# There are some NA values and variables that we do not need at the moment.
# Let us keep:  # mwbi: mental well being with imputed values (DV)
# sp_wd: smartphone used during weekdays (IV)
# sp_we: smartphone used during weekends (IV)
filterNA <- dataWB %>% 
  select(mwbi, sp_wd, sp_we) %>%
  filter(complete.cases(.))   %>% tbl_df()

dim(filterNA)
head(filterNA)
summary(filterNA)

hist(filterNA$mwbi)
qqnorm(filterNA$mwbi)

# xy plot of mental well being to the average smartphone usage during weekdays
aggregate(mwbi ~ sp_wd, data = filterNA, FUN = mean)
filterNA %>% group_by(sp_wd) %>% summarize(mean_mwb = mean(mwbi)) %>%
  ggplot(aes(x = sp_wd, y = mean_mwb)) + geom_line()

# Quick check to see if aggregating with respect to weekday is correct (intermediate step of only selecting weekdays)
filterNA %>% select(sp_wd, mwbi) %>% group_by(sp_wd) %>% summarize(mean_mwb = mean(mwbi)) %>%
  ggplot(aes(x = sp_wd, y = mean_mwb)) + geom_line()

# Separate lines for weekdays and weekends
gather(filterNA, Day, Hours, sp_wd:sp_we) %>% group_by(Day, Hours) %>% 
  summarize(mean_mwb = mean(mwbi), CI_mwb = 1.96 * (sd(mwbi)/sqrt(dim(.)[1]/2))) %>%
  ggplot(aes(x = Hours, y = mean_mwb, colour = Day)) + geom_line(size = 1.2) +
  geom_linerange(aes(ymin=mean_mwb-CI_mwb, ymax=mean_mwb+CI_mwb), size = 1.4) + 
  scale_x_continuous(name = 'Hours spent using smartphone (self-reported)',
                     breaks = c(0:7)) +
  scale_y_continuous(name = 'Average mental well being (95% CI)') +
  scale_colour_manual(values = c('#d8b365', '#5ab4ac'), labels = c('Weekdays', 'Weekend'))


##
###############
### Replicating results
###############
##

# Results for smartphone usage in paper (and using SPSS), mind rounding.
SPSS_smartphone <- 
  dataWB %>% select(mwbi, sp_wd, sp_wd_sq) %>% 
  filter(complete.cases(.)) %>% 
  lm(mwbi ~ sp_wd + sp_wd_sq, data = .)
summary(SPSS_smartphone);confint(SPSS_smartphone)

# The authors of the paper recoded the variable sp_wd (amount of hours spent with smartphone) from the used Likert scale to the exact amount of hours.
# E.g. Likert = 1 equals 0 hours spent.
# However, for the squared variable, they did not use the recoded variables for some reason.
# This is the analysis with the squared, recoded variable.
SPSS_smartphone_sq <- 
  dataWB %>% select(mwbi, sp_wd) %>% 
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>% 
  lm(mwbi ~ sp_wd + sp_wd_sq, data = .)
summary(SPSS_smartphone_sq);confint(SPSS_smartphone_sq)
plot(SPSS_smartphone_sq)

# Weekend: quadratic regression based on original variable
dataWB %>% filter(complete.cases(.)) %>%
  select(mwbi, sp_we, sp_we_sq) %>%
  lm(mwbi ~ sp_we + sp_we_sq, data = .) %>% step(direction = "forward") %>%
  summary(.)

# Weekend: quadratic regression based on recoded variable
dataWB %>% filter(complete.cases(.)) %>%
  select(mwbi, sp_we) %>% mutate(sp_we_sq = sp_we**2) %>% 
  lm(mwbi ~ sp_we + sp_we_sq, data = .) %>% step(direction = "forward") %>%
  summary(.)

# Next, as we are using a quadratic term, it is sometimes better to center the predictor first
# see http://www.ats.ucla.edu/stat/mult_pkg/faq/general/curves.htm
dataWB %>% filter(complete.cases(.)) %>%
  select(mwbi, sp_we) %>% mutate(sp_we_c = sp_we - mean(sp_we)) %>% 
  mutate(sp_we_sq_c = sp_we_c**2) %>% 
  lm(mwbi ~ sp_we_c + sp_we_sq_c, data = .)  %>%
  summary(.)
# As expected, we do find a linear trend now!

# Add control variables
dataWB %>% select(mwbi, sp_wd, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>% 
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .) %>%
  summary(.)


##
###############
### Getting parameters for simulation study
###############
##

# Watching movies
dataWB %>% select(mwbi, watch_we, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  mutate(watch_we_sq = watch_we**2) %>% 
  lm(mwbi ~ watch_we + watch_we_sq + male + minority + deprived, data = .) %>%
  summary(.)

# Smartphone screen usage: without quadratic trend
dataWB %>% select(mwbi, sp_wd, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>% 
  lm(mwbi ~ sp_wd + male + minority + deprived, data = .) %>%
  summary(.)

# Spread of data
sd(dataWB$mwbi, na.rm = TRUE)
hist(dataWB$mwbi)

# proportion of gender, minority and deprived
summary(dataWB$male)
summary(dataWB$deprived)
summary(dataWB$minority)


##
###############
### PIM: single data partitioning
###############
##


# Let us use the single data partitioning algorithm
# We can then use parallel version over all available cores
# Record time to estimate model!
# Smartphone screen usage: without quadratic trend
# Select the complete cases
complMWB <- dataWB %>% 
  select(mwbi, sp_wd, male, minority, deprived) %>%
  filter(complete.cases(.)) %>% tbl_df()

# Amount of observations
n <- dim(complMWB)[1]

# Length of partitions (min 1000) and hence amount of partitions
sizePart <- 1000
S <- ceiling(n/sizePart)

# First shuffle data (safety)
set.seed(159)
NewOrder <- sample(x = 1:n, size = n, replace = FALSE)
ShufCompMWB <- complMWB %>% slice(NewOrder)

# Now construct function which we can use over different cores
# sID is the ID of partition S, sizePart is the size of the partitions, compl_data is the original dataset
SDPfunction <- function(sID, sizePart, compl_data){
  Part_est <- data.frame() %>% tbl_df()
  start <- sID + (sID - 1) * sizePart
  end <- start + sizePart
  # Slice automatically stops when end < than n! ==> handy here
  subsetData <- compl_data %>% slice(start:end)
  # Try to fit PIM. If fails, return message. 
  PIMfit <- try(pim(formula = mwbi ~ sp_wd + male + minority + deprived, data = subsetData,
                        link = 'probit', model = 'difference'), silent = TRUE)
  if(class(PIMfit)[1] == 'try-error'){
      print(paste0('Error in partition ',sID, '. Message = ', attr(PIMfit,"condition")))
    }else{
      # Names of the objects (if no error)
      namesObject <- tidy(PIMfit@coef) %>% data.table::transpose() %>% slice(.,1)
      
      # Save beta value if no error.
      PIMvalues <- tidy(PIMfit@coef) %>% data.table::transpose() %>% slice(.,2)
      names(PIMvalues) <- namesObject
      # Save sandwich variance estimators (only variance on diagonal)
      PIMvariances <- tidy(diag(PIMfit@vcov)) %>% data.table::transpose() %>% slice(2)
      names(PIMvariances) <- paste(namesObject, '_Svar', sep = '')
      
      # Now bind cols of estimates and variances
      Part_est <- bind_cols(PIMvalues, PIMvariances)
    }
  return(Part_est)
}

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Now run the SDPfunction (single data partitioning)
StartTime <- Sys.time()
SDP_estimate_results <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = compl_data)
SDP_estimates <- do.call(rbind, SDP_estimate_results)
RecordedTime <- difftime(time1 = Sys.time(), time2 = StartTime, units = 'mins');RecordedTime
stopCluster(cl)

# Why is it character?
mean(as.numeric(SDP_estimates$sp_wd))
