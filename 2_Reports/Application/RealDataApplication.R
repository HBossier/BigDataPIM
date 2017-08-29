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

# Read in the dataset
dataWB <- read.csv(file = paste(wd, '/OSF_Przybylski2017/data.csv', sep = ''))


##
###############
### Exploring data
###############
##

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

# Different fitted lines (mean, lm, gam)
filterNA %>% ggplot(aes(x = sp_wd, y = mwbi)) + 
  geom_jitter(height = 0.5, width = 0.5, alpha = .05, size = .2) +
  scale_x_continuous("Hours", breaks = c(0,0.5,1,2,3,4,5,6,7)) +
  scale_y_continuous("Mental well-being") +
  stat_summary(fun.y = mean,geom="line", colour = '#b3e2cd', size = .7) +
  geom_smooth(method = 'lm', colour = '#fdcdac', size = .7) +
  stat_smooth(method = "gam", 
    formula = y ~ s(x, bs = "cs", k = 3), col = "purple", size = .8) +
  theme_bw()

# Fit with control variables
fitLM <- dataWB %>% select(mwbi, sp_wd, male, minority, deprived) %>% 
    filter(complete.cases(.)) %>%
    mutate(sp_wd_sq = sp_wd**2) %>%
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .)
dataWB %>% select(mwbi, sp_wd, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>%
  ggplot(aes(x = sp_wd, y = mwbi)) + 
  geom_jitter(height = 0.5, width = 0.5, alpha = .05, size = .2) +
  scale_x_continuous("Hours", breaks = c(0,0.5,1,2,3,4,5,6,7)) +
  scale_y_continuous("Mental well-being") +
  geom_abline(intercept = fitLM$coefficients[1], 
              slope = fitLM$coefficients[2],
              colour = '#fdcdac', size = .7)


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
# see http://www.ats.ucla.edu/stat/mult_pkg/faq/general/curves.html
# new url: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-the-sign-of-the-quadratic-term-in-a-polynomial-regression/
dataWB %>% filter(complete.cases(.)) %>%
  select(mwbi, sp_we) %>% mutate(sp_we_c = sp_we - mean(sp_we)) %>% 
  mutate(sp_we_sq_c = sp_we_c**2) %>% 
  lm(mwbi ~ sp_we_c + sp_we_sq_c, data = .)  %>%
  summary(.)
# As expected, we do find a linear downwards trend now, when x = 0!

# Add control variables
dataWB %>% select(mwbi, sp_wd, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>% 
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .) %>%
  summary(.)

# Same results as paper:
coe <- dataWB %>% select(mwbi, sp_wd, sp_wd_sq, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .) %>%
  step(direction = "forward") %>% coefficients(.)
  summary(.)

# Small plot with the fitted line
pl <- coe[1] + c(0,0.5,1:7) * coe[2] + c(0,0.5,1:7)**2 * coe[3]
plot(x = c(0,0.5,1:7), y = pl, type = 'l')
  
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




##########################################################################################
##########################################################################################
################################## MAIN TEXT ANALYSES ####################################
##########################################################################################
##########################################################################################





# Smartphone screen usage: without quadratic trend
# Select the complete cases
complMWB <- dataWB %>% 
  select(mwbi, sp_wd, male, minority, deprived) %>%
  filter(complete.cases(.)) %>% tbl_df()

# Summary
summary(complMWB)
sd(complMWB$mwbi)

# Amount of observations
n <- dim(complMWB)[1]


##
###############
### Plots for main text
###############
##

# Could use segment with start and enpoints and then plot x as factor.
# However, this distorts the image (it is a lm, not an anova).
yStart <- coef(lm(mwbi ~ sp_wd, data = complMWB))[1]
yEnd <- coef(lm(mwbi ~ sp_wd, data = complMWB))[1] + 7*coef(lm(mwbi ~ sp_wd, data = complMWB))[2]
scatter <- complMWB %>% 
  ggplot(aes(x = sp_wd, y = mwbi)) + 
  geom_jitter(height = 0.5, width = 0.5, alpha = .05, size = .2) +
  scale_x_continuous("Hours spent using smartphone (self-reported)", breaks = c(0,0.5,1,2,3,4,5,6,7)) +
  scale_y_continuous("Mental well-being") +
  #geom_segment(aes(x = 1, xend = 9, y = yStart, yend = yEnd))
  geom_smooth(method = 'lm', colour = '#fdcdac', size = .7) +
  ggtitle("Scatter plot with y ~ x fitted linear regression") +
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 12))
scatter
Average <- gather(filterNA, Day, Hours, sp_wd:sp_we) %>% 
  group_by(Day, Hours) %>% 
  summarize(mean_mwb = mean(mwbi), CI_mwb = 1.96 * (sd(mwbi)/sqrt(dim(.)[1]/2))) %>%
  filter(Day == 'sp_wd') %>% filter(complete.cases(.)) %>%
  ggplot(aes(x = Hours, y = mean_mwb)) + geom_line(size = 1.2) +
  geom_linerange(aes(ymin=mean_mwb-CI_mwb, ymax=mean_mwb+CI_mwb), size = 1.4) + 
  scale_x_continuous(name = 'Hours spent using smartphone (self-reported)',
                     breaks = c(0:7)) +
  scale_y_continuous(name = 'Mental well being') +
  ggtitle("Average mental well being with 95% CI") + 
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 12))
Average
cowplot::plot_grid(scatter, Average, align = 'h')


MaleFemale <- complMWB %>% 
  group_by(male, sp_wd) %>% summarise(mean_mwbi = mean(mwbi)) %>%
  ggplot(., aes(x = sp_wd, y = mean_mwbi, group = factor(male))) + 
    geom_line(aes(colour = factor(male)), size = 1.1) +
    scale_color_manual('Gender', labels = c('Female', 'Male'), values = c('#1b9e77', '#d95f02')) +
    scale_x_continuous(name = 'Hours spent using smartphone (self-reported)',
                     breaks = c(0:7)) +
    scale_y_continuous(name = 'Mental well being') +
    ggtitle("Average mental well being - according to gender") + 
    theme_bw() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 12))
MaleFemale


##
###############
### Results OLS: main text
###############
##

# Linear regression without control variables
# Smartphone screen usage: without quadratic trend
fitLM_wc <- complMWB %>% 
  lm(mwbi ~ sp_wd, data = .) %>%
  summary(.)
fitLM_wc

# With control variables
fitLM_c <- complMWB %>% 
  lm(mwbi ~ sp_wd + male + minority + deprived, data = .) %>%
  summary(.)
fitLM_c

##
###############
### PIM: single data partitioning: linear trend
###############
##


# Let us use the single data partitioning algorithm
# We can then use parallel version over all available cores
# Record time to estimate model!

# Length of partitions (min 1000) and hence amount of partitions
sizePart <- 1000
S <- ceiling(n/sizePart)

# First shuffle data (safety)
set.seed(159)
NewOrder <- sample(x = 1:n, size = n, replace = FALSE)
ShufCompMWB <- complMWB %>% slice(NewOrder)

# Now construct function which we can use over different cores
# sID is the ID of partition S, sizePart is the size of the partitions, compl_data is the original dataset
SDPfunction <- function(sID, sizePart, compl_data, form){
  Part_est <- data.frame() %>% tbl_df()
  start <- sID + (sID - 1) * sizePart
  end <- start + sizePart
  # Slice automatically stops when end < than n! ==> handy here
  subsetData <- compl_data %>% slice(start:end)
  # Try to fit PIM. If fails, return message. 
  PIMfit <- try(pim(formula = form, data = subsetData,
                        link = 'logit', model = 'difference'), silent = TRUE)
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

###################################################
# WITHOUT COVARIATES
###################################################

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Construct the formula for PIM
form_woc <- formula("mwbi ~ sp_wd")

# Now run the SDPfunction (single data partitioning)
StartTime_woc <- Sys.time()
SDP_estimate_results_woc <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_woc)
SDP_estimates_woc <- do.call(bind_rows, SDP_estimate_results_woc)
SDP_estimates_woc <- data.frame(apply(SDP_estimates_woc, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_woc <- summarise(SDP_estimates_woc, PIMbeta = mean(sp_wd))
PIM_SE_est_woc <- SDP_estimates_woc %>% select(sp_wd_Svar) %>% summarise(sum_svar = sum(sp_wd_Svar)) %>%
  mutate(SE = sqrt((1/S)**2 * sum_svar)) %>% select(SE)
zValue_woc <- PIM_beta_est_woc/PIM_SE_est_woc
PVal_woc <- 2 * pnorm(q = as.numeric(abs(zValue_woc)), lower.tail = FALSE)

Shuffle_woc <- data.frame(PIM_beta_est_woc,
                            PIM_SE_est_woc,
                            zValue_woc,
                            PVal_woc)
colnames(Shuffle_woc) <- c('Beta', 'SE', 'Zval', 'Pval')
Shuffle_woc

# Time of fitting and combining estimates
RecordedTime_woc <- difftime(time1 = Sys.time(), time2 = StartTime_woc, units = 'mins');RecordedTime_woc

###################################################
# WITH COVARIATES
###################################################

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Construct the formula for PIM
form_c <- formula("mwbi ~ sp_wd + male + minority + deprived")

# Now run the SDPfunction (single data partitioning)
StartTime_c <- Sys.time()
SDP_estimate_results_c <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_c)
SDP_estimates_c <- do.call(bind_rows, SDP_estimate_results_c)
SDP_estimates_c <- data.frame(apply(SDP_estimates_c, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_c <- SDP_estimates_c %>% select(1:4) %>% summarise_all(mean)
PIM_SE_est_c <- SDP_estimates_c %>% select(5:8) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% select(RowN, SE)
zValue_c <- PIM_beta_est_c/PIM_SE_est_c$SE
PVal_c <- 2 * pnorm(q = as.numeric(abs(zValue_c)), lower.tail = FALSE)

Shuffle_c <- data.frame('Parameter' = names(PIM_beta_est_c),
                        'Beta' = t(PIM_beta_est_c),
                        'SE' = PIM_SE_est_c$SE,
                        'Zval' = t(zValue_c),
                        'Pval' = PVal_c)
Shuffle_c

# Time of fitting and combining estimates
RecordedTime_c <- difftime(time1 = Sys.time(), time2 = StartTime_c, units = 'mins');RecordedTime_c

##
###############
### PIM: re shuffle data
###############
##

# First shuffle data (safety)
set.seed(241)
NewOrder <- sample(x = 1:n, size = n, replace = FALSE)
ShufCompMWB <- complMWB %>% slice(NewOrder)

###################################################
# WITHOUT COVARIATES
###################################################

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Construct the formula for PIM
form_woc <- formula("mwbi ~ sp_wd")

# Now run the SDPfunction (single data partitioning)
StartTime_woc_RE <- Sys.time()
SDP_estimate_results_woc_RE <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_woc)
SDP_estimates_woc_RE <- do.call(bind_rows, SDP_estimate_results_woc_RE)
SDP_estimates_woc_RE <- data.frame(apply(SDP_estimates_woc_RE, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_woc_RE <- summarise(SDP_estimates_woc_RE, PIMbeta = mean(sp_wd))
PIM_SE_est_woc_RE <- SDP_estimates_woc_RE %>% select(sp_wd_Svar) %>% summarise(sum_svar = sum(sp_wd_Svar)) %>%
  mutate(SE = sqrt((1/S)**2 * sum_svar)) %>% select(SE)
zValue_woc_RE <- PIM_beta_est_woc_RE/PIM_SE_est_woc_RE
PVal_woc_RE <- 2 * pnorm(q = as.numeric(abs(zValue_woc_RE)), lower.tail = FALSE)

ReShuffle_woc <- data.frame(PIM_beta_est_woc_RE,
  PIM_SE_est_woc_RE,
  zValue_woc_RE,
  PVal_woc_RE)
colnames(ReShuffle_woc) <- c('Beta', 'SE', 'Zval', 'Pval')
ReShuffle_woc
Shuffle_woc

# Time of fitting and combining estimates
RecordedTime_woc_RE <- difftime(time1 = Sys.time(), time2 = StartTime_woc_RE, units = 'mins');RecordedTime_woc_RE

###################################################
# WITH COVARIATES
###################################################

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Construct the formula for PIM
form_c <- formula("mwbi ~ sp_wd + male + minority + deprived")

# Now run the SDPfunction (single data partitioning)
StartTime_c_RE <- Sys.time()
SDP_estimate_results_c_RE <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_c)
SDP_estimates_c_RE <- do.call(bind_rows, SDP_estimate_results_c_RE)
SDP_estimates_c_RE <- data.frame(apply(SDP_estimates_c_RE, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_c_RE <- SDP_estimates_c_RE %>% select(1:4) %>% summarise_all(mean)
PIM_SE_est_c_RE <- SDP_estimates_c_RE %>% select(5:8) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% select(RowN, SE)
zValue_c_RE <- PIM_beta_est_c_RE/PIM_SE_est_c_RE$SE
PVal_c_RE <- 2 * pnorm(q = as.numeric(abs(zValue_c_RE)), lower.tail = FALSE)

ReShuffle_c <- data.frame('Parameter' = names(PIM_beta_est_c_RE),
                        'Beta' = t(PIM_beta_est_c_RE),
                        'SE' = PIM_SE_est_c_RE$SE,
                        'Zval' = t(zValue_c_RE),
                        'Pval' = PVal_c_RE)
ReShuffle_c
Shuffle_c

# Time of fitting and combining estimates
RecordedTime_c_RE <- difftime(time1 = Sys.time(), time2 = StartTime_c_RE, units = 'mins');RecordedTime_c_RE




################################################
################ MARGINAL MODEL ################ 
################################################


##
###############
### PIM: single data partitioning: marginal model
###############
##


# Let us use the single data partitioning algorithm
# We can then use parallel version over all available cores
# Record time to estimate model!

# Length of partitions: much less than before due to N**2 comparisons
sizePart <- 250
S <- ceiling(n/sizePart)

# First shuffle data (safety)
set.seed(159)
NewOrder <- sample(x = 1:n, size = n, replace = FALSE)
ShufCompMWB <- complMWB %>% slice(NewOrder)
ShufCompMWB$sp_wdF <- factor(ShufCompMWB$sp_wd, levels = c(0,0.5,1:7), labels = c(1:9))

# Now construct function which we can use over different cores: marginal model here!
# sID is the ID of partition S, sizePart is the size of the partitions, compl_data is the original dataset
Marginal_SDPfunction <- function(sID, sizePart, compl_data, form){
  Part_est <- data.frame() %>% tbl_df()
  start <- sID + (sID - 1) * sizePart
  end <- start + sizePart
  # Slice automatically stops when end < than n! ==> handy here
  subsetData <- compl_data %>% slice(start:end)
  # Try to fit PIM. If fails, return message. 
  PIMfit <- try(pim(formula = form, data = subsetData,
                 link = 'identity', compare = 'all', vcov.estim = score.vcov), silent = TRUE)
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

###################################################
# WITHOUT COVARIATES
###################################################

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Construct the formula for PIM
form_woc <- formula("mwbi ~ R(sp_wdF) - 1")

# Now run the Marginal_SDPfunction (single data partitioning - marginal version)
StartTime_wocM <- Sys.time()
SDP_estimate_results_wocM <- clusterApply(cl, 1:(S-2), fun = Marginal_SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_woc)
SDP_estimates_wocM <- do.call(bind_rows, SDP_estimate_results_wocM)
SDP_estimates_wocM <- data.frame(apply(SDP_estimates_wocM, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_wocM <- SDP_estimates_wocM %>% select(1:9) %>% summarise_all(mean)
PIM_SE_est_wocM <- SDP_estimates_wocM %>% select(10:18) %>% summarise_all(sum) %>% t(.) %>% 
                      data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% select(RowN, SE)
zValue_wocM <- PIM_beta_est_wocM/PIM_SE_est_wocM$SE
PVal_wocM <- 2 * pnorm(q = as.numeric(abs(zValue_wocM)), lower.tail = FALSE)
Shuffle_wocM <- data.frame('Parameter' = names(PIM_beta_est_wocM),
                          'Beta' = t(PIM_beta_est_wocM),
                          'SE' = PIM_SE_est_wocM$SE,
                          'Zval' = t(zValue_wocM),
                          'Pval' = PVal_wocM)
rownames(Shuffle_wocM) <- 1:9
Shuffle_wocM

# Time of fitting and combining estimates
RecordedTime_wocM <- difftime(time1 = Sys.time(), time2 = StartTime_wocM, units = 'mins');RecordedTime_wocM

# Plot of probability and 95% CI
Shuffle_wocM %>% select(Beta, SE) %>% mutate(Group = factor(c(0,0.5,1,2,3,4,5,6,7))) %>%
  mutate(CIlow = Beta - qnorm(0.025, lower.tail = FALSE) * SE,
         CIup = Beta + qnorm(0.025, lower.tail = FALSE) * SE) %>%
  mutate(Perc = exp(Beta) / (1 + exp(Beta)))








################################################
################ PAIRWISE MODEL ################ 
################################################


##
###############
### PIM: single data partitioning: pairwise model
###############
##


# Let us use the single data partitioning algorithm
# We can then use parallel version over all available cores
# Record time to estimate model!

# Length of partitions: much less than before due to N**2 comparisons
sizePart <- 1000
S <- ceiling(n/sizePart)

# First shuffle data (safety)
set.seed(159)
NewOrder <- sample(x = 1:n, size = n, replace = FALSE)
ShufCompMWB <- complMWB %>% slice(NewOrder)
ShufCompMWB$sp_wdF <- factor(ShufCompMWB$sp_wd, levels = c(0,0.5,1:7), labels = c(1:9))
ShufCompMWB <- mutate(.data = ShufCompMWB, GroupID = paste('G', sp_wdF, sep = ''))

# Now construct function which we can use over different cores: marginal model here!
# sID is the ID of partition S, sizePart is the size of the partitions, compl_data is the original dataset
Pairwise_SDPfunction <- function(sID, sizePart, compl_data, form, IndForm){
  Part_est <- data.frame() %>% tbl_df()
  start <- sID + (sID - 1) * sizePart
  end <- start + sizePart
  # Slice automatically stops when end < than n! ==> handy here
  subsetData <- compl_data %>% slice(start:end)
  # Expand with indicator variables for the pairwise comparisons
  subsetData <- data.frame(mwbi = subsetData$mwbi, model.matrix(IndForm, data=subsetData))
  # Rename columns
  ToRename <- dim(subsetData)[2]
  colnames(subsetData)[c(ToRename - 8):ToRename] <- paste('G', 1:9, sep = '')
  # Try to fit PIM. If fails, return message. 
  PIMfit <- try(pim(formula = form, data = subsetData,
                    compare = 'unique', model = "customized"), silent = TRUE)
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

###################################################
# WITHOUT COVARIATES
###################################################

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Construct the custom formula for PIM
IndForm_woc <- formula("mwbi ~ GroupID - 1")
form_woc <- formula("PO(L(mwbi), R(mwbi)) ~ I(R(G9) * L(G1)) + I(R(G8) * L(G1)) + I(R(G7) * L(G1)) +
    I(R(G6) * L(G1)) + I(R(G5) * L(G1)) + I(R(G4) * L(G1)) + I(R(G3) * L(G1)) +
                    I(R(G2) * L(G1))")

# Now run the Pairwise_SDPfunction (single data partitioning)
StartTime_wocP <- Sys.time()
SDP_estimate_results_wocP <- clusterApply(cl, 1:(S-1), fun = Pairwise_SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_woc, IndForm = IndForm_woc)
SDP_estimates_wocP <- do.call(bind_rows, SDP_estimate_results_wocP)
SDP_estimates_wocP <- data.frame(apply(SDP_estimates_wocP, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_wocP <- SDP_estimates_wocP %>% select(1:8) %>% summarise_all(mean)
PIM_SE_est_wocP <- SDP_estimates_wocP %>% select(9:16) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% select(RowN, SE)
zValue_wocP <- PIM_beta_est_wocP/PIM_SE_est_wocP$SE
PVal_wocP <- 2 * pnorm(q = as.numeric(abs(zValue_wocP)), lower.tail = FALSE)
NAMEcontrasts <- c('0 < 7', '0 < 6', '0 < 5', '0 < 4', '0 < 3', '0 < 2', '0 < 1', '0 < 0.5')
Shuffle_wocP <- data.frame('Parameter' = NAMEcontrasts,
                           'Beta' = t(PIM_beta_est_wocP),
                           'SE' = PIM_SE_est_wocP$SE,
                           'Zval' = t(zValue_wocP),
                           'Pval' = PVal_wocP)
rownames(Shuffle_wocP) <- 1:8
Shuffle_wocP

# Time of fitting and combining estimates
RecordedTime_wocP <- difftime(time1 = Sys.time(), time2 = StartTime_wocP, units = 'mins');RecordedTime_wocP


###################################################
# WITH COVARIATES
###################################################

# Detect and start the workers
P <- detectCores(logical = FALSE) # physical cores
cl <- makeCluster(P)

# Initialize them with the data set and load libraries
clusterExport(cl, "ShufCompMWB")
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(pim))
clusterEvalQ(cl, library(nleqslv))
clusterEvalQ(cl, library(broom))

# Construct the custom formula for PIM
IndForm_c <- formula("mwbi ~ male + minority + deprived + GroupID - 1")
form_c <- formula("PO(L(mwbi), R(mwbi)) ~ I(R(G9) * L(G1)) + I(R(G8) * L(G1)) + I(R(G7) * L(G1)) +
                    I(R(G6) * L(G1)) + I(R(G5) * L(G1)) + I(R(G4) * L(G1)) + I(R(G3) * L(G1)) +
                    I(R(G2) * L(G1)) + I(R(male) * L(male)) + I(R(minority) * L(minority)) + I(R(deprived) * L(deprived))")

# Now run the Pairwise_SDPfunction (single data partitioning)
StartTime_cP <- Sys.time()
SDP_estimate_results_cP <- clusterApply(cl, 1:(S-1), fun = Pairwise_SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_c, IndForm = IndForm_c)
SDP_estimates_cP <- do.call(bind_rows, SDP_estimate_results_cP)
SDP_estimates_cP <- data.frame(apply(SDP_estimates_cP, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_cP <- SDP_estimates_cP %>% select(1:11) %>% summarise_all(mean)
PIM_SE_est_cP <- SDP_estimates_cP %>% select(12:22) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% select(RowN, SE)
zValue_cP <- PIM_beta_est_cP/PIM_SE_est_cP$SE
PVal_cP <- 2 * pnorm(q = as.numeric(abs(zValue_cP)), lower.tail = FALSE)
NAMEcontrasts <- c('0 < 7', '0 < 6', '0 < 5', '0 < 4', '0 < 3', '0 < 2', '0 < 1', '0 < 0.5', 'gender', 'deprived', 'minority')
Shuffle_cP <- data.frame('Parameter' = NAMEcontrasts,
                           'Beta' = t(PIM_beta_est_cP),
                           'SE' = PIM_SE_est_cP$SE,
                           'Zval' = t(zValue_cP),
                           'Pval' = PVal_cP)
rownames(Shuffle_cP) <- 1:11
Shuffle_cP

# Time of fitting and combining estimates
RecordedTime_cP <- difftime(time1 = Sys.time(), time2 = StartTime_cP, units = 'mins');RecordedTime_cP

# Plot of the probabilities
extrRow <- data.frame(Parameter = '0 = 0', Beta = 0, SE = 0,
                      Zval = 0, Pval = 0, 
                      row = 0, CIlow = 0,
                      CIup = 0, ProbBeta = 0.5,
                      ProbCIlow = 0, ProbCIup = 0)
LabLX <- c('0h\n---\n0h*','0h\n---\n0.5h*', '0h\n---\n1h*', '0h\n---\n2h*', '0h\n---\n3h*', '0h\n---\n4h*', '0h\n---\n5h*', '0h\n---\n6h*', '0h\n---\n7h*')
PlotProb <- Shuffle_cP %>% filter(!Parameter %in% c('gender', 'deprived', 'minority')) %>% 
  mutate(row = row_number(),
    CIup = Beta - (qnorm(0.025, lower.tail = TRUE) * SE),
    CIlow = Beta + (qnorm(0.025, lower.tail = TRUE) * SE),
    ProbBeta = exp(Beta) / (1 + exp(Beta)),
    ProbCIlow = exp(CIlow) / (1 + exp(CIlow)),
    ProbCIup = exp(CIup) / (1 + exp(CIup))) %>%
    arrange(-row_number()) %>% mutate(row = row_number()) %>% 
    bind_rows(extrRow, .)

PlotProb %>% ggplot(aes(x = row, y = ProbBeta)) + geom_line(size = 1.2) +
  geom_point() + 
  geom_segment(aes(x = row, xend = row, y = ProbCIlow, yend = ProbCIup)) +
  scale_y_continuous("P[MWBI <= MWBI*]", limits = c(min(PlotProb$ProbBeta) - 0.009, max(PlotProb$ProbBeta) + 0.009)) + 
  scale_x_continuous("Comparison", breaks = 0:8, labels = LabLX) +
  ggtitle('Pairwise probabilities: 0 versus X hours') +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 0, hjust = 0, size = 9),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.position = "top",
        legend.margin=margin(t = -0.2, unit='cm'),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
#width: 18,38 cm
#height: 14,29 cm


