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
library(xtable)

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
  dplyr::select(mwbi, sp_wd, sp_we) %>%
  filter(complete.cases(.)) %>% tbl_df()

dim(filterNA)
head(filterNA)
summary(filterNA)

hist(filterNA$mwbi)
qqnorm(filterNA$mwbi)

# xy plot of mental well being to the average smartphone usage during weekdays
aggregate(mwbi ~ sp_wd, data = filterNA, FUN = mean)
filterNA %>% group_by(sp_wd) %>% summarize(mean_mwb = mean(mwbi)) %>%
  ggplot(aes(x = sp_wd, y = mean_mwb)) + geom_line()

# Quick check to see if aggregating with respect to weekday is correct (intermediate step of only dplyr::selecting weekdays)
filterNA %>% dplyr::select(sp_wd, mwbi) %>% group_by(sp_wd) %>% summarize(mean_mwb = mean(mwbi)) %>%
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
fitLM <- dataWB %>% dplyr::select(mwbi, sp_wd, male, minority, deprived) %>% 
    filter(complete.cases(.)) %>%
    mutate(sp_wd_sq = sp_wd**2) %>%
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .)
dataWB %>% dplyr::select(mwbi, sp_wd, male, minority, deprived) %>% 
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
  dataWB %>% dplyr::select(mwbi, sp_wd, sp_wd_sq) %>% 
  filter(complete.cases(.)) %>% 
  lm(mwbi ~ sp_wd + sp_wd_sq, data = .)
summary(SPSS_smartphone);confint(SPSS_smartphone)

# The authors of the paper recoded the variable sp_wd (amount of hours spent with smartphone) from the used Likert scale to the exact amount of hours.
# E.g. Likert = 1 equals 0 hours spent.
# However, for the squared variable, they did not use the recoded variables for some reason.
# This is the analysis with the squared, recoded variable.
SPSS_smartphone_sq <- 
  dataWB %>% dplyr::select(mwbi, sp_wd) %>% 
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>% 
  lm(mwbi ~ sp_wd + sp_wd_sq, data = .)
summary(SPSS_smartphone_sq);confint(SPSS_smartphone_sq)
plot(SPSS_smartphone_sq)

# Weekend: quadratic regression based on original variable
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_we, sp_we_sq) %>%
  lm(mwbi ~ sp_we + sp_we_sq, data = .) %>% step(direction = "forward") %>%
  summary(.)

# Weekend: quadratic regression based on recoded variable
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_we) %>% mutate(sp_we_sq = sp_we**2) %>% 
  lm(mwbi ~ sp_we + sp_we_sq, data = .) %>% step(direction = "forward") %>%
  summary(.)

# Next, as we are using a quadratic term, it is sometimes better to center the predictor first
# see http://www.ats.ucla.edu/stat/mult_pkg/faq/general/curves.html
# new url: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-the-sign-of-the-quadratic-term-in-a-polynomial-regression/
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_we) %>% mutate(sp_we_c = sp_we - mean(sp_we)) %>% 
  mutate(sp_we_sq_c = sp_we_c**2) %>% 
  lm(mwbi ~ sp_we_c + sp_we_sq_c, data = .)  %>%
  summary(.)
# As expected, we do find a linear downwards trend now, when x = 0!

# Add control variables
dataWB %>% dplyr::select(mwbi, sp_wd, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>% 
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .) %>%
  summary(.)

# Same results as paper:
coe <- dataWB %>% dplyr::select(mwbi, sp_wd, sp_wd_sq, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .) %>%
  step(direction = "forward") %>% summary(.)
  

# Small plot with the fitted line
pl <- coe[1] + c(0,0.5,1:7) * coe[2] + c(0,0.5,1:7)**2 * coe[3]
plot(x = c(0,0.5,1:7), y = pl, type = 'l')
  
##
###############
### Getting parameters for simulation study
###############
##

# Watching movies
dataWB %>% dplyr::select(mwbi, watch_we, male, minority, deprived) %>% 
  filter(complete.cases(.)) %>%
  mutate(watch_we_sq = watch_we**2) %>% 
  lm(mwbi ~ watch_we + watch_we_sq + male + minority + deprived, data = .) %>%
  summary(.)

# Smartphone screen usage: without quadratic trend
dataWB %>% dplyr::select(mwbi, sp_wd, male, minority, deprived) %>% 
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
# dplyr::select the complete cases
complMWB <- dataWB %>% 
  dplyr::select(mwbi, sp_wd, male, minority, deprived) %>%
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
  ggtitle("Scatter plot", subtitle = "Added fitted linear regression line") +
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
scatter
Average <- complMWB %>% group_by(sp_wd) %>% summarise(mean_mwb = mean(mwbi), sd_mwbi = sd(mwbi)) %>%
        mutate(CIlow = mean_mwb - (qt(0.975, df = dim(complMWB)[1] - 1) * sd_mwbi / sqrt(dim(complMWB)[1])),
               CIup = mean_mwb + (qt(0.975, df = dim(complMWB)[1] - 1) * sd_mwbi / sqrt(dim(complMWB)[1]))) %>%
  ggplot(aes(x = sp_wd, y = mean_mwb)) + geom_line(size = 0.6) +
  geom_segment(aes(x = sp_wd, xend = sp_wd, y = CIlow, yend = CIup), colour = 'red', size = 1) +
  scale_x_continuous(name = 'Hours spent using smartphone (self-reported)',
                     breaks = c(0:7)) +
  scale_y_continuous(name = 'Mental well-being', limits = c(14,70)) +
  ggtitle("Average mental well-being with 95% CI") + 
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 12))
Average
cowplot::plot_grid(scatter, Average, align = 'h')
# Dimensions:
# width = 760, height = 400, png

MaleFemale <- complMWB %>% 
  group_by(male, sp_wd) %>% summarise(mean_mwbi = mean(mwbi), sd_mwbi = sd(mwbi)) %>%
  mutate(CIlow = mean_mwbi - (qt(0.975, df = dim(complMWB)[1] - 1) * sd_mwbi / sqrt(dim(complMWB)[1]/2)),
         CIup = mean_mwbi + (qt(0.975, df = dim(complMWB)[1] - 1) * sd_mwbi / sqrt(dim(complMWB)[1]/2))) %>%
  ggplot(., aes(x = sp_wd, y = mean_mwbi, group = factor(male))) + 
    geom_segment(aes(x = sp_wd, xend = sp_wd, y = CIlow, yend = CIup, colour = factor(male)), size = 2) +
    geom_line(aes(colour = factor(male)), size = 1.1) +
    scale_color_manual('Gender', labels = c('Female', 'Male'), values = c('#1b9e77', '#d95f02')) +
    scale_x_continuous(name = 'Hours spent using smartphone (self-reported)',
                     breaks = c(0:7)) +
    scale_y_continuous(name = 'Mental well-being') +
    ggtitle("Average mental well-being - according to gender", 
            subtitle = "Error bars are 95% CI") + 
    theme_bw() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
MaleFemale
# Dimensions:
  # width = 

# Histogram
Hist <- ggplot(complMWB, aes(x = mwbi)) + geom_histogram(bins = 20) +
  scale_x_continuous("Mental well-being") +
  scale_y_continuous("Frequency") +
  theme_bw()
Hist

##
###############
### Results OLS: main text
###############
##

# Linear regression without control variables
# Smartphone screen usage: without quadratic trend
fitLM_woc <- complMWB %>% 
  lm(mwbi ~ sp_wd, data = .) %>%
  summary(.)
fitLM_woc

# With control variables
fitLM_c <- complMWB %>% 
  lm(mwbi ~ sp_wd + male + minority + deprived, data = .) %>%
  summary(.)
fitLM_c

# Linear regression without control variables
# Smartphone screen usage: with quadratic trend: X = 0
fitLM_Q_wocX0 <- complMWB %>% mutate(sp_wd_q = sp_wd**2) %>%
  lm(mwbi ~ sp_wd + sp_wd_q, data = .) %>%
  summary(.)
fitLM_Q_wocX0

# With control variables
fitLM_Q_cX0 <- complMWB %>% mutate(sp_wd_q = sp_wd**2) %>%
  lm(mwbi ~ sp_wd + sp_wd_q + male + minority + deprived, data = .) %>%
  summary(.)
fitLM_Q_cX0

# Linear regression without control variables
# Smartphone screen usage: with quadratic trend: X = group mean centered
fitLM_Q_wocXm <- complMWB %>% mutate(sp_wd_c = sp_wd - mean(sp_wd)) %>%
                              mutate(sp_wd_c_q = sp_wd_c**2) %>%
  lm(mwbi ~ sp_wd_c + sp_wd_c_q, data = .) %>%
  summary(.)
fitLM_Q_wocXm

# With control variables
fitLM_Q_cXm <- complMWB %>% mutate(sp_wd_c = sp_wd - mean(sp_wd)) %>%
                            mutate(sp_wd_c_q = sp_wd_c**2) %>%
  lm(mwbi ~ sp_wd_c + sp_wd_c_q + male + minority + deprived, data = .) %>%
  summary(.)
fitLM_Q_cXm

### For LaTeX
xtable(fitLM_Q_wocX0)
xtable(fitLM_Q_wocX0)
xtable(fitLM_Q_wocXm)
xtable(fitLM_Q_cX0)
xtable(fitLM_Q_cXm)


# Normal QQ-plots
qqnorm(residuals(fitLM_woc));qqline(residuals(fitLM_woc), col = 2)
qqnorm(residuals(fitLM_c));qqline(residuals(fitLM_c), col = 2)
qqnorm(residuals(fitLM_Q_wocX0));qqline(residuals(fitLM_Q_wocX0), col = 2)
qqnorm(residuals(fitLM_Q_cX0));qqline(residuals(fitLM_Q_cX0), col = 2)
qqnorm(residuals(fitLM_Q_cXm));qqline(residuals(fitLM_Q_cXm), col = 2)

## Transformations
# Log
log_fitLM_woc <- complMWB %>% mutate(log_mwbi = log(mwbi, base = 10)) %>%
  lm(log_mwbi ~ sp_wd, data = .) %>%
  summary(.)
qqnorm(residuals(log_fitLM_woc));qqline(residuals(log_fitLM_woc), col = 2)

# ln
ln_fitLM_woc <- complMWB %>% mutate(ln_mwbi = log(mwbi)) %>%
  lm(ln_mwbi ~ sp_wd, data = .) %>%
  summary(.)
qqnorm(residuals(ln_fitLM_woc));qqline(residuals(ln_fitLM_woc), col = 2)

# Boxcox
library(MASS)
lambda_fitLM_woc <- lm(mwbi ~ sp_wd, data = complMWB)
bc <- boxcox(lambda_fitLM_woc)
optLam <- bc$x[which.max(bc$y)]
bc_fitLM_woc <- complMWB %>% mutate(bc_mwbi = ((mwbi^(optLam) - 1)/optLam)) %>%
  lm(bc_mwbi ~ sp_wd, data = .) %>%
  summary(.)
qqnorm(residuals(bc_fitLM_woc));qqline(residuals(bc_fitLM_woc), col = 2)

# Boxcox for full model: linear trend
lambda_fitLM_c <- lm(mwbi ~ sp_wd + male + minority + deprived , data = complMWB)
bc <- boxcox(lambda_fitLM_c)
optLam <- bc$x[which.max(bc$y)]
bc_fitLM_c <- complMWB %>% mutate(bc_mwbi = ((mwbi^(optLam) - 1)/optLam)) %>%
  lm(bc_mwbi ~ sp_wd + male + minority + deprived, data = .) %>%
  summary(.)
qqnorm(residuals(bc_fitLM_c));qqline(residuals(bc_fitLM_c), col = 2)

# Boxcox for full model: quadratic trend, group mean centered X
tmp_bc_data <- complMWB %>% mutate(sp_wd_c = sp_wd - mean(sp_wd)) %>%
  mutate(sp_wd_c_q = sp_wd_c**2)
lambda_fitLM_Q_cXm <- lm(mwbi ~ sp_wd + sp_wd_c_q + male + minority + deprived , data = tmp_bc_data)
bc <- boxcox(lambda_fitLM_Q_cXm)
optLam <- bc$x[which.max(bc$y)]
bc_fitLM_Q_cXm <- tmp_bc_data %>% mutate(bc_mwbi = ((mwbi^(optLam) - 1)/optLam)) %>%
  lm(bc_mwbi ~ sp_wd_c + sp_wd_c_q + male + minority + deprived, data = .) %>%
  summary(.)
qqnorm(residuals(bc_fitLM_Q_cXm));qqline(residuals(bc_fitLM_Q_cXm), col = 2)

# Boxcox for full model: quadratic trend, X = 0
tmp_bc_data <- complMWB %>% mutate(sp_wd_q = sp_wd**2)
lambda_fitLM_Q_cX0 <- lm(mwbi ~ sp_wd + sp_wd_q + male + minority + deprived , data = tmp_bc_data)
bc <- boxcox(lambda_fitLM_Q_cX0)
optLam <- bc$x[which.max(bc$y)]
bc_fitLM_Q_cX0 <- tmp_bc_data %>% mutate(bc_mwbi = ((mwbi^(optLam) - 1)/optLam)) %>%
  lm(bc_mwbi ~ sp_wd + sp_wd_q + male + minority + deprived, data = .) %>%
  summary(.)
qqnorm(residuals(bc_fitLM_Q_cX0));qqline(residuals(bc_fitLM_Q_cX0), col = 2)


# Square root
sq_fitLM_woc <- complMWB %>% mutate(sq_mwbi = sqrt(mwbi)) %>%
  lm(sq_mwbi ~ sp_wd, data = .) %>%
  summary(.)
qqnorm(residuals(sq_fitLM_woc));qqline(residuals(sq_fitLM_woc), col = 2)

# Inverse
in_fitLM_woc <- complMWB %>% mutate(in_mwbi = 1/mwbi) %>%
  lm(in_mwbi ~ sp_wd, data = .) %>%
  summary(.)
qqnorm(residuals(in_fitLM_woc));qqline(residuals(in_fitLM_woc), col = 2)

# Cube root
cr_fitLM_woc <- complMWB %>% mutate(cr_mwbi = mwbi^(1/3)) %>%
  lm(cr_mwbi ~ sp_wd, data = .) %>%
  summary(.)
qqnorm(residuals(cr_fitLM_woc));qqline(residuals(cr_fitLM_woc), col = 2)


## Plot Normal QQ plot of quadratic (X = 0) and transformation
par(mfrow = c(1,2))
qqnorm(residuals(fitLM_Q_cX0), main = "Original")
qqline(residuals(fitLM_Q_cX0), col = 2)
qqnorm(residuals(bc_fitLM_Q_cX0), 
       main = "Box Cox Transformation",
       xlab = "Theoretical Quantiles - Lambda = 1.55")
qqline(residuals(bc_fitLM_Q_cX0), col = 2)
par(mfrow = c(1,1))

## Residuals
residual_OLS <- data.frame(Residuals = residuals(fitLM_Q_cX0),
           X = complMWB$sp_wd) %>% tbl_df() %>%
  ggplot(., aes(x = X, y = Residuals)) + 
  geom_jitter(height = NULL, width = 0.45, size = 0.2, alpha = 0.2) +
  scale_x_continuous(name = 'Hours spent using smartphone (self-reported)') +
  ggtitle("Residual plot") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 0, hjust = 0, size = 9),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.position = "bottom",
        legend.margin=margin(b = -0.2, unit='cm'),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
residual_OLS
# Dimensions
# width = 570, height = 410, png
  


## Multiple linear regression
dataWB %>%
  dplyr::select(watch_wd, watch_we,
         play_wd, play_we, comp_wd, comp_we ,
         sp_wd, sp_we, watch_wd_sq, watch_we_sq, play_wd_sq,
         play_we_sq, comp_wd_sq, comp_we_sq, sp_wd_sq, sp_we_sq,mwbi, sp_wd, male, minority, deprived) %>%
  filter(complete.cases(.)) %>% tbl_df() %>%
  lm(mwbi ~ watch_wd + watch_we +
     play_wd + play_we + comp_wd + comp_we +
     sp_wd + sp_we + watch_wd_sq + watch_we_sq + play_wd_sq +
     play_we_sq + comp_wd_sq + comp_we_sq + sp_wd_sq + sp_we_sq + male + minority + deprived, data = .) %>%
  summary(.)


## Expected average mental well-being for gils, wealthy and majority group
xValues <- c(0, 0.5, 1:7)
Est_LM_woc <- fitLM_woc$coefficients[1] + (xValues*fitLM_woc$coefficients[2])
Est_LM_c <- fitLM_c$coefficients[1] + (xValues*fitLM_c$coefficients[2])
Est_LM_Q_wocX0 <- fitLM_Q_wocX0$coefficients[1] + (xValues*fitLM_Q_wocX0$coefficients[2]) + 
                  (xValues^2*fitLM_Q_wocX0$coefficients[3])
Est_LM_Q_cX0 <- fitLM_Q_cX0$coefficients[1] + (xValues*fitLM_Q_cX0$coefficients[2]) +
                  (xValues^2*fitLM_Q_cX0$coefficients[3])

datEst <- data.frame('X' = rep(xValues, 4), 
                     'Estimates' = c(Est_LM_woc,Est_LM_c,
                                   Est_LM_Q_wocX0,Est_LM_Q_cX0),
                     'Model' = factor(rep(c('Linear model - w/o covariates',
                             'Linear model - w covariates',
                             'Polynomial model (X = 0) - w/o covariates',
                             'Polynomial model (X = 0) - w covariates'), 
                           each = length(xValues))))

OLSest <- ggplot(datEst, aes(x = X, y = Estimates, group = Model)) + 
  geom_line(aes(colour = Model), size = 0.7) + geom_point(aes(colour = Model), size = 0.7) +
  scale_x_continuous(name = 'Hours spent using smartphone (self-reported)',
                     breaks = xValues) +
  scale_y_continuous(name = 'Average mental well-being') +
  scale_color_brewer("", type = 'qual', palette = 'Set1') +
  ggtitle('Estimated average mental well-being', subtitle = 'OLS') +
  theme_minimal() +
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.text.x=element_text(angle = 0, hjust = 0, size = 9),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.position = "bottom",
        legend.margin=margin(b = -0.2, unit='cm'),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
OLSest
# Dimensions:
# width = 560, height = 430 png


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
PIM_SE_est_woc <- SDP_estimates_woc %>% dplyr::select(sp_wd_Svar) %>% summarise(sum_svar = sum(sp_wd_Svar)) %>%
  mutate(SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(SE)
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
PIM_beta_est_c <- SDP_estimates_c %>% dplyr::select(1:4) %>% summarise_all(mean)
PIM_SE_est_c <- SDP_estimates_c %>% dplyr::select(5:8) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
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
PIM_SE_est_woc_RE <- SDP_estimates_woc_RE %>% dplyr::select(sp_wd_Svar) %>% summarise(sum_svar = sum(sp_wd_Svar)) %>%
  mutate(SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(SE)
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
PIM_beta_est_c_RE <- SDP_estimates_c_RE %>% dplyr::select(1:4) %>% summarise_all(mean)
PIM_SE_est_c_RE <- SDP_estimates_c_RE %>% dplyr::select(5:8) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
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
############### QUADRATIC MODEL ################ 
################################################

# Adding quadratic term into the model!

# Let us use the single data partitioning algorithm
# We can then use parallel version over all available cores
# Record time to estimate model!

# Length of partitions (min 1000) and hence amount of partitions
sizePart <- 1000
S <- ceiling(n/sizePart)

# First shuffle data (safety), then add quadratic term
set.seed(159)
NewOrder <- sample(x = 1:n, size = n, replace = FALSE)
ShufCompMWB <- complMWB %>% slice(NewOrder) %>% mutate(sp_wd_q = sp_wd**2)


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
form_woc_Q <- formula("mwbi ~ sp_wd + sp_wd_q")

# Now run the SDPfunction (single data partitioning)
StartTime_woc_Q <- Sys.time()
SDP_estimate_results_woc_Q <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_woc_Q)
SDP_estimates_woc_Q <- do.call(bind_rows, SDP_estimate_results_woc_Q)
SDP_estimates_woc_Q <- data.frame(apply(SDP_estimates_woc_Q, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_woc_Q <- SDP_estimates_woc_Q %>% dplyr::select(1:2) %>% summarise_all(mean)
PIM_SE_est_woc_Q <- SDP_estimates_woc_Q %>% dplyr::select(3:4) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
zValue_woc_Q <- PIM_beta_est_woc_Q/PIM_SE_est_woc_Q$SE
PVal_woc_Q <- 2 * pnorm(q = as.numeric(abs(zValue_woc_Q)), lower.tail = FALSE)

Shuffle_woc_Q <- data.frame('Parameter' = names(PIM_beta_est_woc_Q),
                        'Beta' = t(PIM_beta_est_woc_Q),
                        'SE' = PIM_SE_est_woc_Q$SE,
                        'Zval' = t(zValue_woc_Q),
                        'Pval' = PVal_woc_Q)
rownames(Shuffle_woc_Q) <- 1:2
Shuffle_woc_Q

# Estimated probabilities of comparing each hour with 0
Xvalues <- c(0, 0.5,1:7)
betaX <- Shuffle_woc_Q[which(Shuffle_woc_Q$Parameter == 'sp_wd'),'Beta'] * Xvalues + Shuffle_woc_Q[which(Shuffle_woc_Q$Parameter == 'sp_wd_q'),'Beta'] * (Xvalues**2)
PredProb <- exp(betaX) / (exp(betaX) + 1)
plot(PredProb ~ c(1:9), type = 'l')

# Time of fitting and combining estimates
RecordedTime_woc_Q <- difftime(time1 = Sys.time(), time2 = StartTime_woc_Q, units = 'mins');RecordedTime_woc_Q

# Get in LaTeX
xtable(Shuffle_woc_Q)

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
form_c_Q <- formula("mwbi ~ sp_wd + sp_wd_q + male + minority + deprived")

# Now run the SDPfunction (single data partitioning)
StartTime_c_Q <- Sys.time()
SDP_estimate_results_c_Q <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_c_Q)
SDP_estimates_c_Q <- do.call(bind_rows, SDP_estimate_results_c_Q)
SDP_estimates_c_Q <- data.frame(apply(SDP_estimates_c_Q, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_c_Q <- SDP_estimates_c_Q %>% dplyr::select(1:5) %>% summarise_all(mean)
PIM_SE_est_c_Q <- SDP_estimates_c_Q %>% dplyr::select(6:10) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
zValue_c_Q <- PIM_beta_est_c_Q/PIM_SE_est_c_Q$SE
PVal_c_Q <- 2 * pnorm(q = as.numeric(abs(zValue_c_Q)), lower.tail = FALSE)

Shuffle_c_Q <- data.frame('Parameter' = names(PIM_beta_est_c_Q),
                        'Beta' = t(PIM_beta_est_c_Q),
                        'SE' = PIM_SE_est_c_Q$SE,
                        'Zval' = t(zValue_c_Q),
                        'Pval' = PVal_c_Q)
rownames(Shuffle_c_Q) <- 1:5
Shuffle_c_Q

# Estimated probabilities of comparing each hour with 0
Xvalues <- c(0, 0.5,1:7)
betaX <- Shuffle_c_Q[which(Shuffle_c_Q$Parameter == 'sp_wd'),'Beta'] * Xvalues + Shuffle_c_Q[which(Shuffle_c_Q$Parameter == 'sp_wd_q'),'Beta'] * (Xvalues**2)
PredProb <- exp(betaX) / (exp(betaX) + 1)
plot(PredProb ~ c(1:9), type = 'l')

# Time of fitting and combining estimates
RecordedTime_c_Q <- difftime(time1 = Sys.time(), time2 = StartTime_c_Q, units = 'mins');RecordedTime_c_Q

# Get in LaTeX
xtable(Shuffle_c_Q)



###################################################
# WITHOUT COVARIATES: INTERACTION TERM
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
form_woc_Q <- formula("mwbi ~ sp_wd * sp_wd_q")

# Now run the SDPfunction (single data partitioning)
StartTime_woc_Q <- Sys.time()
SDP_estimate_results_woc_Q <- clusterApply(cl, 1:(S-1), fun = SDPfunction, sizePart = sizePart, compl_data = ShufCompMWB, form = form_woc_Q)
SDP_estimates_woc_Q <- do.call(bind_rows, SDP_estimate_results_woc_Q)
SDP_estimates_woc_Q <- data.frame(apply(SDP_estimates_woc_Q, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_woc_Q <- SDP_estimates_woc_Q %>% dplyr::select(1:3) %>% summarise_all(mean)
PIM_SE_est_woc_Q <- SDP_estimates_woc_Q %>% dplyr::select(4:6) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
zValue_woc_Q <- PIM_beta_est_woc_Q/PIM_SE_est_woc_Q$SE
PVal_woc_Q <- 2 * pnorm(q = as.numeric(abs(zValue_woc_Q)), lower.tail = FALSE)

Shuffle_woc_Q <- data.frame('Parameter' = names(PIM_beta_est_woc_Q),
                            'Beta' = t(PIM_beta_est_woc_Q),
                            'SE' = PIM_SE_est_woc_Q$SE,
                            'Zval' = t(zValue_woc_Q),
                            'Pval' = PVal_woc_Q)
rownames(Shuffle_woc_Q) <- 1:3
Shuffle_woc_Q

# Estimated probabilities of comparing each hour with 0
Xvalues <- c(0, 0.5,1:7)
betaX <- Shuffle_woc_Q[which(Shuffle_woc_Q$Parameter == 'sp_wd'),'Beta'] * Xvalues + Shuffle_woc_Q[which(Shuffle_woc_Q$Parameter == 'sp_wd_q'),'Beta'] * (Xvalues**2) +
  Shuffle_woc_Q[which(Shuffle_woc_Q$Parameter == 'sp_wd_q'),'Beta'] * Shuffle_woc_Q[which(Shuffle_woc_Q$Parameter == 'sp_wd'),'Beta'] * (Xvalues**2) * (Xvalues)
PredProb <- exp(betaX) / (exp(betaX) + 1)
plot(PredProb ~ Xvalues, type = 'l')

# Time of fitting and combining estimates
RecordedTime_woc_Q <- difftime(time1 = Sys.time(), time2 = StartTime_woc_Q, units = 'mins');RecordedTime_woc_Q

# Get in LaTeX
xtable(Shuffle_woc_Q)





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
PIM_beta_est_wocM <- SDP_estimates_wocM %>% dplyr::select(1:9) %>% summarise_all(mean)
PIM_SE_est_wocM <- SDP_estimates_wocM %>% dplyr::select(10:18) %>% summarise_all(sum) %>% t(.) %>% 
                      data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
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
Shuffle_wocM %>% dplyr::select(Beta, SE) %>% mutate(Group = factor(c(0,0.5,1,2,3,4,5,6,7))) %>%
  mutate(CIlow = Beta - qnorm(0.025, lower.tail = FALSE) * SE,
         CIup = Beta + qnorm(0.025, lower.tail = FALSE) * SE) %>%
  mutate(Perc = exp(Beta) / (1 + exp(Beta)))







################################################
################## ANOVA MODEL ################# 
################################################


##
###############
### PIM: single data partitioning: ANOVA model
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

# Now construct function which we can use over different cores: anova model here!
# sID is the ID of partition S, sizePart is the size of the partitions, compl_data is the original dataset
Pairwise_SDPfunction_aov <- function(sID, sizePart, compl_data, form){
  Part_est <- data.frame() %>% tbl_df()
  start <- sID + (sID - 1) * sizePart
  end <- start + sizePart
  # Slice automatically stops when end < than n! ==> handy here
  subsetData <- compl_data %>% slice(start:end)
  # Try to fit PIM. If fails, return message. 
  PIMfit <- try(pim(formula = form, data = subsetData, 
                    link = 'probit', model = "difference"), silent = TRUE)
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
form_woc_aov <- formula("mwbi ~ sp_wdF")

# Now run the Pairwise_SDPfunction (single data partitioning)
StartTime_woc_aov <- Sys.time()
SDP_estimate_results_woc_aov <- clusterApply(cl, 1:(S-1), fun = Pairwise_SDPfunction_aov, sizePart = sizePart, compl_data = ShufCompMWB, form = form_woc_aov)
SDP_estimates_woc_aov <- do.call(bind_rows, SDP_estimate_results_woc_aov)
SDP_estimates_woc_aov <- data.frame(apply(SDP_estimates_woc_aov, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_woc_aov <- SDP_estimates_woc_aov %>% dplyr::select(1:8) %>% summarise_all(mean)
PIM_SE_est_woc_aov <- SDP_estimates_woc_aov %>% dplyr::select(9:16) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
zValue_woc_aov <- PIM_beta_est_woc_aov/PIM_SE_est_woc_aov$SE
PVal_woc_aov <- 2 * pnorm(q = as.numeric(abs(zValue_woc_aov)), lower.tail = FALSE)
NAMEcontrasts <- rev(c('0 < 7', '0 < 6', '0 < 5', '0 < 4', '0 < 3', '0 < 2', '0 < 1', '0 < 0.5'))
Shuffle_woc_aov <- data.frame('Parameter' = NAMEcontrasts,
                           'Beta' = t(PIM_beta_est_woc_aov),
                           'SE' = PIM_SE_est_woc_aov$SE,
                           'Zval' = t(zValue_woc_aov),
                           'Pval' = PVal_woc_aov)
rownames(Shuffle_woc_aov) <- 1:8
Shuffle_woc_aov

# Time of fitting and combining estimates
RecordedTime_woc_aov <- difftime(time1 = Sys.time(), time2 = StartTime_woc_aov, units = 'mins')
RecordedTime_woc_aov


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
form_c_aov <- formula("mwbi ~ sp_wdF + male + minority + deprived")

# Now run the Pairwise_SDPfunction (single data partitioning)
StartTime_c_aov <- Sys.time()
SDP_estimate_results_c_aov <- clusterApply(cl, 1:(S-1), fun = Pairwise_SDPfunction_aov, sizePart = sizePart, compl_data = ShufCompMWB, form = form_c_aov)
SDP_estimates_c_aov <- do.call(bind_rows, SDP_estimate_results_c_aov)
SDP_estimates_c_aov <- data.frame(apply(SDP_estimates_c_aov, 2, as.numeric))
stopCluster(cl)

# Calculate final estimates
PIM_beta_est_c_aov <- SDP_estimates_c_aov %>% dplyr::select(1:11) %>% summarise_all(mean)
PIM_SE_est_c_aov <- SDP_estimates_c_aov %>% dplyr::select(12:22) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
zValue_c_aov <- PIM_beta_est_c_aov/PIM_SE_est_c_aov$SE
PVal_c_aov <- 2 * pnorm(q = as.numeric(abs(zValue_c_aov)), lower.tail = FALSE)
NAMEcontrasts <- c(rev(c('0 < 7', '0 < 6', '0 < 5', '0 < 4', '0 < 3', '0 < 2', '0 < 1', '0 < 0.5')), 'gender', 'deprived', 'minority')
Shuffle_c_aov <- data.frame('Parameter' = NAMEcontrasts,
                         'Beta' = t(PIM_beta_est_c_aov),
                         'SE' = PIM_SE_est_c_aov$SE,
                         'Zval' = t(zValue_c_aov),
                         'Pval' = PVal_c_aov)
rownames(Shuffle_c_aov) <- 1:11
Shuffle_c_aov

# Time of fitting and combining estimates
RecordedTime_c_aov <- difftime(time1 = Sys.time(), time2 = StartTime_c_aov, units = 'mins')
RecordedTime_c_aov

# latex
xtable(Shuffle_c_aov,digits = 4)

# Plot of the probabilities
extrRow <- data.frame(Parameter = '0 = 0', Beta = 0, SE = 0,
                      Zval = 0, Pval = 0, 
                      row = 0, CIlow = 0,
                      CIup = 0, ProbBeta = 0.5,
                      ProbCIlow = 0, ProbCIup = 0)
LabLX <- c('0h\n---\n0h*','0h\n---\n0.5h*', '0h\n---\n1h*', '0h\n---\n2h*', '0h\n---\n3h*', '0h\n---\n4h*', '0h\n---\n5h*', '0h\n---\n6h*', '0h\n---\n7h*')
PlotProb <- Shuffle_c_aov %>% filter(!Parameter %in% c('gender', 'deprived', 'minority')) %>% 
  mutate(row = row_number(),
         CIup = Beta - (qnorm(0.025, lower.tail = TRUE) * SE),
         CIlow = Beta + (qnorm(0.025, lower.tail = TRUE) * SE),
         ProbBeta = pnorm(Beta),
         ProbCIlow = pnorm(CIlow),
         ProbCIup = pnorm(CIup)) %>%
  arrange(row_number()) %>% mutate(row = row_number()) %>% 
  bind_rows(extrRow, .)

PlotProb %>% ggplot(aes(x = row, y = ProbBeta)) + geom_line(size = 1.2) +
  geom_point() + 
  geom_segment(aes(x = row, xend = row, y = ProbCIlow, yend = ProbCIup)) +
  scale_y_continuous("P[MWBI <= MWBI*]", limits = c(min(PlotProb$ProbBeta) - 0.009, max(PlotProb$ProbBeta) + 0.009)) + 
  scale_x_continuous("Comparison", breaks = 0:8, labels = LabLX) +
  ggtitle('ANOVA: comparing 0 versus X* hours') +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 0, hjust = 0, size = 9),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.position = "top",
        legend.margin=margin(t = -0.2, unit='cm'),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
#width: 18,38 cm
#height: 14,29 cm






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
PIM_beta_est_wocP <- SDP_estimates_wocP %>% dplyr::select(1:8) %>% summarise_all(mean)
PIM_SE_est_wocP <- SDP_estimates_wocP %>% dplyr::select(9:16) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
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
PIM_beta_est_cP <- SDP_estimates_cP %>% dplyr::select(1:11) %>% summarise_all(mean)
PIM_SE_est_cP <- SDP_estimates_cP %>% dplyr::select(12:22) %>% summarise_all(sum) %>% t(.) %>% 
  data.frame(sum_svar = .) %>% mutate(RowN = rownames(.), SE = sqrt((1/S)**2 * sum_svar)) %>% dplyr::select(RowN, SE)
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
  ggtitle('Pairwise probabilities: 0 versus X* hours') +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 0, hjust = 0, size = 9),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        legend.position = "top",
        legend.margin=margin(t = -0.2, unit='cm'),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
#width: 18,38 cm
#height: 14,29 cm




##
###############
### Goodness-of-fit
###############
##

# Randomly sample 50 observations
set.seed(246)
takeN <- 50
LenghtN <- takeN * (takeN - 1)/2
GOFdata <- sample_n(complMWB, takeN, replace = FALSE) %>% mutate(sp_wd_q = sp_wd**2)

# Fit 4 PIM models 
fitLwoc <- pim(mwbi ~ sp_wd, data = GOFdata, keep.data = TRUE)
fitQwoc <- pim(mwbi ~ sp_wd + sp_wd_q, data = GOFdata, keep.data = TRUE)
fitLc <- pim(mwbi ~ sp_wd + male + minority + deprived, data = GOFdata, keep.data = TRUE)
fitQc <- pim(mwbi ~ sp_wd + sp_wd_q + male + minority + deprived, data = GOFdata, keep.data = TRUE)

# 4 OLS models: take as many datapoints as PIM
GOFdata_LM <- sample_n(complMWB, LenghtN, replace = FALSE) %>% mutate(sp_wd_q = sp_wd**2)
OLSfitLwoc <- lm(mwbi ~ sp_wd, data = GOFdata_LM)
OLSfitQwoc <- lm(mwbi ~ sp_wd + sp_wd_q, data = GOFdata_LM)
OLSfitLc <- lm(mwbi ~ sp_wd + male + minority + deprived, data = GOFdata_LM)
OLSfitQc <- lm(mwbi ~ sp_wd + sp_wd_q + male + minority + deprived, data = GOFdata_LM)

# Gather all residuals with fitted values for OLS and sequence for PIM in data frame
res <- data.frame(
  residuals = 
    c((fitLwoc@response - fitLwoc@fitted), (fitQwoc@response - fitQwoc@fitted),
    (fitLc@response - fitLc@fitted), (fitQc@response - fitQc@fitted),
    residuals(OLSfitLwoc), residuals(OLSfitQwoc),
    residuals(OLSfitLc), residuals(OLSfitQc)),
  predicted = 
    c(#fitLwoc@fitted, fitQwoc@fitted, fitLc@fitted, fitQc@fitted,
      rep(c(1:LenghtN), 4),
      fitted(OLSfitLwoc), fitted(OLSfitQwoc), fitted(OLSfitLc),fitted(OLSfitQc)),
  type = 
    c(rep(c('Linear No C', 'Quadratic No C', 'Linear C', 'Quadratic C'), each = LenghtN),
          rep(c('Linear No C', 'Quadratic No C', 'Linear C', 'Quadratic C'), each = LenghtN)),
  model = 
    c(rep('PIM', LenghtN * 4), rep('OLS', LenghtN * 4))
)

# Plot
residualPlot <- ggplot(res, aes(x = predicted, y = residuals)) + geom_point(size = 0.7, alpha = 0.7)  + 
  geom_smooth(method = 'loess', colour = '#fdcdac') +
  scale_x_continuous("") +
  facet_wrap(model~type, scales = 'free', nrow = 2) +
  ggtitle("Residual plots with loess smoother according to 4 structural models") +
  theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))
residualPlot






# Test code
test <- data.frame(residuals = residuals(OLSfitQwoc), predicted = fitted(OLSfitQwoc))
ggplot(test, aes(x = predicted, y = residuals)) + geom_jitter(width = 0.1, height = NULL) +
  geom_smooth(method = 'loess', colour = '#fdcdac')

test <- data.frame(residuals = (fitLwoc@response - fitLwoc@fitted), predicted = fitLwoc@fitted)
ggplot(test, aes(x = predicted, y = residuals)) + geom_jitter(width = 0.1, height = 0.1) +
  geom_smooth(method = 'loess', colour = '#fdcdac')


plot((fitLwoc@response - fitLwoc@fitted) ~ fitLwoc@fitted)
plot(residuals(OLSfitLc) ~ GOFdata_LM$sp_wd)
plot(OLSfitLc)
plot(residuals(OLSfitLc) ~ fitted(OLSfitLwoc))
plot(residuals(OLSfitLc))


takeN <- 100
xlab <- 1:(takeN * (takeN - 1)/2)
random <- sample(xlab,size = length(xlab), replace = FALSE)
te <- 
tes <- pim::pim(mwbi ~ sp_wd, data = te, keep.data = TRUE)
tes@fitted
tes@response
tes@penv$sp_wd
res <- data.frame(residuals = c(tes@response - tes@fitted), fitted = tes@fitted)
#res <- res[order(res$residuals),]
# res$xlab <- xlab
plot(res$residuals)
ggplot(res, aes(x = seq_along(residuals), y = residuals)) + geom_smooth() + geom_point()
ggplot(res, aes(x = seq_along(residuals), y = residuals)) + geom_smooth()
ggplot(res, aes(x = fitted, y = residuals)) + geom_point()
  geom_smooth(method = 'gam')
ggplot(res, aes(y = residuals)) + geom_smooth(method = 'gam')




### Test code for OLS


dataWB %>% dplyr::select(mwbi, sp_we, sp_we_sq) %>%
  lm(mwbi ~ sp_we + sp_we_sq, data = .) %>%
  summary(.)

# Weekend
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_we, sp_we_sq) %>%
  mutate(sp_we_c = sp_we - mean(sp_we)) %>%
  lm(mwbi ~ sp_we_c + sp_we_sq, data = .) %>%
  summary(.)

# No centering: original variable
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_wd, sp_wd_sq) %>%
  mutate(sp_wd_c = sp_wd - mean(sp_wd),
         sp_wd_c_sq = sp_wd_c**2) %>%
  lm(mwbi ~ sp_wd + sp_wd_sq, data = .) %>%
  summary(.)

# Centering: original variable
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_wd, sp_wd_sq) %>%
  mutate(sp_wd_c = sp_wd - mean(sp_wd),
         sp_wd_c_sq = sp_wd_c**2) %>%
  lm(mwbi ~ sp_wd_c + sp_wd_c_sq, data = .) %>%
  summary(.)

#####
#####
# No centering: new variable
dataWB %>% 
  dplyr::select(mwbi, sp_wd) %>%
  filter(complete.cases(.)) %>%
  mutate(sp_wd_sq = sp_wd**2) %>%
  lm(mwbi ~ sp_wd + sp_wd_sq, data = .) %>%
  summary(.)

# Centering: new variable
dataWB %>% 
  dplyr::select(mwbi, sp_wd) %>%
  filter(complete.cases(.)) %>%
  mutate(sp_wd_c = sp_wd - mean(sp_wd),
         sp_wd_c_sq = sp_wd_c**2,
         sp_wd_sq_2 = sp_wd_c**2) %>%
  lm(mwbi ~ sp_wd_c + sp_wd_sq_2, data = .) %>%
  summary(.)


# No centering: with covariates
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_wd, male, minority, deprived) %>%
  mutate(sp_wd_c = sp_wd - mean(sp_wd),
         sp_wd_sq = sp_wd**2) %>%
  lm(mwbi ~ sp_wd + sp_wd_sq + male + minority + deprived, data = .) %>%
  summary(.)

# Centering: with covariates
dataWB %>% filter(complete.cases(.)) %>%
  dplyr::select(mwbi, sp_wd, male, minority, deprived) %>%
  mutate(sp_wd_c = sp_wd - mean(sp_wd),
         sp_wd_sq = sp_wd_c**2) %>%
  lm(mwbi ~ sp_wd_c + sp_wd_sq + male + minority + deprived, data = .) %>%
  summary(.)


