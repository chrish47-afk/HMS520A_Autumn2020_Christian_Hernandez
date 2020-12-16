############################################################################################
# Assignment 5
# November 25th, 2020
############################################################################################
install.packages('lme4')
install.packages('metafor')
############################################################################################
rm(list = ls())
#library(lintr)
library(broom)
library(tidyverse)
library(stats)
library(dplyr)  
library(data.table)
library(tidyr)
library(reshape)
library(ggplot2)
library(lme4)
library(metafor)
###############################################################################################
#Problem 1
###############################################################################################
Assignment5_data <- read.csv("C:\\Users\\chris\\Desktop\\R, SQL, Python_notes\\HM_Course_Notes\\assignment5_data.csv")

ggplot(data = Assignment5_data) +
  geom_point(mapping = aes(x = exposure, y = obs)) +
  facet_grid(facets = 'study_id1')

ggplot(data = Assignment5_data) +
  geom_point(mapping = aes(x = exposure, y = obs)) +
  facet_grid(facets = 'study_id2')

# From an observational stand-point, study_id1 has more spread than study_id2. But both are really similar. I think study_id1 will be a better group_id going forward, since its the one showing more correlation.
###############################################################################################
#Problem 2
###############################################################################################

## study_group1 ---------------------
data_id1 <- regress_group_data(data = Assignment5_data, group_id = 'study_id1', obs = 'obs' , covs = 'exposure',include_intercept = FALSE)
data_id1 <- setnames(data_id1, old = 'exposure', new = 'coefficient')

# - Clean up and Residual ( study_id1)
final_data_id1 <- merge(data_id1, Assignment5_data, by = "study_id1")
final_data_id1$study_id2 <- NULL
final_data_id1$residual <- (final_data_id1$obs - (final_data_id1$exposure*final_data_id1$coefficient))

# - Correlation
cor(final_data_id1$residual, final_data_id1$cov1) #cov1 A Strong Correlation
cor(final_data_id1$residual, final_data_id1$cov2) #cov2 A Moderate Negaive Correlation
cor(final_data_id1$residual, final_data_id1$cov3) #cov3 A Weak Negative Correlation 
## For study_id1, it seems that cov1 has the stronger Correlation. Hence a better fit for the model.

## study_group2 ---------------------
data_id2 <- regress_group_data(data = Assignment5_data, group_id = 'study_id2', obs = 'obs' , covs = 'exposure',include_intercept = FALSE)
data_id2 <- setnames(data_id2, old = 'exposure', new = 'coefficient')

# - Clean up and Residual ( study_id2)
final_data_id2 <- merge(data_id2, Assignment5_data, by = "study_id2")
final_data_id2$study_id1 <- NULL
final_data_id2$residual <- (final_data_id2$obs - (final_data_id2$exposure*final_data_id2$coefficient))
# - Correlation
cor(final_data_id2$residual, final_data_id2$cov1) #cov1 A Moderate Correlation
cor(final_data_id2$residual, final_data_id2$cov2) #cov2 A Moderate Negaive Correlation
cor(final_data_id2$residual, final_data_id2$cov3) #cov3 A Weak Negative Correlation 
#For study_id2, either cov1 or cov2 are a good fit. They both have a moderate correlation.

###############################################################################################
#Problem 2 - Peng's recommendation [UPDATES]
###############################################################################################
# - (1) and (2) are above. 

# - (3) & (4)
# - Cov1
cov1_coefficients <- regress_group_data(data = final_data_id1, group_id = 'study_id1', obs = 'residual', covs = 'cov1', include_intercept = FALSE )
test_data_id1_cov1 <- merge(final_data_id1, cov1_coefficients, by = 'study_id1')
test_data_id1_cov1$residual_cov1 <- (test_data_id1_cov1$residual - (test_data_id1_cov1$cov1.x*test_data_id1_cov1$cov1.y))
#cov1.y are the coefficients

# - Cov2
cov2_coefficients <- regress_group_data(data = final_data_id1, group_id = 'study_id1', obs = 'residual', covs = 'cov2', include_intercept = FALSE )
test_data_id1_cov2 <- merge(final_data_id1, cov2_coefficients, by = 'study_id1')
test_data_id1_cov2$residual_cov2 <- (test_data_id1_cov2$residual - (test_data_id1_cov2$cov2.x*test_data_id1_cov2$cov2.y))
#cov2.y, are the coefficients

# - Cov3
regress_group_data(data = final_data_id1, group_id = 'study_id1', obs = 'residual', covs = 'cov3', include_intercept = FALSE )

# Based on the criteria for inclusion, it looks like Cov1 and Cov2 should be adequately included in modeling this for the next step(s).

###############################################################################################
#Problem 3 
###############################################################################################
# According to problem 1 and 2, selecting study_group1 and cov 1 for this problem
# study_id1 and cov1 (Selection)
regress_group_data(data = Assignment5_data, group_id = 'study_id1', obs = 'obs', covs = c('exposure', 'cov1'), include_intercept = FALSE)

#For the best-selected study and covariate according to my selection. The studies with the ray of coefficients are Study 3, 8, and 9. As they have a coefficient greater than 2. That differ significantly from the other coefficients. 

#  If results are do not vary by much, then they should not be included. Variation seems to be present, but not super significant. 

###############################################################################################
#Problem 4
###############################################################################################
# Setting up a lme4 model
# Study id, covariates and random effects information we obtained from the previous problem.
# Plot our results

# -------------------------------
#   study_group1 and cov 1
# -------------------------------
# y(i) = B + u(i) + e(i)
# y(i) = B(0) + u(i0) + x(i)B(1) + e(i)   [Case 1]
# y(i) = B(0) + u(i0) + x(i)(B(1) + u(i1)) + e(i)  [Case 2]

# - Metafor
## This replaces the method of meta-analyzing the data manually
## Here the first rnorm samples the random effects, and the second rnorm samples measurement errors. metafor assume each observation come from a single group (child) which is very fitting in this case. We use rma function to solve this problem,

#model_2 <- lmer(obs~exposure+(exposure||cov1), data = Assignment5_data)
#summary(model_2)

# - Model, inputting cov1
## NOTE: Determination of implementing this lmer function needs to be further reviewed. 
model <- lmer(obs~exposure + cov1 + (1|study_id1), data = Assignment5_data)
summary(model)

# - Predict( fit line)
Assignment5_data$fit <- predict(model)

# - Plotting
ggplot(Assignment5_data, aes(obs, exposure, group = study_id1))+
  facet_grid(~study_id1)+
  geom_line(aes(y = fit, x = exposure), size = 0.8)+
  geom_point(alpha = 0.3, )+
  geom_hline(yintercept = 0, linetype = 'dashed')+
  theme_bw()

