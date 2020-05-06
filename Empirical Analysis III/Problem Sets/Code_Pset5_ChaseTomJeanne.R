## ---------------------------
##
## Script name: Problem Set 5
##
## Purpose of script: 
##
## Author: Chase Abram, Tom Hierons, Jeanne Sorin
##
## Date Created: 2020-05-01
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC
rm(list=ls())

# Jeanne's
setwd("~/Documents/PhD/Skwad/Empirical Analysis III/Problem Sets/")

# Chase's
setwd("~/UChiGit/Skwad/Empirical Analysis III/Problem Sets")

# Tom's
# setwd()


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)

packages <- c("tidyverse", "data.table", "foreign", 
              "stargazer", "rdd", "ggplot2", "lfe", 
              "rdrobust", "ivpack", "codebook")
lapply(packages, library, character.only = TRUE)


## ---------------------------

## load up data
data <- read.dta("PS5.dta")
# See codebook (downloaded from Card's website, and in Git) for variable key
# It's mildly helpful

## ---------------------------



## ----- 
# 1. Describe the data
stargazer(data, digits=2, out="Pset5_ChaseTomJeanne/table_q1.tex")


## ----- 
# 2. Estimate the regression on the sample of fast food restaurants in Feb-Mar 1992
q2 <- felm(data=data[data$post==0,], empft ~ minwage + nregs + hrsopen + d2 + d3 + d4)
stargazer(q2, out="Pset5_ChaseTomJeanne/table_q2.tex")

# needed for anova
q2_unres <- lm(data=data[data$post==0,], empft ~ minwage + nregs + hrsopen + d2 + d3 + d4)
q2_res <- lm(data=data[data$post==0,], empft ~ minwage + nregs + hrsopen + d4)
q2_res_eq <- lm(data=data[data$post==0,], empft ~ minwage + nregs + hrsopen + I(d2 + d3) + d4)
## ------
# 3. Interpret gamma, the coefficient in front of min-wage (TODO) & calculate a 90\% confidence interval
# CI = gamma +/- t_{0.9} se/((N)^0.5)
# t_{0.9} = 1.64
CI_gamma = cbind(CIlower = q2$coefficients[2] - 1.64 * q2$se[2] / (q2$N)^0.5, 
                 CIupper = q2$coefficients[2] + 1.64 * q2$se[2] / (q2$N)^0.5)
CI_gamma


## -----
# 4. Use the sum of squares table from the regression output to calculate the R^2 and the standard error of the regression
pre = data[data$post==0,] #restrict to Feb-March data --> actually use q2$response instead, as not all obs were used in the regression because NA
ybar = mean(q2$response) # on
SStot = sum((q2$response - ybar)^2)
SSres = sum(q2$residuals^2)
R2 = 1 - SSres / SStot
R2

# Why is it not this? I am prolly missing something dumb, but
# I'm not sure why SStot is calculated as deviation from mean 
# and not deviation from y
ESS = sum(q2$fitted.values^2)
TSS = sum(q2$response^2)
R2alt = ESS/TSS
R2alt


RMSE = sqrt(mean((q2$fitted.values - q2$response)^2))
RMSE


## -----
# 5. Give an economic interpretation of the coef nu2 - nu4. What might explain the relatively large coeff on d4?



## -----
# 6. Test : nu2 = nu3 = 0 (Not sure really what else they're asking for. Manually?)

#linearHypothesis(q2, c("d2=d3", "d3=0"))
linearHypothesis(q2, c("d2=0", "d3=0"))

# heteroskedasticity robust (doesn't change answer)
linearHypothesis(q2, c("d2=0", "d3=0"), white.adjust = "hc1")
stargazer(ht_6)

# anova does a proper F test -> both return the same pr. >
ht6 <- anova(q2_res,q2_unres)
stargazer(ht6, out="Pset5_ChaseTomJeanne/table_ht6.tex")

## -----
# 7. Test the hypothesis H0 : nu2 = nu3 using the estimated covariance matrix of the coefficients. 
# Verify your answer by performing a F-test

# nu2 = nu3 --> nu2 - nu3 = 0
# var(nu2 - nu3) = var(nu2) + var(nu3) - 2 cov(nu2, nu3)
# t = [nu2 - nu3] / [se(nu2 - nu3)]
# Reject if t > c
vcov_nu = vcov(q2)[5:6,5:6]
var_dnu = vcov_nu[1,1] + vcov_nu[2,2] - 2*vcov_nu[1,2]
t = (q2$coefficients[5] - q2$coefficients[6])/(var_dnu^0.5)
t
c = 1.96 # 95% - should adjust for 390 degrees of freedom?
(t <= c) # Doesn't reject the null

# Why does this not match either check?

# check >- Yes, with one restriction, F test is chi-square
linearHypothesis(q2, c("d2 = d3"))

# anova
ht7 <- anova(q2_res_eq,q2_unres)
stargazer(ht7, out="Pset5_ChaseTomJeanne/table_ht7.tex")



## ----
# Now we want to control for potential selection issues by using panel structure of our data.
# 8. See overleaf


## ----
# 9. See overleaf


## ----
# 10. Generate a table of means, a table of standard errors and a table of frequencies for -empft- in each state, each time period
nrows = nrow(data)
data_sum <- data %>%
  group_by(state, post) %>%
  summarize(mean = round(mean(empft, na.rm=T), 3),
            se = round(sd(empft, na.rm=T), 3),
            count = n(),
            freq = round(count / nrows, 3))
stargazer(data_sum, summary=FALSE, digits=3, out="Pset5_ChaseTomJeanne/table_q10.tex")


## -----
# 11. Using these stat, calculate a DD estimate of the impact of the min wage law on employment
treat_before = data_sum$mean[data_sum$state==1 & data_sum$post==0]
treat_after = data_sum$mean[data_sum$state==1 & data_sum$post==1]
control_before = data_sum$mean[data_sum$state==0 & data_sum$post==0]
control_after = data_sum$mean[data_sum$state==0 & data_sum$post==1]

DiD11 = (treat_after - control_after) - (treat_before - control_before)
DiD11

## -----
# 12. Specify and estimate the corresponding regression
DiD12 <- felm(data = data, empft ~ post + state + post:state)
stargazer(DiD12, out="Pset5_ChaseTomJeanne/table_q12.tex")
stargazer(DiD12, type="text")

# Why is our estimate not matching the slides?
# Answer: we use full-time, not all total emp. -> not including part-time peeps
# -> Does it match the other group?


## ----- 
# 13. How much does this suggest that the min wage affects full employment in fast food restaurants

## ----- 
# 14. 
# Explain why the t-test from the regression above may understate the uncertainty in the effect of min wage on full time employment?
# How could you correct the standard errors?
# Compare the t-values with and without this correction

# Not sure what he is getting at.


## ---- 
# 15. What regression would you run to estimate the DD model including control variables
# Run this regression using robust standard errors

# No need to cluster se?
# perhaps add clustering a la White?
q15 <- felm(data=data, empft ~ state + post + state:post + nregs + hrsopen + d2 + d3 + d4)
stargazer(q15, se=list(q15$rse), out="Pset5_ChaseTomJeanne/table_q15.tex")
stargazer(q15, type="text")

## ---- 
# 16 How might you test the key identifying assumptions underlying your DiD-estimation in this application? In general?

