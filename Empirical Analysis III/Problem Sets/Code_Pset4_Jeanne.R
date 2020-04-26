## ---------------------------
##
## Script name: Econ 312 : Problem Set 4
##
## Purpose of script: Self explanatory
##
## Author: Jeanne Sorin
##
## Date Created: 2020-04-25
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC

setwd("~/Documents/PhD/Skwad/")    

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
packages <- c("tidyverse", "data.table", "foreign", "stargazer", "rdd", "ggplot2", "lfe")
lapply(packages, library, character.only = TRUE)

## ---------------------------

## load up our functions into memory

# source("~/Documents/PhD/Skwad/Random_Resources/JeanneR_formula.R") 

## ---------------------------

data <- read.dta("Empirical Analysis III/Problem Sets/data_pset4/final5.dta")

# 1. Estimate the effect of class size on math scores using OLS without controls
# then adding percentage of disadvantaged students
# adding enrollment

data$schlcode = factor(data$schlcode)
q1_a <- felm(avgmath ~ classize |0|0| schlcode, data=data)
q1_b <- felm(avgmath ~ classize + tipuach|0|0| schlcode, data=data)
q1_c <- felm(avgmath ~ classize + tipuach + c_size|0|0| schlcode, data=data)

stargazer(q1_a, q1_b, q1_c, no.space=TRUE, out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q1.tex")
stargazer(q1_a, q1_b, q1_c, no.space=TRUE, type="text")


q1_a2 <- lm(avgmath ~ classize, data=data)
q1_b2 <- lm(avgmath ~ classize + tipuach, data=data)

q1_a2_rob_se <-list(sqrt(diag(vcovHC(q1_a2, type = "HC1"))))
q1_b2_rob_se <-list(sqrt(diag(vcovHC(q1_b2, type = "HC1"))))
stargazer(q1_a2, q1_b2, title = "Instrument Variables Estimates", se = c(q1_a2_rob_se, q1_b2_rob_se), 
          digits = 3, header = F, type="text")# out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q7.tex")  


#Start by limiting the sample to schools with enrollment between 20 and 60 students. 
#Generate a (predicted) large class dummy based on the first discontinuity at 40 students.
sub <- data %>%
  filter(c_size > 20 & c_size < 60) %>%
  mutate(large = ifelse(c_size > 40, 1, 0))

#2. Use OLS to estimate the effect of being in a large class on math scores assuming 
#that you have a sharp RDD around this discontinuity. 
#Control for the percentage of disadvantaged students in the class and a linear trend 
#in enrollment.

q2_a <- felm(avgmath ~ large |0|0| schlcode, data=sub)
q2_b <- felm(avgmath ~ large + tipuach |0|0| schlcode, data=sub)
q2_c <- felm(avgmath ~ large + tipuach + c_size |0|0| schlcode, data=sub)

stargazer(q2_a, q2_b, q2_c, no.space=TRUE, out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q2.tex")
stargazer(q2_a, q2_b, q2_c, no.space=TRUE, type="text")


# Select only those at the discontinuity (for the full sample)
# disc <- data %>%
#   filter(c_size %in% c(36:45, 76:85, 116:125)) %>%
#   mutate(large = ifelse(c_size %in% c(41:45, 81:85, 121:125), 1, 0))
# 
# q2_d <- felm(avgmath ~ large |0|0| schlcode, data=disc)
# q2_e <- felm(avgmath ~ large + tipuach |0|0| schlcode, data=disc)
# q2_f <- felm(avgmath ~ large + tipuach + c_size |0|0| schlcode, data=disc)
# 
# stargazer(q2_d, q2_e, q2_f, no.space=TRUE, type="text")


#3. Use Local Linear Regression to get a point estimate of the effect of being in a large class 
#on math scores assuming you have a sharp RDD. 
#Finally, use a nonparametric bootstrap to estimate the standard error on your RDD point 
#estimate. Compare these results to the estimates you obtained with OLS.



# Local linear regression
loc_pol = loess(avgmath ~ large, data=sub) 
sub <- sub %>%
  mutate(local = predict(loc_pol, newdata = sub))
local_effect = mean(sub$local[sub$large==1]) - mean(sub$local[sub$large==0])
local_effect


# Bootstrap to estimate standard errors



#4. Estimate the effect of class size on math scores using fuzzy RDD. 
#Control for the percentage of disadvantaged students in the class and a linear 
#trend in enrollment.
fuzzy1 <- RDestimate(avgmath ~ c_size + classize , data=sub, cutpoint = 40, bw = NULL,
           kernel = "triangular", se.type = "HC1", cluster = NULL,
           verbose = FALSE, model = FALSE, frame = FALSE)

fuzzy2 <- RDestimate(avgmath ~ c_size + classize| tipuach + c_size, data=sub, cutpoint = 40, bw = NULL,
           kernel = "triangular", se.type = "HC1", cluster = NULL,
           verbose = FALSE, model = FALSE, frame = FALSE)

fuzzy1
fuzzy2


reg <- lm(data = data, avgmath ~ tipuach + c_size)

dta <- data.frame(res = reg$residuals,
                  enrollment = reg$model$c_size) %>%
  

ggplot() + geom_line(data=dta, aes(enrollment, res)) + geom_line(data=data, aes(c_size, pred))


#5. (*) Use -rdrobust- to estimate the effect of class size on math scores and compare your results.
# Now use the complete sample, and define the following variable 
#predicted class size = enrollment /(int((enrollment − 1)/40) + 1)

# Not working
rdrobust(sub$avgmath, sub$c_size, c=40, fuzzy=TRUE, cluster=sub$schlcode)


data <- data %>%
  mutate(pred = c_size/(trunc((c_size-1)/40)+1)) 


spline.d <- as.data.frame(spline(data$c_size, data$classize))
data = cbind(data, spline.d)
ggplot() + geom_line(data = spline.d, aes(x = x, y = y))

#6. Plot average class size as a function of enrollment. Add predicted class size to the plot.
pdf("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q6_plot.pdf")
ggplot() + theme_classic() +
  geom_point(data = data, aes(c_size, classize), pch=20, color="grey") +
  geom_hline(yintercept=c(20, 27, 30, 40), lty=3, color="magenta") +
  geom_vline(xintercept = c(40, 80, 120, 160), lty=3, color="blue") +
  geom_line(data = data, aes(c_size, pred), color="red") + xlab("Enrollment Count") + ylab("Class Size") +
  geom_line(data = spline.d, aes(x = x, y = y)) 
dev.off()

#7. Estimate the effect of class size on math scores using IV.
iv_model <- ivreg(avgmath ~ classize | pred, data=data)
iv_rob_se <-list(sqrt(diag(vcovHC(iv_model, type = "HC1"))))
stargazer(iv_model,title = "Instrument Variables Estimates", se = rob_se, 
          digits = 3, header = F,  out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q7.tex")  

#stage1 <- lm(classize ~ pred, data=data)  
#stage2 <- lm(avgmath ~ pred, data=data)

#beta = stage2$coefficients[2] / stage1$coefficients[2]
#beta

#8. If the RDD is valid, then the coefficient of interest should not change significantly if we
#include or exclude covariates. Check whether this is the case.
stage1 <- lm(classize ~ pred + tipuach + c_size, data=data)  
stage2 <- lm(avgmath ~ pred + tipuach + c_size, data=data)

beta = stage2$coefficients[2] / stage1$coefficients[2]
beta

