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
packages <- c("tidyverse", "data.table", "foreign", "stargazer", "rdd", "ggplot2", "lfe", "rdrobust")
lapply(packages, library, character.only = TRUE)

## ---------------------------

## load up our functions into memory

# source("~/Documents/PhD/Skwad/Random_Resources/JeanneR_formula.R") 


## ---------------------------




## ---------------------------

data <- read.dta("Empirical Analysis III/Problem Sets/data_pset4/final5.dta")

# 1. Estimate the effect of class size on math scores using OLS without controls
# then adding percentage of disadvantaged students
# adding enrollment

data$schlcode = factor(data$schlcode)
q1_a <- felm(avgmath ~ classize | 0 | 0 | schlcode, data=data)
q1_b <- felm(avgmath ~ classize + tipuach|0|0| schlcode, data=data)
q1_c <- felm(avgmath ~ classize + tipuach + c_size|0|0| schlcode, data=data)
q1_a_cse <- list(sqrt(diag(q1_a$clustervcv)))
q1_b_cse <- list(sqrt(diag(q1_b$clustervcv)))
q1_c_cse <- list(sqrt(diag(q1_c$clustervcv)))

stargazer(q1_a, q1_b, q1_c, se = c(q1_a_cse, q1_b_cse, q1_c_cse), type="text", notes = c("Standard Errors clustered at the schlcode level."))
stargazer(q1_a, q1_b, q1_c, se = c(q1_a_cse, q1_b_cse, q1_c_cse), #type="text", 
          notes = c("Standard Errors clustered at the schlcode level."), 
          out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q1.tex")



#Start by limiting the sample to schools with enrollment between 20 and 60 students. 
#Generate a (predicted) large class dummy based on the first discontinuity at 40 students.
sub <- data %>%
  filter(c_size > 20 & c_size < 60) %>%
  mutate(large = ifelse(c_size > 40, 1, 0))

#2. Use OLS to estimate the effect of being in a large class on math scores assuming 
#that you have a sharp RDD around this discontinuity. 
#Control for the percentage of disadvantaged students in the class and a linear trend 
#in enrollment.
#To control for the linear trend in enrollment, we compute the number of enrolled students 
#above 40 and interact it with the large class dummy.

# Y = a + b Z + c (c_size-40) + d (c_size-40)*Z + e X + u
#sub$c_sizeadj = sub$c_size-40
q2_a <- felm(avgmath ~ large*c_sizeadj + tipuach |0|0| schlcode, data=sub)
q2_b <- felm(avgmath ~ large + classize + tipuach |0|0| schlcode, data=sub)

q2_a_cse <- list(sqrt(diag(q2_a$clustervcv)))
q2_b_cse <- list(sqrt(diag(q2_b$clustervcv)))

#stargazer(q2_a, q2_b, se = c(q2_a_cse, q2_b_cse), type="text", notes = c("Standard Errors clustered at the schlcode level."))
stargazer(q2_b, se = c(q2_b_cse), type="text", notes = c("Standard Errors clustered at the schlcode level."))
stargazer(q2_b, se = c(q2_b_cse), #type="text", 
          notes = c("Standard Errors clustered at the schlcode level."), 
          out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q2.tex")


# Select only those at the discontinuity (for the full sample)
# disc <- data %>%
#   filter(c_size %in% c(36:45, 76:85, 116:125)) %>%
#   mutate(large = ifelse(c_size %in% c(41:45, 81:85, 121:125), 1, 0))
# 
# q2_d <- felm(avgmath ~ large |0|0| schlcode, data=disc)
# q2_e <- felm(avgmath ~ large + tipuach |0|0| schlcode, data=disc)
# q2_f <- felm(avgmath ~ large + tipuach + c_size |0|0| schlcode, data=disc)
# 


#3. Use Local Linear Regression to get a point estimate of the effect of being in a large class 
#on math scores assuming you have a sharp RDD. 
#Finally, use a nonparametric bootstrap to estimate the standard error on your RDD point 
#estimate. Compare these results to the estimates you obtained with OLS.

# Local linear regression
loc_pol1 = loess(avgmath ~ large*c_size, data=sub) 
sub <- sub %>%
  mutate(local1 = predict(loc_pol1, newdata = sub))
local_effect1 = mean(sub$local1[sub$c_size<=43 & sub$c_size >= 41]) - mean(sub$local1[sub$c_size>=37 & sub$c_size<=40])
local_effect1

loc_sharp1 <- RDestimate(avgmath ~ c_size , data=sub, cutpoint = 40, bw = NULL,
                         kernel = "triangular", se.type = "HC1", cluster = NULL,
                         verbose = FALSE, model = FALSE, frame = FALSE)
print(loc_sharp1)
plot(loc_sharp1)
local_effect1


# Bootstrap to estimate standard errors
# Bootstrap
S = 10000
N = nrow(sub)
beta_hats1 = rep(0, S)
beta_hats2 = rep(0, S)

for (s in 1:S){
  print(s)
  new_num <- sample(1:N, N, replace=TRUE)
  new <- sub[new_num,]
  
  local = RDestimate(avgmath ~ c_size , data=new, cutpoint = 40, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = FALSE, frame = FALSE)
  beta_hats1[s] <- as.numeric(local$est[1])
  
  loc_polsub = loess(avgmath ~ large*c_size, data=new) 
  new <- new %>%
    mutate(local1 = predict(loc_polsub, newdata = new))
  beta_hats2[s]  = mean(new$local1[new$c_size<=43 & new$c_size >= 41]) - mean(new$local1[new$c_size>=37 & new$c_size<=40])
}
mean(beta_hats1)
sqrt(mean(beta_hats1^2) - mean(beta_hats1)^2)

mean(beta_hats2)
sqrt(mean(beta_hats2^2) - mean(beta_hats2)^2)



#4. Estimate the effect of class size on math scores using fuzzy RDD. 
#Control for the percentage of disadvantaged students in the class and a linear 
#trend in enrollment.
fuzzy1 <- RDestimate(avgmath ~ c_size + classize | tipuach, data=sub, cutpoint = 40, bw = NULL,
           kernel = "triangular", se.type = "HC1", cluster = NULL,
           verbose = FALSE, model = FALSE, frame = FALSE)

fuzzy2 <- RDestimate(avgmath ~ c_size + classize | tipuach + classize, data=sub, cutpoint = 40, bw = NULL,
           kernel = "triangular", se.type = "HC1", cluster = NULL,
           verbose = FALSE, model = FALSE, frame = FALSE)

# fuzzy3 <- RDestimate(avgmath ~ c_size | tipuach + classize, data=sub, cutpoint = 40, bw = NULL,
#                      kernel = "triangular", se.type = "HC1", cluster = NULL,
#                      verbose = FALSE, model = FALSE, frame = FALSE)

print(fuzzy1)
print(fuzzy2)
#print(fuzzy3)
plot(fuzzy1)
plot(fuzzy2)



#5. (*) Use -rdrobust- to estimate the effect of class size on math scores and compare your results.
# Now use the complete sample, and define the following variable 
#predicted class size = enrollment /(int((enrollment − 1)/40) + 1)

# Not working
rdrobust(sub$avgmath, sub$c_size, c=40, fuzzy=TRUE, cluster=sub$schlcode)


data <- data %>%
  mutate(pred = c_size/(trunc((c_size-1)/40)+1)) 


spline.d <- as.data.frame(spline(data$c_size, data$classize))
ggplot() + geom_line(data = spline.d, aes(x = x, y = y)) + xla



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
stargazer(iv_model, se = iv_rob_se, type="text")
stargazer(iv_model,title = "Instrument Variables Estimates", se = iv_rob_se, 
          digits = 3, header = F,  out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q7.tex")  



#8. If the RDD is valid, then the coefficient of interest should not change significantly if we
#include or exclude covariates. Check whether this is the case.
iv_model_covariates <- ivreg(avgmath ~ classize + tipuach + c_size| pred + tipuach + c_size, data=data)
iv_cov_rob_se <-list(sqrt(diag(vcovHC(iv_model_covariates, type = "HC1"))))
stargazer(iv_model_covariates, se = iv_cov_rob_se, type="text")

#stage1 <- lm(classize ~ pred + tipuach + c_size, data=data)  
#stage2 <- lm(avgmath ~ pred + tipuach + c_size, data=data)
#beta = stage2$coefficients[2] / stage1$coefficients[2]
#beta

#9. Manipulation: Plot the distribution of the assignment variable.
pdf("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q9_plot.pdf")
ggplot(data=data, aes(x=c_size)) + theme_bw() +
  geom_histogram(aes(y=..density..), alpha=.2, fill="#FF6666", 
                 breaks=seq(from = 5, to = 220, by = 5))+ 
  geom_density()+
  geom_vline(xintercept = c(40, 80, 120, 160), lty=3, color="blue") 
dev.off()

#10. Misspecification 1: Present a graph using binned local averages of class size and math score 
# against enrollment. Use bins of width 20 and make sure that the bins do not cover the discontinuities. 
#Can you see the discontinuity in class size and math scores?

data$group_test = unlist(lapply(data$c_size, function(x) ceiling(max(x)/20)*20))

data <- data %>%
  group_by(group_test) %>%
  mutate(mean_math = mean(avgmath, na.rm=T),
         mean_classize = mean(classize, na.rm=T)) %>%
  ungroup()
pdf("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q10_plot.pdf")
coeff <- 2
ggplot(data=data[data$mean_classize>20,], aes(x=c_size)) + theme_bw() +
  geom_line(aes(y=mean_classize), col="purple")+ 
  geom_line(aes(y=mean_math / coeff), col="red") +
  geom_vline(xintercept = c(40, 80, 120, 160), lty=3, color="blue") +
  scale_y_continuous(
    name = "Average Classize",
    sec.axis = sec_axis(~.*coeff, name="Mean Math Score")) +
  theme(axis.title.y = element_text(color = "purple", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) 
dev.off()


data$c_size2 = data$c_size*data$c_size

linear_math = lm(data=data, avgmath ~ c_size)
quad_math = lm(data=data, avgmath ~ c_size2 + c_size)
linear_size = lm(data=data, classize ~ c_size)
quad_size = lm(data=data, classize ~ c_size2 + c_size)

pred1 <- data.frame(avgmath = linear_math$model$avgmath,
                    c_size = linear_math$model$c_size,
                    fit_l_math = linear_math$fitted.values,
                    fit_q_math = quad_math$fitted.values)
pred2 <- data.frame(c_size = linear_size$model$c_size,
                    classize = linear_size$model$classize,
                    fit_l_size = linear_size$fitted.values,
                    fit_q_size = quad_size$fitted.values)


pdf("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q11_plot.pdf")
coeff <- 2.5
ggplot() + theme_bw() +
  geom_line(data=data[data$mean_classize>20,], aes(x=c_size, y=mean_classize), col="purple")+ 
  geom_line(data=pred2, aes(x=c_size, y=fit_q_size), col="purple", lty=2)+
  geom_line(data=pred2, aes(x=c_size, y=fit_l_size), col="purple", lty=2)+
  geom_line(data=data[data$mean_classize>20,], aes(x=c_size, y=mean_math / coeff), col="red") +
  geom_line(data=pred1, aes(x=c_size, y=fit_q_math / coeff), col="red", lty=2)+
  geom_line(data=pred1, aes(x=c_size, y=fit_l_math / coeff), col="red", lty=2)+
  geom_vline(xintercept = c(40, 80, 120, 160), lty=3, color="blue") +
  scale_y_continuous(
    name = "Average Classize",
    sec.axis = sec_axis(~.*coeff, name="Mean Math Score")) +
  theme(axis.title.y = element_text(color = "purple", size=13),
        axis.title.y.right = element_text(color = "red", size=13)) 
dev.off()


#12. Misspecification 3: Explore the sensitivity of the results in 7) to 1) bandwidths 
#(restrict the estimation sample to intervals around the discontinuities), 
#and 2) how you control for enrollment.

disc0 <- data %>%
  filter(c_size %in% c(30:50, 70:90, 110:130)) 
disc1 <- data %>%
    filter(c_size %in% c(36:45, 76:85, 116:125)) 
disc2 <- data %>%
  filter(c_size %in% c(37:44, 75:84, 117:124)) 
disc3 <- data %>%
  filter(c_size %in% c(38:43, 74:83, 118:123)) 

iv_model_120 <- ivreg(avgmath ~ classize | pred, data=disc0)
iv_rob_se120 <-list(sqrt(diag(vcovHC(iv_model_120, type = "HC1"))))
iv_model_121 <- ivreg(avgmath ~ classize | pred, data=disc1)
iv_rob_se121 <-list(sqrt(diag(vcovHC(iv_model_121, type = "HC1"))))
iv_model_122 <- ivreg(avgmath ~ classize | pred, data=disc2)
iv_rob_se122 <-list(sqrt(diag(vcovHC(iv_model_122, type = "HC1"))))
iv_model_123 <- ivreg(avgmath ~ classize | pred, data=disc3)
iv_rob_se123 <-list(sqrt(diag(vcovHC(iv_model_123, type = "HC1"))))

stargazer(iv_model_120, iv_model_121, iv_model_122, iv_model_123, 
          column.labels=c("+-10","+-5", "+-4", "+-3"),
          se = c(iv_rob_se120, iv_rob_se121, iv_rob_se122, iv_rob_se123), type="text")

stargazer(iv_model_120, iv_model_121, iv_model_122, iv_model_123, 
          se = c(iv_rob_se120, iv_rob_se121, iv_rob_se122, iv_rob_se123),
          title = "Instrument Variables Estimates - Multiple Bandwidth", 
          column.labels=c("+-10","+-5", "+-4", "+-3"),
          digits = 3, header = F,  
          out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q12a.tex")  

# Control for enrollment
iv_model <- ivreg(avgmath ~ classize + c_size | pred + c_size, data=data)
iv_rob_se <-list(sqrt(diag(vcovHC(iv_model, type = "HC1"))))
stargazer(iv_model, se = iv_rob_se, type="text")
stargazer(iv_model,title = "Instrument Variables Estimates", se = iv_rob_se, 
          digits = 3, header = F,  out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q12b.tex")  




#13. Placebo check: Conduct the RD analysis where your outcome is percentage disadvantaged pupils.

# Simple OLS
lm(data=sub, tipuach ~ large)

# IV
ivreg(tipuach ~ classize | pred, data=data)

# Local linear estimate : Sharp RDD
loc_sharp1 <- RDestimate(tipuach ~ c_size , data=sub, cutpoint = 40, bw = NULL,
                         kernel = "triangular", se.type = "HC1", cluster = NULL,
                         verbose = FALSE, model = FALSE, frame = FALSE)
print(loc_sharp1)
plot(loc_sharp1)

# Local linear estimate : Fuzzy RDD
fuzzy1 <- RDestimate(tipuach ~  c_size + classize, data=sub, cutpoint = 40, bw = NULL,
                     kernel = "triangular", se.type = "HC1", cluster = NULL,
                     verbose = FALSE, model = FALSE, frame = FALSE)

print(fuzzy1)
plot(fuzzy1)



