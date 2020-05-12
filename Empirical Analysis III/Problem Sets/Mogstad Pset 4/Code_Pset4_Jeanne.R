## ---------------------------
##
## Script name: Econ 312 : Problem Set 4
##
## Purpose of script: Self explanatory
##
## Author: Chase Abram, Tom Hierons, Jeanne Sorin
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

rm(list=ls())
setwd("~/Documents/PhD/Skwad/")    

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
packages <- c("tidyverse", "data.table", "foreign", "stargazer", "rdd", "ggplot2", "lfe", "rdrobust", "ivpack")
lapply(packages, library, character.only = TRUE)


tolatexRDestimate <- function(output, title="title", path = "Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q3.tex"){
  d <- data.frame(Bandwidth = output$bw,
                  Observations = output$obs,
                  Estimate = output$est,
                  StdError = output$se,
                  pvalue = output$p)
  stargazer(d, summary=FALSE, out=path, #type="text",
            notes="Output from RDestimate, cutpoint = 40, triangular kernel, se.type=HC1. Standard errors clustered at schlcode level")
}

tolatexrdrobust <- function(output, title="title", path = "Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q3.tex"){
  d <- data.frame(Coefficient = output$coef,
                  StdError = output$se,
                  z = output$z,
                  PValue = output$pv)
  stargazer(d, summary=FALSE, out=path, 
            #type="text",
            title=title,
            notes="Output from rdrobust, cutpoint = 40, triangular kernel, outcome var = avgmath.")
}


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

stargazer(q1_a, q1_b, q1_c, se = c(list(q1_a$cse), list(q1_b$cse), list(q1_c$cse)), type="text", notes = c("Standard Errors clustered at the schlcode level."))
stargazer(q1_a, q1_b, q1_c, se = c(list(q1_a$cse), list(q1_b$cse), list(q1_c$cse)),
          notes = c("Standard Errors clustered at the schlcode level."), 
          out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q1.tex")



#Start by limiting the sample to schools with enrollment between 20 and 60 students. 
#Generate a (predicted) large class dummy based on the first discontinuity at 40 students.
sub <- data %>%
  filter(c_size > 20 & c_size < 60) %>%
  mutate(large = ifelse(c_size <= 40, 1, 0))

#2. Use OLS to estimate the effect of being in a large class on math scores assuming 
#that you have a sharp RDD around this discontinuity. 
#Control for the percentage of disadvantaged students in the class and a linear trend 
#in enrollment.

q2_b <- felm(avgmath ~ large + c_size + tipuach |0|0| schlcode, data=sub)

stargazer(q2_b, se = c(list(q2_b$cse)), 
          out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q2.tex",
          notes=c("Standard errors clustered at the School code level"))

#3. Use Local Linear Regression to get a point estimate of the effect of being in a large class 
#on math scores assuming you have a sharp RDD. 
#Finally, use a nonparametric bootstrap to estimate the standard error on your RDD point 
#estimate. Compare these results to the estimates you obtained with OLS.

loc_sharp1 <- RDestimate(avgmath ~ c_size , data=sub, cutpoint = 40, bw = NULL,
                         kernel = "triangular", se.type = "HC1", cluster = sub$schlcode,
                         verbose = FALSE, model = FALSE, frame = FALSE)

summary(loc_sharp1)
plot(loc_sharp1)

tolatexRDestimate(loc_sharp1, title="Sharp RD",
        path = "Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q3.tex")


# Bootstrap to estimate standard errors
S = 10000
N = nrow(sub)
beta_hats1 = rep(0, S)
# Keep track of the BW of the original estimation
h = loc_sharp1$bw[1]
#beta_hats2 = rep(0, S)

for (s in 1:S){
  new_num <- sample(1:N, N, replace=TRUE)
  new <- sub[new_num,]
  local = RDestimate(avgmath ~ c_size , data=new, cutpoint = 40, bw = h,
                     kernel = "triangular", se.type = "HC1", cluster = new$schlcode,
                     verbose = FALSE, model = FALSE, frame = FALSE)
  beta_hats1[s] <- as.numeric(local$est[1])
}

mean(beta_hats1)
sqrt(mean(beta_hats1^2) - mean(beta_hats1)^2)
pdf("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q3_plot.pdf")
ggplot() + theme_bw() + 
  geom_histogram(aes(x=beta_hats1), bins = 100, lty=0.2) +
  geom_vline(xintercept =  mean(beta_hats1), col="blue") +     #mean of the estimate from Bootstrap
  geom_vline(xintercept =  loc_sharp1$est["LATE"], col="red") +  #actual estimate
  xlab("Betas from Bootstrapping") + 
  scale_colour_manual(name = "",
                      values = c("blue","grey","red"),
                      labels=c("Polynomial Fit","Data", "Maimonides' Rule"))
dev.off()


#4. Estimate the effect of class size on math scores using fuzzy RDD. 
#Control for the percentage of disadvantaged students in the class and a linear 
#trend in enrollment.
fuzzy_rd <- felm(avgmath ~ c_size + tipuach|0| (classize ~ large) |schlcode,
                 data = sub)
stargazer(fuzzy_rd, title = "Fuzzy RD", digits = 3, header = F, se=list(fuzzy_rd$cse),
          notes = c("Standard Errors clustered at the schlcode level."),  
          out = "Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q4.tex")

# Check with ivreg
# iv_rd <- ivreg(avgmath ~classize + c_size + tipuach| 
#                  tipuach + c_size +large, data = sub)
# summary(iv_rd)
# 

#5. (*) Use -rdrobust- to estimate the effect of class size on math scores and compare your results.
# Now use the complete sample, and define the following variable 
# predicted class size = enrollment /(int((enrollment − 1)/40) + 1)
sub$large_sign = (-1)*sub$large
robust_fuzzy <- rdrobust(y = sub$avgmath, x = sub$c_size, c = 40, 
                         fuzzy = sub$large_sign)
robust_sharp <- rdrobust(y = sub$avgmath, x = sub$c_size, c=40)
summary(robust_sharp)
summary(robust_fuzzy)


tolatexrdrobust(robust_sharp, title="Sharp RD",
                path = "Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q5_sharp.tex")
tolatexrdrobust(robust_fuzzy, title="Fuzzy RD",
                path = "Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q5_fuzzy.tex")




data <- data %>%
  mutate(pred = c_size/(trunc((c_size-1)/40)+1)) 


#6. Plot average class size as a function of enrollment. Add predicted class size to the plot.
pdf("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q6_plot.pdf")
ggplot(data) +
  coord_fixed()+
  geom_point(aes(c_size,classize, colour = "grey"), size = 0.8)+
  geom_smooth(method = "loess", span = 0.05, 
              formula = y~x, mapping = aes(c_size,classize,colour="blue"), size = 0.5)+
  geom_line(aes(c_size,pred,colour = "red"))+
  labs(title = "Predicted against actual class size", 
       x = "Enrollment", y = "Class Size")+ 
  scale_colour_manual(name = "",
                      values = c("blue","grey","red"),
                      labels=c("Polynomial Fit","Data", "Maimonides' Rule"))
dev.off()



#7. Estimate the effect of class size on math scores using IV.
# Check Chase's code to cluster ivreg
data2 = data[!(is.na(data$schlcode) | is.na(data$avgmath)
                     | is.na(data$classize) | is.na(data$pred)),]
data2$schlcode = as.factor(data2$schlcode)
iv_model <- ivreg(avgmath ~ classize | pred, data=data2)
iv_rob_se <- list(cluster.robust.se(iv_model, data2$schlcode))

stargazer(iv_model, se = iv_rob_se, type="text")
stargazer(iv_model,title = "Instrument Variables Estimates", se = iv_rob_se, 
          digits = 3, header = F,  out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q7.tex")  



#8. If the RDD is valid, then the coefficient of interest should not change significantly if we
#include or exclude covariates. Check whether this is the case.
iv_model_covariates <- ivreg(avgmath ~ classize + tipuach + c_size| pred + tipuach + c_size, data=data)
iv_cov_rob_se <-list(sqrt(diag(vcovHC(iv_model_covariates, type = "HC1"))))
stargazer(iv_model_covariates, se = iv_cov_rob_se, type="text")
stargazer(iv_model_covariates,title = "Instrument Variables Estimates", #se = iv_rob_se, 
          notes=c("Standard Errors clustered at the schlcode level"),
          digits = 3, header = F,  out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q8.tex")  

iv_con <- felm(avgmath ~c_size + tipuach|0|(classize~pred)|schlcode,
               data = data)
summary(iv_con)


# 8. IV +/- Controls -----------------------------------------------------------
iv_con <- felm(avgmath ~c_size + tipuach|0|(classize~pred)|schlcode,
               data = data)
summary(iv_con)

# Double check using ivreg
iv_cov <- ivreg(avgmath ~ classize + tipuach + c_size|
                  pred + tipuach + c_size, data=data)
summary(iv_cov)
iv_nocov <-ivreg(avgmath ~ classize| pred, data=data)
summary(iv_nocov)

# Highlights the function form --> need to control for covariates
# stage1 <- lm(classize ~ pred + tipuach + c_size, data=data)
# stage2 <- lm(avgmath ~ pred + tipuach + c_size, data=data)
# beta = stage2$coefficients[2] / stage1$coefficients[2]
# beta


#9. Manipulation: Plot the distribution of the assignment variable.
pdf("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q9_plot.pdf")
ggplot(data=data, aes(x=c_size)) + theme_bw() +
  geom_histogram(aes(y=..density..), alpha=.2, fill="#FF6666", 
                 breaks=seq(from = 5, to = 220, by = 1))+ 
  geom_density()+
  geom_vline(xintercept = c(40, 80, 120, 160), lty=3, color="blue") 
dev.off()



#10. Misspecification 1: Present a graph using binned local averages of class size and math score 
# against enrollment. Use bins of width 20 and make sure that the bins do not cover the discontinuities. 
#Can you see the discontinuity in class size and math scores?

# 10.& 11. Miscpecification 1 & 2 ----------------------------------------------
# Aggregate up
data <- data %>% 
  mutate(enroll_cat = cut(c_size, breaks = seq(from = 0, to = 250, by = 20)))

binned <- data %>% group_by(enroll_cat)%>%
  summarise_at(c("avgmath", "classize"), mean, na.rm=TRUE)
binned$midpoint = seq(from = 10, to = 230, by = 20)

# Plot binned data for maths
ggplot(data = binned, aes(x = midpoint,y=avgmath))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x, se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL) +
  xlab("Enrollment")+
  ggsave("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q10_mathlinearfit.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=avgmath))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  xlab("Enrollment")+
  ggsave("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q10_mathquadratic.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=avgmath))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2)+I(x^3) +I(x^4), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  xlab("Enrollment")+
  ggsave("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q10_mathpolyfit.png", width = 7, height = 7)

# And for class size
ggplot(data = binned, aes(x = midpoint,y=classize))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x, se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  xlab("Enrollment")+
  ggsave("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q10_classlinearfit.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=classize))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  xlab("Enrollment")+
  ggsave("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q10_classquadratic.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=classize))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2)+I(x^3) +I(x^4), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  xlab("Enrollment")+
  ggsave("Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q10_classpolyfit.png", width = 7, height = 7)



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
  filter(c_size %in% c(31:50, 71:90, 111:130)) 
disc1 <- data %>%
    filter(c_size %in% c(36:45, 76:85, 116:125)) 
disc2 <- data %>%
  filter(c_size %in% c(37:44, 77:84, 117:124)) 
disc3 <- data %>%
  filter(c_size %in% c(38:43, 78:83, 118:123))


# 1) Bandwidth 
for (b in seq(from=2,to=20,by=2)){
  trimmed <- filter(data, 
                    (abs(c_size-40)<b)|(abs(c_size-80)<b)|
                      (abs(c_size-120)<b)|(abs(c_size-180)<b)|
                      (abs(c_size-220)<b))
  trim_iv <- felm(avgmath ~c_size + tipuach|0|(classize~pred)|schlcode,
                  data = trimmed)
  print(trim_iv[[7]][4])
}


# 2) Controlling for enrollment with a quadratic term and a polynomial
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
          notes=c("Standard Errors clustered at the scholcode level"),
          column.labels=c("+-10","+-5", "+-4", "+-3"),
          digits = 3, header = F,  
          out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q12a.tex")  

# Control for enrollment
iv_quad <- felm(avgmath ~c_size + I(c_size^2) + tipuach|0|(classize~pred)|schlcode,
                data = data)
summary(iv_quad)

iv_poly <- felm(avgmath ~c_size + I(c_size^2)+I(c_size^3)+I(c_size^4) + tipuach|0|(classize~pred)|schlcode,
                data = data)
summary(iv_poly)

stargazer(iv_quad, iv_poly, type="text")
stargazer(iv_quad, iv_poly,title = "Instrument Variables Estimates - Different Enrollemnt Specifications", se = iv_rob_se, 
          notes=c("Standard Errors clustered at the scholcode level"),
          digits = 3, header = F,  out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q12b.tex")  




#13. Placebo check: Conduct the RD analysis where your outcome is percentage disadvantaged pupils.
iv_placebo <- ivreg(tipuach ~ classize | pred, data=data)
iv_rob_se <-list(sqrt(diag(vcovHC(iv_placebo, type = "HC1"))))
stargazer(iv_placebo, se = iv_rob_se, type="text")
stargazer(iv_placebo,title = "Instrument Variables Estimates", se = iv_rob_se, 
          notes=c("Standard Errors clustered at the scholcode level"),
          digits = 3, header = F,  out="Empirical Analysis III/Problem Sets/Pset4_Tables_Jeanne/q13.tex")  
