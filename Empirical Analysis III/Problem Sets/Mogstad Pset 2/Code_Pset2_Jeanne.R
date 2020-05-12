## ---------------------------
##
## Script name: Empirical Analysis III - Pset 2
##
## Purpose of script: self-explanatory
##
## Author: Jeanne Sorin
##
## Date Created: 2020-04-13
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC

setwd("~/Documents/PhD/Skwad")    

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)
require(foreign)
require(cobalt)
require(ggplot2)
require(gridExtra)
require(MatchIt)


## ---------------------------

## load up our functions into memory

# source("~/Documents/PhD/Skwad/Random_Resources/JeanneR_formula.R") 

## ---------------------------





#################################################################################################
                      # Problem 2 #
#################################################################################################

### Import dataset

data <- read.dta("Empirical Analysis III/Problem Sets/lalonde2.dta")
data <- as.data.table(data)



###### NSW
nsw <- data[sample=="NSW"]

### a) Investigate whether the data is consistent with randomization of the treatment

# Check balance
covs <- nsw[, -c("treated", "sample", "re78", "idnum")]
bal.tab(covs, treat = nsw$treated, stats = c("m") )
# Usefull Code from https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt_A0_basic_use.html
# covs <- subset(lalonde, select = -c(treat, re78, nodegree, married))
# # Generating ATT weights as specified in Austin (2011)
# lalonde$p.score <- glm(f.build("treat", covs), data = lalonde,
#                        family = "binomial")$fitted.values
# lalonde$att.weights <- with(lalonde, treat + (1-treat)*p.score/(1-p.score))
# bal.tab(covs, treat = lalonde$treat, weights = lalonde$att.weights)


# Alternatively (simpler approach): ttest
ttest_nsw = lapply(nsw[,c("educ", "age", "black", "married", "nodegree", "re74", "re75", "hisp", "kids18", "kidmiss")], function(x) t.test(x ~ nsw$treated, var.equal = TRUE))
data.frame(p.value = sapply(ttest_nsw, getElement, name = "p.value"))
data.frame(conf.int = sapply(ttest_nsw, getElement, name = "conf.int")) 



### b) Estimate the effect of the treatment using the experimental sample
lm(re78 ~ ., data=nsw[,-c("idnum", "metro", "early_ra", "sample", "dwincl", "w76")])
lm(re78 ~ treated, data=nsw)



##### Now use the sample consisting in the treated from the NSW sample and the
##### comparison individuals from the CPS sample.

### c) Estimate the effect using OLS (naive)
nsw_cps <- data[(sample=="NSW" & treated == 1 ) | sample == "CPS"]
nsw_cps[is.na(nsw_cps$treated)]$treated <- 0
lm(re78 ~ ., data=nsw_cps[,-c("idnum", "metro", "early_ra", "sample", "dwincl", "w76")])
lm(re78 ~ treated, data=nsw_cps)

# The naive approach --> negative impact of the treatment, which is totally opposite to what we found before...
# Let's do a little more work here...



### d) Investigate covariate balancing and support between the treated and the CPS sample
# Covariate balancing
ttest_nsw_cps = lapply(nsw_cps[,c("educ", "age", "black", "married", "nodegree", "re74", "re75", "hisp", "kids18", "kidmiss")], function(x) t.test(x ~ nsw_cps$treated, var.equal = TRUE))
data.frame(p.value = sapply(ttest_nsw_cps, getElement, name = "p.value"))
data.frame(conf.int = sapply(ttest_nsw_cps, getElement, name = "conf.int")) 
# OLS
lm(treated ~ ., data=(nsw_cps[,c("treated","educ", "age", "black", "married", "nodegree", "re74", "re75", "hisp", "kids18", "kidmiss")]))
# Probit
glm(treated ~ ., data=(nsw_cps[,c("treated","educ", "age", "black", "married", "nodegree", "re74", "re75", "hisp", "kids18", "kidmiss")]),
    family = binomial(link = "logit"))



# Investigate support visually for non-binary variables
nsw_cps$treated = as.factor(nsw_cps$treated)
p1 <- ggplot(nsw_cps, aes(x=age, color=treated)) +
  geom_density()
p2 <- ggplot(nsw_cps, aes(x=educ, color=treated)) +
  geom_density()
p3 <- ggplot(nsw_cps, aes(x=re74, color=treated)) +
  geom_density()
p4 <- ggplot(nsw_cps, aes(x=re75, color=treated)) +
  geom_density()
p5 <- ggplot(nsw_cps, aes(x=kids18, color=treated)) +
    geom_density()

# Could get fancy by trimming (especially for kids18, re74, re75)
p3b <- ggplot(nsw_cps[re74>0,], aes(x=re74, color=treated)) +
  geom_density() + xlab("re74 > 0")
p4b <- ggplot(nsw_cps[re75 > 0,], aes(x=re75, color=treated)) +
  geom_density()+ xlab("re75 > 0")
p5b <- ggplot(nsw_cps[kids18>0,], aes(x=kids18, color=treated)) +
  geom_density() + xlab("kids18 > 0")

grid.arrange(p1+ theme(legend.position="none"),
             p2+ theme(legend.position="none"),
             p3+ theme(legend.position="none"), 
             p3b+ theme(legend.position="none"),
             p4+ theme(legend.position="none"), 
             p4b+ theme(legend.position="none"),
             p5+ theme(legend.position="none"),
             p5b, ncol=2)

# Anyways, it's not exactly a perfect match in terms of the covariates...




### e) Estimate the effect using 1 nearest neighbor propensity score matching

# Great matching resource https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html  
nsw_cps_nomiss <- nsw_cps[, c("treated", "age", "educ", "black", "married", "nodegree", "re74", "re75", "re78", "hisp", "kids18", 
                              "kidmiss")] %>% na.omit()  
match <- matchit(treated ~ age + educ + black + married + nodegree + re74 + re75 + hisp + kids18 + kidmiss,
                 method="nearest", data=nsw_cps_nomiss,
                 replace=TRUE)
plot(match, type = 'jitter', interactive = FALSE)

matched.df <- match.data(match)

# Alternatively (still unsure why pr_score different than distance (not only in magnitude, also in order)...)
model<- glm(data=matched.df,
            treated ~ age + educ + black + married + nodegree + re74 + re75 + hisp + kids18 + kidmiss,
            family="binomial"(link="logit"))
matched.df$pr_score <- predict(model, matched.df, type="response")


# Check balance again, with the matched data
matched_ttest = lapply(matched.df[,c("educ", "age", "black", "married", "nodegree", "re74", "re75", "hisp", "kids18", "kidmiss")], function(x) t.test(x ~ matched.df$treated, var.equal = TRUE))
data.frame(p.value = sapply(matched_ttest, getElement, name = "p.value"))
# Compare the ttest on the matched data...
data.frame(conf.int = sapply(matched_ttest, getElement, name = "conf.int")) 
# ... to the ttest on the unmatched data.
data.frame(conf.int = sapply(ttest_nsw_cps, getElement, name = "conf.int")) 
# The former does much better.



### Now estimate the effect
matched.df$treated = as.factor(matched.df$treated)
with(matched.df, t.test(re78 ~ treated))
lm(re78 ~ treated, data = matched.df)
lm(re78 ~ ., data = matched.df %>% select(-c("distance", "weights")))
# Still the wrong sign... :'( 




### f) Estimate the effect using the propensity score and local linear regression.
# distance = propensity score

# Simple regression
lm(re78 ~ distance, data = matched.df)
lm(re78 ~ pr_score, data = matched.df)

# Local Linear Regression
matched.df <- matched.df[order(matched.df$distance, matched.df$re78),]

lor10 <- loess(re78 ~ distance, data=matched.df, span=0.1) # 10% smoothing span
lor20 <- loess(re78 ~ distance, data=matched.df, span=0.2) # 10% smoothing span
lor30 <- loess(re78 ~ distance, data=matched.df, span=0.3) # 10% smoothing span
lor40 <- loess(re78 ~ distance, data=matched.df, span=0.4) # 10% smoothing span
lor50 <- loess(re78 ~ distance, data=matched.df, span=0.5) # 10% smoothing span

smoothed10 <- predict(lor10) 
smoothed20 <- predict(lor20) 
smoothed30 <- predict(lor30) 
smoothed40 <- predict(lor40) 
smoothed50 <- predict(lor50) 

plot(matched.df[matched.df$re78<40000,]$re78, x=matched.df[matched.df$re78<40000,]$distance, type="l", main="Loess Smoothing and Prediction", xlab="Pscore", ylab="re78")
lines(smoothed10, x=matched.df$distance, col="red")
lines(smoothed20, x=matched.df$distance, col="green")
lines(smoothed50, x=matched.df$distance, col="blue")


# Local Linear Regression with pr_score
matched.df <- matched.df[order(matched.df$pr_score, matched.df$re78),]

lor10 <- loess(re78 ~ pr_score, data=matched.df, span=0.1) # 10% smoothing span
lor20 <- loess(re78 ~ pr_score, data=matched.df, span=0.2) # 10% smoothing span
lor30 <- loess(re78 ~ pr_score, data=matched.df, span=0.3) # 10% smoothing span
lor40 <- loess(re78 ~ pr_score, data=matched.df, span=0.4) # 10% smoothing span
lor50 <- loess(re78 ~ pr_score, data=matched.df, span=0.5) # 10% smoothing span

smoothed10 <- predict(lor10) 
smoothed20 <- predict(lor20) 
smoothed30 <- predict(lor30) 
smoothed40 <- predict(lor40) 
smoothed50 <- predict(lor50) 

plot(matched.df[matched.df$re78<40000,]$re78, x=matched.df[matched.df$re78<40000,]$pr_score, type="l", main="Loess Smoothing and Prediction", xlab="Pscore", ylab="re78")
lines(smoothed10, x=matched.df$pr_score, col="red")
lines(smoothed20, x=matched.df$pr_score, col="green")
lines(smoothed50, x=matched.df$pr_score, col="blue")





