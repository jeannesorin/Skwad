# Setup: Load packages and prepare data
packages <- c("tidyverse", "foreign", "AER", "stargazer")
lapply(packages, library, character.only = TRUE)
data = as_tibble(read.dta("lottery.dta")) %>%
  mutate(lotcateg = as.factor(lotcateg))

# (b) Instrument Relevance -----------------------------------------------------
# Run a first stage with (HC) robust standard errors 
first_stage <- lm(d ~ z, data)
rob_se <- list(sqrt(diag(vcovHC(first_stage, type = "HC1"))))
# Note: HC1 matches STATA output, some evidence it is worse than R's default
# in finite sample (R:1, STATA:0)

# Nicely formatted TeX table:
stargazer(first_stage,title = "First Stage Regression", se = rob_se, digits = 3, 
          header = F,  out = "tables/first_stage.tex")             

# (c) Simple IV Estimate -------------------------------------------------------
# Fit an IV model with robust standard errrors and output table
iv_model <- ivreg(lnw ~ d | z, data=data)
iv_rob_se <-list(sqrt(diag(vcovHC(iv_model, type = "HC1"))))
stargazer(iv_model,title = "Instrument Variables Estimates", se = rob_se, 
          digits = 3, header = F,  out = "tables/iv_model.tex")   

# (d) Complier Comparisons -----------------------------------------------------
# Include gender interactions in the first stage 
gender <- summary(lm(d ~ z*female, data))

# Proportion of women in the population
mean(data$female)

# See write up for details and argument

# (f) Y_0 & Y_1 Distributions for Compliers ------------------------------------

# Means 
# IV regressions to get the means (see write up for argument)
summary(ivreg(lnw*d ~ d|z, data = data))  
summary(ivreg(lnw*(1-d)~d|z, data = data))  

# Full distributions:



# (h) Category x Year Specific LATE's ------------------------------------------
