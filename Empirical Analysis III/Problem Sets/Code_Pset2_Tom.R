# Setup: Load packages and prepare data
packages <- c("tidyverse", "foreign", "MatchIt")
lapply(packages, library, character.only = TRUE)
set.seed(19042020)
data = as_tibble(read.dta("lalonde2.dta"))
nsw = filter(data,sample == "NSW")%>%
  select(-c("idnum", "metro", "sample", "dwincl", "w76"))
nonexp = filter(data, (sample == "NSW" & treated == 1)| sample == "CPS") %>%
  select(-c("idnum", "sample","metro", "dwincl", "w76", "early_ra"))%>%
  replace_na(list(treated=0))%>%
  mutate(treated = as.factor(treated))

# (a) Randomisation Check ------------------------------------------------------
vars = names(select(nsw,-c("re78","treated")))
lapply(vars,function(x) t.test(nsw[[x]]~nsw$treated)) # run t-test on variables
  
# (b) Experimental Estimate ----------------------------------------------------
summary(lm(re78 ~ treated, nsw)) # ols without covariates
summary(lm(re78 ~ ., nsw)) # with covariates

# (c) Non-Experimental OLS -----------------------------------------------------
summary(lm(re78 ~ treated, nonexp)) # ols without covariates
summary(lm(re78 ~ ., nonexp)) # with covariates

# (d) Balancing and Support for CPS --------------------------------------------
# Test balance
vars = names(select(nonexp,-c("re78","treated")))
lapply(vars,function(x) t.test(nsw[[x]]~nsw$treated)) # t-test on variables

# Plot the support 
supp_test <- function(x){
  ggplot(nonexp, aes_string(x, 
                            y = "..density..", fill = "treated"))+
    geom_histogram() +
    ggsave(paste0("plots/hist_",x,".png"))
}
lapply(vars, supp_test)


# (e) Nearest-Neighbour PSM matching -------------------------------------------
matched <- matchit(treated ~ age + educ + black + married + nodegree +
                     re74 + re75 + hisp + kids18 + kidmiss, data = nonexp,
                   method = "nearest", distance = "probit")
matched <- match.data(matched)
summary(lm(re78 ~ treated, data=matched))
summary(lm(re78 ~ ., data=matched))

# (f) PSM and Local Linear Regression ------------------------------------------
# Fit probit on data
probit_fit = glm(treated ~ age + educ + black + married + nodegree +
                   re74 + re75 + hisp + kids18 + kidmiss,
                 family = binomial(link = "probit"), data = nonexp)

# Add the propensity score
nonexp <- mutate(nonexp,
                 pscore = predict(probit_fit,newdata =nonexp))

# Examine support
supp_test("propscore")

# Local linear regression
loc_pol = loess(re78 ~ pscore, data=filter(nonexp,treated==0)) 
nonexp <- mutate(nonexp,
       f_re78 = predict(loc_pol, newdata = nonexp))
# ATT
mean(filter(nonexp,treated==1&!is.na(f_re78))$re78)-
  mean(filter(nonexp,treated==1&!is.na(f_re78))$f_re78)

