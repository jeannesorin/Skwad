# Empirical Analysis III
# Mogstad
# Problem Set 2
# 
# Chase Abram, Tom Hierons, Jeanne Sorin


library(haven)
library(MatchIt)
# install.packages("dplyr")
library(dplyr)

# Read in data, subset appropriately
data <- read_dta(file = "lalonde2.dta")
data$treated[is.na(data$treated)] <- 0
nsw <- subset(data, sample == 1)
cps <- subset(data, sample == 2)
psid <- subset(data, sample == 3)

# a) 
# subset nsw into treated and un
nswT <- subset(nsw, treated == 1)
nswUT <- subset(nsw, treated == 0)

# demographic and ex-ante vars should be independent of treatment
vars = c("age", "educ", "black",
         "hisp", "kidmiss", "kids18",
         "married", "nodegree", "re74")

# run t-tests
for(a in vars) {
  atest <- t.test(nswT[,a], nswUT[,a])
  cat(a, ": ", atest$p.value, "\n")
}

# see if any vars "predict" treatment
indep_lm <- glm(treated ~ age + educ + black + hisp
                + hisp + kidmiss + kids18
                + married + nodegree + metro + re74, data = nsw)



# b) If treatment is random, then E[Y(1) - Y(0)] = E[Y(1) | D = 1] - E[Y(0) | D = 0]
# estimate with above relationship
betaindep <- mean(nswT$re78) - mean(nswUT$re78)
# estimate with least-squares
betaols <- glm(re78 ~ treated, data = nsw)
# These two methods should give roughly the same estimate, if CIA holds

# c) 

# combine nsw and cps into one data frame
nswcps <- subset(data, (sample == 1 & treated == 1) | sample == 2)

# naive model only looking at treatment
my_simple_model <- glm(re78 ~ treated, data = nswcps)

# less naive model, includes other demographic factors
my_standard_model <- glm(re78 ~ treated + age + educ + black
                         + hisp + kidmiss + kids18
                         + married + nodegree + metro + re74, data = nswcps)


# d)

# plot histograms treated and untreated, to check support/matching
for(a in vars) {
  temp_nsw_a <- as.numeric(unlist(nsw[,a]))
  nsw_hist <- hist(temp_nsw_a)
  
  temp_cps_a <- as.numeric(unlist(cps[,a]))
  cps_hist <- hist(temp_cps_a,xlim = c(0,30))
  
  xmin <- min(temp_nsw_a,temp_cps_a)
  ymin <- min(nsw_hist$density, cps_hist$density)
  xmax <- max(temp_nsw_a,temp_cps_a)
  ymax <- max(nsw_hist$density, cps_hist$density)
  
  plot(nsw_hist, col=rgb(0,0,1,1/4),freq = FALSE, 
       main = c("nsw vs. cps for ",a),
       xlab = a,
       xlim = c(xmin,xmax), ylim = c(ymin,ymax))
  plot(cps_hist, col=rgb(1,0,0,1/4),freq=FALSE, add=T)
}

#e)

# drop the NA columns
drops <- c("dwincl", "early_ra")
nswcps_m <- nswcps[,!(names(nswcps) %in% drops)]

# drop (if any) rows with NA
nswcps_m <- na.omit(nswcps_m)

# matchit is a propensity matching method
# use probit model, discard stuff outside prop support
nn_match <- matchit(treated ~ age + educ + black
                    + hisp + kidmiss + kids18
                    + married + nodegree + metro + re74,
                    method = "nearest",
                    data = nswcps_m,
                    distance = "probit",
                    discard = "both")

# summary(nn_model)
# plot(nn_model)

# Nice little "jitter" plot that shows how well matching worked
plot(nn_match, type = 'jitter', interactive = FALSE)

# get matched dataset
nswcps_match <- match.data(nn_match)

# naive model only looking at treatment
nn_simple_model <- glm(re78 ~ treated, data = nswcps_match)

# less naive model, includes other demographic factors
nn_standard_model <- glm(re78 ~ treated + age + educ + black 
                         + hisp + kidmiss + kids18
                         + married + nodegree + metro + re74, 
                         data = nswcps_match)

# check for propensity score support and balancing
nn_prop_model <- glm(treated ~ age + educ + black 
                     + hisp + kidmiss + kids18
                     + married + nodegree + metro + re74, 
                     family = binomial(link = "probit"),
                     data = nswcps_match)

# add propensity scores to nsw + cps data rows
nn_prop_df <- cbind(prop_score = predict(nn_prop_model,
                                      type = "response"),
                 nswcps_match)

# subset back into treated/untreated (nsw/cps)
nn_prop_df_tr <- subset(nn_prop_df, treated == 1)
nn_prop_df_untr <- subset(nn_prop_df, treated == 0)

# Occular inspection of propensity score support
hist(nn_prop_df_tr$prop_score, freq = FALSE)
hist(nn_prop_df_untr$prop_score, freq = FALSE)

# ATT
ATT_match <- mean(nn_prop_df_tr$re78) - mean(nn_prop_df_untr$re78)

match_lm <- glm(re78 ~ treated, data = nn_prop_df)


# f)

# add a column where we will put the counterfactual "untreated"
nn_prop_df_tr$re78cf <-rep(0,len = length(nn_prop_df_tr[,1]))

# bandwidth
h <- 0.1

# For each treated, find the local linear fitted guess of untreated outcome
for(i in 1:length(nn_prop_df_tr[,1])) {
  # construct weights
  difs <- nn_prop_df_tr$prop_score[i] - nn_prop_df_untr$prop_score
  p_weights <- dnorm(difs/h)
  
  # generate local linear model
  i_lm <- glm(nn_prop_df_untr$re78 ~ difs, weights = p_weights)
  
  # Get estimated ourcome of re78
  nn_prop_df_tr$re78cf[i] <- i_lm$coefficients["(Intercept)"]
}

# Get estimated ATT
ATT_ll <- mean(nn_prop_df_tr$re78 - nn_prop_df_tr$re78cf)

