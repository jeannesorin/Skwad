# Empirical Analysis III
# Mogstad
# Problem Set 2
# 
# Chase Abram, Tom Hierons, Jeanne Sorin


library(haven)

# Read in data, subset appropriately
data <- read_dta(file = "lalonde2.dta")
data$treated[is.na(data$treated)] <- 0
nsw <- subset(data, sample == 1)
cps <- subset(data, sample == 2)
psid <- subset(data, sample == 3)

# a) If treatment is random, then E[Y(1) - Y(0)] = E[Y(1) | D = 1] - E[Y(0) | D = 0]
nswT <- subset(nsw, treated == 1)
nswUT <- subset(nsw, treated == 0)

# demographic vars should be independent of treatment
ols <- function(X, Y) {
  return(solve(t(X)%*%X)%*%t(X)%*%Y)
}

olsindeptest <- function(X,Y) {
  beta = solve(t(X)%*%X)%*%t(X)%*%Y
  return(beta[2])
}

aug <- function(X) {
  return(matrix(c(rep(1,len=length(X)), X), ncol = 2))
}

re74test <- olsindeptest(aug(nsw$re74), nsw$treated)
agetest <- olsindeptest(aug(nsw$age), nsw$treated)
eductest <- olsindeptest(aug(nsw$educ), nsw$treated)
blacktest <- olsindeptest(aug(nsw$black), nsw$treated)
hisptest <- olsindeptest(aug(nsw$hisp), nsw$treated)
marriedtest <- olsindeptest(aug(nsw$married), nsw$treated)
nodegreetest <- olsindeptest(aug(nsw$nodegree), nsw$treated)
w76test <- olsindeptest(aug(nsw$w76), nsw$treated)


# b) If treatment is random, then E[Y(1) - Y(0)] = E[Y(1) | D = 1] - E[Y(0) | D = 0]
nswT <- subset(nsw, treated == 1)
nswUT <- subset(nsw, treated == 0)
betaindep <- mean(nswT$re78) - mean(nswUT$re78)
betaols <- ols(aug(nsw$treated), nsw$re78)

# c) 

nswcps <- subset(data, (sample == 1 & treated == 1) | sample == 2)
betanswcps <- ols(aug(nswcps$treated), nswcps$re78)


