# Empirical Analysis III
# Mogstad
# Problem Set 1
# 
# Chase Abram, Tom Hierons, Jeanne Sorin

# Get packages(s)
# install.packages("mvtnorm");
library(mvtnorm)

# Parameters
sigma = 2
rho = 0.5
N = 10000

# Generate data
mymean = c(0,0)
mysigma = matrix(c(sigma^2, rho*sigma, rho*sigma, 1), ncol = 2)
U <- rmvnorm(n=N, mean=mymean, sigma = mysigma)

# Spot check
plot(U, xlab="U_0", ylab="U_1", main="(U_0, U_1)")

# returns subset of X which is treated
treated <- function(X) {
  treats = matrix(, nrow = 0, ncol = 2)
  for(i in 1:length(X[,1]))
    if(X[i,2] > X[i,1]) {
      treats <- rbind(treats, X[i,])
    }
  return(treats)
}

# returns subset of X which is untreated
untreated <- function(X) {
  untreats = matrix(, nrow = 0, ncol = 2)
  for(i in 1:length(X[,1]))
    if(X[i,2] <= X[i,1]) {
      untreats <- rbind(untreats, X[i,])
    }
  return(untreats)
}

# Get treated and untreated groups
# Note that we have the counterfactuals in this experiment
tr <- treated(U)
ut <- untreated(U)

# Calculate treatment effects
ATE <- mean(U[,2] - U[,1])
ATT <- mean(tr[,2] - tr[,1])
ATUT <- mean(ut[,2] - ut[,1])
betaOLS <- 1/2*mean(tr[,2])

# E[Y|D=1] - E[Y|D=0]
dif = mean(tr[,2]) - mean(ut[,1])
print(sqrt((1-2*rho*sigma + sigma^2)*2/pi))
print(mean(tr[,1]))
print(mean(tr[,2]))
print(mean(ut[,1]))
print(mean(ut[,2]))




